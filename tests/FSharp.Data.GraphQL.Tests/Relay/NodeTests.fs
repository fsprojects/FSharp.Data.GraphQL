// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.Relay.NodeTests

#nowarn "40"

open System
open System.Collections.Immutable
open System.Text.Json
open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Server.Relay
open FSharp.Data.GraphQL.Shared

type Person = { Id : string; Name : string; Age : int }
type Car = { Id : string; Model : string }

let people = [
    { Id = "1"; Name = "Alice"; Age = 18 }
    { Id = "2"; Name = "Bob"; Age = 23 }
    { Id = "3"; Name = "Susan"; Age = 37 }
]

let cars = [ { Id = "1"; Model = "Tesla S" }; { Id = "2"; Model = "Shelby GT500" } ]

let rec Person =
    Define.Object<Person> (
        name = "Person",
        interfaces = [ Node ],
        fields = [
            Define.Field ("id", IDType, resolve = (fun _ person -> toGlobalId "person" person.Id))
            Define.Field ("name", Nullable StringType, (fun _ person -> Some person.Name))
            Define.Field ("age", IntType, (fun _ person -> person.Age))
        ]
    )

and Car =
    Define.Object<Car> (
        name = "Car",
        interfaces = [ Node ],
        fields = [
            Define.Field ("id", IDType, (fun _ car -> toGlobalId "car" car.Id))
            Define.Field ("model", Nullable StringType, (fun _ car -> Some car.Model))
        ]
    )

and resolve _ _ id =
    match id with
    | GlobalId ("person", id) -> people |> List.tryFind (fun person -> person.Id = id) |> Option.map box
    | GlobalId ("car", id) -> cars |> List.tryFind (fun car -> car.Id = id) |> Option.map box
    | _ -> None

and Node = Define.Node (fun () -> [ Person; Car ])

let schema =
    Schema<unit> (Define.Object ("Query", [ Define.NodeField (Node, resolve) ]), config = { SchemaConfig.Default with Types = [ Person; Car ] })


let execAndValidateNode (query : string) expectedDirect expectedDeferred =
    let result = sync <| Executor(schema).AsyncExecute (query, getMockInputContext)
    match expectedDeferred with
    | Some expectedDeferred ->
        ensureDeferred result
        <| fun data errors deferred ->
            let expectedItemCount = Seq.length expectedDeferred
            empty errors
            data |> equals (upcast NameValueLookup.ofList [ "node", upcast expectedDirect ])
            use sub = Observer.create deferred
            sub.WaitCompleted (expectedItemCount)
            (sub.Received |> withoutCompleted)
            |> Seq.cast<GQLDeferredResponseContent>
            |> Seq.iter (fun ad -> expectedDeferred |> contains ad |> ignore)
    | None ->
        ensureDirect result <| fun data errors ->
            empty errors
            data |> equals (upcast NameValueLookup.ofList [ "node", upcast expectedDirect ])

[<Fact>]
let ``Node with global ID gets correct record - Defer`` () =
    let query1 =
        """query ExampleQuery {
        node(id: "cGVyc29uOjE=") {
            ...on Person {
                name @defer,
                age
            }
        }
    }"""
    let expectedDirect1 = NameValueLookup.ofList [ "name", null; "age", upcast 18 ]
    let expectedDeferred1 = Some [ DeferredResult ("Alice", [ "node"; "name" ]) ]
    execAndValidateNode query1 expectedDirect1 expectedDeferred1
    let query2 =
        """query ExampleQuery {
        node(id: "Y2FyOjE=") {
            ...on Car {
                model @defer
            }
        }
    }"""
    let expectedDirect2 = NameValueLookup.ofList [ "model", null ]
    let expectedDeferred2 = Some [ DeferredResult ("Tesla S", [ "node"; "model" ]) ]
    execAndValidateNode query2 expectedDirect2 expectedDeferred2

[<Fact>]
let ``Node with global ID gets correct record`` () =
    let query1 =
        """query ExampleQuery {
        node(id: "cGVyc29uOjE=") {
            ...on Person {
                name,
                age
            }
        }
    }"""
    let expected1 = NameValueLookup.ofList [ "name", upcast "Alice"; "age", upcast 18 ]
    execAndValidateNode query1 expected1 None
    let query2 =
        """query ExampleQuery {
        node(id: "Y2FyOjE=") {
            ...on Car {
                model
            }
        }
    }"""
    let expected2 = NameValueLookup.ofList [ "model", upcast "Tesla S" ]
    execAndValidateNode query2 expected2 None

[<Fact>]
let ``Node with global ID gets correct type`` () =
    execAndValidateNode
        """{ node(id: "cGVyc29uOjI=") { id, __typename } }"""
        (NameValueLookup.ofList [ "id", upcast "cGVyc29uOjI="; "__typename", upcast "Person" ])
        None
    execAndValidateNode
        """{ node(id: "Y2FyOjI=") { id, __typename } }"""
        (NameValueLookup.ofList [ "id", upcast "Y2FyOjI="; "__typename", upcast "Car" ])
        None

// A global ID comes from the client, so whatever it sends must produce "no value", never an exception
[<Theory>]
[<InlineData("", "an empty string")>]
[<InlineData(" ", "only a space")>]
[<InlineData("\t\r\n", "only whitespace characters")>]
[<InlineData("%%%%", "characters outside the base64 alphabet")>]
[<InlineData("cGVyc29uOjE", "a length that is not a multiple of four")>]
[<InlineData("cGVyc29uOjE==", "excess padding")>]
[<InlineData("====", "nothing but padding")>]
[<InlineData("=cGVyc29uOjE", "padding in front")>]
[<InlineData("Привіт==", "non-ASCII characters")>]
[<InlineData("cGVyc29uOjE＝", "a full-width padding character")>]
[<InlineData("cGVyc29u", "no separator between the type name and the local ID")>]
[<InlineData("/zox", "bytes that are not UTF-8")>]
let ``fromGlobalId returns no value for a malformed global ID`` (id : string, reason : string) =
    match fromGlobalId id with
    | ValueNone -> ()
    | ValueSome (typeName, localId) ->
        fail $"Expected no value for a global ID with {reason} ('{id}'), but it was read as type '{typeName}' and local ID '{localId}'"

[<Theory>]
[<InlineData("person", "1")>]
[<InlineData("Персона", "ідентифікатор")>]
[<InlineData("car", "🚗")>]
[<InlineData("person", "local:ID:with:separators")>]
[<InlineData("person", " 1 ")>]
[<InlineData("person", "")>]
let ``fromGlobalId reads back the type name and local ID toGlobalId wrote`` (typeName : string, localId : string) =
    let globalId = toGlobalId typeName localId
    match fromGlobalId globalId with
    | ValueSome (actualTypeName, actualLocalId) ->
        Assert.True (
            String.Equals (typeName, actualTypeName, StringComparison.Ordinal)
            && String.Equals (localId, actualLocalId, StringComparison.Ordinal),
            $"Expected the global ID '{globalId}' to be read as type '{typeName}' and local ID '{localId}', but it was read as type '{actualTypeName}' and local ID '{actualLocalId}'"
        )
    | ValueNone -> fail $"Expected the global ID '{globalId}' to be read as type '{typeName}' and local ID '{localId}', but it was read as no value"

[<Theory>]
[<InlineData("", "an empty string")>]
[<InlineData(" ", "only a space")>]
[<InlineData("%%%%", "characters outside the base64 alphabet")>]
[<InlineData("cGVyc29uOjE", "a length that is not a multiple of four")>]
[<InlineData("Привіт==", "non-ASCII characters")>]
[<InlineData("cGVyc29u", "no separator between the type name and the local ID")>]
[<InlineData("/zox", "bytes that are not UTF-8")>]
// person:4, well-formed but naming no person
[<InlineData("cGVyc29uOjQ=", "a local ID that matches no object")>]
let ``Node field returns null without errors for a malformed or unknown global ID`` (id : string, reason : string) =
    // The ID goes through a variable, so that any string reaches the resolver unchanged by GraphQL string escaping
    let variables = ImmutableDictionary<string, JsonElement>.Empty.Add ("id", JsonSerializer.SerializeToElement id)
    let result =
        Executor(schema).AsyncExecute ("query ($id: ID!) { node(id: $id) { id } }", getMockInputContext, variables = variables)
        |> sync
    ensureDirect result <| fun data errors ->
        Assert.True (List.isEmpty errors, $"Expected no errors for a global ID with {reason} ('{id}'), but got %A{errors}")
        data |> equals (upcast NameValueLookup.ofList [ "node", null ])
