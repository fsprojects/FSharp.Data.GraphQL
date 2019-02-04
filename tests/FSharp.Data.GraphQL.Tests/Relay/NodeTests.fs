/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.Relay.NodeTests

#nowarn "40"

open System
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Relay
open FSharp.Data.GraphQL.Execution

type Person = { Id: string; Name: string; Age: int } 
type Car = { Id: string; Model: string; } 

let people = [
    { Id = "1"; Name = "Alice"; Age = 18 }
    { Id = "2"; Name = "Bob"; Age = 23 }
    { Id = "3"; Name = "Susan"; Age = 37 }]
    
let cars = [
    { Id = "1"; Model = "Tesla S" }
    { Id = "2"; Model = "Shelby GT500" }]

let rec Person =
  Define.Object<Person>(
    name = "Person",
    interfaces = [ Node ],
    fields = [
        Define.Field("id", ID, resolve = fun _ person -> toGlobalId "person" person.Id)
        Define.Field("name", String, fun _ person -> person.Name)
        Define.Field("age", Int, fun _ person -> person.Age) ])

and Car =
  Define.Object<Car>(
    name = "Car",
    interfaces = [ Node ],
    fields = [
        Define.Field("id", ID, fun _ car -> toGlobalId "car" car.Id)
        Define.Field("model", String, fun _ car -> car.Model) ])

and resolve _ _ id =
    match id with
    | GlobalId("person", id) -> people |> List.tryFind (fun person -> person.Id = id) |> Option.map box
    | GlobalId("car", id) -> cars |> List.tryFind (fun car -> car.Id = id) |> Option.map box
    | _ -> None

and Node = Define.Node (fun () -> [ Person; Car ])

let schema = Schema<unit>(Define.Object("Query", [ Define.NodeField(Node, resolve) ]), config = { SchemaConfig.Default with Types = [ Person; Car ] })

open Xunit
open System.Threading
open System.Collections.Concurrent

let execAndValidateNode (query: string) expectedDirect expectedDeferred =
    let result = sync <| Executor(schema).AsyncExecute(query)
    match expectedDeferred with
    | Some expectedDeferred ->
        match result with
        | Deferred(data, errors, deferred) ->
            let expectedItemCount = Seq.length expectedDeferred
            empty errors
            data.["data"] |> equals (upcast NameValueLookup.ofList ["node", upcast expectedDirect])
            use sub = Observer.create deferred
            sub.WaitCompleted(expectedItemCount)
            sub.Received
            |> Seq.cast<NameValueLookup>
            |> Seq.iter (fun ad -> expectedDeferred |> contains ad |> ignore)
        | _ ->  fail "Expected a deferred GQLResponse"
    | None ->
        match result with
        | Direct(data, errors) -> 
            empty errors
            data.["data"] |> equals (upcast NameValueLookup.ofList ["node", upcast expectedDirect])
        | _ ->  fail "Expected a direct GQLResponse"

[<Fact>]
let ``Node with global ID gets correct record - Defer`` () =
    let query1 = """query ExampleQuery {
        node(id: "cGVyc29uOjE=") {
            name @defer,
            age
        }
    }"""
    let expectedDirect1 =
      NameValueLookup.ofList [
        "name", null
        "age", upcast 18]
    let expectedDeferred1 = Some [
        NameValueLookup.ofList [
            "data", upcast "Alice"
            "path", upcast ["node"; "name"]]]
    execAndValidateNode query1 expectedDirect1 expectedDeferred1
    let query2 = """query ExampleQuery {
        node(id: "Y2FyOjE=") {
            model @defer
        }
    }"""
    let expectedDirect2 =    
      NameValueLookup.ofList [
        "model", null ]
    let expectedDeferred2 = Some [
        NameValueLookup.ofList [
            "data", upcast "Tesla S"
            "path", upcast ["node"; "model"]]]
    execAndValidateNode query2 expectedDirect2 expectedDeferred2

[<Fact>]
let ``Node with global ID gets correct record`` () =
    let query1 = """query ExampleQuery {
        node(id: "cGVyc29uOjE=") {
            name,
            age
        }
    }"""
    let expected1 =
      NameValueLookup.ofList [
        "name", upcast "Alice"
        "age", upcast 18]
    execAndValidateNode query1 expected1 None
    let query2 = """query ExampleQuery {
        node(id: "Y2FyOjE=") {
            model
        }
    }"""
    let expected2 =    
      NameValueLookup.ofList [
        "model", upcast "Tesla S" ]
    execAndValidateNode query2 expected2 None

[<Fact>]
let ``Node with global ID gets correct type`` () =
    execAndValidateNode """{ node(id: "cGVyc29uOjI=") { id, __typename } }""" (NameValueLookup.ofList ["id", upcast "cGVyc29uOjI="; "__typename", upcast "Person" ]) None
    execAndValidateNode """{ node(id: "Y2FyOjI=") { id, __typename } }""" (NameValueLookup.ofList ["id", upcast "Y2FyOjI="; "__typename", upcast "Car" ]) None