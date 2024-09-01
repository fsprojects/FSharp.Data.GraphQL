// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc
module FSharp.Data.GraphQL.Tests.PropertyTrackerTests

open System.Collections.Immutable

#nowarn "40"

open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Linq
open FSharp.Data.GraphQL.Server.Relay

type Person = { Id : int; FirstName : string; LastName : string; Friends : Person list }
type Droid = { Id : int; Number : string; Function : string }

let track name tFrom tTo = { ReturnType = tTo; ParentType = tFrom; Name = Option.ofObj name }

let john = { Id = 1; FirstName = "John"; LastName = "Doe"; Friends = [] }
let ben = { Id = 2; FirstName = "Ben"; LastName = "Adams"; Friends = [ john ] }
let at22 = { Id = 3; Number = "AT22"; Function = "combat wombat" }

let rec Person =
    Define.Object<Person> (
        name = "Person",
        interfaces = [ Node ],
        fieldsFn =
            fun () -> [
                Define.GlobalIdField (fun _ p -> string p.Id)
                Define.Field ("firstName", StringType, (fun _ p -> p.FirstName))
                Define.Field ("lastName", StringType, (fun _ p -> p.LastName))
                Define.Field ("fullName", StringType, (fun _ p -> p.FirstName + " " + p.LastName))
                Define.Field (
                    "friends",
                    ConnectionOf Person,
                    "",
                    Connection.forwardArgs,
                    fun ctx p ->
                        let data =
                            match ctx with
                            | SliceInfo (Forward (first, after)) ->
                                match after with
                                | ValueSome (GlobalId ("Person", id)) ->
                                    let friendId = int id
                                    p.Friends
                                    |> List.sortBy (fun f -> f.Id)
                                    |> List.filter (fun f -> f.Id > friendId)
                                    |> List.take first
                                    |> List.map (fun f -> { Cursor = toGlobalId "Person" (string f.Id); Node = f })
                                | _ ->
                                    p.Friends
                                    |> List.sortBy (fun f -> f.Id)
                                    |> List.take first
                                    |> List.map (fun f -> { Cursor = toGlobalId "Person" (string f.Id); Node = f })
                            | _ ->
                                p.Friends
                                |> List.sortBy (fun f -> f.Id)
                                |> List.map (fun f -> { Cursor = toGlobalId "Person" (string f.Id); Node = f })
                        {
                            TotalCount = async { return p.Friends |> List.length |> Some }
                            PageInfo = {
                                HasNextPage = async { return true }
                                HasPreviousPage = async { return true }
                                StartCursor = async { return data |> List.tryHead |> Option.map (fun edge -> edge.Cursor) }
                                EndCursor = async { return data |> List.tryLast |> Option.map (fun edge -> edge.Cursor) }
                            }
                            Edges = async { return data }
                        }
                )
            ]
    )
and Droid =
    Define.Object<Droid> (
        name = "Droid",
        interfaces = [ Node ],
        fieldsFn =
            fun () -> [
                Define.GlobalIdField (fun _ d -> string d.Id)
                Define.Field ("number", StringType, (fun _ d -> d.Number))
                Define.Field ("function", StringType, (fun _ d -> d.Function))
            ]
    )
and Node = Define.Node<obj> (fun () -> [ Person; Droid ])
and Query =
    Define.Object<obj list> (
        "Query",
        [
            Define.Field ("all", ListOf Node, (fun _ data -> data))
            Define.Field (
                "people",
                ListOf Person,
                fun _ data ->
                    data
                    |> List.choose (fun (o : obj) ->
                        match o with
                        | :? Person as p -> Some p
                        | _ -> None)
            )
            Define.Field (
                "droid",
                ListOf Droid,
                fun _ data ->
                    data
                    |> List.choose (fun (o : obj) ->
                        match o with
                        | :? Droid as d -> Some d
                        | _ -> None)
            )
        ]
    )

let schema = Schema (Query)
let schemaProcessor = Executor (schema)

[<Fact>]
let ``Property tracker can track indirect properties`` () =
    let plan =
        schemaProcessor.CreateExecutionPlanOrFail (
            """
    {
        people {
            fullName
        }
    }
    """
        )
    let info = plan.["people"]
    let expected =
        Compose (
            track null typeof<obj list> typeof<Person list>,
            [],
            Set.ofList [
                Tracker.Direct (track "FirstName" typeof<Person> typeof<string>, [])
                Tracker.Direct (track "LastName" typeof<Person> typeof<string>, [])
            ]
        )
    let actual = tracker ImmutableDictionary.Empty info
    actual |> equals expected

[<Fact>]
let ``Property tracker can correctly jump over properties not being part of the tracked chain`` () =
    let plan =
        schemaProcessor.CreateExecutionPlanOrFail (
            """
    {
        people {
            friends {
                edges {
                    node {
                        ... on Person {
                            firstName
                        }
                    }
                }
            }
        }
    }
    """
        )
    let info = plan.["people"]
    let expected =
        Compose (
            { Name = None; ParentType = typeof<obj list>; ReturnType = typeof<Person list> },
            [],
            set [
                Direct (
                    {
                        Name = Some "Friends"
                        ParentType = typeof<Person>
                        ReturnType = typeof<Person list>
                    },
                    []
                )
                Direct ({ Name = Some "Id"; ParentType = typeof<Person>; ReturnType = typeof<int> }, [])
            ]
        )

    let actual = tracker ImmutableDictionary.Empty info
    actual |> equals expected
