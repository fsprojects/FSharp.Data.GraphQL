/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
module FSharp.Data.GraphQL.Tests.PropertyTrackerTests

open Xunit
open FsCheck
open System
open System.Linq
open System.Collections.Generic
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Linq
open FSharp.Data.GraphQL.Relay
open FSharp.Data.GraphQL.Execution

type Person = { Id: int; FirstName: string; LastName: string; Friends: Person list }

let track name tFrom tTo = { ReturnType = tTo; ParentType = tFrom; Name = Option.ofObj name }

let john = { Id = 1; FirstName = "John"; LastName = "Doe"; Friends = [] }
let ben = { Id = 2; FirstName = "Ben"; LastName = "Adams"; Friends = [ john ] }

let rec Person = Define.Object(
    name = "Person", 
    interfaces = [ Node ],
    fieldsFn = fun () -> [
        Define.GlobalIdField(fun _ p -> string p.Id)
        Define.Field("firstName", String, fun _ p -> p.FirstName)
        Define.Field("lastName", String, fun _ p -> p.LastName)
        Define.Field("fullName", String, fun _ p -> p.FirstName + " " + p.LastName)
        Define.Field("friends", ConnectionOf Person, "", Connection.forwardArgs, fun ctx p -> 
            let data = 
                match ctx with
                | SliceInfo(Forward(first, after)) ->
                    match after with
                    | Some(GlobalId("Person", id)) -> 
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
            { TotalCount = p.Friends |> List.length |> Some
              PageInfo = 
                { HasNextPage = true
                  HasPreviousPage = true
                  StartCursor = data |> List.tryHead |> Option.map (fun edge -> edge.Cursor)
                  EndCursor = data |> List.tryLast |> Option.map (fun edge -> edge.Cursor) }
              Edges = data }
        ) ])
and Node = Define.Node<obj>(fun () -> [ Person ])
and Query = Define.Object("Query", [
    Define.Field("people", ListOf Person, fun ctx data -> data)
])

let schema = Schema<Person list>(Query)

[<Fact>]
let ``Property tracker can track indirect properties`` () =
    let plan = schema.CreateExecutionPlan("""
    {
        people {
            fullName
        }
    }
    """)
    let info = plan.["people"]
    let expected = 
        Compose(track null typeof<Person list> typeof<Person list>, [], Set.ofList [
            Direct(track "FirstName" typeof<Person> typeof<string>, [])
            Direct(track "LastName" typeof<Person> typeof<string>, []) ])
    let actual = tracker Map.empty info
    actual |> equals expected

[<Fact>]
let ``Property tracker can correctly jump over properties not being part of the tracked chain`` () =
    let plan = schema.CreateExecutionPlan("""
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
    """)
    let info = plan.["people"]
    let expected = 
        Compose(track null typeof<Person list> typeof<Person list>, [], Set.ofList [
            Direct(track "Id" typeof<Person> typeof<int>, [])
            Compose(track "Friends" typeof<Person> typeof<Person list>, [], Set.ofList [
                Direct(track "FirstName" typeof<Person> typeof<string>, []) ]) ])
    let actual = tracker Map.empty info
    actual |> equals expected