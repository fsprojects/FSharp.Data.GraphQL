module FSharp.Data.GraphQL.IntegrationTests.SwapiRemoteProviderTests

open Xunit
open Helpers
open FSharp.Data.GraphQL
open System.Threading.Tasks

type Provider = GraphQLProvider<"http://localhost:8086">

type Episode = Provider.Types.Episode

module SimpleOperation =
    let operation =
        Provider.Operation<"""query Q {
hero (id: "1000") {
  name
  appearsIn
  homePlanet
  friends {
    totalCount
    pageInfo {
      hasNextPage
      hasPreviousPage
    }
    edges {
      cursor
      node {
        ... on Human {
          name
          homePlanet
        }
        ... on Droid {
          name
          primaryFunction
        }
      }
    }
  }
}
          }""">()

    type Operation = Provider.Operations.Q

    let validateResult (result : Operation.OperationResult) =
        result.CustomData.ContainsKey("documentId") |> equals true
        result.Errors |> equals [||]
        result.Data.IsSome |> equals true
        result.Data.Value.Hero.IsSome |> equals true
        result.Data.Value.Hero.Value.AppearsIn |> equals [| Episode.NewHope; Episode.Empire; Episode.Jedi |]
        let expectedFriends : Operation.Types.HeroFields.FriendsFields.EdgesFields.NodeFields.Character array =
          [| Operation.Types.HeroFields.FriendsFields.EdgesFields.NodeFields.Human(name = "Han Solo")
             Operation.Types.HeroFields.FriendsFields.EdgesFields.NodeFields.Human(name = "Leia Organa", homePlanet = "Alderaan")
             Operation.Types.HeroFields.FriendsFields.EdgesFields.NodeFields.Droid(name = "C-3PO", primaryFunction = "Protocol")
             Operation.Types.HeroFields.FriendsFields.EdgesFields.NodeFields.Droid(name = "R2-D2", primaryFunction = "Astromech") |]
        let friends = result.Data.Value.Hero.Value.Friends.Edges |> Array.map (fun e -> e.Node)
        friends |> equals expectedFriends
        result.Data.Value.Hero.Value.HomePlanet |> equals (Some "Tatooine")
        let actual = normalize <| sprintf "%A" result.Data
        let expected = normalize <| """Some
            {Hero = Some
            {AppearsIn = [|NewHope; Empire; Jedi|];
            Friends = {Edges = [|{Cursor = "RnJpZW5kOjEwMDI=";
            Node = {HomePlanet = <null>;
            Name = Some "Han Solo";};};
            {Cursor = "RnJpZW5kOjEwMDM=";
            Node = {HomePlanet = Some "Alderaan";
            Name = Some "Leia Organa";};};
            {Cursor = "RnJpZW5kOjIwMDA=";
            Node = {Name = Some "C-3PO";
            PrimaryFunction = Some "Protocol";};};
            {Cursor = "RnJpZW5kOjIwMDE=";
            Node = {Name = Some "R2-D2";
            PrimaryFunction = Some "Astromech";};}|];
            PageInfo = {HasNextPage = false;
            HasPreviousPage = false;};
            TotalCount = Some 4;};
            HomePlanet = Some "Tatooine";
            Name = Some "Luke Skywalker";};}"""
        actual |> equals expected

[<Fact; Trait("Execution", "Sync")>]
let ``Should be able to start a simple query operation synchronously`` () =
    SimpleOperation.operation.Run()
    |> SimpleOperation.validateResult

[<Fact; Trait("Execution", "Async")>]
let ``Should be able to start a simple query operation asynchronously`` () : Task = task {
    let! result = SimpleOperation.operation.AsyncRun()
    result |> SimpleOperation.validateResult
}

[<Fact; Trait("Execution", "Sync")>]
let ``Should be able to use pattern matching methods on an union type`` () =
    let result = SimpleOperation.operation.Run()
    result.Data.IsSome |> equals true
    result.Data.Value.Hero.IsSome |> equals true
    let friends = result.Data.Value.Hero.Value.Friends.Edges |> Array.map (fun e -> e.Node)
    friends
    |> Array.choose (fun x -> x.TryAsHuman())
    |> equals [|
        SimpleOperation.Operation.Types.HeroFields.FriendsFields.EdgesFields.NodeFields.Human(name = "Han Solo")
        SimpleOperation.Operation.Types.HeroFields.FriendsFields.EdgesFields.NodeFields.Human(name = "Leia Organa", homePlanet = "Alderaan") |]
    friends
    |> Array.choose (fun x -> x.TryAsDroid())
    |> equals [|
        SimpleOperation.Operation.Types.HeroFields.FriendsFields.EdgesFields.NodeFields.Droid(name = "C-3PO", primaryFunction = "Protocol")
        SimpleOperation.Operation.Types.HeroFields.FriendsFields.EdgesFields.NodeFields.Droid(name = "R2-D2", primaryFunction = "Astromech") |]
    try
      friends |> Array.map (fun x -> x.AsDroid()) |> ignore
      failwith "Expected exception when trying to get all friends as droids!"
    with _ -> ()
    try
      friends |> Array.map (fun x -> x.AsHuman()) |> ignore
      failwith "Expected exception when trying to get all friends as humans!"
    with _ -> ()
    friends
    |> Array.filter (fun x -> x.IsHuman())
    |> Array.map (fun x -> x.AsHuman())
    |> equals [|
        SimpleOperation.Operation.Types.HeroFields.FriendsFields.EdgesFields.NodeFields.Human(name = "Han Solo")
        SimpleOperation.Operation.Types.HeroFields.FriendsFields.EdgesFields.NodeFields.Human(name = "Leia Organa", homePlanet = "Alderaan") |]
    friends
    |> Array.filter (fun x -> x.IsDroid())
    |> Array.map (fun x -> x.AsDroid())
    |> equals [|
        SimpleOperation.Operation.Types.HeroFields.FriendsFields.EdgesFields.NodeFields.Droid(name = "C-3PO", primaryFunction = "Protocol")
        SimpleOperation.Operation.Types.HeroFields.FriendsFields.EdgesFields.NodeFields.Droid(name = "R2-D2", primaryFunction = "Astromech") |]

module MutationOperation =
    let operation =
        Provider.Operation<"""mutation M {
            setMoon (id: "1", isMoon: true) {
                id
                name
                isMoon
              }
            }""">()

    type Operation = Provider.Operations.M

    let validateResult (result : Operation.OperationResult) =
        result.CustomData.ContainsKey("documentId") |> equals true
        result.Errors |> equals [||]
        result.Data.IsSome |> equals true
        result.Data.Value.SetMoon.IsSome |> equals true
        result.Data.Value.SetMoon.Value.Id |> equals "1"
        result.Data.Value.SetMoon.Value.Name |> equals (Some "Tatooine")
        result.Data.Value.SetMoon.Value.IsMoon |> equals (Some true)

[<Fact; Trait("Execution", "Sync")>]
let ``Should be able to run a mutation synchronously`` () =
    MutationOperation.operation.Run()
    |> MutationOperation.validateResult

[<Fact; Trait("Execution", "Async")>]
let ``Should be able to run a mutation asynchronously`` () : Task = task {
    let! result = MutationOperation.operation.AsyncRun()
    result |> MutationOperation.validateResult
}

module FileOperation =

    let fileOp = Provider.Operation<"operation.graphql">()
    type Operation = Provider.Operations.FileOp

    let validateResult (result : Operation.OperationResult) =
        result.CustomData.ContainsKey("documentId") |> equals true
        result.Errors |> equals [||]
        result.Data.IsSome |> equals true
        result.Data.Value.Hero.IsSome |> equals true
        result.Data.Value.Hero.Value.AppearsIn |> equals [| Episode.NewHope; Episode.Empire; Episode.Jedi |]
        let expectedFriends : Operation.Types.HeroFields.FriendsFields.EdgesFields.NodeFields.Character array =
          [| Operation.Types.HeroFields.FriendsFields.EdgesFields.NodeFields.Human(name = "Han Solo")
             Operation.Types.HeroFields.FriendsFields.EdgesFields.NodeFields.Human(name = "Leia Organa", homePlanet = "Alderaan")
             Operation.Types.HeroFields.FriendsFields.EdgesFields.NodeFields.Droid(name = "C-3PO", primaryFunction = "Protocol")
             Operation.Types.HeroFields.FriendsFields.EdgesFields.NodeFields.Droid(name = "R2-D2", primaryFunction = "Astromech") |]
        let friends = result.Data.Value.Hero.Value.Friends.Edges |> Array.map (fun x -> x.Node)
        friends |> equals expectedFriends
        result.Data.Value.Hero.Value.HomePlanet |> equals (Some "Tatooine")
        let actual = normalize <| sprintf "%A" result.Data
        let expected = normalize <| """Some
            {Hero = Some
            {AppearsIn = [|NewHope; Empire; Jedi|];
            Friends = {Edges = [|{Cursor = "RnJpZW5kOjEwMDI=";
            Node = {HomePlanet = <null>;
            Name = Some "Han Solo";};};
            {Cursor = "RnJpZW5kOjEwMDM=";
            Node = {HomePlanet = Some "Alderaan";
            Name = Some "Leia Organa";};};
            {Cursor = "RnJpZW5kOjIwMDA=";
            Node = {Name = Some "C-3PO";
            PrimaryFunction = Some "Protocol";};};
            {Cursor = "RnJpZW5kOjIwMDE=";
            Node = {Name = Some "R2-D2";
            PrimaryFunction = Some "Astromech";};}|];
            PageInfo = {HasNextPage = false;
            HasPreviousPage = false;};
            TotalCount = Some 4;};
            HomePlanet = Some "Tatooine";
            Name = Some "Luke Skywalker";};}"""
        actual |> equals expected

[<Fact; Trait("Execution", "Sync")>]
let ``Should be able to run a query from a query file`` () =
    FileOperation.fileOp.Run()
    |> FileOperation.validateResult
