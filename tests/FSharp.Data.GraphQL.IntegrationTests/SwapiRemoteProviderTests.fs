module FSharp.Data.GraphQL.IntegrationTests.SwapiRemoteProviderTests

open Xunit
open Helpers
open FSharp.Data.GraphQL
open System.Threading.Tasks

type Provider = GraphQLProvider<"introspection.json">

let connection = TestHosts.createStarWarsConnection ()
let context = Provider.GetContext(serverUrl = TestHosts.starWarsServerUrl, connectionFactory = fun () -> connection)

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
        let friends = result.Data.Value.Hero.Value.Friends.Edges |> Array.map (fun e -> e.Node)
        friends.Length |> equals 4
        let friend0 = friends[0]
        friend0.IsHuman() |> equals true
        friend0.AsHuman().Name |> equals (Some "Han Solo")
        let friend1 = friends[1]
        friend1.IsHuman() |> equals true
        friend1.AsHuman().Name |> equals (Some "Leia Organa")
        friend1.AsHuman().HomePlanet |> equals (Some "Alderaan")
        let friend2 = friends[2]
        friend2.IsDroid() |> equals true
        friend2.AsDroid().Name |> equals (Some "C-3PO")
        friend2.AsDroid().PrimaryFunction |> equals (Some "Protocol")
        let friend3 = friends[3]
        friend3.IsDroid() |> equals true
        friend3.AsDroid().Name |> equals (Some "R2-D2")
        friend3.AsDroid().PrimaryFunction |> equals (Some "Astromech")
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
    SimpleOperation.operation.Run(context)
    |> SimpleOperation.validateResult

[<Fact; Trait("Execution", "Async")>]
let ``Should be able to start a simple query operation asynchronously`` () : Task = task {
    let! result = SimpleOperation.operation.AsyncRun(context)
    result |> SimpleOperation.validateResult
}

[<Fact; Trait("Execution", "Sync")>]
let ``Should be able to use pattern matching methods on an union type`` () =
    let result = SimpleOperation.operation.Run(context)
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
    MutationOperation.operation.Run(context)
    |> MutationOperation.validateResult

[<Fact; Trait("Execution", "Async")>]
let ``Should be able to run a mutation asynchronously`` () : Task = task {
    let! result = MutationOperation.operation.AsyncRun(context)
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
        let friends = result.Data.Value.Hero.Value.Friends.Edges |> Array.map _.Node
        friends.Length |> equals 4
        let friend0 = friends[0]
        friend0.IsHuman() |> equals true
        friend0.AsHuman().Name |> equals (Some "Han Solo")
        let friend1 = friends[1]
        friend1.IsHuman() |> equals true
        friend1.AsHuman().Name |> equals (Some "Leia Organa")
        friend1.AsHuman().HomePlanet |> equals (Some "Alderaan")
        let friend2 = friends[2]
        friend2.IsDroid() |> equals true
        friend2.AsDroid().Name |> equals (Some "C-3PO")
        friend2.AsDroid().PrimaryFunction |> equals (Some "Protocol")
        let friend3 = friends[3]
        friend3.IsDroid() |> equals true
        friend3.AsDroid().Name |> equals (Some "R2-D2")
        friend3.AsDroid().PrimaryFunction |> equals (Some "Astromech")
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
    FileOperation.fileOp.Run(context)
    |> FileOperation.validateResult
