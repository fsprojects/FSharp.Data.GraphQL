module FSharp.Data.GraphQL.IntegrationTests.SwapiLocalProviderTests

open Xunit
open Helpers
open FSharp.Data.GraphQL
open System.Net.Http

// Local provider should be able to be created from local introspection json file.
type Provider = GraphQLProvider<"introspection.json">

// We are going to re-use the same HttpClient through all requests.
let connection = new GraphQLClientConnection(new HttpClient())

// As we are not using a connection to a server to get the introspection, we need a runtime context.
let getContext() = Provider.GetContext(serverUrl = "http://localhost:8086", connectionFactory = fun () -> connection)

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

[<Fact>]
let ``Should be able to start a simple query operation synchronously`` () =
    use context = getContext()
    SimpleOperation.operation.Run(context)
    |> SimpleOperation.validateResult

[<Fact>]
let ``Should be able to start a simple query operation asynchronously`` () =
    use context = getContext()
    SimpleOperation.operation.AsyncRun(context)
    |> Async.RunSynchronously
    |> SimpleOperation.validateResult

[<Fact>]
let ``Should be able to use pattern matching methods on an union type`` () =
    use context = getContext()
    let result = SimpleOperation.operation.Run(context)
    result.Data.IsSome |> equals true
    result.Data.Value.Hero.IsSome |> equals true
    let friends = result.Data.Value.Hero.Value.Friends.Edges |> Array.map (fun x -> x.Node)
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

[<Fact>]
let ``Should be able to run a mutation synchronously`` () =
    use context = getContext()
    MutationOperation.operation.Run(context)
    |> MutationOperation.validateResult

[<Fact>]
let ``Should be able to run a mutation asynchronously`` () =
    use context = getContext()
    MutationOperation.operation.AsyncRun(context)
    |> Async.RunSynchronously
    |> MutationOperation.validateResult

module FileOperation =

    let fileop = Provider.Operation<"operation.graphql">()
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

[<Fact>]
let ``Should be able to run a query from a query file`` () =
    use context = getContext()
    FileOperation.fileop.Run(context)
    |> FileOperation.validateResult
