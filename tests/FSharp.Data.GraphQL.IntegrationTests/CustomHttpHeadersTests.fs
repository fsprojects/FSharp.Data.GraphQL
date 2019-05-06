module FSharp.Data.GraphQL.IntegrationTests.CustomHttpHeadersTests

open Xunit
open Helpers
open FSharp.Data.GraphQL

// Local provider should be able to be created from local introspection json file.
// We should be able to provide custom HTTP headers.
type Provider = GraphQLProvider<"http://localhost:8084", "UserData: 45883115-db2f-4ccc-ae6f-21ec17d4a7a1">
type Episode = Provider.Types.Episode

// We should be able to create instances of schema types.
let ball = Provider.Types.Ball(form = "Spheric", format = "Spheric", id = "1")
let box = Provider.Types.Box(form = "Cubic", format = "Cubic", id = "2")
let things : Provider.Types.IThing list = [ball; box]

module SimpleOperation =
    let operation = 
        Provider.Operation<"""query MyQuery {
            hero (id: "1000") {
              name
              appearsIn
              homePlanet
              friends {
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
          }""">()

    type Operation = Provider.MyQuery

    let validateResult (userData : string option) (result : Operation.OperationResult) =
        result.CustomData.IsSome |> equals true
        result.CustomData.Value.ContainsKey("documentId") |> equals true
        result.CustomData.Value.ContainsKey("userData") |> equals true
        match userData with
        | Some userData -> result.CustomData.Value.["userData"] |> equals (upcast userData)
        | None -> result.CustomData.Value.["userData"] |> equals (upcast "45883115-db2f-4ccc-ae6f-21ec17d4a7a1")
        result.Errors |> equals None
        result.Data.IsSome |> equals true
        result.Data.Value.Hero.IsSome |> equals true
        result.Data.Value.Hero.Value.AppearsIn |> equals [| Episode.NewHope; Episode.Empire; Episode.Jedi |]
        let expectedFriends : Option<Operation.Types.Hero.Friends.Character> [] = 
          [| Some (upcast Operation.Types.Hero.Friends.Human(name = Some "Han Solo", homePlanet = None))
             Some (upcast Operation.Types.Hero.Friends.Human(name = Some "Leia Organa", homePlanet = Some "Alderaan"))
             Some (upcast Operation.Types.Hero.Friends.Droid(name = Some "C-3PO", primaryFunction = Some "Protocol"))
             Some (upcast Operation.Types.Hero.Friends.Droid(name = Some "R2-D2", primaryFunction = Some "Astromech")) |]
        result.Data.Value.Hero.Value.Friends |> equals expectedFriends
        result.Data.Value.Hero.Value.HomePlanet |> equals (Some "Tatooine")
        let actual = normalize <| sprintf "%A" result.Data
        let expected = normalize <| """Some
  {Hero = Some
  {Name = Some "Luke Skywalker";
  AppearsIn = [|NewHope; Empire; Jedi|];
  HomePlanet = Some "Tatooine";
  Friends = [|Some {Name = Some "Han Solo";
  HomePlanet = <null>;};
  Some {Name = Some "Leia Organa";
  HomePlanet = Some "Alderaan";};
  Some {Name = Some "C-3PO";
  PrimaryFunction = Some "Protocol";};
  Some {Name = Some "R2-D2";
  PrimaryFunction = Some "Astromech";}|];};}"""
        actual |> equals expected

[<Fact>]
let ``Should be able to pretty print schema types`` () =
    let actual = normalize <| sprintf "%A" things
    let expected = normalize <| """[{Form = "Spheric";
        Format = "Spheric";
        Id = "1";};
         {Form = "Cubic";
        Format = "Cubic";
        Id = "2";}]"""
    actual |> equals expected

[<Fact>]
let ``Should be able to start a simple query operation synchronously`` () =
    SimpleOperation.operation.Run()
    |> SimpleOperation.validateResult None

[<Fact>]
let ``Should be able to start a simple query operation asynchronously`` () =
    SimpleOperation.operation.AsyncRun()
    |> Async.RunSynchronously
    |> SimpleOperation.validateResult None

[<Fact>]
let ``Should be able to start a simple query operation synchronously with custom HTTP headers`` () =
    let userData = "45e7ca6f-4384-4da7-ad97-963133e6f0fb"
    let context = Provider.GetContext([| "UserData", userData |], "http://localhost:8084")
    let result = SimpleOperation.operation.Run(context)
    SimpleOperation.validateResult (Some userData) result
    result.CustomData.IsSome |> equals true
    result.CustomData.Value.ContainsKey("userData") |> equals true
    result.CustomData.Value.["userData"] |> equals (upcast userData)

[<Fact>]
let ``Should be able to start a simple query operation asynchronously with custom HTTP headers`` () =
    let userData = "45e7ca6f-4384-4da7-ad97-963133e6f0fb"
    let context = Provider.GetContext([| "UserData", userData |], "http://localhost:8084")
    let result = SimpleOperation.operation.AsyncRun(context) |> Async.RunSynchronously
    SimpleOperation.validateResult (Some userData) result
    result.CustomData.IsSome |> equals true
    result.CustomData.Value.ContainsKey("userData") |> equals true
    result.CustomData.Value.["userData"] |> equals (upcast userData)

[<Fact>]
let ``Should be able to use pattern matching methods on an union type`` () =
    let result = SimpleOperation.operation.Run()
    result.Data.IsSome |> equals true
    result.Data.Value.Hero.IsSome |> equals true
    let friends = result.Data.Value.Hero.Value.Friends |> Array.choose id
    friends 
    |> Array.choose (fun x -> x.TryAsHuman()) 
    |> equals [|
        SimpleOperation.Operation.Types.Hero.Friends.Human(name = Some "Han Solo", homePlanet = None)
        SimpleOperation.Operation.Types.Hero.Friends.Human(name = Some "Leia Organa", homePlanet = Some "Alderaan") |]
    friends
    |> Array.choose (fun x -> x.TryAsDroid())
    |> equals [|
        SimpleOperation.Operation.Types.Hero.Friends.Droid(name = Some "C-3PO", primaryFunction = Some "Protocol")
        SimpleOperation.Operation.Types.Hero.Friends.Droid(name = Some "R2-D2", primaryFunction = Some "Astromech") |]
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
        SimpleOperation.Operation.Types.Hero.Friends.Human(name = Some "Han Solo", homePlanet = None)
        SimpleOperation.Operation.Types.Hero.Friends.Human(name = Some "Leia Organa", homePlanet = Some "Alderaan") |]
    friends
    |> Array.filter (fun x -> x.IsDroid())
    |> Array.map (fun x -> x.AsDroid())
    |> equals [|
        SimpleOperation.Operation.Types.Hero.Friends.Droid(name = Some "C-3PO", primaryFunction = Some "Protocol")
        SimpleOperation.Operation.Types.Hero.Friends.Droid(name = Some "R2-D2", primaryFunction = Some "Astromech") |]
  
module InterfaceOperation =
    let operation =
        Provider.Operation<"""query TestQuery {
            things {
              id
              format
              ...on Ball {
                form
              }
              ...on Box {
                form
              }
            }
          }""">()
    
    type Operation = Provider.TestQuery

    let validateResult (userData : string option) (result : Operation.OperationResult) =
        result.CustomData.IsSome |> equals true
        result.CustomData.Value.ContainsKey("documentId") |> equals true
        result.CustomData.Value.ContainsKey("userData") |> equals true
        match userData with
        | Some userData -> result.CustomData.Value.["userData"] |> equals (upcast userData)
        | None -> result.CustomData.Value.["userData"] |> equals (upcast "45883115-db2f-4ccc-ae6f-21ec17d4a7a1")
        result.CustomData.Value.ContainsKey("documentId") |> equals true
        result.Errors |> equals None
        result.Data.IsSome |> equals true
        let expectedThings : Operation.Types.Things.Thing [] =
          [| Operation.Types.Things.Ball(id = "1", format = "Spheric", form = "Spheric")
             Operation.Types.Things.Box(id = "2", format = "Cubic", form = "Cubic") |]
        result.Data.Value.Things |> equals expectedThings
        let actual = normalize <| sprintf "%A" result.Data
        let expected = normalize <| """Some
{Things = [|{Id = "1";
Format = "Spheric";
Form = "Spheric";};
{Id = "2";
Format = "Cubic";
Form = "Cubic";}|];}"""
        actual |> equals expected

[<Fact>]
let ``Should be able to run a query with interface types synchronously`` () =
    InterfaceOperation.operation.Run()
    |> InterfaceOperation.validateResult None

[<Fact>]
let ``Should be able to run a query with interface types asynchronously`` () =
    InterfaceOperation.operation.AsyncRun()
    |> Async.RunSynchronously
    |> InterfaceOperation.validateResult None

[<Fact>]
let ``Should be able to use pattern matching methods on an interface type`` () =
    let result = InterfaceOperation.operation.Run()
    result.Data.IsSome |> equals true
    let things = result.Data.Value.Things
    let balls = things |> Array.choose (fun x -> x.TryAsBall())
    balls |> equals [| InterfaceOperation.Operation.Types.Things.Ball(id = "1", format = "Spheric", form = "Spheric") |]
    let boxes = things |> Array.choose (fun x -> x.TryAsBox())
    boxes |> equals [| InterfaceOperation.Operation.Types.Things.Box(id = "2", format = "Cubic", form = "Cubic") |]
    try
      things |> Array.map (fun x -> x.AsBall()) |> ignore
      failwith "Expected exception when trying to get all things as balls!"
    with _ -> ()
    try
      things |> Array.map (fun x -> x.AsBox()) |> ignore
      failwith "Expected exception when trying to get all things as boxes!"
    with _ -> ()
    things
    |> Array.filter (fun x -> x.IsBall())
    |> Array.map (fun x -> x.AsBall())
    |> equals [| InterfaceOperation.Operation.Types.Things.Ball(id = "1", format = "Spheric", form = "Spheric") |]
    things
    |> Array.filter (fun x -> x.IsBox())
    |> Array.map (fun x -> x.AsBox())
    |> equals [| InterfaceOperation.Operation.Types.Things.Box(id = "2", format = "Cubic", form = "Cubic") |]

module MutationOperation =
    let operation =
        Provider.Operation<"""mutation m {
            setMoon (id: "1", isMoon: true) {
                id
                name
                isMoon
              }
            }""">()

    type Operation = Provider.M

    let validateResult (userData : string option) (result : Operation.OperationResult) =
        result.CustomData.IsSome |> equals true
        result.CustomData.Value.ContainsKey("documentId") |> equals true
        result.CustomData.Value.ContainsKey("userData") |> equals true
        match userData with
        | Some userData -> result.CustomData.Value.["userData"] |> equals (upcast userData)
        | None -> result.CustomData.Value.["userData"] |> equals (upcast "45883115-db2f-4ccc-ae6f-21ec17d4a7a1")
        result.CustomData.Value.ContainsKey("documentId") |> equals true
        result.Errors |> equals None
        result.Data.IsSome |> equals true
        result.Data.Value.SetMoon.IsSome |> equals true
        result.Data.Value.SetMoon.Value.Id |> equals "1"
        result.Data.Value.SetMoon.Value.Name |> equals (Some "Tatooine")
        result.Data.Value.SetMoon.Value.IsMoon |> equals (Some true)

[<Fact>]
let ``Should be able to run a mutation synchronously`` () =
    MutationOperation.operation.Run()
    |> MutationOperation.validateResult None

[<Fact>]
let ``Should be able to run a mutation asynchronously`` () =
    MutationOperation.operation.AsyncRun()
    |> Async.RunSynchronously
    |> MutationOperation.validateResult None

module VariablesOperation =
    let operation =
        Provider.Operation<"""query FilterQuery($filter: ThingFilter!) {
            things(filter: $filter) {
              id
              format
            }
          }""">()

    type Operation = Provider.FilterQuery

    let validateResult (userData : string option) (filter : Provider.Types.ThingFilter) (result : Operation.OperationResult) =
        result.CustomData.IsSome |> equals true
        result.CustomData.Value.ContainsKey("documentId") |> equals true
        result.CustomData.Value.ContainsKey("userData") |> equals true
        match userData with
        | Some userData -> result.CustomData.Value.["userData"] |> equals (upcast userData)
        | None -> result.CustomData.Value.["userData"] |> equals (upcast "45883115-db2f-4ccc-ae6f-21ec17d4a7a1")
        result.CustomData.Value.ContainsKey("documentId") |> equals true
        result.Errors |> equals None
        result.Data.IsSome |> equals true
        hasItems result.Data.Value.Things
        result.Data.Value.Things |> Array.forall (fun x -> x.Format = filter.Format) |> equals true


module FileOperation =
    let fileOp = Provider.Operation<"operation.graphql">()
    type Operation = Provider.FileOp
    let validateResult (userData : string option) (result : Operation.OperationResult) =
        result.CustomData.IsSome |> equals true
        result.CustomData.Value.ContainsKey("documentId") |> equals true
        result.CustomData.Value.ContainsKey("userData") |> equals true
        match userData with
        | Some userData -> result.CustomData.Value.["userData"] |> equals (upcast userData)
        | None -> result.CustomData.Value.["userData"] |> equals (upcast "45883115-db2f-4ccc-ae6f-21ec17d4a7a1")
        result.Errors |> equals None
        result.Data.IsSome |> equals true
        result.Data.Value.Hero.IsSome |> equals true
        result.Data.Value.Hero.Value.AppearsIn |> equals [| Episode.NewHope; Episode.Empire; Episode.Jedi |]
        let expectedFriends : Option<Operation.Types.Hero.Friends.Character> [] = 
          [| Some (upcast Operation.Types.Hero.Friends.Human(name = Some "Han Solo", homePlanet = None))
             Some (upcast Operation.Types.Hero.Friends.Human(name = Some "Leia Organa", homePlanet = Some "Alderaan"))
             Some (upcast Operation.Types.Hero.Friends.Droid(name = Some "C-3PO", primaryFunction = Some "Protocol"))
             Some (upcast Operation.Types.Hero.Friends.Droid(name = Some "R2-D2", primaryFunction = Some "Astromech")) |]
        result.Data.Value.Hero.Value.Friends |> equals expectedFriends
        result.Data.Value.Hero.Value.HomePlanet |> equals (Some "Tatooine")
        let actual = normalize <| sprintf "%A" result.Data
        let expected = normalize <| """Some
  {Hero = Some
  {Name = Some "Luke Skywalker";
  AppearsIn = [|NewHope; Empire; Jedi|];
  HomePlanet = Some "Tatooine";
  Friends = [|Some {Name = Some "Han Solo";
  HomePlanet = <null>;};
  Some {Name = Some "Leia Organa";
  HomePlanet = Some "Alderaan";};
  Some {Name = Some "C-3PO";
  PrimaryFunction = Some "Protocol";};
  Some {Name = Some "R2-D2";
  PrimaryFunction = Some "Astromech";}|];};}"""
        actual |> equals expected
[<Fact>]
let ``Should be able to run a query with variables syncrhonously`` () =
    let filter = Provider.Types.ThingFilter(format = "Cubic")
    VariablesOperation.operation.Run(filter)
    |> VariablesOperation.validateResult None filter

[<Fact>]
let ``Should be able to run a query with variables asyncrhonously`` () =
    let filter = Provider.Types.ThingFilter(format = "Cubic")
    VariablesOperation.operation.AsyncRun(filter)
    |> Async.RunSynchronously
    |> VariablesOperation.validateResult None filter

[<Fact>]
let ``Should be able to run a query from a query file`` () =
    FileOperation.fileOp.Run()
    |> FileOperation.validateResult None