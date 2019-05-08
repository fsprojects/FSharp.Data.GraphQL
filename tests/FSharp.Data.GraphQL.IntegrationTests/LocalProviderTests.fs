module FSharp.Data.GraphQL.IntegrationTests.LocalProviderTests

open System
open Xunit
open Helpers
open FSharp.Data.GraphQL

// Local provider should be able to be created from local introspection json file.
type Provider = GraphQLProvider<"introspection.json">
type Episode = Provider.Types.Episode

// As we are not using a connection to a server to get the introspection, we need a runtime context.
let getContext() = Provider.GetContext(serverUrl = "http://localhost:8084")

// We should be able to create instances of schema types.
let ball = Provider.Types.Ball(form = "Spheric", format = "Spheric", id = "1")
let box = Provider.Types.Box(form = "Cubic", format = "Cubic", id = "2")
let things : Provider.Types.IThing list = [ball; box]

module SimpleOperation =
    let operation = 
        Provider.Operation<"""query Q {
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

    type Operation = Provider.Operations.Q

    let validateResult (result : Operation.OperationResult) =
        result.CustomData.IsSome |> equals true
        result.CustomData.Value.ContainsKey("documentId") |> equals true
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
let ``Should be able to start a simple query operation synchronously with custom HTTP headers`` () =
    let userData = Guid.NewGuid().ToString()
    use context = { getContext() with HttpHeaders = [| "UserData", userData |] }
    let result = SimpleOperation.operation.Run(context)
    SimpleOperation.validateResult result
    result.CustomData.IsSome |> equals true
    result.CustomData.Value.ContainsKey("userData") |> equals true
    result.CustomData.Value.["userData"] |> equals (upcast userData)

[<Fact>]
let ``Should be able to start a simple query operation asynchronously with custom HTTP headers`` () =
    let userData = Guid.NewGuid().ToString()
    use context = { getContext() with HttpHeaders = [| "UserData", userData |] }
    let result = SimpleOperation.operation.AsyncRun(context) |> Async.RunSynchronously
    SimpleOperation.validateResult result
    result.CustomData.IsSome |> equals true
    result.CustomData.Value.ContainsKey("userData") |> equals true
    result.CustomData.Value.["userData"] |> equals (upcast userData)

[<Fact>]
let ``Should be able to use pattern matching methods on an union type`` () =
    use context = getContext()
    let result = SimpleOperation.operation.Run(context)
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
    
    type Operation = Provider.Operations.TestQuery

    let validateResult (result : Operation.OperationResult) =
        result.CustomData.IsSome |> equals true
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
    use context = getContext()
    InterfaceOperation.operation.Run(context)
    |> InterfaceOperation.validateResult

[<Fact>]
let ``Should be able to run a query with interface types asynchronously`` () =
    use context = getContext()
    InterfaceOperation.operation.AsyncRun(context)
    |> Async.RunSynchronously
    |> InterfaceOperation.validateResult

[<Fact>]
let ``Should be able to use pattern matching methods on an interface type`` () =
    use context = getContext()
    let result = InterfaceOperation.operation.Run(context)
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
        Provider.Operation<"""mutation M {
            setMoon (id: "1", isMoon: true) {
                id
                name
                isMoon
              }
            }""">()

    type Operation = Provider.Operations.M

    let validateResult (result : Operation.OperationResult) =
        result.CustomData.IsSome |> equals true
        result.CustomData.Value.ContainsKey("documentId") |> equals true
        result.Errors |> equals None
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

module VariablesOperation =
    let operation =
        Provider.Operation<"""query ThingsQuery($filter: ThingFilter!) {
            things(filter: $filter) {
              id
              format
            }
          }""">()

    type Operation = Provider.Operations.ThingsQuery

    let validateResult (filter : Provider.Types.ThingFilter) (result : Operation.OperationResult) =
        result.CustomData.IsSome |> equals true
        result.CustomData.Value.ContainsKey("documentId") |> equals true
        result.Errors |> equals None
        result.Data.IsSome |> equals true
        hasItems result.Data.Value.Things
        result.Data.Value.Things |> Array.forall (fun x -> x.Format = filter.Format) |> equals true

[<Fact>]
let ``Should be able to run a query with variables syncrhonously`` () =
    let filter = Provider.Types.ThingFilter(format = "Cubic")
    use context = getContext()
    VariablesOperation.operation.Run(context, filter)
    |> VariablesOperation.validateResult filter

[<Fact>]
let ``Should be able to run a query with variables asyncrhonously`` () =
    let filter = Provider.Types.ThingFilter(format = "Cubic")
    use context = getContext()
    VariablesOperation.operation.AsyncRun(context, filter)
    |> Async.RunSynchronously
    |> VariablesOperation.validateResult filter

module FileOperation =
    let fileop = Provider.Operation<"operation.graphql">()
    type Operation = Provider.Operations.FileOp
    let validateResult (result : Operation.OperationResult) =
        result.CustomData.IsSome |> equals true
        result.CustomData.Value.ContainsKey("documentId") |> equals true
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
let ``Should be able to run a query from a query file`` () =
    use context = getContext()
    FileOperation.fileop.Run(context)
    |> FileOperation.validateResult