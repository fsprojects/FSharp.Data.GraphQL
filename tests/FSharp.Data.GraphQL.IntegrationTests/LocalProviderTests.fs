module FSharp.Data.GraphQL.IntegrationTests.LocalProviderTests

open System
open Xunit
open Helpers
open FSharp.Data.GraphQL

// Local provider should be able to be created from local introspection json file.
type Provider = GraphQLProvider<"introspection.json">
type Episode = Provider.Types.Episode

// We should be able to create instances of schema types.
let ball = Provider.Types.Ball(form = "Spheric", format = "Spheric", id = "1")
let box = Provider.Types.Box(form = "Cubic", format = "Cubic", id = "2")
let things : Provider.Types.IThing list = [ball; box]

// We should be able to open up a context to the Giraffe test server.
let context = Provider.GetContext("http://localhost:8084")

module SimpleOperation =
  // Sample operations to be used for testing
  let operation = 
    context.Operation<"""query q {
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
  type Operation = Provider.Context.Operationd550cd8e988f8683eefaadf91ed4cd15
  let validate (result : Operation.OperationResult) =
      result.CustomData.ContainsKey("documentId") |> equals true
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
    |> SimpleOperation.validate

[<Fact>]
let ``Should be able to start a simple query operation asynchronously`` () =
    SimpleOperation.operation.AsyncRun()
    |> Async.RunSynchronously
    |> SimpleOperation.validate

[<Fact>]
let ``Should be able to start a simple query operation synchronously with custom HTTP headers`` () =
    let userData = Guid.NewGuid().ToString()
    let result = SimpleOperation.operation.Run([|"UserData", userData|])
    SimpleOperation.validate result
    result.CustomData.ContainsKey("userData") |> equals true
    result.CustomData.["userData"] |> equals (upcast userData)

[<Fact>]
let ``Should be able to start a simple query operation asynchronously with custom HTTP headers`` () =
    let userData = Guid.NewGuid().ToString()
    let result = SimpleOperation.operation.AsyncRun([|"UserData", userData|]) |> Async.RunSynchronously
    SimpleOperation.validate result
    result.CustomData.ContainsKey("userData") |> equals true
    result.CustomData.["userData"] |> equals (upcast userData)