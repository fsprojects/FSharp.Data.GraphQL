module FSharp.Data.GraphQL.IntegrationTests.LocalProviderTests

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
        hero (id: "1001") {
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
  let validate (result : Provider.Context.Operation214c5a8ac156977d19d6049385aa20bf.OperationResult) =
      result.CustomData.ContainsKey("documentId") |> equals true
      result.Errors |> equals None
      result.Data.IsSome |> equals true
      result.Data.Value.Hero.IsSome |> equals true
      result.Data.Value.Hero.Value.AppearsIn |> equals [| Episode.NewHope; Episode.Empire; Episode.Jedi |]
      let expectedFriend : Provider.Context.Operation214c5a8ac156977d19d6049385aa20bf.Types.Hero.Friends.Character = 
        upcast Provider.Context.Operation214c5a8ac156977d19d6049385aa20bf.Types.Hero.Friends.Human(name = Some "Wilhuff Tarkin", homePlanet = None)
      result.Data.Value.Hero.Value.Friends |> equals [| Some expectedFriend |]
      result.Data.Value.Hero.Value.HomePlanet |> equals (Some "Tatooine")
      let actual = normalize <| sprintf "%A" result.Data
      let expected = normalize <| """Some
          {Hero = Some
          {Name = Some "Darth Vader";
          AppearsIn = [|NewHope; Empire; Jedi|];
          HomePlanet = Some "Tatooine";
          Friends = [|Some {Name = Some "Wilhuff Tarkin";
          HomePlanet = <null>;}|];};}"""
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