module FSharp.Data.GraphQL.IntegrationTests.LocalProviderTests

open Xunit
open Helpers
open FSharp.Data.GraphQL

// Local provider should be able to be created from local introspection json file.
type Provider = GraphQLProvider<"introspection.json">

// We should be able to create instances of schema types.
let ball = Provider.Types.Ball(form = "Spheric", format = "Spheric", id = "1")
let box = Provider.Types.Box(form = "Cubic", format = "Cubic", id = "2")
let things : Provider.Types.IThing list = [ball; box]

let getContext () = Provider.GetContext("http://localhost:8084")

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
let ``Should be able to start a context`` () =
    getContext () |> ignore

[<Fact>]
let ``Should be able to start a simple query operation`` () =
    let context = getContext ()
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
    let result = operation.Run()
    result.CustomData.ContainsKey("documentId") |> equals true
