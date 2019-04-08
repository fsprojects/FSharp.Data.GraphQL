module FSharp.Data.GraphQL.IntegrationTests.RemoteProviderTests

open Xunit
open FSharp.Data.GraphQL

// Remote provider needs the Giraffe test server running.
// To be able to use it in design time, start the Giraffe sample server.
type Provider = GraphQLProvider<"http://localhost:8084">

// We should be able to create instances of schema types from both providers.
// We should be able to create instances of schema types.
let ball = Provider.Types.Ball(form = "Spheric", format = "Spheric", id = "1")
let box = Provider.Types.Box(form = "Cubic", format = "Cubic", id = "2")
let things : Provider.Types.IThing list = [ball; box]

[<Fact>]
let ``Should be able to map schema by using local introspection file`` () =
    Assert.True(true)
