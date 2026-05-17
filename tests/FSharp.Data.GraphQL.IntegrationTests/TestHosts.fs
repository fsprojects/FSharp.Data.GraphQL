module FSharp.Data.GraphQL.IntegrationTests.TestHosts

open FSharp.Data.GraphQL
open Microsoft.AspNetCore.Mvc.Testing
open System.Net.Http

type IntegrationServerApplicationFactory () =
    inherit WebApplicationFactory<FSharp.Data.GraphQL.IntegrationTests.Server.Startup> ()

type StarWarsApplicationFactory () =
    inherit WebApplicationFactory<FSharp.Data.GraphQL.Samples.StarWarsApi.Startup> ()

let private integrationClient : Lazy<HttpClient> =
    lazy
        (let factory = new IntegrationServerApplicationFactory ()
         factory.CreateClient ())

let private starWarsClient : Lazy<HttpClient> =
    lazy
        (let factory = new StarWarsApplicationFactory ()
         factory.CreateClient ())

let integrationServerUrl = integrationClient.Value.BaseAddress.ToString().TrimEnd '/'
let starWarsServerUrl = starWarsClient.Value.BaseAddress.ToString().TrimEnd '/'

let createIntegrationConnection () = new GraphQLClientConnection (integrationClient.Value)
let createStarWarsConnection () = new GraphQLClientConnection (starWarsClient.Value)
