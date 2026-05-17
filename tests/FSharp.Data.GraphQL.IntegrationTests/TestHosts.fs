module FSharp.Data.GraphQL.IntegrationTests.TestHosts

open FSharp.Data.GraphQL
open Microsoft.AspNetCore.Mvc.Testing
open System.Net.Http
open System

type IntegrationServerApplicationFactory () =
    inherit WebApplicationFactory<FSharp.Data.GraphQL.IntegrationTests.Server.Startup> ()

type StarWarsApplicationFactory () =
    inherit WebApplicationFactory<FSharp.Data.GraphQL.Samples.StarWarsApi.Startup> ()

let private integrationFactory = lazy (new IntegrationServerApplicationFactory ())
let private starWarsFactory = lazy (new StarWarsApplicationFactory ())

let private integrationClient : Lazy<HttpClient> =
    lazy (integrationFactory.Value.CreateClient ())

let private starWarsClient : Lazy<HttpClient> =
    lazy (starWarsFactory.Value.CreateClient ())

do
    AppDomain.CurrentDomain.ProcessExit.Add(fun _ ->
        if integrationClient.IsValueCreated then
            integrationClient.Value.Dispose ()

        if starWarsClient.IsValueCreated then
            starWarsClient.Value.Dispose ()

        if integrationFactory.IsValueCreated then
            integrationFactory.Value.Dispose ()

        if starWarsFactory.IsValueCreated then
            starWarsFactory.Value.Dispose ())

let integrationServerUrl = integrationClient.Value.BaseAddress.ToString().TrimEnd '/'
let starWarsServerUrl = starWarsClient.Value.BaseAddress.ToString().TrimEnd '/'

let createIntegrationConnection () = new GraphQLClientConnection (integrationClient.Value)
let createStarWarsConnection () = new GraphQLClientConnection (starWarsClient.Value)
let createIntegrationHttpClient () = integrationFactory.Value.CreateClient ()
