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

let createIntegrationHttpClient () : HttpClient =
    integrationFactory.Value.CreateClient ()

let createStarWarsHttpClient () : HttpClient =
    starWarsFactory.Value.CreateClient ()

let private getIntegrationServerUrl () =
    use client = createIntegrationHttpClient ()
    client.BaseAddress.ToString().TrimEnd '/'

let private getStarWarsServerUrl () =
    use client = createStarWarsHttpClient ()
    client.BaseAddress.ToString().TrimEnd '/'

do
    AppDomain.CurrentDomain.ProcessExit.Add(fun _ ->
        if integrationFactory.IsValueCreated then
            integrationFactory.Value.Dispose ()

        if starWarsFactory.IsValueCreated then
            starWarsFactory.Value.Dispose ())

let integrationServerUrl = getIntegrationServerUrl ()
let starWarsServerUrl = getStarWarsServerUrl ()

let createIntegrationConnection () = new GraphQLClientConnection (createIntegrationHttpClient ())
let createStarWarsConnection () = new GraphQLClientConnection (createStarWarsHttpClient ())
