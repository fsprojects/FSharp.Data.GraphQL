namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Hosting
open Giraffe
open FSharp.Data.GraphQL.Server.AspNetCore
open FSharp.Data.GraphQL.Server.AspNetCore.Giraffe

type Startup private () =

    let rootFactory (ctx) : Root = Root (ctx)

    new (configuration : IConfiguration) as this =
        Startup ()
        then this.Configuration <- configuration

    member _.ConfigureServices (services : IServiceCollection) =
        services
            .AddGiraffe()
            .AddGraphQLOptions<Root> (Schema.executor, rootFactory, "/ws")
        |> ignore

    member _.Configure
        (
            app : IApplicationBuilder,
            env : IHostEnvironment
        ) =
        let errorHandler (ex : Exception) (log : ILogger) =
            log.LogError (EventId (), ex, "An unhandled exception has occurred while executing the request.")
            clearResponse >=> setStatusCode 500

        if env.IsDevelopment () then
            app.UseGraphQLPlayground ("/playground") |> ignore
            app.UseGraphQLVoyager ("/voyager") |> ignore
            app.UseRouting () |> ignore
            app.UseEndpoints (fun endpoints -> endpoints.MapBananaCakePop (PathString "/cakePop") |> ignore)
            |> ignore

        app
            .UseGiraffeErrorHandler(errorHandler)
            .UseWebSockets()
            .UseWebSocketsForGraphQL<Root>()
            .UseGiraffe (
                // Set CORS to allow external servers (React samples) to call this API
                setHttpHeader "Access-Control-Allow-Origin" "*"
                >=> setHttpHeader "Access-Control-Allow-Headers" "content-type"
                >=> (setHttpHeader "Request-Type" "Classic") // For integration testing purposes
                >=> HttpHandlers.graphQL<Root>
            )

    member val Configuration : IConfiguration = null with get, set
