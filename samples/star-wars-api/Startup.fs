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

    /// Added for testing purposes because in .NET 8 JsonSerializerOptions become frozen after the first use
    /// and using this function caused an exception.
    let configure (options : GraphQLOptions<_>) =
        options.SerializerOptions.Converters.Add (new System.Text.Json.Serialization.JsonStringEnumConverter ())
        options

    new (configuration : IConfiguration) as this =
        Startup ()
        then this.Configuration <- configuration

    member _.ConfigureServices (services : IServiceCollection) =
        services
            .AddGiraffe()
            .AddGraphQL<Root> (Schema.executor, rootFactory, configure = configure)
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
            app.UseGraphQLAltair "/altair" |> ignore
            app.UseGraphQLGraphiQL "/graphiql" |> ignore
            app.UseGraphQLPlayground "/playground" |> ignore
            app.UseGraphQLVoyager "/voyager" |> ignore
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
