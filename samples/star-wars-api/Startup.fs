namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Hosting
open Oxpecker
open FSharp.Data.GraphQL.Server.AspNetCore
open FSharp.Data.GraphQL.Server.AspNetCore.Oxpecker

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

    member _.ConfigureServices (services : IServiceCollection) : unit =
        services
            .AddOxpecker()
            .AddGraphQL<Root> (Schema.executor, rootFactory, configure = configure)
        |> ignore

    member _.Configure (app : IApplicationBuilder, env : IHostEnvironment) : unit =

        if env.IsDevelopment () then
            app.UseGraphQLAltair "/altair" |> ignore
            app.UseGraphQLGraphiQL "/graphiql" |> ignore
            app.UseGraphQLPlayground "/playground" |> ignore
            app.UseGraphQLVoyager "/voyager" |> ignore
            app.UseRouting () |> ignore
            app.UseEndpoints (fun endpoints -> endpoints.MapBananaCakePop (PathString "/cakePop") |> ignore)
            |> ignore

        app
            //.UseGiraffeErrorHandler(errorHandler)
            .UseRouting()
            .UseWebSockets()
            .UseWebSocketsForGraphQL<Root>()
            .UseEndpoints (fun endpoints ->
                // Simple declaration
                //endpoints.MapOxpeckerEndpoint (HttpEndpoints.graphQL<Root>("/", id))
                let handler =
                    setHttpHeader "Access-Control-Allow-Origin" "*"
                    >=> setHttpHeader "Access-Control-Allow-Headers" "content-type"
                    >=> (setHttpHeader "Request-Type" "Classic") // For integration testing purposes
                    >=> HttpEndpointHandlers.graphQL<Root>
                endpoints.MapOxpeckerEndpoint (route "/" handler))
        |> ignore

    member val Configuration : IConfiguration = null with get, set
