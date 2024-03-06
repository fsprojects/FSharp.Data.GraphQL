namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open Giraffe
open FSharp.Data.GraphQL.Server.AspNetCore.Giraffe
open FSharp.Data.GraphQL.Server.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Server.Kestrel.Core
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open System
open Microsoft.Extensions.Hosting

module Constants =
  let [<Literal>] Indented = "Indented"

type Startup private () =

    let rootFactory (ctx) : Root = Root(ctx)

    new (configuration: IConfiguration) as this =
        Startup() then
        this.Configuration <- configuration

    member _.ConfigureServices(services: IServiceCollection) =
        services
            .AddGiraffe()
            .AddGraphQLOptions<Root>(
                Schema.executor,
                rootFactory,
                "/ws"
            )
        |> ignore

    member _.Configure(app: IApplicationBuilder, env: IHostEnvironment, applicationLifetime : IHostApplicationLifetime, loggerFactory : ILoggerFactory) =
        let errorHandler (ex : Exception) (log : ILogger) =
            log.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
            clearResponse >=> setStatusCode 500

        if env.IsDevelopment() then
            app.UseGraphQLPlayground("/playground") |> ignore
            app.UseGraphQLVoyager("/voyager") |> ignore
            app.UseRouting() |> ignore
            app.UseEndpoints(fun endpoints -> endpoints.MapBananaCakePop(PathString "/cakePop") |> ignore) |> ignore

        app
            .UseGiraffeErrorHandler(errorHandler)
            .UseWebSockets()
            .UseWebSocketsForGraphQL<Root>()
            .UseGiraffe
              (HttpHandlers.handleGraphQLWithResponseInterception<Root>
                applicationLifetime.ApplicationStopping
                (loggerFactory.CreateLogger("HttpHandlers.handlerGraphQL"))
                (setHttpHeader "Request-Type" "Classic"))

    member val Configuration : IConfiguration = null with get, set
