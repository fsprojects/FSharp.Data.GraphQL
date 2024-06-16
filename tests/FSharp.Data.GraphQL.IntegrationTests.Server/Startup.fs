namespace FSharp.Data.GraphQL.IntegrationTests.Server

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
open FSharp.Data.GraphQL.Samples.StarWarsApi

module Constants =
  let [<Literal>] Indented = "Indented"

type Startup private () =

    let rootFactory (httpContext: HttpContext) : Root =
      Root(httpContext)

    new (configuration: IConfiguration) as this =
        Startup() then
        this.Configuration <- configuration

    member __.ConfigureServices(services: IServiceCollection) =
        services
            .AddGiraffe()
            .AddGraphQLOptions<Root>(
              Schema.executor,
              rootFactory,
              "/ws"
            )
            |> ignore

    member __.Configure(app: IApplicationBuilder) =
        let errorHandler (ex : Exception) (log : ILogger) =
            log.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
            clearResponse >=> setStatusCode 500
        let applicationLifeTime = app.ApplicationServices.GetRequiredService<IHostApplicationLifetime>()
        let loggerFactory = app.ApplicationServices.GetRequiredService<ILoggerFactory>()
        app
            .UseGiraffeErrorHandler(errorHandler)
            .UseGiraffe (
                (setHttpHeader "Request-Type" "Classic")
                >=> HttpHandlers.graphQL<Root>)

    member val Configuration : IConfiguration = null with get, set
