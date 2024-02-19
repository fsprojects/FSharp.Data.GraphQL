namespace FSharp.Data.GraphQL.IntegrationTests.Server

open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Server.Kestrel.Core
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Options
open Giraffe

open FSharp.Data.GraphQL.Server.AspNetCore
open FSharp.Data.GraphQL.Server.AspNetCore.Giraffe
open FSharp.Data.GraphQL.Samples.StarWarsApi
open Microsoft.Extensions.Hosting

// See https://learn.microsoft.com/en-us/dotnet/api/microsoft.aspnetcore.mvc.jsonoptions
type MvcJsonOptions = Microsoft.AspNetCore.Mvc.JsonOptions
// See https://learn.microsoft.com/en-us/dotnet/api/microsoft.aspnetcore.http.json.jsonoptions
type HttpClientJsonOptions = Microsoft.AspNetCore.Http.Json.JsonOptions

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
            .Configure(Action<KestrelServerOptions>(fun x -> x.AllowSynchronousIO <- true))
            .Configure(Action<IISServerOptions>(fun x -> x.AllowSynchronousIO <- true))
            .AddGraphQLOptions<Root>(
              Schema.executor,
              rootFactory,
              "/ws"
            )
            // Surprisingly minimal APIs use Microsoft.AspNetCore.Http.Json.JsonOptions
            // Use if you want to return HTTP responses using minmal APIs IResult interface
            .Configure<HttpClientJsonOptions>(
                Action<HttpClientJsonOptions>(fun o ->
                    Json.configureDefaultSerializerOptions Seq.empty o.SerializerOptions
                )
            )
            // // Use for pretty printing in logs
            .Configure<HttpClientJsonOptions>(
                Constants.Indented,
                Action<HttpClientJsonOptions>(fun o ->
                    Json.configureDefaultSerializerOptions Seq.empty o.SerializerOptions
                    o.SerializerOptions.WriteIndented <- true
                )
            )
            // Replace Newtonsoft.Json and use the same settings in Giraffe
            .AddSingleton<Json.ISerializer, SystemTextJson.Serializer>(fun sp ->
                let options = sp.GetService<IOptions<HttpClientJsonOptions>>()
                SystemTextJson.Serializer(options.Value.SerializerOptions))

            |> ignore

    member __.Configure(app: IApplicationBuilder) =
        let errorHandler (ex : Exception) (log : ILogger) =
            log.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
            clearResponse >=> setStatusCode 500
        let applicationLifeTime = app.ApplicationServices.GetRequiredService<IHostApplicationLifetime>()
        let loggerFactory = app.ApplicationServices.GetRequiredService<ILoggerFactory>()
        app
            .UseGiraffeErrorHandler(errorHandler)
            .UseGiraffe
              (HttpHandlers.handleGraphQLWithResponseInterception<Root>
                applicationLifeTime.ApplicationStopping
                (loggerFactory.CreateLogger("HttpHandlers.handleGraphQL"))
                (setHttpHeader "Request-Type" "Classic"))

    member val Configuration : IConfiguration = null with get, set
