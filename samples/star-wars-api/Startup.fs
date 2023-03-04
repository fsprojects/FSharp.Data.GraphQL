namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open System
open Giraffe
open FSharp.Data.GraphQL.Server.AppInfrastructure
open FSharp.Data.GraphQL.Server.AppInfrastructure.Giraffe
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Json
open Microsoft.AspNetCore.Server.Kestrel.Core
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Options

type Startup private () =

    new (configuration: IConfiguration) as this =
        Startup() then
        this.Configuration <- configuration

    member _.ConfigureServices(services: IServiceCollection) =
        services
            .AddGiraffe()
            .AddGraphQLOptions<Root>(
                Schema.executor,
                Root,
                "/ws"
            )
            .Configure(Action<KestrelServerOptions>(fun x -> x.AllowSynchronousIO <- true))
            .Configure(Action<IISServerOptions>(fun x -> x.AllowSynchronousIO <- true))
            // Surprisingly minimal APIs use Microsoft.AspNetCore.Http.Json.JsonOptions
            // Use if you want to return HTTP responses using minmal APIs IResult interface
            .Configure<HttpClientJsonOptions>(
                Action<HttpClientJsonOptions>(fun o ->
                    Json.configureDefaultSerializerOptions Seq.empty o.SerializerOptions
                )
            )
            // Use for pretty printing in logs
            .Configure<HttpClientJsonOptions>(
                Constants.Idented,
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
            .UseWebSocketsForGraphQL<Root>()
            .UseGiraffe HttpHandlers.webApp

    member val Configuration : IConfiguration = null with get, set
