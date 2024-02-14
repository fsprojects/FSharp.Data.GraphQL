namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open Giraffe
open FSharp.Data.GraphQL.Server.AspNetCore.Giraffe
open FSharp.Data.GraphQL.Server.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Json
open Microsoft.AspNetCore.Server.Kestrel.Core
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open System
open Microsoft.AspNetCore.Server.Kestrel.Core
open Microsoft.Extensions.Hosting

type Startup private () =

    let rootFactory (ctx) : Root =
        { RequestId = Guid.NewGuid().ToString() }

    new (configuration: IConfiguration) as this =
        Startup() then
        this.Configuration <- configuration

    member _.ConfigureServices(services: IServiceCollection) =
        services
            .AddGiraffe()
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
            .UseGiraffe (HttpHandlers.handleGraphQL<Root>
                            applicationLifetime.ApplicationStopping
                            (loggerFactory.CreateLogger("HttpHandlers.handlerGraphQL"))
                        )

    member val Configuration : IConfiguration = null with get, set
