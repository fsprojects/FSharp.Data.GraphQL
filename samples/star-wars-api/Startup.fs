namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Json
open Microsoft.AspNetCore.Server.Kestrel.Core
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Options
open Giraffe

type Startup private () =
    new (configuration: IConfiguration) as this =
        Startup() then
        this.Configuration <- configuration

    member _.ConfigureServices(services: IServiceCollection) =
        services.AddGiraffe()
                .Configure(Action<KestrelServerOptions>(fun x -> x.AllowSynchronousIO <- true))
                .Configure(Action<IISServerOptions>(fun x -> x.AllowSynchronousIO <- true))
                .Configure<JsonOptions>(Action<JsonOptions>(fun s -> (Json.configureDefaultSerializerOptions Seq.empty s.SerializerOptions) |> ignore))
                .AddSingleton<Json.ISerializer,Json.ISerializer>(
                    fun sp ->
                        let options = sp.GetRequiredService<IOptions<JsonOptions>>().Value.SerializerOptions
                        SystemTextJson.Serializer(options))
        |> ignore

    member _.Configure(app: IApplicationBuilder, env: IHostEnvironment) =
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
            //.UseMiddleware<GraphQLWebSocketMiddleware<Root>>(Schema.executor, fun () -> { RequestId = Guid.NewGuid().ToString() })
            .UseGiraffe HttpHandlers.webApp

    member val Configuration : IConfiguration = null with get, set
