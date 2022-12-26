namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Microsoft.Extensions.Logging
open System
open Microsoft.AspNetCore.Server.Kestrel.Core
open GraphQL.Server.Ui.Playground
open Microsoft.Extensions.Hosting

type Startup private () =
    new (configuration: IConfiguration) as this =
        Startup() then
        this.Configuration <- configuration

    member _.ConfigureServices(services: IServiceCollection) =
        services.AddGiraffe()
                .Configure(Action<KestrelServerOptions>(fun x -> x.AllowSynchronousIO <- true))
                .Configure(Action<IISServerOptions>(fun x -> x.AllowSynchronousIO <- true))
        |> ignore

    member _.Configure(app: IApplicationBuilder, env: IHostEnvironment) =
        let errorHandler (ex : Exception) (log : ILogger) =
            log.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
            clearResponse >=> setStatusCode 500
        app
            .UseGiraffeErrorHandler(errorHandler)
            .UseWebSockets()
            //.UseMiddleware<GraphQLWebSocketMiddleware<Root>>(Schema.executor, fun () -> { RequestId = Guid.NewGuid().ToString() })
            .UseGiraffe HttpHandlers.webApp

        if env.IsDevelopment() then
            app.UseGraphQLPlayground("/") |> ignore
            app.UseGraphQLVoyager("/LaunchUrl") |> ignore
            

    member val Configuration : IConfiguration = null with get, set
