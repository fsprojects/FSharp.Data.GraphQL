namespace FSharp.Data.GraphQL.Samples.ChatApp

open Giraffe
open FSharp.Data.GraphQL.Server.AspNetCore
open FSharp.Data.GraphQL.Server.AspNetCore.Giraffe
open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Server.Kestrel.Core
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging

module Program =

    let rootFactory () : Root =
        { RequestId = Guid.NewGuid().ToString() }

    let errorHandler (ex : Exception) (log : ILogger) =
        log.LogError(EventId(), ex, "An unhandled exception has occurred while executing this request.")
        clearResponse >=> setStatusCode 500

    [<EntryPoint>]
    let main args =
        let builder = WebApplication.CreateBuilder(args)
        builder.Services
            .AddGiraffe()
            .Configure(Action<KestrelServerOptions>(fun x -> x.AllowSynchronousIO <- true))
            .AddGraphQLOptions<Root>(
                Schema.executor,
                rootFactory,
                "/ws"
            )
        |> ignore

        let app = builder.Build()

        let applicationLifetime = app.Services.GetRequiredService<IHostApplicationLifetime>()
        let loggerFactory = app.Services.GetRequiredService<ILoggerFactory>()

        app
            .UseGiraffeErrorHandler(errorHandler)
            .UseWebSockets()
            .UseWebSocketsForGraphQL<Root>()
            .UseGiraffe
                (HttpHandlers.handleGraphQL<Root>
                    applicationLifetime.ApplicationStopping
                    (loggerFactory.CreateLogger("HttpHandlers.handlerGraphQL"))
                )

        app.Run()

        0 // Exit code

