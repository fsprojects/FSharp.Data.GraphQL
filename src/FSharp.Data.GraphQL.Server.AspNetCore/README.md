## Usage

### Server

In a `Startup` class...
```fsharp
namespace MyApp

open Giraffe
open FSharp.Data.GraphQL.Server.AspNetCore.Giraffe
open FSharp.Data.GraphQL.Server.AspNetCore
open Microsoft.AspNetCore.Server.Kestrel.Core
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open System
open System.Text.Json

type Startup private () =
    // Factory for object holding request-wide info. You define Root somewhere else.
    let rootFactory () : Root =
        { RequestId = Guid.NewGuid().ToString() }

    new (configuration: IConfiguration) as this =
        Startup() then
        this.Configuration <- configuration

    member _.ConfigureServices(services: IServiceCollection) =
        services.AddGiraffe()
                .Configure(Action<KestrelServerOptions>(fun x -> x.AllowSynchronousIO <- true))
                .AddGraphQLOptions<Root>( // STEP 1: Setting the options
                    Schema.executor, // --> Schema.executor is defined by you somewhere else (in another file)
                    rootFactory,
                    "/ws" // --> endpoint for websocket connections
                )
        |> ignore

    member _.Configure(app: IApplicationBuilder, applicationLifetime : IHostApplicationLifetime, loggerFactory : ILoggerFactory) =
        let errorHandler (ex : Exception) (log : ILogger) =
            log.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
            clearResponse >=> setStatusCode 500
        app
            .UseGiraffeErrorHandler(errorHandler)
            .UseWebSockets()
            .UseWebSocketsForGraphQL<Root>() // STEP 2: using the GraphQL websocket middleware
            .UseGiraffe
                (HttpHandlers.handleGraphQL<Root>
                    applicationLifetime.ApplicationStopping
                    (loggerFactory.CreateLogger("HttpHandlers.handlerGraphQL"))
                )

    member val Configuration : IConfiguration = null with get, set

```

In your schema, you'll want to define a subscription, like in (example taken from the star-wars-api sample in the "samples/" folder):

```fsharp
    let Subscription =
        Define.SubscriptionObject<Root>(
            name = "Subscription",
            fields = [
                Define.SubscriptionField(
                    "watchMoon",
                    RootType,
                    PlanetType,
                    "Watches to see if a planet is a moon.",
                    [ Define.Input("id", String) ],
                    (fun ctx _ p -> if ctx.Arg("id") = p.Id then Some p else None)) ])
```

Don't forget to notify subscribers about new values:

```fsharp
    let Mutation =
        Define.Object<Root>(
            name = "Mutation",
            fields = [
                Define.Field(
                    "setMoon",
                    Nullable PlanetType,
                    "Defines if a planet is actually a moon or not.",
                    [ Define.Input("id", String); Define.Input("isMoon", Boolean) ],
                    fun ctx _ ->
                        getPlanet (ctx.Arg("id"))
                        |> Option.map (fun x ->
                            x.SetMoon(Some(ctx.Arg("isMoon"))) |> ignore
                            schemaConfig.SubscriptionProvider.Publish<Planet> "watchMoon" x // here you notify the subscribers upon a mutation
                            x))])
```

Finally run the server (e.g. make it listen at `localhost:8086`).

There's a demo chat application backend in the `samples/chat-app` folder that showcases the use of `FSharp.Data.GraphQL.Server.AspNetCore` in a real-time application scenario, that is: with usage of GraphQL subscriptions (but not only).
The tried and trusted `star-wars-api` also shows how to use subscriptions, but is a more basic example. As a side note, the implementation in `star-wars-api` was used as a starting point for the development of `FSharp.Data.GraphQL.Server.AspNetCore`.

### Client
Using your favorite (or not :)) client library (e.g.: [Apollo Client](https://www.apollographql.com/docs/react/get-started), [Relay](https://relay.dev), [Strawberry Shake](https://chillicream.com/docs/strawberryshake/v13), [elm-graphql](https://github.com/dillonkearns/elm-graphql) ❤️), just point to `localhost:8086/graphql` (as per the example above) and, as long as the client implements the `graphql-transport-ws` subprotocol, subscriptions should work.
