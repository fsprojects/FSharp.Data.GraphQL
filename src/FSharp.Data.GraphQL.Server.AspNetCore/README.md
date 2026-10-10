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
                .AddGraphQL<Root>( // STEP 1: Setting the options
                    Schema.executor, // --> Schema.executor is defined by yourself somewhere else (in another file)
                    rootFactory,
                    "/ws" // --> endpoint for websocket connections (optional. Default value: "/ws")
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
            .UseGiraffe (HttpHandlers.handleGraphQL<Root>)

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
                    [ Define.Input("id", StringType) ],
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
                    [ Define.Input("id", StringType); Define.Input("isMoon", BooleanType) ],
                    fun ctx _ ->
                        getPlanet (ctx.Arg("id"))
                        |> Option.map (fun x ->
                            x.SetMoon(Some(ctx.Arg("isMoon"))) |> ignore
                            schemaConfig.SubscriptionProvider.Publish<Planet> "watchMoon" x // here you notify the subscribers upon a mutation
                            x))])
```

Finally run the server (e.g. make it listen at `localhost:8086`).

### Cross-site request forgery (CSRF) prevention

A browser sends some requests to another site without asking that site first through a [CORS preflight](https://developer.mozilla.org/en-US/docs/Glossary/Preflight_request), and with the cookies of that site: a `GET`, `HEAD` or `POST` request without custom headers whose `Content-Type` is missing or one of `application/x-www-form-urlencoded`, `multipart/form-data` and `text/plain`. A page of any other site could use such a request to run an operation on behalf of a user signed in to your API.

Like Apollo Server's `csrfPrevention`, the GraphQL HTTP handler therefore rejects every request with such a `Content-Type`, or without one, with `400 Bad Request` and a GraphQL error whose `extensions.code` is `BAD_REQUEST`, unless it carries a non-empty value in one of these headers:

* `GraphQL-Preflight`, which the GraphQL client provider of this library sends with its `GET` requests and file uploads
* `Apollo-Require-Preflight` and `X-Apollo-Operation-Name`, the headers Apollo Server accepts, so that clients written for it work unchanged

A browser never sends a custom header to another site without a preflight, which your CORS policy then allows or refuses. The method of a request is not checked, since a middleware overriding the method from a form field, such as `UseHttpMethodOverride`, would turn a form posted from another site into a request with any method; only a CORS preflight itself always passes. A request with `Content-Type: application/json`, which is what most GraphQL clients send, is not affected. A client that sends file uploads (`multipart/form-data`) or introspects the schema through `GET` must send one of the headers, for example `GraphQL-Preflight: 1`. If browsers of other origins call your API, also allow that header in the CORS `Access-Control-Allow-Headers` response header.

WebSocket connections are handled by the WebSocket middleware and are not checked. A browser opens a WebSocket connection to another site with the cookies of that site too, so a server that authenticates its WebSocket connections through cookies must check their `Origin` header itself.

The protection is on by default. Configure it through `GraphQLOptions.CsrfPrevention`, either to accept other headers or, for a server that is never called by browsers or that does not authenticate requests through cookies, to turn it off:

```fsharp
services.AddGraphQL<Root> (
    Schema.executor,
    rootFactory,
    // Accept only GraphQL-Preflight and X-Requested-With
    configure = fun options -> { options with CsrfPrevention = ValueSome { RequestHeaders = ImmutableHashSet.Create (CsrfPreventionHeaders.GraphQLPreflight, "X-Requested-With") } }
    // Or turn the protection off
    // configure = fun options -> { options with CsrfPrevention = ValueNone }
)
```

A custom `GraphQLRequestHandler<'Root>` that overrides `HandleAsync` without calling the base implementation should call `CheckCsrfPrevention ()` first.

There's a demo chat application backend in the `samples/chat-app` folder that showcases the use of `FSharp.Data.GraphQL.Server.AspNetCore` in a real-time application scenario, that is: with usage of GraphQL subscriptions (but not only).
The tried and trusted `star-wars-api` also shows how to use subscriptions, but is a more basic example in that regard. As a side note, the implementation in `star-wars-api` was used as a starting point for the development of `FSharp.Data.GraphQL.Server.AspNetCore`.

### Client
Using your favorite (or not :)) client library (e.g.: [Apollo Client](https://www.apollographql.com/docs/react/get-started), [Relay](https://relay.dev), [Strawberry Shake](https://chillicream.com/docs/strawberryshake/v13), [elm-graphql](https://github.com/dillonkearns/elm-graphql) ❤️), just point to `localhost:8086/graphql` (as per the example above) and, as long as the client implements the `graphql-transport-ws` subprotocol, subscriptions should work.
