## Usage

### Server

```fsharp
open Suave
open FSharp.Data.GraphQL.Server.Suave

// Factory for object holding request-wide info. You define Root somewhere else.
let rootFactory (_ctx : Http.HttpContext) : Root =
    { RequestId = System.Guid.NewGuid().ToString() }

// --> Schema.executor is defined by yourself somewhere else (in another file)
let app : WebPart = GraphQL.graphQL Schema.executor rootFactory

startWebServer defaultConfig app
```

This single `WebPart` serves GraphQL queries, mutations and introspection for every `GET`/`POST` request it
receives (including the GraphQL multipart request specification for file uploads), and accepts
`graphql-transport-ws` WebSocket connections (used for subscriptions) at `/ws` by default. Compose it with Suave's
usual routing combinators (`path`, `pathStarts`, `choose`, ...) the way you would any other `WebPart` – just make
sure the WebSocket endpoint configured in `GraphQLOptions.WebsocketOptions.EndpointUrl` (an absolute path) is
still reachable through whatever routing you put in front of it.

To customize the WebSocket endpoint path, the connection init timeout, a custom `ping`/`pong` handler or the
JSON serializer options, build a `GraphQLOptions<'Root>` and call `GraphQL.graphQLWithOptions` instead:

```fsharp
let defaultOptions = GraphQLOptions.create Schema.executor rootFactory

let options =
    { defaultOptions with
        WebsocketOptions = { defaultOptions.WebsocketOptions with EndpointUrl = "/subscriptions" } }

let app : WebPart = GraphQL.graphQLWithOptions options
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

### Client

Using your favorite (or not :)) client library (e.g.: [Apollo Client](https://www.apollographql.com/docs/react/get-started), [Relay](https://relay.dev), [Strawberry Shake](https://chillicream.com/docs/strawberryshake/v13), [elm-graphql](https://github.com/dillonkearns/elm-graphql) ❤️), just point to your server's address (`localhost:8080` for `defaultConfig`, as per the example above) and, as long as the client implements the `graphql-transport-ws` subprotocol, subscriptions should work.
