# Magic Eight Ball

A minimal example of hosting a GraphQL schema with the `FSharp.Data.GraphQL.Server.Suave` package.

## Running

```bash
dotnet run --project samples/magic-eight-ball/magic-eight-ball.fsproj
```

The server listens on `http://localhost:8080` (Suave's `defaultConfig`). Ask it a question:

```bash
curl http://localhost:8080 \
    -H "Content-Type: application/json" \
    -d '{ "query": "{ ask(question: \"Will it build?\") }" }'
```

Or open [http://localhost:8080/graphiql](http://localhost:8080/graphiql) to ask it from the GraphiQL IDE. 

## Subscriptions (WebSockets)

The `shake` mutation notifies anyone subscribed to `onShake` with the new answer, over the `graphql-transport-ws`
protocol at `ws://localhost:8080/ws`. Open two tabs of the GraphiQL IDE above to see it live: run

```graphql
subscription { onShake }
```

in one, then run

```graphql
mutation { shake }
```

in the other - the answer shows up in the first tab as soon as it's published.
