Using FSharp.Data.GraphQL on the client side
========================

FSharp.Data.GraphQL offers a client-side library, which can be used to query **any** GraphQL-compatible server. It includes a type provider (erasing one), which is able to introspect remotelly a schema of the server it connects to, and give you the ability to make typesafe queries from the client side. To use GraphQL type provider from the client side, simply supply it an endpoint URL of target GraphQL server:

```fsharp
open FSharp.Data.GraphQL
type GraphQlClient = GraphQLProvider<"http://graphql-server.com">
```

This way we get type provider, which exposes top level queries and mutations in form of the methods and fields. F# client also gives a custom DSL that alows to construct GraphQL queries programatically. For example, client may execute GraphQL query string as defined below:

```graphql
async {
    let! data = GraphQlClient.Query """
        {
            hero(id: "1000") {
                name,
                friends {
                    name
                }
                ... on Human {
                    appearsIn
                }
            }
        }
        """
    return data
}
```

However this may not be a typesafe way of working with type providers. Other approach is to use GraphQL query DSL for that:

```fsharp
async {
    let! data = GraphQlClient.Queries.Hero("1000", fun c -> Fields(
            c.name,
            Selection(c.friends, fun f -> Fields(
                f.name
            )),
            GraphQlClient.Types.Human.On(fun h -> Fields(
                h.appearsIn
            ))
        ))
    return data
}
```

GraphQL client is fully compatible with [Fable compiler](https://fable-compiler.github.io/), which gives you a possiblity to write F# applications targeted for the browsers.