module FSharp.Data.GraphQL.Tests.Literals

open FSharp.Data.LiteralProviders

let [<Literal>] IntrospectionSchemaJson =
    TextFile<"../FSharp.Data.GraphQL.IntegrationTests/introspection.json", EnsureExists = true>.Text
