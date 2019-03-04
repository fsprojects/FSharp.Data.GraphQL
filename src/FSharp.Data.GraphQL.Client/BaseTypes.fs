namespace FSharp.Data.GraphQL

open FSharp.Data.GraphQL.Types.Introspection

type GraphQLContextBase(serverUrl : string, schema : IntrospectionSchema) =
    member __.ServerUrl = serverUrl
    member __.Schema = schema