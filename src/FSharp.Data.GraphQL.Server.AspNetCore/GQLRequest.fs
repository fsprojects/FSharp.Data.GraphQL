namespace FSharp.Data.GraphQL.Server.AspNetCore

open System.Collections.Immutable
open System.Text.Json
open System.Text.Json.Serialization
open FSharp.Data.GraphQL

[<Struct>]
type public GQLRequestContent = {
    Query: string
    OperationName: string Skippable
    Variables: ImmutableDictionary<string, JsonElement> Skippable
}

module GQLRequestContent =

    let expectedJSON = """{
        "query": "query { ... }",
        "operationName": "operationName", // when multiple operations defined in query
        "variables": { "variableName": "value" } // when query has parameters
    }"""

[<Struct>]
type public ParsedGQLQueryRequestContent = {
    Query: string
    Ast: Ast.Document
    OperationName: string Skippable
    Variables: ImmutableDictionary<string, JsonElement> Skippable
}

[<Struct>]
type public OperationType =
    | IntrospectionQuery of Introspection: Ast.Document voption
    | OperationQuery of Operation: ParsedGQLQueryRequestContent
