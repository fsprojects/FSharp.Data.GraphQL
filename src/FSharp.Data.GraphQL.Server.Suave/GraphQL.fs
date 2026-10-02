/// <summary>
/// Suave <see cref="Suave.Http.WebPart"/>s that serve a GraphQL schema over HTTP and, for subscriptions, over the
/// <c>graphql-transport-ws</c> WebSocket protocol.
/// </summary>
module FSharp.Data.GraphQL.Server.Suave.GraphQL

open Suave
open Suave.Http
open Suave.Operators

open FSharp.Data.GraphQL

/// <summary>
/// Builds a <see cref="Suave.Http.WebPart"/> that serves GraphQL queries, mutations and introspection over HTTP
/// (<c>GET</c> and <c>POST</c>, including the GraphQL multipart request specification for file uploads), and
/// GraphQL subscriptions over the <c>graphql-transport-ws</c> WebSocket protocol, at the WebSocket endpoint
/// configured in <paramref name="options"/>.
/// </summary>
let graphQLWithOptions (options : GraphQLOptions<'Root>) : WebPart =
    choose [
        Filters.path options.WebsocketOptions.EndpointUrl
        >=> GraphQLWebSocketHandler.handleGraphQLWebSocket options
        (Filters.GET <|> Filters.POST)
        >=> GraphQLHttpHandler.handleGraphQL options
    ]

/// <summary>
/// Builds a <see cref="Suave.Http.WebPart"/> that serves GraphQL queries, mutations, introspection and (over the
/// <c>graphql-transport-ws</c> WebSocket protocol, at the default <c>/ws</c> endpoint) subscriptions, for the given
/// schema executor and root value factory.
/// </summary>
let graphQL (executor : Executor<'Root>) (rootFactory : HttpContext -> 'Root) : WebPart =
    graphQLWithOptions (GraphQLOptions.create executor rootFactory)
