namespace FSharp.Data.GraphQL.Server.Suave

open System
open System.Text.Json
open Suave.Http

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Shared

/// A custom handler invoked for the <c>ping</c>/<c>pong</c> messages of the <c>graphql-transport-ws</c> protocol.
type PingHandler = JsonDocument voption -> Async<JsonDocument voption>

/// Default values used to configure <see cref="GraphQLOptions{Root}"/>.
[<RequireQualifiedAccess>]
module GraphQLOptionsDefaults =

    /// The default path at which <c>graphql-transport-ws</c> WebSocket connections are accepted.
    [<Literal>]
    let WebSocketEndpoint = "/ws"

    /// The default timeout, in milliseconds, to wait for a <c>connection_init</c> message before closing the socket.
    [<Literal>]
    let WebSocketConnectionInitTimeoutInMs = 3000.0

/// <summary>Options that configure the <c>graphql-transport-ws</c> WebSocket protocol.</summary>
[<NoComparison>]
[<NoEquality>]
type GraphQLTransportWSOptions = {
    /// <summary>The path at which WebSocket connections for GraphQL subscriptions are accepted.</summary>
    EndpointUrl : string
    /// <summary>How long to wait for a <c>connection_init</c> message from the client before closing the socket.</summary>
    ConnectionInitTimeout : TimeSpan
    /// <summary>An optional custom handler invoked for <c>ping</c>/<c>pong</c> messages.</summary>
    CustomPingHandler : PingHandler voption
}

/// <summary>Options used to configure the GraphQL <see cref="Suave.Http.WebPart"/>s exposed by this library.</summary>
[<NoComparison>]
[<NoEquality>]
type GraphQLOptions<'Root> = {
    /// <summary>The schema executor used to run GraphQL operations.</summary>
    SchemaExecutor : Executor<'Root>
    /// <summary>Builds the root value used to execute a GraphQL operation from the current <see cref="Suave.Http.HttpContext"/>.</summary>
    RootFactory : HttpContext -> 'Root
    /// <summary>The <see cref="System.Text.Json.JsonSerializerOptions"/> used to (de)serialize GraphQL requests, responses and WebSocket messages.</summary>
    SerializerOptions : JsonSerializerOptions
    /// <summary>Options for the <c>graphql-transport-ws</c> WebSocket protocol.</summary>
    WebsocketOptions : GraphQLTransportWSOptions
}

/// <summary>Functions to create <see cref="GraphQLOptions{Root}"/>.</summary>
module GraphQLOptions =

    /// <summary>Creates <see cref="GraphQLOptions{Root}"/> with default settings for the given executor and root factory.</summary>
    let create (executor : Executor<'Root>) (rootFactory : HttpContext -> 'Root) : GraphQLOptions<'Root> = {
        SchemaExecutor = executor
        RootFactory = rootFactory
        SerializerOptions = Json.getWSSerializerOptions Seq.empty
        WebsocketOptions = {
            EndpointUrl = GraphQLOptionsDefaults.WebSocketEndpoint
            ConnectionInitTimeout = TimeSpan.FromMilliseconds GraphQLOptionsDefaults.WebSocketConnectionInitTimeoutInMs
            CustomPingHandler = ValueNone
        }
    }
