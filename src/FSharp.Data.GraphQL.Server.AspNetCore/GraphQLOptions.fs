namespace FSharp.Data.GraphQL.Server.AspNetCore

open FSharp.Data.GraphQL
open System
open System.Text.Json
open System.Threading.Tasks
open Microsoft.AspNetCore.Http

type PingHandler = IServiceProvider -> JsonDocument voption -> Task<JsonDocument voption>

[<RequireQualifiedAccess>]
module GraphQLOptionsDefaults =

    let [<Literal>] ReadBufferSize = 4096
    let [<Literal>] WebSocketEndpoint = "/ws"
    let [<Literal>] WebSocketConnectionInitTimeoutInMs = 3000.0
    /// <summary>The default of <see cref="GraphQLTransportWSOptions.MaxReceiveMessageSize"/>: 4 MiB.</summary>
    let [<Literal>] WebSocketMaxReceiveMessageSize = 4 * 1024 * 1024

module GraphQLOptions =

    let [<Literal>] IndentedOptionsName = "Indented"

type GraphQLTransportWSOptions = {
    EndpointUrl : string
    ConnectionInitTimeout : TimeSpan
    CustomPingHandler : PingHandler voption
    /// <summary>
    /// The maximum size in bytes of a message a client may send, <see cref="GraphQLOptionsDefaults.WebSocketMaxReceiveMessageSize"/> by default.
    /// <para>
    /// A message is only buffered up to this size: as soon as a message exceeds it, the connection stops reading from the socket and is closed with
    /// <see cref="System.Net.WebSockets.WebSocketCloseStatus.MessageTooBig"/> (1009). Must be positive.
    /// </para>
    /// </summary>
    MaxReceiveMessageSize : int
}

type IGraphQLOptions =
    abstract member SerializerOptions : JsonSerializerOptions
    abstract member WebsocketOptions : GraphQLTransportWSOptions

type GraphQLOptions<'Root> = {
    mutable SchemaExecutor : Executor<'Root>
    mutable RootFactory : HttpContext -> 'Root
    /// The minimum rented array size to read a message from WebSocket
    ReadBufferSize : int
    SerializerOptions : JsonSerializerOptions
    WebsocketOptions : GraphQLTransportWSOptions
} with

    interface IGraphQLOptions with
        member this.SerializerOptions = this.SerializerOptions
        member this.WebsocketOptions = this.WebsocketOptions

