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
    let [<Literal>] WebSocketConnectionInitTimeoutInMs = 3000

type GraphQLTransportWSOptions = {
    EndpointUrl : string
    ConnectionInitTimeout : TimeSpan
    CustomPingHandler : PingHandler voption
}

type IGraphQLOptions =
    abstract member SerializerOptions : JsonSerializerOptions
    abstract member WebsocketOptions : GraphQLTransportWSOptions

type GraphQLOptions<'Root> = {
    SchemaExecutor : Executor<'Root>
    RootFactory : HttpContext -> 'Root
    /// The minimum rented array size to read a message from WebSocket
    ReadBufferSize : int
    SerializerOptions : JsonSerializerOptions
    WebsocketOptions : GraphQLTransportWSOptions
} with

    interface IGraphQLOptions with
        member this.SerializerOptions = this.SerializerOptions
        member this.WebsocketOptions = this.WebsocketOptions

