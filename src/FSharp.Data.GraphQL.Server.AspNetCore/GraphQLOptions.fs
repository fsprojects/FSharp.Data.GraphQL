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
    let [<Literal>] MaskUnexpectedErrors = true

module GraphQLOptions =

    let [<Literal>] IndentedOptionsName = "Indented"

type GraphQLTransportWSOptions = {
    EndpointUrl : string
    ConnectionInitTimeout : TimeSpan
    CustomPingHandler : PingHandler voption
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
    /// <summary>
    /// Whether an error caused by an unexpected exception reaches the client with a generic message instead of the
    /// exception's own message; <see langword="true"/> by default.
    /// </summary>
    /// <remarks>
    /// <para>
    /// An error is unexpected when its <see cref="GQLProblemDetails.Exception"/> is neither a
    /// <see cref="GraphQLException"/> nor an <see cref="IGQLError"/>, such as a <see cref="GQLMessageException"/>,
    /// nor an <see cref="AggregateException"/> of only such exceptions. An error without an exception, such as a
    /// validation error or one added through
    /// <see cref="FSharp.Data.GraphQL.Types.ResolveFieldContext.AddError(System.String)"/>, is never masked; neither is
    /// an error that a custom <see cref="SchemaConfig.ParseError"/> returns without an exception.
    /// </para>
    /// <para>
    /// A masked error gets the message <c>Unexpected error</c> and keeps its <c>path</c>, its <c>locations</c> and its
    /// <c>kind</c> extension, but no other extension; its exception is logged at the error level instead. Masking
    /// applies to HTTP responses and to the <c>next</c> and <c>error</c> messages of <c>graphql-transport-ws</c>,
    /// including the incremental payloads of <c>@defer</c> and <c>@stream</c>. The failure of a subscription's source
    /// is reported as <c>Unexpected error during subscription</c> while masking is on.
    /// </para>
    /// <para>Set it to <see langword="false"/> only for development: exception messages can disclose implementation details.</para>
    /// </remarks>
    MaskUnexpectedErrors : bool
} with

    interface IGraphQLOptions with
        member this.SerializerOptions = this.SerializerOptions
        member this.WebsocketOptions = this.WebsocketOptions

