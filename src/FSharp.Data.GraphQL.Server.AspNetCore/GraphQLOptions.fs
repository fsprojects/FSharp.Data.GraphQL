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

    /// <summary>Whether errors caused by unexpected exceptions are masked by default; see <see cref="GraphQLOptions{Root}.MaskUnexpectedErrors"/>.</summary>
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
    /// <c>kind</c> extension, but no other extension. Its exception is logged instead: the exceptions masked in one
    /// response or message are logged once at the error level, with their number, their first paths and the first
    /// exception, so that a client cannot multiply the entries of the log, and each one at the debug level. An error of
    /// an <see cref="IGQLError"/> exception that reports the message of the exception gets the message the exception
    /// declares for clients instead.
    /// </para>
    /// <para>
    /// Masking applies to HTTP responses and to the <c>next</c> and <c>error</c> messages of <c>graphql-transport-ws</c>,
    /// the incremental payloads of <c>@defer</c> and <c>@stream</c> included. An unexpected failure of a subscription's
    /// source is reported as <c>Unexpected error during subscription</c> while masking is on.
    /// </para>
    /// <para>Set it to <see langword="false"/> only for development: exception messages can disclose implementation details.</para>
    /// </remarks>
    MaskUnexpectedErrors : bool
} with

    interface IGraphQLOptions with
        member this.SerializerOptions = this.SerializerOptions
        member this.WebsocketOptions = this.WebsocketOptions

