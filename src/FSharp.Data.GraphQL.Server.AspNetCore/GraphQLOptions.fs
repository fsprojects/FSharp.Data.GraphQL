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

module GraphQLOptions =

    let [<Literal>] IndentedOptionsName = "Indented"

type GraphQLTransportWSOptions = {
    EndpointUrl : string
    ConnectionInitTimeout : TimeSpan
    CustomPingHandler : PingHandler voption
}

/// The names of the request headers that show a GraphQL server a browser would have sent the request only after a CORS preflight.
[<RequireQualifiedAccess>]
module CsrfPreventionHeaders =

    /// <summary>
    /// The header the GraphQL client provider of this library sends, with the value <c>1</c>, with every request a browser
    /// would send without a preflight: <c>GET</c> requests and multipart file uploads.
    /// </summary>
    [<Literal>]
    let GraphQLPreflight = "GraphQL-Preflight"

    /// The header Apollo Server's CSRF prevention accepts from clients that send otherwise simple requests, such as file uploads.
    [<Literal>]
    let ApolloRequirePreflight = "Apollo-Require-Preflight"

    /// The header with the operation name that Apollo Server's CSRF prevention accepts, and some Apollo clients send.
    [<Literal>]
    let ApolloOperationName = "X-Apollo-Operation-Name"

/// <summary>
/// The protection of the GraphQL HTTP endpoint against cross-site request forgery (CSRF), modeled on Apollo Server's
/// <c>csrfPrevention</c>.
/// <para>
/// A browser sends a <c>GET</c>, <c>HEAD</c> or <c>POST</c> request to another site without asking it first through a CORS
/// preflight when the request has no custom headers and its <c>Content-Type</c> is either missing or one of
/// <c>application/x-www-form-urlencoded</c>, <c>multipart/form-data</c> and <c>text/plain</c>. Such a request carries the
/// cookies of the site it goes to, so a page of any other site could run an operation on behalf of the user signed in there,
/// even though it cannot read the response.
/// </para>
/// <para>
/// The request handler therefore rejects every request with such a <c>Content-Type</c> with <c>400 Bad Request</c>, unless
/// it carries one of the <see cref="RequestHeaders"/> with a non-empty value. A browser never sends a custom header to
/// another site without a preflight, which the CORS policy of the server then allows or refuses. The method of a request is
/// not checked, since a middleware overriding the method from a form field would turn a form posted from another site into
/// a request with any method; only a CORS preflight itself always passes.
/// </para>
/// </summary>
type CsrfPreventionOptions = {
    /// <summary>
    /// The names of the request headers that let a request a browser would not preflight through, when any of them has a
    /// non-empty value. Header names are case-insensitive. An empty list rejects every such request, so that only requests
    /// with another <c>Content-Type</c>, such as <c>application/json</c>, are executed.
    /// </summary>
    RequestHeaders : string list
} with

    /// <summary>
    /// The default protection: a request a browser would not preflight passes with a non-empty
    /// <see cref="CsrfPreventionHeaders.GraphQLPreflight"/>, <see cref="CsrfPreventionHeaders.ApolloRequirePreflight"/> or
    /// <see cref="CsrfPreventionHeaders.ApolloOperationName"/> header.
    /// </summary>
    /// <remarks>
    /// Apollo's header names are accepted too, so that a client written for Apollo Server's CSRF prevention works unchanged:
    /// a custom header of any name makes a browser preflight the request, so each of them protects equally well.
    /// </remarks>
    static member Default = {
        RequestHeaders = [
            CsrfPreventionHeaders.GraphQLPreflight
            CsrfPreventionHeaders.ApolloRequirePreflight
            CsrfPreventionHeaders.ApolloOperationName
        ]
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
    /// The protection of the HTTP endpoint against cross-site request forgery, <see cref="CsrfPreventionOptions.Default"/>
    /// unless configured otherwise. <c>ValueNone</c> turns it off, for example for a server that only serves clients
    /// other than browsers or that authenticates requests without cookies.
    /// </summary>
    CsrfPrevention : CsrfPreventionOptions voption
} with

    interface IGraphQLOptions with
        member this.SerializerOptions = this.SerializerOptions
        member this.WebsocketOptions = this.WebsocketOptions

