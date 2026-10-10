namespace FSharp.Data.GraphQL.Server.AspNetCore

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Options
open Microsoft.Extensions.Primitives

open FSharp.Data.GraphQL
open FsToolkit.ErrorHandling

open FSharp.Data.GraphQL.Server
open FSharp.Data.GraphQL.Shared

[<AutoOpen>]
module private DeferredEventLogging =

    /// The path of a deferred event as one string for the log, its segments joined as GraphQL error paths print them
    let formatPath (path : obj list) = String.Join<obj> ("/", path)

/// <summary>
/// Recognizes requests for Apollo automatic persisted queries, which this server does not support.
/// </summary>
/// <remarks>
/// <para>
/// Apollo Client's persisted query link first sends only a hash of the query in <c>extensions.persistedQuery</c>, and sends
/// the full query instead once the server answers with the <c>PersistedQueryNotSupported</c> error.
/// </para>
/// <para>
/// The answer follows Apollo Server 5.5.1 with persisted queries disabled: <c>processGraphQLRequest</c> in
/// <c>packages/server/src/requestPipeline.ts</c> rejects every request whose <c>extensions.persistedQuery</c> is truthy,
/// whether it carries the query too or not, before looking at anything else, with the <c>PersistedQueryNotSupportedError</c>
/// of <c>packages/server/src/internalErrorClasses.ts</c>: HTTP 200 and <c>cache-control: private, no-cache, must-revalidate</c>.
/// </para>
/// <para>
/// The <c>subscribe</c> message of <c>graphql-transport-ws</c> is deliberately not checked: the protocol requires its payload
/// to carry the query, a payload without one is already rejected, and its query is always executed, never answered with the
/// introspection result. Apollo Client's <c>GraphQLWsLink</c> sends the full query together with the operation's extensions,
/// a persisted query hash included, and the <c>graphql-ws</c> reference server Apollo Server's subscription setup relies on
/// does not process persisted queries either, so rejecting such a payload would only break subscriptions that work.
/// </para>
/// </remarks>
module internal PersistedQueries =

    [<Literal>]
    let NotSupportedMessage = "PersistedQueryNotSupported"

    [<Literal>]
    let NotSupportedCode = "PERSISTED_QUERY_NOT_SUPPORTED"

    /// The member of the request extensions carrying the persisted query hash
    [<Literal>]
    let ExtensionName = "persistedQuery"

    /// The query string parameter of a GET request carrying the request extensions as JSON
    [<Literal>]
    let ExtensionsParameterName = "extensions"

    /// Forbids caching the answer, so that the full query the client sends next is not answered from a cache
    let notSupportedCacheControl = StringValues "private, no-cache, must-revalidate"

    /// The error Apollo Server answers with. Shared by every request, which is safe because both the record and its
    /// immutable extensions dictionary cannot be changed.
    let notSupportedError =
        let extensions = ImmutableDictionary.CreateRange (StringComparer.Ordinal, [ KeyValuePair ("code", box NotSupportedCode) ])
        GQLProblemDetails.Create (NotSupportedMessage, extensions :> IReadOnlyDictionary<string, obj>)

/// <summary>
/// Whether the <c>extensions</c> of a request ask for an Apollo automatic persisted query: whether their
/// <c>persistedQuery</c> member is truthy in JavaScript, which is how Apollo Server tests it.
/// </summary>
/// <remarks>
/// <para>
/// <see cref="PersistedQueryRequestConverter"/> reads it straight from the JSON of the request and skips every other value,
/// so that extensions of any size are never copied: a client could otherwise make every request allocate a copy of a
/// body as large as the server accepts.
/// </para>
/// <para>
/// It is a struct rather than a record: the converters of the serializer options take precedence over the converter of a
/// type, and the F# converter among them would read a record as one.
/// </para>
/// </remarks>
[<Struct; JsonConverter(typeof<PersistedQueryRequestConverter>)>]
type internal PersistedQueryRequest
    /// <param name="isRequested">Whether the extensions ask for a persisted query.</param>
    (isRequested : bool) =

    /// Whether the extensions ask for a persisted query
    member _.IsRequested = isRequested

    /// <summary>
    /// Whether the <c>extensions</c> query string parameter of a GET request asks for a persisted query. A value that is not
    /// valid JSON is ignored, as the GET request handling ignores every other parameter.
    /// </summary>
    static member IsRequestedByQueryString (extensions : StringValues) =
        extensions
        |> Seq.exists (fun value ->
            match value with
            | null -> false
            | value ->
                try
                    (JsonSerializer.Deserialize<PersistedQueryRequest> value).IsRequested
                with :? JsonException ->
                    false)

/// <summary>Reads a <see cref="PersistedQueryRequest"/> from the <c>extensions</c> of a request.</summary>
and [<Sealed>] internal PersistedQueryRequestConverter () =
    inherit JsonConverter<PersistedQueryRequest> ()

    /// <summary>
    /// Whether the JSON value the reader is at is truthy in JavaScript, leaving the reader at the end of the value
    /// </summary>
    static member private IsTruthy (reader : byref<Utf8JsonReader>) =
        match reader.TokenType with
        | JsonTokenType.StartObject
        | JsonTokenType.StartArray ->
            reader.Skip ()
            true
        | JsonTokenType.True -> true
        // The raw text of a string is empty only when the string is
        | JsonTokenType.String ->
            if reader.HasValueSequence then
                reader.ValueSequence.Length > 0L
            else
                reader.ValueSpan.Length > 0
        | JsonTokenType.Number ->
            match reader.TryGetDouble () with
            | true, number -> number <> 0.0
            // Out of the range of a double, so not zero
            | false, _ -> true
        | _ -> false

    override _.Read (reader, _, _) =
        let mutable isRequested = false
        if reader.TokenType = JsonTokenType.StartObject then
            // Each property name is followed by its value; with duplicate members the last one counts, as in JSON.parse
            while reader.Read () && reader.TokenType <> JsonTokenType.EndObject do
                let isPersistedQuery = reader.ValueTextEquals PersistedQueries.ExtensionName
                reader.Read () |> ignore
                if isPersistedQuery then
                    isRequested <- PersistedQueryRequestConverter.IsTruthy (&reader)
                else
                    reader.Skip ()
        else
            // Extensions that are not an object ask for nothing, as they hold no persistedQuery member
            reader.Skip ()
        PersistedQueryRequest isRequested

    override _.Write (_, _, _) = raise (NotSupportedException "Persisted query requests are only read.")

/// <summary>
/// The members of a GraphQL over HTTP request body that <see cref="GQLRequestContent"/> does not carry.
/// </summary>
/// <remarks>
/// Only the request handler reads it, alongside <see cref="GQLRequestContent"/>, so the public request type stays unchanged.
/// </remarks>
[<Struct>]
type internal GQLRequestEnvelope = {
    /// <summary>
    /// Whether the <c>extensions</c> member of the request asks for a persisted query, the only extension the handler reads.
    /// </summary>
    /// <remarks>
    /// <see cref="Skippable{T}"/>, as the optional members of <see cref="GQLRequestContent"/>, because the configured
    /// FSharp.SystemTextJson converter rejects a body that leaves out a value option member.
    /// </remarks>
    Extensions : PersistedQueryRequest Skippable
}

/// Handles GraphQL requests using a provided root schema.
type DefaultGraphQLRequestHandler<'Root>
    /// <summary>
    /// Initializes a new instance of the <see cref="DefaultGraphQLRequestHandler{T}"/> class.
    /// </summary>
    /// <param name="httpContextAccessor">The accessor to the current HTTP context.</param>
    /// <param name="options">The options monitor for GraphQL options.</param>
    /// <param name="logger">The logger to log messages.</param>
    (
        httpContextAccessor : IHttpContextAccessor,
        options : IOptionsMonitor<GraphQLOptions<'Root>>,
        logger : ILogger<DefaultGraphQLRequestHandler<'Root>>
    ) =
    inherit GraphQLRequestHandler<'Root> (httpContextAccessor, options, logger)

/// Provides logic to parse and execute GraphQL requests.
and [<AbstractClass>] GraphQLRequestHandler<'Root>
    /// <summary>
    /// Initializes a new instance of the <see cref="GraphQLRequestHandler{T}"/> class.
    /// </summary>
    /// <param name="httpContextAccessor">The accessor to the current HTTP context.</param>
    /// <param name="options">The options monitor for GraphQL options.</param>
    /// <param name="logger">The logger to log messages.</param>
    (httpContextAccessor : IHttpContextAccessor, options : IOptionsMonitor<GraphQLOptions<'Root>>, logger : ILogger) =

    let ctx = httpContextAccessor.HttpContext
    let getInputContext () = ctx.RequestServices.GetRequiredService<IInputExecutionContext>()

    let toResponse { DocumentId = documentId; Content = content; Metadata = metadata } =

        let serializeIndented value =
            let jsonSerializerOptions = options.Get(GraphQLOptions.IndentedOptionsName).SerializerOptions
            JsonSerializer.Serialize (value, jsonSerializerOptions)

        match content with
        | Direct (data, errs) ->
            logger.LogDebug ("Produced direct GraphQL response with documentId = '{documentId}' and metadata:\n{metadata}", documentId, metadata)

            if logger.IsEnabled LogLevel.Trace then
                logger.LogTrace ("GraphQL response data:\n{data}", serializeIndented (data |> ValueOption.toObj))

            GQLResponse.Direct (documentId, data |> ValueOption.toObj, errs)
        | Deferred (data, errs, deferred) ->
            logger.LogDebug ("Produced deferred GraphQL response with documentId = '{documentId}' and metadata:\n{metadata}", documentId, metadata)

            if logger.IsEnabled LogLevel.Debug then
                deferred
                |> Observable.add (function
                    | DeferredPending (path, label, isStream, _) ->
                        let fieldKind = if isStream then "streamed" else "deferred"
                        logger.LogDebug ("Announced GraphQL deferred field at path: {path}", formatPath path)
                        match label with
                        | ValueSome label -> logger.LogDebug ("Deferred field label: {label}; kind: {kind}", label, fieldKind)
                        | ValueNone -> logger.LogDebug ("Deferred field kind: {kind}", fieldKind)
                    | DeferredResult (data, path) ->
                        logger.LogDebug ("Produced GraphQL deferred result for path: {path}", formatPath path)

                        if logger.IsEnabled LogLevel.Trace then
                            logger.LogTrace ("GraphQL deferred data:\n{data}", serializeIndented data)
                    | DeferredErrors (data, errors, path) ->
                        logger.LogDebug ("Produced GraphQL deferred errors for path: {path}", formatPath path)

                        if logger.IsEnabled LogLevel.Trace then
                            logger.LogTrace ("GraphQL deferred errors:\n{errors}\nGraphQL deferred data:\n{data}", errors, serializeIndented data)
                    | DeferredCompleted path ->
                        logger.LogDebug ("Completed GraphQL deferred field at path: {path}", formatPath path)
                    | DeferredFragmentPending (path, label, fragmentId) ->
                        logger.LogDebug (
                            "Announced GraphQL deferred fragment #{fragmentId} (label: {label}) at path: {path}",
                            fragmentId,
                            label |> ValueOption.toObj,
                            formatPath path
                        )
                    | DeferredFragmentResult (data, errors, path, fragmentId) ->
                        logger.LogDebug ("Produced GraphQL deferred fragment #{fragmentId} result for path: {path}", fragmentId, formatPath path)

                        if logger.IsEnabled LogLevel.Trace then
                            logger.LogTrace ("GraphQL deferred fragment errors:\n{errors}\nGraphQL deferred fragment data:\n{data}", errors, serializeIndented (data |> ValueOption.toObj))
                    | DeferredFragmentCompleted (path, fragmentId) ->
                        logger.LogDebug ("Completed GraphQL deferred fragment #{fragmentId} at path: {path}", fragmentId, formatPath path))

            GQLResponse.Direct (documentId, data, errs)

        | Stream stream ->
            logger.LogDebug ("Produced stream GraphQL response with documentId = '{documentId}' and metadata:\n{metadata}", documentId, metadata)

            if logger.IsEnabled LogLevel.Debug then
                stream
                |> Observable.add (function
                    | SubscriptionResult data ->
                        logger.LogDebug ("Produced GraphQL subscription result")

                        if logger.IsEnabled LogLevel.Trace then
                            logger.LogTrace ("GraphQL subscription data:\n{data}", serializeIndented data)
                    | SubscriptionErrors (ValueNone, errors) ->
                        logger.LogDebug ("Produced GraphQL subscription errors")

                        if logger.IsEnabled LogLevel.Trace then
                            logger.LogTrace ("GraphQL subscription errors:\n{errors}", errors)
                    | SubscriptionErrors (ValueSome data, errors) ->
                        logger.LogDebug ("Produced GraphQL subscription result with errors")

                        if logger.IsEnabled LogLevel.Trace then
                            logger.LogTrace (
                                "GraphQL subscription errors:\n{errors}\nGraphQL subscription data:\n{data}",
                                errors,
                                serializeIndented data
                            ))

            GQLResponse.Stream documentId

        | RequestError errs ->
            logger.LogWarning (
                "Produced request error GraphQL response with documentId = '{documentId}' and metadata:\n{metadata}",
                documentId,
                metadata
            )

            GQLResponse.RequestError (documentId, errs)

    /// Checks if the request contains a body
    let checkIfHasBody (request : HttpRequest) = task {
        if request.Body.CanSeek then
            return (request.Body.Length > 0L)
        else
            request.EnableBuffering ()
            let body = request.Body
            let buffer = Array.zeroCreate 1
            let! bytesRead = body.ReadAsync (buffer, 0, 1)
            body.Seek (0, SeekOrigin.Begin) |> ignore
            return bytesRead > 0
    }

    /// Rejects a request for an Apollo automatic persisted query with the answer Apollo Server gives when they are disabled,
    /// so that Apollo Client's persisted query link sends the full query instead (see the PersistedQueries module)
    let ensureNotPersistedQuery (isPersistedQuery : bool) : Result<unit, IResult> =
        if not isPersistedQuery then
            Ok ()
        else
            logger.LogDebug "Request asks for an automatic persisted query, which is not supported"
            ctx.Response.Headers.CacheControl <- PersistedQueries.notSupportedCacheControl
            // Only the errors, as Apollo Server answers: Apollo Client 4 takes a result with any other top-level member than
            // data, errors and extensions for no GraphQL result at all, and then never falls back to the full query
            let response = {| errors = [ PersistedQueries.notSupportedError ] |}
            // HTTP 200 as Apollo Server answers, so that the client's handling of other statuses cannot mask the error;
            // the error result stops the request from going any further
            Error (TypedResults.Ok response :> IResult)

    /// Execute default or custom introspection query
    abstract ExecuteIntrospectionQuery : ast : Ast.Document voption -> Task<IResult>

    default _.ExecuteIntrospectionQuery (ast : Ast.Document voption) : Task<IResult> = task {
        let executor = options.CurrentValue.SchemaExecutor
        let! result =
            match ast with
            | ValueNone -> executor.AsyncExecute (IntrospectionQuery.Definition, getInputContext)
            | ValueSome ast -> executor.AsyncExecute (ast, getInputContext)

        let response = result |> toResponse
        return (TypedResults.Ok response) :> IResult
    }

    /// <summary>
    /// Check if the request is an introspection query
    /// by first checking on such properties as <c>GET</c> method or <c>empty request body</c>
    /// and lastly by parsing document AST for introspection operation definition.
    /// </summary>
    /// <remarks>
    /// <para>
    /// This consumes the request body: after JSON binding, the body stream position stays at its end,
    /// so calling this more than once (e.g. once from a derived handler and again from
    /// <see cref="HandleAsync"/>) will fail to bind the request on the second call.
    /// </para>
    /// <para>
    /// Apollo automatic persisted queries are not supported. A request whose <c>extensions.persistedQuery</c> is set to anything
    /// but <c>null</c>, <c>false</c>, <c>0</c> or <c>""</c> (the values JavaScript treats as false, as Apollo Server tests it),
    /// in the JSON body or in the <c>extensions</c> query string parameter of a <c>GET</c> request, whether it carries the query
    /// too or not, is answered as Apollo Server answers it with persisted queries disabled: the error result is an HTTP 200
    /// response with a single <c>PersistedQueryNotSupported</c> error whose <c>extensions.code</c> is
    /// <c>PERSISTED_QUERY_NOT_SUPPORTED</c>, and the <c>Cache-Control</c> header of the response is set to
    /// <c>private, no-cache, must-revalidate</c>.
    /// </para>
    /// </remarks>
    /// <returns>Result of check of <see cref="OperationType"/></returns>
    member _.CheckOperationType () = taskResult {

        let checkAnonymousFieldsOnly (ctx : HttpContext) = taskResult {
            // Read ahead of the request itself, which cannot be bound without the query a persisted query request leaves out
            let! envelope = ctx.TryBindJsonAsync<GQLRequestEnvelope>(GQLRequestContent.expectedJSON)
            do!
                envelope.Extensions
                |> Skippable.toValueOption
                |> ValueOption.exists _.IsRequested
                |> ensureNotPersistedQuery

            // Binding the envelope read a JSON body to its end, while a form's operations field is read again from the parsed form
            let body = ctx.Request.Body
            if body.CanSeek then
                body.Seek (0L, SeekOrigin.Begin) |> ignore

            let! gqlRequest = ctx.TryBindJsonAsync<GQLRequestContent>(GQLRequestContent.expectedJSON)
            let! ast = Parser.parseOrIResult ctx.Request.Path.Value gqlRequest.Query
            let operationName = gqlRequest.OperationName |> Skippable.toValueOption

            let createParsedContent () = {
                Query = gqlRequest.Query
                Ast = ast
                OperationName = gqlRequest.OperationName
                Variables = gqlRequest.Variables
            }
            if ast.IsEmpty then
                logger.LogTrace ("Request is not GET, but 'query' field is an empty string. Must be an introspection query")
                return IntrospectionQuery <| ValueNone
            else
                match Ast.tryFindOperationByName operationName ast with
                | None ->
                    logger.LogTrace "Document has no operation"
                    return IntrospectionQuery <| ValueNone
                | Some op ->
                    if not (op.OperationType = Ast.Query) then
                        logger.LogTrace "Document operation is not of type Query"
                        return createParsedContent () |> OperationQuery
                    else
                        let hasNonMetaFields =
                            Ast.containsFieldsBeyond
                                Ast.metaTypeFields
                                (fun field -> logger.LogTrace ("Operation Selection in Field with name: {fieldName}", field.Name))
                                (fun _ -> logger.LogTrace "Operation Selection is non-Field type")
                                op

                        if hasNonMetaFields then
                            return createParsedContent () |> OperationQuery
                        else
                            return IntrospectionQuery <| ValueSome ast
        }

        let request = ctx.Request

        if HttpMethods.Get = request.Method then
            do!
                request.Query[PersistedQueries.ExtensionsParameterName]
                |> PersistedQueryRequest.IsRequestedByQueryString
                |> ensureNotPersistedQuery
            logger.LogTrace ("Request is GET. Must be an introspection query")
            return IntrospectionQuery <| ValueNone
        else
            let! hasBody = checkIfHasBody request

            if not hasBody then
                logger.LogTrace ("Request is not GET, but has no body. Must be an introspection query")
                return IntrospectionQuery <| ValueNone
            else
                return! checkAnonymousFieldsOnly ctx
    }

    /// Execute the operation for given request
    abstract ExecuteOperation : content : ParsedGQLQueryRequestContent -> Task<IResult>

    default _.ExecuteOperation (content) = task {

        let operationName =
            content.OperationName
            |> Skippable.filter (not << isNull)
            |> Skippable.toValueOption
        let variables =
            content.Variables
            |> Skippable.filter (not << isNull)
            |> Skippable.toValueOption

        operationName
        |> ValueOption.iter (fun on -> logger.LogTrace ("GraphQL operation name: '{operationName}'", on))

        logger.LogTrace ("Executing GraphQL query:\n{query}", content.Query)

        variables
        |> ValueOption.iter (fun v -> logger.LogTrace ("GraphQL variables:\n{variables}", v))

        let root = options.CurrentValue.RootFactory ctx

        let! result =
            let executor = options.CurrentValue.SchemaExecutor
            Async.StartImmediateAsTask (
                executor.AsyncExecute (content.Ast, getInputContext, root, ?variables = variables, ?operationName = operationName),
                cancellationToken = ctx.RequestAborted
            )

        let response = result |> toResponse
        return (TypedResults.Ok response) :> IResult
    }

    /// Handle the request and return the result
    abstract HandleAsync : unit -> Task<Result<IResult, IResult>>

    default handler.HandleAsync () : Task<Result<IResult, IResult>> = taskResult {
        if ctx.RequestAborted.IsCancellationRequested then
            return TypedResults.Empty
        else
            match! handler.CheckOperationType () with
            | IntrospectionQuery optionalAstDocument -> return! handler.ExecuteIntrospectionQuery optionalAstDocument
            | OperationQuery content -> return! handler.ExecuteOperation (content)
    }
