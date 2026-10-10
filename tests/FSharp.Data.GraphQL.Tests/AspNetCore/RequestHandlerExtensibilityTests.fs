module FSharp.Data.GraphQL.Tests.AspNetCore.RequestHandlerExtensibilityTests

open System
open System.IO
open System.Net.Http
open System.Text
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Options
open Xunit

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Server.AspNetCore
open FSharp.Data.GraphQL.Shared

/// A handler that records every call to the overridable execution steps, then defers to the base
/// implementation. Asserting on the recorded calls proves `HandleAsync` reaches these members through
/// virtual dispatch rather than through closed-over `let` functions, which no override could intercept.
type private RecordingHandler
    (httpContextAccessor : IHttpContextAccessor, options : IOptionsMonitor<GraphQLOptions<Root>>, logger : ILogger<RecordingHandler>) =
    inherit GraphQLRequestHandler<Root> (httpContextAccessor, options, logger)

    let introspectionCalls = ResizeArray<Ast.Document voption>()
    let operationCalls = ResizeArray<ParsedGQLQueryRequestContent>()

    member _.IntrospectionCalls = introspectionCalls
    member _.OperationCalls = operationCalls

    override _.ExecuteIntrospectionQuery (ast : Ast.Document voption) : Task<IResult> =
        introspectionCalls.Add ast
        base.ExecuteIntrospectionQuery ast

    override _.ExecuteOperation (content : ParsedGQLQueryRequestContent) : Task<IResult> =
        operationCalls.Add content
        base.ExecuteOperation content

/// A handler that overrides only `HandleAsync`, to check that `HandleAsync` itself is reached through a
/// `GraphQLRequestHandler` reference rather than always running the base class's own implementation.
type private SentinelHandler
    (httpContextAccessor : IHttpContextAccessor, options : IOptionsMonitor<GraphQLOptions<Root>>, logger : ILogger<SentinelHandler>) =
    inherit GraphQLRequestHandler<Root> (httpContextAccessor, options, logger)

    override _.HandleAsync () : Task<Result<IResult, IResult>> = Task.FromResult (Ok (TypedResults.Ok "sentinel" :> IResult))

let private requestBody (query : string) = JsonSerializer.Serialize {| query = query |}

/// Builds a handler of the given type wired through the real `AddGraphQL` DI registration (the same path
/// a hosted app uses), backed by an `HttpContext` whose request `configureRequest` sets up. Returns it both
/// as the base type (to prove `HandleAsync` dispatches virtually) and downcast to the concrete type (to
/// assert on what it recorded), the `HttpContext` (to execute the handler's result against), plus the DI
/// scope to dispose once the test is done with it.
let private createHandlerFor<'Handler when 'Handler :> GraphQLRequestHandler<Root> and 'Handler : not struct>
    (configureRequest : HttpRequest -> unit)
    : struct (GraphQLRequestHandler<Root> * 'Handler * HttpContext * IDisposable) =
    let services = ServiceCollection ()
    services.AddLogging () |> ignore
    services.AddGraphQL<Root, 'Handler>(TestSchema.executor, (fun _ -> { RequestId = "test" }))
    |> ignore
    let scope = services.BuildServiceProvider().CreateScope()
    let serviceProvider = scope.ServiceProvider

    let ctx = DefaultHttpContext (RequestServices = serviceProvider)
    ctx.Request.Path <- PathString "/graphql"
    configureRequest ctx.Request

    // The handler captures `httpContextAccessor.HttpContext` in its own constructor, so the accessor
    // must carry the request's HttpContext before the handler is resolved from the container.
    serviceProvider.GetRequiredService<IHttpContextAccessor>().HttpContext <- ctx

    let handler = serviceProvider.GetRequiredService<GraphQLRequestHandler<Root>>()
    struct (handler, (handler :?> 'Handler), (ctx :> HttpContext), (scope :> IDisposable))

let private setJsonBody (json : string) (request : HttpRequest) =
    request.ContentType <- "application/json"
    let bytes = Encoding.UTF8.GetBytes json
    request.Body <- new MemoryStream (bytes)
    request.ContentLength <- int64 bytes.Length

/// Builds a handler as `createHandlerFor` does, backed by an `HttpContext` with the given method and JSON body.
let private createHandler<'Handler when 'Handler :> GraphQLRequestHandler<Root> and 'Handler : not struct>
    (method : string)
    (body : string voption)
    : GraphQLRequestHandler<Root> * 'Handler * IDisposable =
    let struct (handler, concreteHandler, _, scope) =
        createHandlerFor<'Handler> (fun request ->
            request.Method <- method
            body |> ValueOption.iter (fun json -> setJsonBody json request))
    handler, concreteHandler, scope

let private assertOkResponse (outcome : Result<IResult, IResult>) =
    match outcome with
    | Ok iresult ->
        Assert.IsType<Microsoft.AspNetCore.Http.HttpResults.Ok<GQLResponse>> iresult
        |> ignore
    | Error iresult -> fail $"Expected HandleAsync to succeed, but it returned an error result: %A{iresult}"

[<Fact>]
let ``GET request is dispatched to overridden ExecuteIntrospectionQuery`` () : Task = task {
    let handler, recorder, scope = createHandler<RecordingHandler> HttpMethods.Get ValueNone
    use _ = scope

    let! outcome = handler.HandleAsync ()

    Assert.Equal (1, recorder.IntrospectionCalls.Count)
    Assert.Equal (ValueNone, recorder.IntrospectionCalls[0])
    Assert.Equal (0, recorder.OperationCalls.Count)
    assertOkResponse outcome
}

[<Fact>]
let ``POST introspection query is dispatched to overridden ExecuteIntrospectionQuery with the parsed document`` () : Task = task {
    let body = requestBody "{ __schema { queryType { name } } }"
    let handler, recorder, scope = createHandler<RecordingHandler> HttpMethods.Post (ValueSome body)
    use _ = scope

    let! outcome = handler.HandleAsync ()

    Assert.Equal (1, recorder.IntrospectionCalls.Count)
    let ast = wantValueSome recorder.IntrospectionCalls[0]
    Assert.False (ast.IsEmpty, "Expected the parsed introspection document to have at least one definition")
    Assert.Equal (0, recorder.OperationCalls.Count)
    assertOkResponse outcome
}

[<Fact>]
let ``POST operation query is dispatched to overridden ExecuteOperation`` () : Task = task {
    let query = "query { hero(id: \"1000\") { id name } }"
    let handler, recorder, scope =
        createHandler<RecordingHandler> HttpMethods.Post (ValueSome (requestBody query))
    use _ = scope

    let! outcome = handler.HandleAsync ()

    Assert.Equal (0, recorder.IntrospectionCalls.Count)
    Assert.Equal (1, recorder.OperationCalls.Count)
    Assert.Equal (query, recorder.OperationCalls[0].Query)
    assertOkResponse outcome
}

[<Fact>]
let ``Overridden HandleAsync is used when the handler is resolved as GraphQLRequestHandler`` () : Task = task {
    let handler, _, scope = createHandler<SentinelHandler> HttpMethods.Get ValueNone
    use _ = scope

    let! outcome = handler.HandleAsync ()

    match outcome with
    | Ok iresult ->
        let ok = Assert.IsType<Microsoft.AspNetCore.Http.HttpResults.Ok<string>> iresult
        Assert.Equal ("sentinel", ok.Value)
    | Error iresult -> fail $"Expected HandleAsync to succeed, but it returned an error result: %A{iresult}"
}

// Automatic persisted queries (APQ): Apollo Client's persisted query link first sends only a hash of the query in
// `extensions.persistedQuery` and falls back to the full query once the server answers `PersistedQueryNotSupported`.

let private persistedQueryName = "HeroName"
let private persistedQueryText = "query HeroName { hero(id: \"1000\") { name } }"

/// The extension Apollo Client's persisted query link sends: the SHA-256 of `persistedQueryText`
let private persistedQueryExtensions =
    """{"persistedQuery":{"version":1,"sha256Hash":"1cbc2ac2578b38723ede16940d5c898aac7cad1fd978cba363154afb07d130fc"}}"""

/// Sets the request up as a multipart request, the way file uploads are sent, carrying the given `operations` JSON
let private setMultipartOperations (operations : string) (request : HttpRequest) =
    use content = new MultipartFormDataContent ()
    content.Add (new StringContent (operations), "operations")
    content.Add (new StringContent ("{}"), "map")
    let body = new MemoryStream ()
    content.CopyTo (body, null, CancellationToken.None)
    body.Position <- 0L
    request.ContentType <- string content.Headers.ContentType
    request.Body <- body
    request.ContentLength <- body.Length

let private setQueryString (parameters : (string * string) list) (request : HttpRequest) =
    request.QueryString <-
        parameters
        |> List.fold (fun (queryString : QueryString) (name, value) -> queryString.Add (name, value)) QueryString.Empty

/// Executes the handler's result against its `HttpContext`, as the Giraffe and Oxpecker endpoints do, and returns the
/// status code and the body written to the response.
let private executeOutcome (ctx : HttpContext) (outcome : Result<IResult, IResult>) : Task<struct (int * string)> = task {
    let result =
        match outcome with
        | Ok result
        | Error result -> result
    use body = new MemoryStream ()
    ctx.Response.Body <- body
    do! result.ExecuteAsync ctx
    return struct (ctx.Response.StatusCode, Encoding.UTF8.GetString (body.ToArray ()))
}

let private wantMember (name : string) (responseBody : string) (element : JsonElement) =
    match element.TryGetProperty name with
    | true, value -> value
    | false, _ ->
        fail $"Expected a '%s{name}' member in %A{element.ValueKind} %s{element.GetRawText ()} of the response:\n%s{responseBody}"
        Unchecked.defaultof<_>

/// Asserts that the request was answered the way Apollo Server answers a persisted query when they are disabled: an HTTP 200
/// response, uncacheable, with a single `PersistedQueryNotSupported` error and no data, and that nothing was executed.
let private assertPersistedQueryNotSupported (recorder : RecordingHandler) (ctx : HttpContext) (outcome : Result<IResult, IResult>) = task {
    // A persisted query request is neither answered with the introspection result nor executed
    Assert.Empty recorder.IntrospectionCalls
    Assert.Empty recorder.OperationCalls

    let! struct (statusCode, responseBody) = executeOutcome ctx outcome

    Assert.True (
        (statusCode = StatusCodes.Status200OK),
        $"Expected HTTP 200, as Apollo Server answers an unsupported persisted query, but got HTTP %d{statusCode} with body:\n%s{responseBody}"
    )
    use document = JsonDocument.Parse responseBody
    let root = document.RootElement
    // Apollo Client 4 falls back to the full query only for a result without other top-level members than data, errors and
    // extensions, so the answer has the errors alone, as Apollo Server's
    let members = root.EnumerateObject () |> Seq.map _.Name |> List.ofSeq
    Assert.True ((members = [ "errors" ]), $"Expected the response to have the member 'errors' alone, but got:\n%s{responseBody}")
    let errors = root |> wantMember "errors" responseBody
    let error = Assert.Single (errors.EnumerateArray ())
    Assert.Equal ("PersistedQueryNotSupported", (error |> wantMember "message" responseBody).GetString ())
    let code = error |> wantMember "extensions" responseBody |> wantMember "code" responseBody
    Assert.Equal ("PERSISTED_QUERY_NOT_SUPPORTED", code.GetString ())
    Assert.Equal ("private, no-cache, must-revalidate", ctx.Response.Headers.CacheControl.ToString ())
}

[<Fact>]
let ``POST with only a persisted query hash is answered with PersistedQueryNotSupported`` () : Task = task {
    let body = $"""{{"operationName":"%s{persistedQueryName}","variables":{{}},"extensions":%s{persistedQueryExtensions}}}"""
    let struct (handler, recorder, ctx, scope) =
        createHandlerFor<RecordingHandler> (fun request ->
            request.Method <- HttpMethods.Post
            setJsonBody body request)
    use _ = scope

    let! outcome = handler.HandleAsync ()

    do! assertPersistedQueryNotSupported recorder ctx outcome
}

[<Fact>]
let ``GET with only a persisted query hash is answered with PersistedQueryNotSupported instead of the introspection result`` () : Task = task {
    let struct (handler, recorder, ctx, scope) =
        createHandlerFor<RecordingHandler> (fun request ->
            request.Method <- HttpMethods.Get
            request
            |> setQueryString [
                "operationName", persistedQueryName
                "variables", "{}"
                "extensions", persistedQueryExtensions
            ])
    use _ = scope

    let! outcome = handler.HandleAsync ()

    do! assertPersistedQueryNotSupported recorder ctx outcome
}

[<Fact>]
let ``POST with both a query and a persisted query hash is answered with PersistedQueryNotSupported as Apollo Server does`` () : Task = task {
    let body =
        $"""{{"query":%s{JsonSerializer.Serialize persistedQueryText},"operationName":"%s{persistedQueryName}","extensions":%s{persistedQueryExtensions}}}"""
    let struct (handler, recorder, ctx, scope) =
        createHandlerFor<RecordingHandler> (fun request ->
            request.Method <- HttpMethods.Post
            setJsonBody body request)
    use _ = scope

    let! outcome = handler.HandleAsync ()

    do! assertPersistedQueryNotSupported recorder ctx outcome
}

[<Fact>]
let ``Multipart POST with a persisted query hash in its operations is answered with PersistedQueryNotSupported`` () : Task = task {
    let operations = $"""{{"operationName":"%s{persistedQueryName}","extensions":%s{persistedQueryExtensions}}}"""
    let struct (handler, recorder, ctx, scope) =
        createHandlerFor<RecordingHandler> (fun request ->
            request.Method <- HttpMethods.Post
            setMultipartOperations operations request)
    use _ = scope

    let! outcome = handler.HandleAsync ()

    do! assertPersistedQueryNotSupported recorder ctx outcome
}

[<Fact>]
let ``Multipart POST operation is executed as before`` () : Task = task {
    let query = "query { hero(id: \"1000\") { id name } }"
    let struct (handler, recorder, _, scope) =
        createHandlerFor<RecordingHandler> (fun request ->
            request.Method <- HttpMethods.Post
            setMultipartOperations (requestBody query) request)
    use _ = scope

    let! outcome = handler.HandleAsync ()

    Assert.True (
        (recorder.OperationCalls.Count = 1),
        $"Expected the operation of a multipart request to be executed once, but ExecuteOperation was called %d{recorder.OperationCalls.Count} time(s); outcome: %A{outcome}"
    )
    Assert.Equal (query, recorder.OperationCalls[0].Query)
    Assert.Equal (0, recorder.IntrospectionCalls.Count)
    assertOkResponse outcome
}

[<Fact>]
let ``GET with both a query and a persisted query hash is answered with PersistedQueryNotSupported as Apollo Server does`` () : Task = task {
    let struct (handler, recorder, ctx, scope) =
        createHandlerFor<RecordingHandler> (fun request ->
            request.Method <- HttpMethods.Get
            request
            |> setQueryString [
                "query", persistedQueryText
                "operationName", persistedQueryName
                "extensions", persistedQueryExtensions
            ])
    use _ = scope

    let! outcome = handler.HandleAsync ()

    do! assertPersistedQueryNotSupported recorder ctx outcome
}

// Apollo Server tests `extensions.persistedQuery` for JavaScript truthiness, so null, false, "" and 0 do not ask for one
[<Theory>]
[<InlineData("null")>]
[<InlineData("""{"tracing":true}""")>]
[<InlineData("""{"persistedQuery":null}""")>]
[<InlineData("""{"persistedQuery":false}""")>]
[<InlineData("""{"persistedQuery":""}""")>]
[<InlineData("""{"persistedQuery":0}""")>]
[<InlineData("""{"persistedQuery":-0}""")>]
[<InlineData("""{"persistedQuery":1e-400}""")>]
[<InlineData("""{"persistedQuery":{},"persistedQuery":null}""")>]
[<InlineData("""{"tracing":{"persistedQuery":true}}""")>]
[<InlineData("""[]""")>]
let ``POST operation whose extensions do not ask for a persisted query is executed as before`` (extensions : string) : Task = task {
    let query = "query { hero(id: \"1000\") { id name } }"
    let body = $"""{{"query":%s{JsonSerializer.Serialize query},"extensions":%s{extensions}}}"""
    let handler, recorder, scope = createHandler<RecordingHandler> HttpMethods.Post (ValueSome body)
    use _ = scope

    let! outcome = handler.HandleAsync ()

    Assert.True (
        (recorder.OperationCalls.Count = 1),
        $"Expected the operation to be executed once despite extensions %s{extensions}, but ExecuteOperation was called %d{recorder.OperationCalls.Count} time(s)"
    )
    Assert.Equal (query, recorder.OperationCalls[0].Query)
    Assert.Equal (0, recorder.IntrospectionCalls.Count)
    assertOkResponse outcome
}

// The last of duplicate members counts, as in JSON.parse
[<Theory>]
[<InlineData("""{"persistedQuery":true}""")>]
[<InlineData("""{"persistedQuery":1}""")>]
[<InlineData("""{"persistedQuery":-1}""")>]
[<InlineData("""{"persistedQuery":1e400}""")>]
[<InlineData("""{"persistedQuery":"x"}""")>]
[<InlineData("""{"persistedQuery":[]}""")>]
[<InlineData("""{"tracing":{"nested":[1,{"persistedQuery":null}]},"persistedQuery":{}}""")>]
[<InlineData("""{"persistedQuery":null,"persistedQuery":{}}""")>]
let ``POST whose extensions ask for a persisted query in any JavaScript truthy way is answered with PersistedQueryNotSupported``
    (extensions : string)
    : Task =
    task {
        let body = $"""{{"query":%s{JsonSerializer.Serialize persistedQueryText},"extensions":%s{extensions}}}"""
        let struct (handler, recorder, ctx, scope) =
            createHandlerFor<RecordingHandler> (fun request ->
                request.Method <- HttpMethods.Post
                setJsonBody body request)
        use _ = scope

        let! outcome = handler.HandleAsync ()

        do! assertPersistedQueryNotSupported recorder ctx outcome
    }

// A value that is not JSON is ignored, as the GET request handling ignores every other parameter
[<Theory>]
[<InlineData("""{"tracing":true}""")>]
[<InlineData("""{"persistedQuery":false}""")>]
[<InlineData("""{"persistedQuery":0}""")>]
[<InlineData("""{"persistedQuery":""}""")>]
[<InlineData("null")>]
[<InlineData("not JSON")>]
let ``GET whose extensions do not ask for a persisted query is still answered with the introspection result`` (extensions : string) : Task = task {
    let struct (handler, recorder, _, scope) =
        createHandlerFor<RecordingHandler> (fun request ->
            request.Method <- HttpMethods.Get
            request |> setQueryString [ "extensions", extensions ])
    use _ = scope

    let! outcome = handler.HandleAsync ()

    Assert.True (
        (recorder.IntrospectionCalls.Count = 1),
        $"Expected the introspection query to be executed once, but ExecuteIntrospectionQuery was called %d{recorder.IntrospectionCalls.Count} time(s)"
    )
    Assert.Equal (ValueNone, recorder.IntrospectionCalls[0])
    Assert.Equal (0, recorder.OperationCalls.Count)
    assertOkResponse outcome
}
