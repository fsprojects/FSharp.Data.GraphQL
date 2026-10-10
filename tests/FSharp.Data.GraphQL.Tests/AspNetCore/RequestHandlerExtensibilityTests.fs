module FSharp.Data.GraphQL.Tests.AspNetCore.RequestHandlerExtensibilityTests

open System
open System.IO
open System.Text
open System.Text.Json
open System.Text.Json.Serialization
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
/// a hosted app uses), with the options adjusted by `configure`, backed by an `HttpContext` with the given
/// method and JSON body. Returns it both as the base type (to prove `HandleAsync` dispatches virtually) and
/// downcast to the concrete type (to assert on what it recorded), plus the DI scope to dispose once the test
/// is done with it.
let private createConfiguredHandler<'Handler when 'Handler :> GraphQLRequestHandler<Root> and 'Handler : not struct>
    (configure : GraphQLOptions<Root> -> GraphQLOptions<Root>)
    (method : string)
    (body : string voption)
    : GraphQLRequestHandler<Root> * 'Handler * IDisposable =
    let services = ServiceCollection ()
    services.AddLogging () |> ignore
    services.AddGraphQL<Root, 'Handler>(TestSchema.executor, (fun _ -> { RequestId = "test" }), Func<_, _> configure)
    |> ignore
    let scope = services.BuildServiceProvider().CreateScope()
    let serviceProvider = scope.ServiceProvider

    let ctx = DefaultHttpContext (RequestServices = serviceProvider)
    ctx.Request.Method <- method
    ctx.Request.Path <- PathString "/graphql"

    body
    |> ValueOption.iter (fun json ->
        ctx.Request.ContentType <- "application/json"
        let bytes = Encoding.UTF8.GetBytes (json : string)
        ctx.Request.Body <- new MemoryStream (bytes)
        ctx.Request.ContentLength <- int64 bytes.Length)

    // The handler captures `httpContextAccessor.HttpContext` in its own constructor, so the accessor
    // must carry the request's HttpContext before the handler is resolved from the container.
    serviceProvider.GetRequiredService<IHttpContextAccessor>().HttpContext <- ctx

    let handler = serviceProvider.GetRequiredService<GraphQLRequestHandler<Root>>()
    handler, (handler :?> 'Handler), (scope :> IDisposable)

/// As createConfiguredHandler, with the options AddGraphQL produces by default
let private createHandler<'Handler when 'Handler :> GraphQLRequestHandler<Root> and 'Handler : not struct>
    (method : string)
    (body : string voption)
    =
    createConfiguredHandler<'Handler> id method body

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

/// Executes the operation through the default handler with the options adjusted by configure, returning the response
/// and the JSON an HTTP client receives for it
let private executeOverHttp (configure : GraphQLOptions<Root> -> GraphQLOptions<Root>) (query : string) : Task<struct (GQLResponse * string)> = task {
    let handler, _, scope =
        createConfiguredHandler<DefaultGraphQLRequestHandler<Root>> configure HttpMethods.Post (ValueSome (requestBody query))
    use _ = scope

    match! handler.HandleAsync () with
    | Ok iresult ->
        let response = (Assert.IsType<Microsoft.AspNetCore.Http.HttpResults.Ok<GQLResponse>> iresult).Value
        return struct (response, JsonSerializer.Serialize (response, Json.getSerializerOptions Seq.empty))
    | Error iresult -> return failwith $"Expected HandleAsync to succeed, but it returned an error result: %A{iresult}"
}

let private errorsOf (response : GQLResponse) =
    match response.Errors with
    | Include errors -> errors
    | Skip -> failwith $"Expected the response to carry errors, but it has none: %A{response}"

[<Fact>]
let ``An unexpected exception of a resolver reaches an HTTP client as a generic error at its path`` () : Task = task {
    let! struct (response, json) = executeOverHttp id """{ hero(id: "1000") { name unexpectedFailure } }"""

    Assert.DoesNotContain (TestSchema.SecretDetail, json)
    let error = errorsOf response |> single
    Assert.Equal (ErrorMasking.UnexpectedErrorMessage, error.Message)
    error.Path |> equals (Include [ box "hero"; box "unexpectedFailure" ])
}

[<Fact>]
let ``An unexpected exception of a resolver keeps its message over HTTP when masking is disabled`` () : Task = task {
    let! struct (response, _) =
        executeOverHttp (fun options -> { options with MaskUnexpectedErrors = false }) """{ hero(id: "1000") { name unexpectedFailure } }"""

    let error = errorsOf response |> single
    Assert.Equal (TestSchema.SecretDetail, error.Message)
    error.Path |> equals (Include [ box "hero"; box "unexpectedFailure" ])
}

[<Fact>]
let ``A GraphQL error a resolver raises on purpose keeps its message over HTTP`` () : Task = task {
    let! struct (response, _) = executeOverHttp id """{ hero(id: "1000") { name deliberateFailure } }"""

    let error = errorsOf response |> single
    Assert.Equal (TestSchema.DeliberateFailureMessage, error.Message)
    error.Path |> equals (Include [ box "hero"; box "deliberateFailure" ])
}

[<Fact>]
let ``A request error caused by an unexpected exception reaches an HTTP client as a generic error, as over WebSocket`` () : Task = task {
    let! struct (response, json) =
        executeOverHttp (fun options -> { options with SchemaExecutor = TestSchema.requestFailureExecutor }) """{ hero(id: "1000") { name } }"""

    Assert.DoesNotContain (TestSchema.SecretDetail, json)
    Assert.True (response.Data.IsSkip, $"A request error must not carry data, but got %A{response.Data}")
    Assert.Equal (ErrorMasking.UnexpectedErrorMessage, (errorsOf response |> single).Message)
}

[<Fact>]
let ``A GraphQL error of an exception declaring another message for clients reaches an HTTP client with that message`` () : Task = task {
    // The default ParseError of a schema reports an exception by its own message, which an IGQLError keeps for the server
    let! struct (response, json) = executeOverHttp id """{ hero(id: "1000") { name divergentFailure } }"""

    Assert.DoesNotContain (TestSchema.SecretDetail, json)
    let error = errorsOf response |> single
    Assert.Equal (TestSchema.DivergentClientMessage, error.Message)
    error.Path |> equals (Include [ box "hero"; box "divergentFailure" ])
}
