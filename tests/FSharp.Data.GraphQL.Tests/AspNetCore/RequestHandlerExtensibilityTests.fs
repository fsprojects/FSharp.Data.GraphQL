module FSharp.Data.GraphQL.Tests.AspNetCore.RequestHandlerExtensibilityTests

open System
open System.IO
open System.Text
open System.Text.Json
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
/// a hosted app uses), backed by an `HttpContext` with the given method and JSON body. Returns it both as
/// the base type (to prove `HandleAsync` dispatches virtually) and downcast to the concrete type (to
/// assert on what it recorded), plus the DI scope to dispose once the test is done with it.
let private createHandler<'Handler when 'Handler :> GraphQLRequestHandler<Root> and 'Handler : not struct>
    (method : string)
    (body : string voption)
    : GraphQLRequestHandler<Root> * 'Handler * IDisposable =
    let services = ServiceCollection ()
    services.AddLogging () |> ignore
    services.AddGraphQL<Root, 'Handler>(TestSchema.executor, (fun _ -> { RequestId = "test" }))
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
