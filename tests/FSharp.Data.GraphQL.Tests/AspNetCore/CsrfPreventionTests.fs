module FSharp.Data.GraphQL.Tests.AspNetCore.CsrfPreventionTests

open System
open System.Collections.Generic
open System.IO
open System.Net.Http
open System.Text
open System.Text.Json
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Primitives
open Xunit

open FSharp.Data.GraphQL.Server.AspNetCore

[<Literal>]
let private Boundary = "csrf-test-boundary"

let private operationJson = JsonSerializer.Serialize {| query = "query { hero(id: \"1000\") { id } }" |}

/// <summary>
/// The body of a GraphQL request in each encoding the handler reads: <c>form</c> and <c>multipart</c> carry the
/// operation in the <c>operations</c> field as the GraphQL multipart request specification does, and <c>json</c>
/// is the operation itself, whatever <c>Content-Type</c> the request claims.
/// </summary>
let private bodyOf (encoding : string) : Task<byte array> =
    match encoding with
    | "form" -> (new FormUrlEncodedContent ([ KeyValuePair ("operations", operationJson) ])).ReadAsByteArrayAsync()
    | "multipart" ->
        let content = new MultipartFormDataContent (Boundary)
        content.Add (new StringContent (operationJson), "operations")
        content.ReadAsByteArrayAsync ()
    | "json" -> Task.FromResult (Encoding.UTF8.GetBytes operationJson)
    | encoding -> raise (ArgumentOutOfRangeException (nameof encoding, encoding, "Unknown request body encoding"))

/// <summary>
/// Runs one request through the handler that the real <c>AddGraphQL</c> registration provides, executes the
/// <see cref="IResult"/> it returns into the response, as the Giraffe and Oxpecker integrations do, and returns the
/// response status code with its JSON body.
/// </summary>
let private send
    (configure : GraphQLOptions<Root> -> GraphQLOptions<Root>)
    (method : string)
    (contentType : string voption)
    (headers : (string * string) list)
    (body : byte array)
    : Task<struct (int * JsonElement)> = task {
    let services = ServiceCollection ()
    services.AddLogging () |> ignore
    services.AddGraphQL<Root>(TestSchema.executor, (fun _ -> { RequestId = "test" }), configure = Func<_, _> configure)
    |> ignore
    use serviceProvider = services.BuildServiceProvider ()
    use scope = serviceProvider.CreateScope ()

    let ctx = DefaultHttpContext (RequestServices = scope.ServiceProvider)
    ctx.Request.Method <- method
    ctx.Request.Path <- PathString "/graphql"
    contentType
    |> ValueOption.iter (fun contentType -> ctx.Request.ContentType <- contentType)
    for name, value in headers do
        ctx.Request.Headers[name] <- StringValues value
    ctx.Request.Body <- new MemoryStream (body)
    ctx.Request.ContentLength <- int64 body.Length
    use responseBody = new MemoryStream ()
    ctx.Response.Body <- responseBody

    // The handler captures `httpContextAccessor.HttpContext` in its own constructor, so the accessor
    // must carry the request's HttpContext before the handler is resolved from the container.
    scope.ServiceProvider.GetRequiredService<IHttpContextAccessor>().HttpContext <- ctx
    let handler = scope.ServiceProvider.GetRequiredService<GraphQLRequestHandler<Root>>()

    let! outcome = handler.HandleAsync ()
    let result =
        match outcome with
        | Ok result
        | Error result -> result
    do! result.ExecuteAsync ctx

    responseBody.Position <- 0L
    use! document = JsonDocument.ParseAsync responseBody
    return struct (ctx.Response.StatusCode, document.RootElement.Clone ())
}

let private sendWithDefaults = send id

let private withoutCsrfPrevention (options : GraphQLOptions<Root>) = { options with CsrfPrevention = ValueNone }

/// The response body for a failure message, cut short so that a whole introspection result does not drown the message
let private describe (body : JsonElement) =
    let text = body.GetRawText ()
    if text.Length <= 300 then
        text
    else
        text.Substring (0, 300) + "…"

/// Asserts that the handler refused to execute the request as a potential cross-site request forgery and returns the error message
let private assertBlocked (struct (statusCode : int, body : JsonElement)) =
    Assert.True (
        (statusCode = StatusCodes.Status400BadRequest),
        $"Expected the request to be blocked with 400 Bad Request, but it got {statusCode}: {describe body}"
    )
    let hasData, _ = body.TryGetProperty "data"
    Assert.False (hasData, $"Expected a blocked request to produce no data, but got: {describe body}")
    match body.TryGetProperty "errors" with
    | true, errors ->
        let error = Assert.Single (errors.EnumerateArray ())
        let message = error.GetProperty("message").GetString()
        Assert.Contains ("blocked as a potential Cross-Site Request Forgery (CSRF)", message)
        message
    | false, _ ->
        fail $"Expected a GraphQL error explaining the block, but the response has no errors: {describe body}"
        ""

/// Asserts that the handler executed the request and the response data carries the given root field
let private assertExecuted (rootField : string) (struct (statusCode : int, body : JsonElement)) =
    Assert.True ((statusCode = StatusCodes.Status200OK), $"Expected the request to be executed with 200 OK, but it got {statusCode}: {describe body}")
    let hasErrors, _ = body.TryGetProperty "errors"
    Assert.False (hasErrors, $"Expected the request to be executed without errors, but got: {describe body}")
    match body.TryGetProperty "data" with
    | true, data ->
        let hasRootField, _ = data.TryGetProperty rootField
        Assert.True (hasRootField, $"Expected the data to contain '{rootField}', but got: {describe body}")
    | false, _ -> fail $"Expected the response to carry data, but got: {describe body}"

/// The operation the request bodies of these tests carry
let private assertOperationExecuted = assertExecuted "hero"

/// What the handler answers a request without an operation with
let private assertIntrospectionExecuted = assertExecuted "__schema"

/// Every simple Content-Type, in spellings a browser may send, with the encoding of a body the handler reads for it
let simpleContentTypes : obj array list = [
    [| box "text/plain"; box "json" |]
    [| box "text/plain;charset=UTF-8"; box "json" |]
    [| box "Text/Plain"; box "json" |]
    [| box " text/plain ; charset=utf-8"; box "json" |]
    [| box "application/x-www-form-urlencoded"; box "form" |]
    [| box "APPLICATION/X-WWW-FORM-URLENCODED; charset=utf-8"; box "form" |]
    [| box $"multipart/form-data; boundary={Boundary}"; box "multipart" |]
    [| box $"Multipart/Form-Data; boundary=\"{Boundary}\""; box "multipart" |]
]

[<Theory>]
[<MemberData(nameof simpleContentTypes)>]
let ``POST with a content type a browser does not preflight is blocked without a preflight header`` (contentType : string, encoding : string) : Task =
    task {
        let! body = bodyOf encoding
        let! response = sendWithDefaults HttpMethods.Post (ValueSome contentType) [] body
        assertBlocked response |> ignore
    }

[<Theory>]
[<MemberData(nameof simpleContentTypes)>]
let ``POST with a content type a browser does not preflight is executed with a preflight header`` (contentType : string, encoding : string) : Task =
    task {
        let! body = bodyOf encoding
        let! response =
            sendWithDefaults HttpMethods.Post (ValueSome contentType) [ "GraphQL-Preflight", "1" ] body
        assertOperationExecuted response
    }

[<Fact>]
let ``POST without a content type is blocked without a preflight header`` () : Task = task {
    let! body = bodyOf "json"
    let! response = sendWithDefaults HttpMethods.Post ValueNone [] body
    assertBlocked response |> ignore
}

[<Fact>]
let ``POST without a content type is executed with a preflight header`` () : Task = task {
    let! body = bodyOf "json"
    let! response = sendWithDefaults HttpMethods.Post ValueNone [ "GraphQL-Preflight", "1" ] body
    assertOperationExecuted response
}

[<Fact>]
let ``POST with an empty preflight header is blocked`` () : Task = task {
    let! body = bodyOf "json"
    let! response =
        sendWithDefaults HttpMethods.Post (ValueSome "text/plain") [ "GraphQL-Preflight", "" ] body
    assertBlocked response |> ignore
}

[<Theory>]
[<InlineData("GraphQL-Preflight")>]
[<InlineData("graphql-preflight")>]
[<InlineData("Apollo-Require-Preflight")>]
[<InlineData("X-Apollo-Operation-Name")>]
let ``Each default preflight header lets a request a browser does not preflight through`` (headerName : string) : Task = task {
    let! body = bodyOf "json"
    let! response = sendWithDefaults HttpMethods.Post (ValueSome "text/plain") [ headerName, "true" ] body
    assertOperationExecuted response
}

[<Fact>]
let ``GET is blocked without a preflight header`` () : Task = task {
    let! response = sendWithDefaults HttpMethods.Get ValueNone [] [||]
    assertBlocked response |> ignore
}

[<Fact>]
let ``GET is executed with a preflight header`` () : Task = task {
    let! response = sendWithDefaults HttpMethods.Get ValueNone [ "GraphQL-Preflight", "1" ] [||]
    assertIntrospectionExecuted response
}

[<Fact>]
let ``GET with a content type a browser preflights is executed without a preflight header`` () : Task = task {
    let! response = sendWithDefaults HttpMethods.Get (ValueSome "application/json") [] [||]
    assertIntrospectionExecuted response
}

[<Theory>]
[<InlineData("application/json")>]
[<InlineData("application/json; charset=utf-8")>]
[<InlineData("application/graphql-response+json")>]
let ``POST with a content type a browser preflights is executed without a preflight header`` (contentType : string) : Task = task {
    let! body = bodyOf "json"
    let! response = sendWithDefaults HttpMethods.Post (ValueSome contentType) [] body
    assertOperationExecuted response
}

[<Theory>]
[<InlineData("OPTIONS")>]
[<InlineData("PUT")>]
let ``A request with a method a browser always preflights is not blocked`` (method : string) : Task = task {
    // A CORS preflight itself is an OPTIONS request without custom headers, and it must not fail
    let! response = sendWithDefaults method ValueNone [] [||]
    assertIntrospectionExecuted response
}

[<Fact>]
let ``The error of a blocked request explains how to get through`` () : Task = task {
    let! response = sendWithDefaults HttpMethods.Get ValueNone [] [||]
    let message = assertBlocked response
    Assert.Equal (
        "This operation has been blocked as a potential Cross-Site Request Forgery (CSRF). "
        + "Please either specify a 'Content-Type' header with a media type that is not one of "
        + "application/x-www-form-urlencoded, multipart/form-data, text/plain, or provide a non-empty value "
        + "for one of the following headers: GraphQL-Preflight, Apollo-Require-Preflight, X-Apollo-Operation-Name.",
        message
    )
}

[<Theory>]
[<MemberData(nameof simpleContentTypes)>]
let ``POST with a content type a browser does not preflight is executed without a preflight header when CSRF prevention is off``
    (contentType : string, encoding : string)
    : Task = task {
    let! body = bodyOf encoding
    let! response = send withoutCsrfPrevention HttpMethods.Post (ValueSome contentType) [] body
    assertOperationExecuted response
}

[<Fact>]
let ``POST without a content type is executed without a preflight header when CSRF prevention is off`` () : Task = task {
    let! body = bodyOf "json"
    let! response = send withoutCsrfPrevention HttpMethods.Post ValueNone [] body
    assertOperationExecuted response
}

[<Fact>]
let ``GET is executed without a preflight header when CSRF prevention is off`` () : Task = task {
    let! response = send withoutCsrfPrevention HttpMethods.Get ValueNone [] [||]
    assertIntrospectionExecuted response
}

[<Fact>]
let ``Configured request headers replace the default ones`` () : Task = task {
    let withCustomHeader (options : GraphQLOptions<Root>) = {
        options with
            CsrfPrevention = ValueSome { RequestHeaders = [ "X-Requested-With" ] }
    }
    let! body = bodyOf "json"

    let! defaultHeaderResponse =
        send withCustomHeader HttpMethods.Post (ValueSome "text/plain") [ "GraphQL-Preflight", "1" ] body
    let message = assertBlocked defaultHeaderResponse
    Assert.EndsWith ("for one of the following headers: X-Requested-With.", message)

    let! customHeaderResponse =
        send withCustomHeader HttpMethods.Post (ValueSome "text/plain") [ "X-Requested-With", "XMLHttpRequest" ] body
    assertOperationExecuted customHeaderResponse
}

[<Fact>]
let ``No configured request header lets only requests a browser preflights through`` () : Task = task {
    let withoutHeaders (options : GraphQLOptions<Root>) = { options with CsrfPrevention = ValueSome { RequestHeaders = [] } }
    let! body = bodyOf "json"

    let! simpleResponse =
        send withoutHeaders HttpMethods.Post (ValueSome "text/plain") [ "GraphQL-Preflight", "1" ] body
    let message = assertBlocked simpleResponse
    Assert.DoesNotContain ("following headers", message)

    let! jsonResponse = send withoutHeaders HttpMethods.Post (ValueSome "application/json") [] body
    assertOperationExecuted jsonResponse
}
