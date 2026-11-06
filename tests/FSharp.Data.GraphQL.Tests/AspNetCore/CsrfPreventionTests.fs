module FSharp.Data.GraphQL.Tests.AspNetCore.CsrfPreventionTests

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Net.Http
open System.Net.Mime
open System.Text
open System.Text.Json
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Primitives
open Microsoft.Net.Http.Headers
open Xunit

open FSharp.Data.GraphQL
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
    Assert.Equal (StatusCodes.Status400BadRequest, statusCode)
    let hasData, _ = body.TryGetProperty "data"
    Assert.False (hasData, $"Expected a blocked request to produce no data, but got: {describe body}")
    match body.TryGetProperty "errors" with
    | true, errors ->
        let error = Assert.Single (errors.EnumerateArray ())
        let message = error.GetProperty("message").GetString()
        Assert.Contains ("blocked as a potential Cross-Site Request Forgery (CSRF)", message, StringComparison.Ordinal)
        // The code Apollo Server reports a blocked request with, which clients written for it may match
        Assert.Equal (CsrfPrevention.BlockedRequestCode, error.GetProperty("extensions").GetProperty("code").GetString ())
        message
    | false, _ ->
        fail $"Expected a GraphQL error explaining the block, but the response has no errors: {describe body}"
        ""

/// Asserts that the handler executed the request and the response data carries the given root field
let private assertExecuted (rootField : string) (struct (statusCode : int, body : JsonElement)) =
    Assert.Equal (StatusCodes.Status200OK, statusCode)
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
    [| box MediaTypeNames.Text.Plain; box "json" |]
    [| box $"{MediaTypeNames.Text.Plain};charset=UTF-8"; box "json" |]
    // Spelled out, as its case differs from the constant on purpose
    [| box "Text/Plain"; box "json" |]
    [| box $" {MediaTypeNames.Text.Plain} ; charset=utf-8"; box "json" |]
    [| box MediaTypeNames.Application.FormUrlEncoded; box "form" |]
    [|
        box (MediaTypeNames.Application.FormUrlEncoded.ToUpperInvariant () + "; charset=utf-8")
        box "form"
    |]
    [| box $"{MediaTypeNames.Multipart.FormData}; boundary={Boundary}"; box "multipart" |]
    // Spelled out, as its case differs from the constant on purpose
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
            sendWithDefaults HttpMethods.Post (ValueSome contentType) [ CsrfPreventionHeaders.GraphQLPreflight,"1" ] body
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
    let! response = sendWithDefaults HttpMethods.Post ValueNone [ CsrfPreventionHeaders.GraphQLPreflight,"1" ] body
    assertOperationExecuted response
}

[<Fact>]
let ``POST with an empty preflight header is blocked`` () : Task = task {
    let! body = bodyOf "json"
    let! response =
        sendWithDefaults HttpMethods.Post (ValueSome MediaTypeNames.Text.Plain) [ CsrfPreventionHeaders.GraphQLPreflight,"" ] body
    assertBlocked response |> ignore
}

[<Theory>]
[<InlineData(CsrfPreventionHeaders.GraphQLPreflight)>]
// Spelled out, as its case differs from the constant on purpose
[<InlineData("graphql-preflight")>]
[<InlineData(CsrfPreventionHeaders.ApolloRequirePreflight)>]
[<InlineData(CsrfPreventionHeaders.ApolloOperationName)>]
let ``Each default preflight header lets a request a browser does not preflight through`` (headerName : string) : Task = task {
    let! body = bodyOf "json"
    let! response = sendWithDefaults HttpMethods.Post (ValueSome MediaTypeNames.Text.Plain) [ headerName, "true" ] body
    assertOperationExecuted response
}

[<Fact>]
let ``GET is blocked without a preflight header`` () : Task = task {
    let! response = sendWithDefaults HttpMethods.Get ValueNone [] [||]
    assertBlocked response |> ignore
}

[<Fact>]
let ``GET is executed with a preflight header`` () : Task = task {
    let! response = sendWithDefaults HttpMethods.Get ValueNone [ CsrfPreventionHeaders.GraphQLPreflight,"1" ] [||]
    assertIntrospectionExecuted response
}

[<Fact>]
let ``GET with a content type a browser preflights is executed without a preflight header`` () : Task = task {
    let! response = sendWithDefaults HttpMethods.Get (ValueSome MediaTypeNames.Application.Json) [] [||]
    assertIntrospectionExecuted response
}

[<Theory>]
[<InlineData(MediaTypeNames.Application.Json)>]
[<InlineData(MediaTypeNames.Application.Json + "; charset=utf-8")>]
// The media type of the GraphQL over HTTP specification, which nothing names yet
[<InlineData("application/graphql-response+json")>]
let ``POST with a content type a browser preflights is executed without a preflight header`` (contentType : string) : Task = task {
    let! body = bodyOf "json"
    let! response = sendWithDefaults HttpMethods.Post (ValueSome contentType) [] body
    assertOperationExecuted response
}

[<Fact>]
let ``A CORS preflight is not blocked`` () : Task = task {
    // A preflight is an OPTIONS request without the custom headers of the request it asks about, so it must pass
    let! response =
        sendWithDefaults HttpMethods.Options ValueNone [ HeaderNames.AccessControlRequestMethod, HttpMethods.Post ] [||]
    assertIntrospectionExecuted response
}

/// The methods a middleware overriding the method from a form field could give a form posted from another site
let overriddenMethods : obj array list = [
    [| box HttpMethods.Put |]
    [| box HttpMethods.Delete |]
    [| box HttpMethods.Patch |]
    [| box HttpMethods.Options |]
]

[<Theory>]
[<MemberData(nameof overriddenMethods)>]
let ``A request with a content type a browser does not preflight is blocked whatever its method`` (method : string) : Task = task {
    // A middleware overriding the method from a form field turns a form posted from another site into a request with any method
    let! body = bodyOf "form"
    let! response = sendWithDefaults method (ValueSome MediaTypeNames.Application.FormUrlEncoded) [] body
    assertBlocked response |> ignore
}

[<Fact>]
let ``The error of a blocked request explains how to get through`` () : Task = task {
    let! response = sendWithDefaults HttpMethods.Get ValueNone [] [||]
    let message = assertBlocked response
    // Both lists are in ordinal order, whatever order the sets they come from enumerate in
    Assert.Equal (
        "This operation has been blocked as a potential Cross-Site Request Forgery (CSRF). "
        + $"Please either specify a '{HeaderNames.ContentType}' header with a media type that is not one of "
        + $"{MediaTypeNames.Application.FormUrlEncoded}, {MediaTypeNames.Multipart.FormData}, {MediaTypeNames.Text.Plain}, "
        + "or provide a non-empty value for one of the following headers: "
        + $"{CsrfPreventionHeaders.ApolloRequirePreflight}, {CsrfPreventionHeaders.GraphQLPreflight}, "
        + $"{CsrfPreventionHeaders.ApolloOperationName}.",
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
            CsrfPrevention = ValueSome { RequestHeaders = ImmutableHashSet.Create HeaderNames.XRequestedWith }
    }
    let! body = bodyOf "json"

    let! defaultHeaderResponse =
        send withCustomHeader HttpMethods.Post (ValueSome MediaTypeNames.Text.Plain) [ CsrfPreventionHeaders.GraphQLPreflight,"1" ] body
    let message = assertBlocked defaultHeaderResponse
    Assert.EndsWith ($"for one of the following headers: {HeaderNames.XRequestedWith}.", message, StringComparison.Ordinal)

    let! customHeaderResponse =
        send withCustomHeader HttpMethods.Post (ValueSome MediaTypeNames.Text.Plain) [ HeaderNames.XRequestedWith, "XMLHttpRequest" ] body
    assertOperationExecuted customHeaderResponse
}

[<Fact>]
let ``No configured request header lets only requests a browser preflights through`` () : Task = task {
    let withoutHeaders (options : GraphQLOptions<Root>) = {
        options with
            CsrfPrevention = ValueSome { RequestHeaders = ImmutableHashSet<string>.Empty }
    }
    let! body = bodyOf "json"

    let! simpleResponse =
        send withoutHeaders HttpMethods.Post (ValueSome MediaTypeNames.Text.Plain) [ CsrfPreventionHeaders.GraphQLPreflight,"1" ] body
    let message = assertBlocked simpleResponse
    Assert.DoesNotContain ("following headers", message, StringComparison.Ordinal)

    let! jsonResponse = send withoutHeaders HttpMethods.Post (ValueSome MediaTypeNames.Application.Json) [] body
    assertOperationExecuted jsonResponse
}
