module FSharp.Data.GraphQL.Tests.AspNetCore.RequestBodyTests

open System
open System.IO
open System.Text
open System.Text.Json
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Features
open Xunit

open FSharp.Data.GraphQL.Server.AspNetCore
open FSharp.Data.GraphQL.Tests.AspNetCore.RequestHandlerExtensibilityTests

/// Text placed into request bodies that no response may repeat back
[<Literal>]
let private Marker = "BODY-MARKER-5f0c2a"

[<Literal>]
let private Boundary = "graphql-test-boundary"

[<Literal>]
let private MultipartContentType = "multipart/form-data; boundary=" + Boundary

[<Literal>]
let private JsonContentType = "application/json"

[<Literal>]
let private ValidOperations = """{"query":"{ hero(id: \"1000\") { id } }"}"""

/// One form field of a multipart body, terminated by the line break that precedes the next boundary
let private formPart (name : string) (value : string) =
    $"--%s{Boundary}\r\nContent-Disposition: form-data; name=\"%s{name}\"\r\n\r\n%s{value}\r\n"

/// One file of a multipart body, terminated by the line break that precedes the next boundary
let private filePart (name : string) (fileName : string) (content : string) =
    $"--%s{Boundary}\r\nContent-Disposition: form-data; name=\"%s{name}\"; filename=\"%s{fileName}\"\r\nContent-Type: text/plain\r\n\r\n%s{content}\r\n"

let private closingBoundary = $"--%s{Boundary}--\r\n"

let private multipartBody (parts : string seq) =
    String.Concat (
        seq {
            yield! parts
            yield closingBoundary
        }
    )

/// The parts of a request that the client of this repository sends: operations, a map keyed by index,
/// and the file named by its upload name rather than by its map key, so the server must not insist on the
/// two matching.
let private clientStyleParts map = [
    formPart "operations" ValidOperations
    formPart "map" map
    filePart "3f2b7a90-upload" "notes.txt" Marker
]

/// What a result wrote into the HTTP response
type private WrittenResponse = { StatusCode : int; ContentType : string; Body : string }

/// A request body that the server refuses to read, as Kestrel's body stream does once a read
/// breaks a server limit such as MaxRequestBodySize or the connection ends too early
type private RejectingRequestBody (error : exn) =
    inherit Stream ()

    override _.CanRead = true
    override _.CanSeek = false
    override _.CanWrite = false
    override _.Length = raise (NotSupportedException ())

    override _.Position
        with get () = raise (NotSupportedException ())
        and set _ = raise (NotSupportedException ())

    override _.Flush () = ()
    override _.Read (_ : byte[], _ : int, _ : int) : int = raise error
    override _.Seek (_ : int64, _ : SeekOrigin) : int64 = raise (NotSupportedException ())
    override _.SetLength (_ : int64) = raise (NotSupportedException ())
    override _.Write (_ : byte[], _ : int, _ : int) = raise (NotSupportedException ())

let private bodyStream (body : string) = new MemoryStream (Encoding.UTF8.GetBytes body) :> Stream

/// Sends a POST request with the given body through the default handler and writes the result it
/// answers with, success or error, into a response the test can inspect.
let private postAsync (contentType : string) (body : Stream) (configureRequest : HttpContext -> unit) : Task<WrittenResponse> = task {
    let handler, _, ctx, scope =
        createHandlerFor<DefaultGraphQLRequestHandler<Root>>(fun ctx ->
            ctx.Request.Method <- HttpMethods.Post
            ctx.Request.ContentType <- contentType
            ctx.Request.Body <- body

            if body.CanSeek then
                ctx.Request.ContentLength <- body.Length

            configureRequest ctx)
    use _ = scope

    let! outcome = task {
        try
            return! handler.HandleAsync ()
        with ex ->
            return failwith $"Expected HandleAsync to answer with a response, but it threw %s{ex.GetType().FullName}: %s{ex.Message}"
    }

    let result =
        match outcome with
        | Ok result
        | Error result -> result

    let responseBody = new MemoryStream ()
    ctx.Response.Body <- responseBody
    do! result.ExecuteAsync ctx

    return {
        StatusCode = ctx.Response.StatusCode
        ContentType =
            match ctx.Response.ContentType with
            | null -> ""
            | contentType -> contentType
        Body = Encoding.UTF8.GetString (responseBody.ToArray ())
    }
}

let private postTextAsync contentType (body : string) = postAsync contentType (bodyStream body) ignore

let private assertStatus (expectedStatus : int) (response : WrittenResponse) =
    if response.StatusCode <> expectedStatus then
        fail $"Expected status %d{expectedStatus}, but the response has status %d{response.StatusCode} and body:\n%s{response.Body}"

/// Asserts the response is a problem details document with the given status and title
let private assertProblem (expectedStatus : int) (expectedTitle : string) (response : WrittenResponse) =
    assertStatus expectedStatus response

    if response.ContentType <> "application/problem+json" then
        fail $"Expected a problem details response, but the content type is '%s{response.ContentType}' and the body is:\n%s{response.Body}"

    use document = JsonDocument.Parse response.Body
    let root = document.RootElement
    let title = root.GetProperty("title").GetString()

    if title <> expectedTitle then
        fail $"Expected the problem title '%s{expectedTitle}', but it is '%s{title}' in:\n%s{response.Body}"

    let status = root.GetProperty("status").GetInt32()

    if status <> expectedStatus then
        fail $"Expected the problem status %d{expectedStatus}, but it is %d{status} in:\n%s{response.Body}"

let private assertDoesNotEcho (text : string) (response : WrittenResponse) =
    if response.Body.Contains (text, StringComparison.Ordinal) then
        fail $"Expected the response not to repeat '%s{text}' from the request body, but it does:\n%s{response.Body}"

[<Fact>]
let ``Multipart request that follows the client format is executed`` () : Task = task {
    let body = multipartBody (clientStyleParts """{"0":["variables.file"]}""")

    let! response = postTextAsync MultipartContentType body

    assertStatus StatusCodes.Status200OK response
    Assert.Contains ("\"1000\"", response.Body)
}

[<Fact>]
let ``Multipart request without an operations part is answered with 400`` () : Task = task {
    let body =
        multipartBody [ formPart "map" """{"0":["variables.file"]}"""; filePart "0" "notes.txt" Marker ]

    let! response = postTextAsync MultipartContentType body

    response
    |> assertProblem StatusCodes.Status400BadRequest "Invalid multipart request"
    response |> assertDoesNotEcho Marker
}

[<Fact>]
let ``Multipart request without a boundary is answered with 400`` () : Task = task {
    let body = multipartBody (clientStyleParts """{"0":["variables.file"]}""")

    let! response = postTextAsync "multipart/form-data" body

    response
    |> assertProblem StatusCodes.Status400BadRequest "Unreadable request body"
    response |> assertDoesNotEcho Marker
}

[<Fact>]
let ``Multipart request with a boundary the body does not use is answered with 400`` () : Task = task {
    let body = multipartBody (clientStyleParts """{"0":["variables.file"]}""")

    let! response = postTextAsync "multipart/form-data; boundary=another-boundary" body

    response
    |> assertProblem StatusCodes.Status400BadRequest "Unreadable request body"
    response |> assertDoesNotEcho Marker
}

[<Fact>]
let ``Truncated multipart request is answered with 400`` () : Task = task {
    let body = multipartBody (clientStyleParts """{"0":["variables.file"]}""")
    let truncatedBody = body.Substring (0, body.Length - closingBoundary.Length - 4)

    let! response = postTextAsync MultipartContentType truncatedBody

    response
    |> assertProblem StatusCodes.Status400BadRequest "Unreadable request body"
    response |> assertDoesNotEcho Marker
}

[<Fact>]
let ``Multipart request with a map that is not JSON is answered with 400`` () : Task = task {
    let body = multipartBody (clientStyleParts ("""{"0": ["variables.file"], """ + Marker))

    let! response = postTextAsync MultipartContentType body

    response
    |> assertProblem StatusCodes.Status400BadRequest "Invalid multipart request"
    response |> assertDoesNotEcho Marker
}

[<Theory>]
[<InlineData("""[["variables.file"]]""")>]
[<InlineData("""{"0":"variables.file"}""")>]
[<InlineData("""{"0":[]}""")>]
[<InlineData("""{"0":[0]}""")>]
[<InlineData("""{"0":["query"]}""")>]
[<InlineData("""{"0":["variables"]}""")>]
[<InlineData("""{"0":["variables."]}""")>]
[<InlineData("""{"0":["variables..file"]}""")>]
[<InlineData("""{"0":["0.variables.file"]}""")>]
let ``Multipart request with a map that is not an object of variable paths is answered with 400`` (map : string) : Task = task {
    let body = multipartBody (clientStyleParts map)

    let! response = postTextAsync MultipartContentType body

    response
    |> assertProblem StatusCodes.Status400BadRequest "Invalid multipart request"
}

[<Fact>]
let ``Multipart request over the form body length limit is answered with 413`` () : Task = task {
    let body = multipartBody (clientStyleParts """{"0":["variables.file"]}""")

    let! response =
        postAsync MultipartContentType (bodyStream body) (fun ctx ->
            (ctx :?> DefaultHttpContext).FormOptions <- FormOptions (MultipartBodyLengthLimit = 16L))

    response
    |> assertProblem StatusCodes.Status413PayloadTooLarge "Request body too large"
    response |> assertDoesNotEcho Marker
}

[<Fact>]
let ``Multipart request over the form value count limit is answered with 413`` () : Task = task {
    let body = multipartBody (clientStyleParts """{"0":["variables.file"]}""")

    let! response =
        postAsync MultipartContentType (bodyStream body) (fun ctx -> (ctx :?> DefaultHttpContext).FormOptions <- FormOptions (ValueCountLimit = 1))

    response
    |> assertProblem StatusCodes.Status413PayloadTooLarge "Request body too large"
}

[<Theory>]
[<InlineData(JsonContentType)>]
[<InlineData(MultipartContentType)>]
let ``Request body over the server body size limit is answered with 413`` (contentType : string) : Task = task {
    // Kestrel reports a body over MaxRequestBodySize by throwing BadHttpRequestException from the body stream
    let error =
        BadHttpRequestException ("Request body too large. The max request body size is 16 bytes.", StatusCodes.Status413PayloadTooLarge)

    let! response = postAsync contentType (new RejectingRequestBody (error) :> Stream) ignore

    response
    |> assertProblem StatusCodes.Status413PayloadTooLarge "Request body too large"
}

[<Theory>]
[<InlineData(JsonContentType)>]
[<InlineData(MultipartContentType)>]
let ``Request body that ends before its declared length is answered with 400`` (contentType : string) : Task = task {
    // Kestrel reports a body shorter than its Content-Length by throwing BadHttpRequestException from the body stream
    let error =
        BadHttpRequestException ("Unexpected end of request content.", StatusCodes.Status400BadRequest)

    let! response = postAsync contentType (new RejectingRequestBody (error) :> Stream) ignore

    response
    |> assertProblem StatusCodes.Status400BadRequest "Unreadable request body"
}

[<Fact>]
let ``JSON body with a syntax error is answered with 400 without repeating the body`` () : Task = task {
    let body = $$"""{"query":"{ hero(id: \"1000\") { id } }","variables":{"token":"{{Marker}}"} oops"""

    let! response = postTextAsync JsonContentType body

    response
    |> assertProblem StatusCodes.Status400BadRequest "Invalid JSON body"
    response |> assertDoesNotEcho Marker
}

[<Fact>]
let ``Multipart request with a malformed part header quotes at most a short excerpt of it`` () : Task = task {
    // The multipart reader repeats a header line without a colon in the message of the exception it throws
    let longHeaderLine = String.replicate 100 Marker
    let body =
        multipartBody [
            formPart "operations" ValidOperations
            $"--%s{Boundary}\r\n%s{longHeaderLine}\r\n\r\ncontent\r\n"
        ]

    let! response = postTextAsync MultipartContentType body

    response
    |> assertProblem StatusCodes.Status400BadRequest "Unreadable request body"
    response |> assertDoesNotEcho longHeaderLine
    Assert.Contains (Marker, response.Body)
}

[<Theory>]
[<InlineData("null")>]
[<InlineData("""{"query":null}""")>]
let ``JSON body that is null or has a null query is answered with 400`` (body : string) : Task = task {
    let! response = postTextAsync JsonContentType body

    assertStatus StatusCodes.Status400BadRequest response
}

[<Fact>]
let ``GraphQL syntax error is answered with 400 quoting at most a short excerpt of the query`` () : Task = task {
    // The parser error quotes the line around the error position, which here is a single very long line
    let longTail = String.replicate 100 Marker
    let body = JsonSerializer.Serialize {| query = "{ hero(id: \"1000\") { id ! " + longTail + " } }" |}

    let! response = postTextAsync JsonContentType body

    response
    |> assertProblem StatusCodes.Status400BadRequest "Cannot parse GraphQL query"
    response |> assertDoesNotEcho longTail
}

[<Fact>]
let ``Multipart operations with a syntax error are answered with 400 without repeating the body`` () : Task = task {
    let body =
        multipartBody [
            formPart "operations" $$"""{"query":"{ hero(id: \"1000\") { id } }","variables":{"token":"{{Marker}}"} oops"""
            formPart "map" "{}"
        ]

    let! response = postTextAsync MultipartContentType body

    response
    |> assertProblem StatusCodes.Status400BadRequest "Invalid JSON body"
    response |> assertDoesNotEcho Marker
}
