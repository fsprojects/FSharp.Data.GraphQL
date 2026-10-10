module FSharp.Data.GraphQL.IntegrationTests.CsrfPreventionTests

open System
open System.Net
open System.Net.Http
open System.Net.Http.Headers
open System.Net.Mime
open System.Text
open System.Threading
open System.Threading.Tasks
open Xunit

open FSharp.Data.GraphQL

[<Literal>]
let private ServerUrl = "http://localhost/graphql"

[<Literal>]
let private UploadMutation = "mutation ($file: File!) { singleUpload(file: $file) { contentAsText } }"

[<Literal>]
let private FileContent = "Hello, CSRF prevention!"

/// Answers every request with an empty GraphQL response and records the preflight header values each request carried
type private RecordingHandler () =
    inherit HttpMessageHandler ()

    let sent = ResizeArray<struct (HttpMethod * string list)>()

    member _.Sent = sent

    override _.SendAsync (request : HttpRequestMessage, _ : CancellationToken) =
        let preflightValues =
            match request.Headers.TryGetValues CsrfPreventionHeaders.GraphQLPreflight with
            | true, values -> List.ofSeq values
            | false, _ -> []
        sent.Add (struct (request.Method, preflightValues))
        Task.FromResult (new HttpResponseMessage (HttpStatusCode.OK, Content = new StringContent ("""{"data":{}}""")))

let private createRecordingConnection () =
    let handler = new RecordingHandler ()
    struct (handler, new GraphQLClientConnection (new HttpClient (handler), true))

let private createRequest (query : string) (variables : (string * obj)[]) = {
    ServerUrl = ServerUrl
    HttpHeaders = Seq.empty
    OperationName = None
    Query = query
    Variables = variables
}

let private createUpload () =
    new Upload (Encoding.UTF8.GetBytes FileContent, "hello.txt", "hello", MediaTypeNames.Text.Plain)

/// Asserts that the client sent at least one request and that each one carried exactly the expected preflight header value
let private assertPreflightSent (expectedValue : string) (handler : RecordingHandler) =
    Assert.NotEmpty handler.Sent
    for struct (_, values) in handler.Sent do
        Assert.Equal<string> ([ expectedValue ], values)

[<Fact>]
let ``The client sends no preflight header with a JSON request`` () : Task = task {
    // A browser preflights a JSON request anyway, and a browser-hosted client would need every server to allow the header
    let struct (handler, connection) = createRecordingConnection ()
    use _ = connection
    use! _ =
        GraphQLClient.sendRequestAsync CancellationToken.None connection (createRequest "query { hero { name } }" [||])
    let struct (_, values) = Assert.Single handler.Sent
    Assert.Empty values
}

[<Fact>]
let ``The client sends the preflight header with a multipart file upload`` () : Task = task {
    let struct (handler, connection) = createRecordingConnection ()
    use _ = connection
    use upload = createUpload ()
    use! _ =
        GraphQLClient.sendMultipartRequestAsync CancellationToken.None connection (createRequest UploadMutation [| "file", box upload |])
    handler |> assertPreflightSent "1"
}

[<Fact>]
let ``The client sends the preflight header with an introspection request`` () : Task = task {
    let struct (handler, connection) = createRecordingConnection ()
    use _ = connection
    use! _ =
        GraphQLClient.sendIntrospectionRequestAsync CancellationToken.None connection ServerUrl Seq.empty
    handler |> assertPreflightSent "1"
}

[<Fact>]
let ``The client keeps a preflight header the caller sets itself`` () : Task = task {
    let struct (handler, connection) = createRecordingConnection ()
    use _ = connection
    let request = {
        createRequest "query { hero { name } }" [||] with
            HttpHeaders = [ CsrfPreventionHeaders.GraphQLPreflight, "custom" ]
    }
    use! _ = GraphQLClient.sendRequestAsync CancellationToken.None connection request
    handler |> assertPreflightSent "custom"
}

[<Fact>]
let ``The client keeps a preflight header the caller sets itself with an introspection request`` () : Task = task {
    let struct (handler, connection) = createRecordingConnection ()
    use _ = connection
    let headers = [ CsrfPreventionHeaders.GraphQLPreflight, "custom" ]
    use! _ = GraphQLClient.sendIntrospectionRequestAsync CancellationToken.None connection ServerUrl headers
    // The GET succeeds, so the client does not fall back to a POST
    let struct (method, values) = Assert.Single handler.Sent
    Assert.Equal (HttpMethod.Get, method)
    Assert.Equal<string> ([ "custom" ], values)
}

/// A multipart file upload of the GraphQL multipart request specification, built without the GraphQL client
let private createUploadContent () =
    let content = new MultipartFormDataContent ()
    content.Add (new StringContent ($"""{{"query":"{UploadMutation}","variables":{{"file":"hello"}}}}"""), "operations")
    content.Add (new StringContent ("""{"0":["variables.file"]}"""), "map")
    let file = new ByteArrayContent (Encoding.UTF8.GetBytes FileContent)
    file.Headers.ContentType <- MediaTypeHeaderValue MediaTypeNames.Text.Plain
    content.Add (file, "hello", "hello.txt")
    content

[<Fact>]
let ``Server blocks a multipart file upload without a preflight header`` () : Task = task {
    use httpClient = TestHosts.createIntegrationHttpClient ()
    use content = createUploadContent ()
    use! response = httpClient.PostAsync ("/", content)
    let! body = response.Content.ReadAsStringAsync ()
    Assert.Equal (HttpStatusCode.BadRequest, response.StatusCode)
    Assert.Contains ("blocked as a potential Cross-Site Request Forgery (CSRF)", body, StringComparison.Ordinal)
}

[<Fact>]
let ``Server executes a multipart file upload with a preflight header`` () : Task = task {
    use httpClient = TestHosts.createIntegrationHttpClient ()
    use content = createUploadContent ()
    use request = new HttpRequestMessage (HttpMethod.Post, "/", Content = content)
    request.Headers.Add (CsrfPreventionHeaders.GraphQLPreflight, "1")
    use! response = httpClient.SendAsync request
    let! body = response.Content.ReadAsStringAsync ()
    Assert.Equal (HttpStatusCode.OK, response.StatusCode)
    Assert.Contains (FileContent, body, StringComparison.Ordinal)
}

[<Fact>]
let ``Server executes a multipart file upload the GraphQL client sends`` () : Task = task {
    use httpClient = TestHosts.createIntegrationHttpClient ()
    use connection = new GraphQLClientConnection (httpClient)
    use upload = createUpload ()
    let request = {
        createRequest UploadMutation [| "file", box upload |] with
            ServerUrl = TestHosts.integrationServerUrl
    }
    // The client throws on a status code other than success, so a blocked upload fails here
    use! response = GraphQLClient.sendMultipartRequestAsync CancellationToken.None connection request
    let! body = response.Content.ReadAsStringAsync ()
    Assert.Contains (FileContent, body, StringComparison.Ordinal)
}
