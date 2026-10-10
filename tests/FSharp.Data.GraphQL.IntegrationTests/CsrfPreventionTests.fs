module FSharp.Data.GraphQL.IntegrationTests.CsrfPreventionTests

open System.Net
open System.Net.Http
open System.Net.Http.Headers
open System.Text
open System.Threading
open System.Threading.Tasks
open Xunit

open FSharp.Data.GraphQL

[<Literal>]
let private PreflightHeaderName = "GraphQL-Preflight"

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
            match request.Headers.TryGetValues PreflightHeaderName with
            | true, values -> List.ofSeq values
            | false, _ -> []
        sent.Add (struct (request.Method, preflightValues))
        Task.FromResult (new HttpResponseMessage (HttpStatusCode.OK, Content = new StringContent ("""{"data":{}}""")))

let private createRecordingConnection () =
    let handler = new RecordingHandler ()
    struct (handler, new GraphQLClientConnection (new HttpClient (handler), true))

let private createRequest (query : string) (variables : (string * obj)[]) = {
    ServerUrl = "http://localhost/graphql"
    HttpHeaders = Seq.empty
    OperationName = None
    Query = query
    Variables = variables
}

let private createUpload () =
    new Upload (Encoding.UTF8.GetBytes FileContent, "hello.txt", "hello", "text/plain")

/// Asserts that the client sent at least one request and that each one carried exactly the expected preflight header value
let private assertPreflightSent (expectedValue : string) (handler : RecordingHandler) =
    Assert.NotEmpty handler.Sent
    for struct (method, values) in handler.Sent do
        Assert.True (
            (values = [ expectedValue ]),
            $"Expected the {method} request to carry '{PreflightHeaderName}: {expectedValue}' once, but it carried %A{values}"
        )

[<Fact>]
let ``The client sends the preflight header with a GraphQL request`` () : Task = task {
    let struct (handler, connection) = createRecordingConnection ()
    use _ = connection
    use! _ =
        GraphQLClient.sendRequestAsync CancellationToken.None connection (createRequest "query { hero { name } }" [||])
    handler |> assertPreflightSent "1"
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
        GraphQLClient.sendIntrospectionRequestAsync CancellationToken.None connection "http://localhost/graphql" Seq.empty
    handler |> assertPreflightSent "1"
}

[<Fact>]
let ``The client keeps a preflight header the caller sets itself`` () : Task = task {
    let struct (handler, connection) = createRecordingConnection ()
    use _ = connection
    let request = {
        createRequest "query { hero { name } }" [||] with
            HttpHeaders = [ PreflightHeaderName, "custom" ]
    }
    use! _ = GraphQLClient.sendRequestAsync CancellationToken.None connection request
    handler |> assertPreflightSent "custom"
}

/// A multipart file upload of the GraphQL multipart request specification, built without the GraphQL client
let private createUploadContent () =
    let content = new MultipartFormDataContent ()
    content.Add (new StringContent ($"""{{"query":"{UploadMutation}","variables":{{"file":"hello"}}}}"""), "operations")
    content.Add (new StringContent ("""{"0":["variables.file"]}"""), "map")
    let file = new ByteArrayContent (Encoding.UTF8.GetBytes FileContent)
    file.Headers.ContentType <- MediaTypeHeaderValue "text/plain"
    content.Add (file, "hello", "hello.txt")
    content

[<Fact>]
let ``Server blocks a multipart file upload without a preflight header`` () : Task = task {
    use httpClient = TestHosts.createIntegrationHttpClient ()
    use content = createUploadContent ()
    use! response = httpClient.PostAsync ("/", content)
    let! body = response.Content.ReadAsStringAsync ()
    Assert.True ((response.StatusCode = HttpStatusCode.BadRequest), $"Expected 400 Bad Request, but got {response.StatusCode}: {body}")
    Assert.Contains ("blocked as a potential Cross-Site Request Forgery (CSRF)", body)
}

[<Fact>]
let ``Server executes a multipart file upload with a preflight header`` () : Task = task {
    use httpClient = TestHosts.createIntegrationHttpClient ()
    use content = createUploadContent ()
    use request = new HttpRequestMessage (HttpMethod.Post, "/", Content = content)
    request.Headers.Add (PreflightHeaderName, "1")
    use! response = httpClient.SendAsync request
    let! body = response.Content.ReadAsStringAsync ()
    Assert.True ((response.StatusCode = HttpStatusCode.OK), $"Expected 200 OK, but got {response.StatusCode}: {body}")
    Assert.Contains (FileContent, body)
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
    Assert.Contains (FileContent, body)
}
