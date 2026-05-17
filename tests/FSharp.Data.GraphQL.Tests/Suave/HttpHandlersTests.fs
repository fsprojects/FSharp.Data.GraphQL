module FSharp.Data.GraphQL.Tests.Suave.HttpHandlersTests

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.Json
open Xunit

open Suave
open Suave.Http
open Suave.Operators

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Server.Suave

type Root = { Path : string }

type UploadPayload = { File : FileData }

let private query =
    Define.Object<Root> (
        name = "Query",
        fields = [
            Define.Field ("path", StringType, resolve = fun _ root -> root.Path)
        ]
    )

let private uploadResult =
    Define.Object<FileData> (
        name = "UploadResult",
        fields = [
            Define.Field ("name", StringType, resolve = fun _ file -> file.FileName)
            Define.Field (
                "content",
                StringType,
                resolve =
                    fun _ file ->
                        file.Stream.Seek (0L, SeekOrigin.Begin) |> ignore
                        use reader = new StreamReader (file.Stream, Encoding.UTF8, leaveOpen = true)
                        reader.ReadToEnd ()
            )
        ]
    )

let private mutation =
    Define.Object<Root> (
        name = "Mutation",
        fields = [
            Define.Field (
                "upload",
                uploadResult,
                args = [ Define.Input ("file", FileType) ],
                resolve = fun ctx _ -> ctx.Arg<FileData> "file"
            )
        ]
    )

let private executor = Executor (Schema (query, mutation))

let private rootFactory (httpContext : HttpContext) = { Path = httpContext.request.path }

let private createRequest
    (methodName : string)
    (path : string)
    (body : byte array)
    (headers : (string * string) list)
    (files : Runtime.HttpUpload list)
    (multiPartFields : (string * string) list)
    =
    { HttpRequest.empty with
        rawMethod = methodName
        rawPath = path
        headers = List<string * string> (headers :> seq<_>)
        rawForm = body
        files = List<Runtime.HttpUpload> (files :> seq<_>)
        multiPartFields = List<string * string> (multiPartFields :> seq<_>) }

let private run webPart httpContext =
    match webPart httpContext |> Async.RunSynchronously with
    | Some handled -> handled
    | None -> failwith "Expected the WebPart to handle the request"

let private responseHeader name (httpContext : HttpContext) =
    httpContext.response.headers
    |> Seq.tryPick (fun (key, value) ->
        if String.Equals (key, name, StringComparison.OrdinalIgnoreCase) then
            Some value
        else
            None
    )

let private responseJson httpContext =
    match httpContext.response.content with
    | HttpContent.Bytes bytes -> JsonDocument.Parse bytes
    | content -> failwith $"Expected response bytes but received %A{content}"

[<Fact>]
let ``Suave GraphQL handler executes JSON requests`` () =
    let body = """{"query":"query { path }"}""" |> Encoding.UTF8.GetBytes
    let request =
        createRequest
            "POST"
            "/graphql"
            body
            [ "Content-Type", "application/json" ]
            []
            []

    let httpContext = { HttpContext.empty with request = request }

    let handled =
        HttpHandlers.setRequestType
        >=> HttpHandlers.graphQL executor rootFactory
        |> fun webPart -> run webPart httpContext

    Assert.Equal (Some "Classic", responseHeader "Request-Type" handled)

    use document = responseJson handled
    let value = document.RootElement.GetProperty("data").GetProperty("path").GetString()
    Assert.Equal ("/graphql", value)

[<Fact>]
let ``Suave GraphQL handler executes GET introspection requests`` () =
    let request = createRequest "GET" "/graphql" Array.empty [ "Accept", "application/json" ] [] []
    let httpContext = { HttpContext.empty with request = request }

    let handled = HttpHandlers.graphQL executor rootFactory |> fun webPart -> run webPart httpContext

    use document = responseJson handled
    let schema = document.RootElement.GetProperty("data").GetProperty("__schema")
    Assert.Equal (JsonValueKind.Object, schema.ValueKind)

[<Fact>]
let ``Suave GraphQL handler executes multipart upload requests`` () =
    let tempFilePath = Path.GetTempFileName ()

    try
        File.WriteAllText (tempFilePath, "uploaded from suave")

        let upload : Runtime.HttpUpload = {
            fieldName = "file0"
            fileName = "hello.txt"
            mimeType = "text/plain"
            tempFilePath = tempFilePath
        }

        let operations =
            """{
                "query": "mutation ($file: File!) { upload(file: $file) { name content } }",
                "variables": {
                    "file": "file0"
                }
            }"""

        let request =
            createRequest
                "POST"
                "/graphql"
                Array.empty
                [ "Content-Type", "multipart/form-data; boundary=graphql" ]
                [ upload ]
                [ "operations", operations ]

        let httpContext = { HttpContext.empty with request = request }

        let handled =
            HttpHandlers.setRequestType
            >=> HttpHandlers.graphQL executor rootFactory
            |> fun webPart -> run webPart httpContext

        Assert.Equal (Some "Multipart", responseHeader "Request-Type" handled)

        use document = responseJson handled
        let uploadResponse = document.RootElement.GetProperty("data").GetProperty("upload")
        Assert.Equal ("hello.txt", uploadResponse.GetProperty("name").GetString())
        Assert.Equal ("uploaded from suave", uploadResponse.GetProperty("content").GetString())
    finally
        if File.Exists tempFilePath then
            File.Delete tempFilePath
