/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open System.IO
open System.Net.Http
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Client
open System.Text

/// The base type for all GraphQLProvider upload types.
/// Upload types are used in GraphQL multipart request spec, mostly for file uploading features.
type UploadBase (stream : Stream, fileName : string, ?contentType : string, ?ownsStream : bool) =
    new(bytes : byte [], fileName, ?contentType) = 
        let stream = new MemoryStream(bytes)
        match contentType with
        | Some ct -> new UploadBase(stream, fileName, ct, true)
        | None -> new UploadBase(stream, fileName, ownsStream = true)

    /// Gets the stream associated to this Upload type.
    member __.Stream = stream

    /// Gets the content type of this Upload type.
    member __.ContentType =
        match contentType with
        | Some ct -> ct
        | None ->
            let ext = Path.GetExtension(fileName)
            match MimeTypes.dict.Force().TryGetValue(ext) with
            | (true, mime) -> mime
            | _ -> "application/octet-stream"
        
    /// Gets the name of the file which contained on the stream.
    member __.FileName = fileName

    /// Gets a boolean value indicating if this Upload type owns the stream associated with it.
    /// If true, it will dispose the stream when this Upload type is disposed.
    member __.OwnsStream = defaultArg ownsStream false

    interface IDisposable with
        member x.Dispose() = if x.OwnsStream then x.Stream.Dispose()

/// The connection component for GraphQLClient module.
type GraphQLClientConnection() =
    let client = new HttpClient()
    member internal __.Client = client
    interface IDisposable with
        member __.Dispose() = client.Dispose()

/// A requrest object for making GraphQL calls using the GraphQL client module.
type GraphQLRequest  =
      /// Gets the URL of the GraphQL server which will be called.
    { ServerUrl : string
      /// Gets custom HTTP Headers to pass with each call using this request.
      HttpHeaders: seq<string * string>
      /// Gets the name of the operation that should run on the server.
      OperationName : string option
      /// Gets the query string which should be executed on the GraphQL server.
      Query : string
      /// Gets variables to be sent with the query.
      Variables : (string * obj) [] }

/// Executes calls to GraphQL servers and return their responses.
module GraphQLClient =
    let private rethrow (exns : exn list) =
        let rec mapper (acc : string) (exns : exn list) =
            let aggregateMapper (ex : AggregateException) = mapper "" (List.ofSeq ex.InnerExceptions)
            match exns with
            | [] -> acc
            | ex :: tail ->
                match ex with
                | :? AggregateException as ex -> aggregateMapper ex
                | ex -> mapper (acc + " " + ex.Message) tail
        failwithf "Failure calling GraphQL server. %s" (mapper "" exns)

    let private addHeaders (httpHeaders : seq<string * string>) (content : HttpContent) =
        if not (isNull httpHeaders)
        then httpHeaders |> Seq.iter (fun (name, value) -> content.Headers.Add(name, value))

    /// Sends a request to a GraphQL server asynchronously.
    let sendRequestAsync (connection : GraphQLClientConnection) (request : GraphQLRequest) =
        async {
            let client = connection.Client
            let variables = 
                match request.Variables with
                | null | [||] -> JsonValue.Null
                | _ -> 
                    let json = Map.ofSeq request.Variables |> Serialization.toJsonValue
                    json.ToString() |> JsonValue.String
            let operationName =
                match request.OperationName with
                | Some x -> JsonValue.String x
                | None -> JsonValue.Null
            let requestJson =         
                [| "operationName", operationName
                   "query", JsonValue.String request.Query
                   "variables", variables |]
                |> JsonValue.Record
            use content = new StringContent(requestJson.ToString(), Encoding.UTF8, "application/json")
            addHeaders request.HttpHeaders content
            let! response = client.PostAsync(request.ServerUrl, content) |> Async.AwaitTask
            return! response.Content.ReadAsStringAsync() |> Async.AwaitTask
        }
    
    /// Sends a request to a GraphQL server.
    let sendRequest client request =
        sendRequestAsync client request
        |> Async.RunSynchronously

    /// Executes an introspection schema request to a GraphQL server asynchronously.
    let sendIntrospectionRequestAsync (connection : GraphQLClientConnection) (serverUrl : string) httpHeaders =
        let sendGet() =
            async {
                let client = connection.Client
                let! response = client.GetAsync(serverUrl) |> Async.AwaitTask
                return! response.Content.ReadAsStringAsync() |> Async.AwaitTask
            }
        async {
            try return! sendGet()
            with getex ->
                let request =
                    { ServerUrl = serverUrl
                      HttpHeaders = httpHeaders
                      OperationName = None
                      Query = Introspection.IntrospectionQuery
                      Variables = [||] }
                try return! sendRequestAsync connection request
                with postex -> return rethrow [getex; postex]
        }

    /// Executes an introspection schema request to a GraphQL server.
    let sendIntrospectionRequest client serverUrl httpHeaders = 
        sendIntrospectionRequestAsync client serverUrl httpHeaders
        |> Async.RunSynchronously

    /// Executes a multipart request to a GraphQL server asynchronously.
    let sendMultipartRequestAsync (connection : GraphQLClientConnection) (request : GraphQLRequest) =
        async {
            let client = connection.Client
            use content = new MultipartContent()
            addHeaders request.HttpHeaders content
            let variables = 
                request.Variables
                |> Array.map (fun (name, value) ->
                    match value with
                    | null -> name, null
                    | :? UploadBase -> name, null
                    | _ -> name, value)
            let files = 
                request.Variables
                |> Array.filter (fun (_, value) -> not (isNull value) && value.GetType() = typeof<UploadBase>)
                |> Array.map (fun (name, value) -> name, value :?> UploadBase)
            use operationContent = 
                let variables = 
                    match request.Variables with
                    | null | [||] -> JsonValue.Null
                    | _ -> 
                        let json = 
                            variables
                            |> Map.ofArray
                            |> Serialization.toJsonValue
                        json.ToString() |> JsonValue.String
                let json =
                    [| "query", JsonValue.String request.Query
                       "variables", variables |]
                    |> JsonValue.Record
                let content = new StringContent(json.ToString())
                content.Headers.Add("Content-Disposition", "form-data; name=\"operations\"")
                content
            content.Add(operationContent)
            use mapContent =
                let files =
                    files
                    |> Array.mapi (fun ix (name, _) -> sprintf "\"%i\"" ix, JsonValue.Array [| JsonValue.String (sprintf "variables.%s" name) |])
                    |> JsonValue.Record
                let content = new StringContent(files.ToString())
                content.Headers.Add("Content-Disposition", "form-data; name=\"map\"")
                content
            content.Add(mapContent)
            let fileContents =
                files
                |> Array.mapi (fun ix (_, value) ->
                    let content = new StreamContent(value.Stream)
                    content.Headers.Add("Content-Disposition", sprintf "form-data; name=\"%i\"; filename=\"%s\"" ix value.FileName)
                    content.Headers.Add("Content-Type", value.ContentType)
                    content)
            fileContents |> Array.iter content.Add
            let! response = client.PostAsync(request.ServerUrl, content) |> Async.AwaitTask
            return! response.Content.ReadAsStringAsync() |> Async.AwaitTask
        }

    /// Executes a multipart request to a GraphQL server.
    let sendMultipartRequest connection request =
        sendMultipartRequestAsync connection request
        |> Async.RunSynchronously