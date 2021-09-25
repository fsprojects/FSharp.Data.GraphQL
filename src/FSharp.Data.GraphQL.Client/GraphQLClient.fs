/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open System.IO
open System.Collections.Generic
open System.Threading
open System.Text
open System.Collections
open System.Net.Http
open FSharp.Reflection
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Client


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

    let rec private mapUploadVariables (accum: (string * Upload) list) (path: string) (key: string) (value : obj) =
        match value with
        | :? string | null ->
            accum
        | :? Upload as x ->
           (path + "." + key, x)::accum
        | :? RecordBase as x ->
            (accum, x.GetProperties())
            ||> Seq.fold(fun accum value -> mapUploadVariables accum (path + "." + key) value.Key value.Value)
        | :? IEnumerable as enumerable ->
            let values = Seq.cast<obj> enumerable
            ((0, accum), values)
            ||> Seq.fold(fun (idx, accum) item ->
                idx + 1, mapUploadVariables accum (path + "." + key) (string idx) item)
            |> snd
        | x ->
            let xtype = x.GetType()
            if xtype.IsGenericType && xtype.GetGenericTypeDefinition() = typedefof<option<_>> then
                match FSharpValue.GetUnionFields(value, xtype) with
                | _, [| innerValue |] -> mapUploadVariables accum path key innerValue
                | _, _ -> accum
            else
                accum

    let extractFileUploadMapping (request: GraphQLRequest) =
        request.Variables
        |> Seq.collect(fun (k,v) -> mapUploadVariables [] "variables" k v)
        |> Seq.toArray

    let private ensureSuccessCode (response : Async<HttpResponseMessage>) =
        async {
            let! response = response
            return response.EnsureSuccessStatusCode()
        }

    let private addHeaders (httpHeaders : seq<string * string>) (requestMessage : HttpRequestMessage) =
        if not (isNull httpHeaders)
        then httpHeaders |> Seq.iter (fun (name, value) -> requestMessage.Headers.Add(name, value))

    let private postAsync (invoker : HttpMessageInvoker) (serverUrl : string) (httpHeaders : seq<string * string>) (content : HttpContent) =
        async {
            use requestMessage = new HttpRequestMessage(HttpMethod.Post, serverUrl)
            requestMessage.Content <- content
            addHeaders httpHeaders requestMessage
            let! response = invoker.SendAsync(requestMessage, CancellationToken.None) |> Async.AwaitTask |> ensureSuccessCode
            let! content = response.Content.ReadAsStreamAsync() |> Async.AwaitTask
            return content
        }

    let private getAsync (invoker : HttpMessageInvoker) (serverUrl : string) =
        async {
            use requestMessage = new HttpRequestMessage(HttpMethod.Get, serverUrl)
            let! response = invoker.SendAsync(requestMessage, CancellationToken.None) |> Async.AwaitTask |> ensureSuccessCode
            let! content = response.Content.ReadAsStreamAsync() |> Async.AwaitTask
            return content
        }

    let private createStreamContent (data: 'T) = async {
        let memoryStream = new MemoryStream()
        do! Serialization.serializeToStream data memoryStream
        let content = new StreamContent(memoryStream)
        return content
    }
    /// Sends a request to a GraphQL server asynchronously.
    let sendRequestAsync (connection : GraphQLClientConnection) (request : GraphQLRequest) =
        async {
            let invoker = connection.Invoker
            let requestJson =
                {| operationName = request.OperationName
                   query = request.Query
                   variables = dict request.Variables |}
            let! content = createStreamContent requestJson
            content.Headers.Add("Content-Type", "application/json")
            return! postAsync invoker request.ServerUrl request.HttpHeaders content
        }

    /// Sends a request to a GraphQL server.
    let sendRequest client request =
        sendRequestAsync client request
        |> Async.RunSynchronously

    /// Executes an introspection schema request to a GraphQL server asynchronously.
    let sendIntrospectionRequestAsync (connection : GraphQLClientConnection) (serverUrl : string) httpHeaders =
        let sendGet() = async { return! getAsync connection.Invoker serverUrl }
        let rethrow (exns : exn list) =
            let rec mapper (acc : string) (exns : exn list) =
                let aggregateMapper (ex : AggregateException) = mapper "" (List.ofSeq ex.InnerExceptions)
                match exns with
                | [] -> acc.TrimEnd()
                | ex :: tail ->
                    match ex with
                    | :? AggregateException as ex -> mapper (acc + aggregateMapper ex + " ") tail
                    | ex -> mapper (acc + ex.Message + " ") tail
            failwithf "Failure trying to recover introspection schema from server at \"%s\". Errors: %s" serverUrl (mapper "" exns)
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
            let invoker = connection.Invoker
            let boundary = "----GraphQLProviderBoundary" + (Guid.NewGuid().ToString("N"))
            let content = new MultipartContent("form-data", boundary)
            let files = extractFileUploadMapping request
            let operationJson =
                    {| operationName = request.OperationName
                       query = request.Query
                       variables =  dict request.Variables |}
            let! operationContent = createStreamContent operationJson
            operationContent.Headers.Add("Content-Disposition", "form-data; name=\"operations\"")
            content.Add(operationContent)
            let filesMapJson =
                files
                |> Array.mapi(fun i (key, v) -> (i.ToString()), [key])
                |> dict
            let! mapContent = createStreamContent filesMapJson
            mapContent.Headers.Add("Content-Disposition", "form-data; name=\"map\"")
            content.Add(mapContent)

            for (i, (_, upload)) in Array.indexed files do
                let uploadContent = new StreamContent(upload.Stream)
                uploadContent.Headers.Add("Content-Disposition", sprintf "form-data; name=\"%i\"; filename=\"%s\"" i upload.FileName)
                uploadContent.Headers.Add("Content-Type", upload.ContentType)
                content.Add uploadContent

            let! result = postAsync invoker request.ServerUrl request.HttpHeaders content
            return result
        }

    /// Executes a multipart request to a GraphQL server.
    let sendMultipartRequest connection request =
        sendMultipartRequestAsync connection request
        |> Async.RunSynchronously