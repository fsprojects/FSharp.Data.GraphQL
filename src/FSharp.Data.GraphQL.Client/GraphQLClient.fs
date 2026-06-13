// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open System.Collections.Generic
open System.Net.Http
open System.Text
open System.Text.Json
open System.Threading
open System.Threading.Tasks

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Client
open ReflectionPatterns

/// A request object for making GraphQL calls using the GraphQL client module.
type GraphQLRequest = {
    /// Gets the URL of the GraphQL server which will be called.
    ServerUrl : string
    /// Gets custom HTTP Headers to pass with each call using this request.
    HttpHeaders : seq<string * string>
    /// Gets the name of the operation that should run on the server.
    OperationName : string option
    /// Gets the query string which should be executed on the GraphQL server.
    Query : string
    /// Gets variables to be sent with the query.
    Variables : (string * obj)[]
    /// Gets the JSON serializer options used for serializing request variables.
    JsonSerializerOptions : JsonSerializerOptions
}

/// Executes calls to GraphQL servers and return their responses.
module GraphQLClient =

    let private ensureSuccessCode (response : Task<HttpResponseMessage>) = task {
        let! response = response
        return response.EnsureSuccessStatusCode ()
    }

    let private addHeaders (httpHeaders : seq<string * string>) (requestMessage : HttpRequestMessage) =
        if not (isNull httpHeaders) then
            httpHeaders
            |> Seq.iter (fun (name, value) -> requestMessage.Headers.Add (name, value))

    let private postAsync ct (invoker : HttpMessageInvoker) (serverUrl : string) (httpHeaders : seq<string * string>) (content : HttpContent) = task {
        use requestMessage = new HttpRequestMessage (HttpMethod.Post, serverUrl)
        requestMessage.Content <- content
        addHeaders httpHeaders requestMessage
        return! invoker.SendAsync (requestMessage, ct) |> ensureSuccessCode
    }

    let private getAsync ct (invoker : HttpMessageInvoker) (serverUrl : string) = task {
        use requestMessage = new HttpRequestMessage (HttpMethod.Get, serverUrl)
        return! invoker.SendAsync (requestMessage, ct) |> ensureSuccessCode
    }

    /// Sends a request to a GraphQL server asynchronously.
    let sendRequestAsync ct (connection : GraphQLClientConnection) (request : GraphQLRequest) = task {
        let invoker = connection.Invoker
        let json = Serialization.buildRequestJson request.JsonSerializerOptions request.OperationName request.Query request.Variables
        let content = new StringContent (json, Encoding.UTF8, "application/json")
        return! postAsync ct invoker request.ServerUrl request.HttpHeaders content
    }

    /// Sends a request to a GraphQL server.
    let sendRequest client request =
        (sendRequestAsync CancellationToken.None client request).GetAwaiter().GetResult ()

    /// Executes an introspection schema request to a GraphQL server asynchronously.
    let sendIntrospectionRequestAsync ct (connection : GraphQLClientConnection) (serverUrl : string) httpHeaders =
        let sendGet () = getAsync ct connection.Invoker serverUrl
        let rethrow (exns : exn list) =
            let rec mapper (acc : string) (exns : exn list) =
                let aggregateMapper (ex : AggregateException) = mapper "" (List.ofSeq ex.InnerExceptions)
                match exns with
                | [] -> acc.TrimEnd ()
                | ex :: tail ->
                    match ex with
                    | :? AggregateException as ex -> mapper (acc + aggregateMapper ex + " ") tail
                    | ex -> mapper (acc + ex.Message + " ") tail
            failwith $"""Failure trying to recover introspection schema from server at "%s{serverUrl}". Errors: %s{mapper "" exns}"""
        task {
            try
                return! sendGet ()
            with getex ->
                let request = {
                    ServerUrl = serverUrl
                    HttpHeaders = httpHeaders
                    OperationName = None
                    Query = IntrospectionQuery.Definition
                    Variables = [||]
                    JsonSerializerOptions = Serialization.defaultSerializerOptions.Value
                }
                try
                    return! sendRequestAsync ct connection request
                with postex ->
                    return rethrow [ getex; postex ]
        }

    /// Executes an introspection schema request to a GraphQL server.
    let sendIntrospectionRequest client serverUrl httpHeaders =
        (sendIntrospectionRequestAsync CancellationToken.None client serverUrl httpHeaders).GetAwaiter().GetResult ()

    /// Executes a multipart request to a GraphQL server asynchronously.
    let sendMultipartRequestAsync ct (connection : GraphQLClientConnection) (request : GraphQLRequest) = task {
        let invoker = connection.Invoker
        let boundary =
            "----GraphQLProviderBoundary"
            + (Guid.NewGuid().ToString ("N"))
        let content = new MultipartContent ("form-data", boundary)
        let files =
            let rec tryMapFileVariable (name : string, value : obj) =
                match value with
                | null
                | :? string -> None
                | :? Upload as x -> Some [| name, x |]
                | OptionValue x -> x |> Option.bind (fun x -> tryMapFileVariable (name, x))
                | :? IDictionary<string, obj> as x ->
                    x
                    |> Seq.collect (fun kvp ->
                        tryMapFileVariable (name + "." + (kvp.Key.FirstCharLower ()), kvp.Value)
                        |> Option.defaultValue [||])
                    |> Array.ofSeq
                    |> Some
                | EnumerableValue x ->
                    x
                    |> Array.mapi (fun ix x -> tryMapFileVariable ($"%s{name}.%i{ix}", x))
                    |> Array.collect (Option.defaultValue [||])
                    |> Some
                | _ -> None
            request.Variables
            |> Array.collect (tryMapFileVariable >> (Option.defaultValue [||]))

        let operationContent =
            let json = Serialization.buildRequestJson request.JsonSerializerOptions request.OperationName request.Query request.Variables
            let content = new StringContent (json)
            content.Headers.Add ("Content-Disposition", "form-data; name=\"operations\"")
            content
        content.Add (operationContent)
        let mapContent =
            let json = Serialization.buildMapJson files
            let content = new StringContent (json)
            content.Headers.Add ("Content-Disposition", "form-data; name=\"map\"")
            content
        content.Add (mapContent)
        let fileContents =
            files
            |> Seq.mapi (fun _ (_, value) ->
                let content = new StreamContent (value.Stream)
                content.Headers.Add ("Content-Disposition", $"form-data; name=\"%s{value.Name}\"; filename=\"%s{value.FileName}\"")
                content.Headers.Add ("Content-Type", value.ContentType)
                content)
        fileContents |> Seq.iter content.Add
        let! result = postAsync ct invoker request.ServerUrl request.HttpHeaders content
        return result
    }

    /// Executes a multipart request to a GraphQL server.
    let sendMultipartRequest connection request =
        (sendMultipartRequestAsync CancellationToken.None connection request).GetAwaiter().GetResult ()
