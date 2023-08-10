// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open System.Collections.Generic
open System.Net.Http
open System.Text
open System.Threading
open System.Threading.Tasks

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Client
open ReflectionPatterns

/// A requrest object for making GraphQL calls using the GraphQL client module.
type GraphQLRequest  =
    { /// Gets the URL of the GraphQL server which will be called.
      ServerUrl : string
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

    let private ensureSuccessCode (response : Task<HttpResponseMessage>) = task {
        let! response = response
        return response.EnsureSuccessStatusCode()
    }

    let private addHeaders (httpHeaders : seq<string * string>) (requestMessage : HttpRequestMessage) =
        if not (isNull httpHeaders)
        then httpHeaders |> Seq.iter (fun (name, value) -> requestMessage.Headers.Add(name, value))

    let private postAsync ct (invoker : HttpMessageInvoker) (serverUrl : string) (httpHeaders : seq<string * string>) (content : HttpContent) = task {
        use requestMessage = new HttpRequestMessage(HttpMethod.Post, serverUrl)
        requestMessage.Content <- content
        addHeaders httpHeaders requestMessage
        return! invoker.SendAsync(requestMessage, ct) |> ensureSuccessCode
    }

    let private getAsync ct (invoker : HttpMessageInvoker) (serverUrl : string) = task {
        use requestMessage = new HttpRequestMessage(HttpMethod.Get, serverUrl)
        return! invoker.SendAsync(requestMessage, ct) |> ensureSuccessCode
    }

    /// Sends a request to a GraphQL server asynchronously.
    let sendRequestAsync ct (connection : GraphQLClientConnection) (request : GraphQLRequest) = task {
        let invoker = connection.Invoker
        let variables =
            match request.Variables with
            | null | [||] -> JsonValue.Null
            | _ -> Map.ofArray request.Variables |> Serialization.toJsonValue
        let operationName =
            match request.OperationName with
            | Some x -> JsonValue.String x
            | None -> JsonValue.Null
        let requestJson =
            [| "operationName", operationName
               "query", JsonValue.String request.Query
               "variables", variables |]
            |> JsonValue.Record
        let content = new StringContent(requestJson.ToString(), Encoding.UTF8, "application/json")
        return! postAsync ct invoker request.ServerUrl request.HttpHeaders content
    }

    /// Sends a request to a GraphQL server.
    let sendRequest client request = (sendRequestAsync CancellationToken.None client request).Result

    /// Executes an introspection schema request to a GraphQL server asynchronously.
    let sendIntrospectionRequestAsync ct (connection : GraphQLClientConnection) (serverUrl : string) httpHeaders =
        let sendGet() = getAsync ct connection.Invoker serverUrl
        let rethrow (exns : exn list) =
            let rec mapper (acc : string) (exns : exn list) =
                let aggregateMapper (ex : AggregateException) = mapper "" (List.ofSeq ex.InnerExceptions)
                match exns with
                | [] -> acc.TrimEnd()
                | ex :: tail ->
                    match ex with
                    | :? AggregateException as ex -> mapper (acc + aggregateMapper ex + " ") tail
                    | ex -> mapper (acc + ex.Message + " ") tail
            failwith $"""Failure trying to recover introspection schema from server at "%s{serverUrl}". Errors: %s{mapper "" exns}"""
        task {
            try return! sendGet()
            with getex ->
                let request =
                    { ServerUrl = serverUrl
                      HttpHeaders = httpHeaders
                      OperationName = None
                      Query = IntrospectionQuery.Definition
                      Variables = [||] }
                try return! sendRequestAsync ct connection request
                with postex -> return rethrow [getex; postex]
        }

    /// Executes an introspection schema request to a GraphQL server.
    let sendIntrospectionRequest client serverUrl httpHeaders =
        (sendIntrospectionRequestAsync CancellationToken.None client serverUrl httpHeaders).Result

    /// Executes a multipart request to a GraphQL server asynchronously.
    let sendMultipartRequestAsync ct (connection : GraphQLClientConnection) (request : GraphQLRequest) = task {
        let invoker = connection.Invoker
        let boundary = "----GraphQLProviderBoundary" + (Guid.NewGuid().ToString("N"))
        let content = new MultipartContent("form-data", boundary)
        let files =
            let rec tryMapFileVariable (name: string, value : obj) =
                match value with
                | null | :? string -> None
                | :? Upload as x -> Some [|name, x|]
                | OptionValue x ->
                    x |> Option.bind (fun x -> tryMapFileVariable (name, x))
                | :? IDictionary<string, obj> as x ->
                    x |> Seq.collect (fun kvp -> tryMapFileVariable (name + "." + (kvp.Key.FirstCharLower()), kvp.Value) |> Option.defaultValue [||])
                      |> Array.ofSeq
                      |> Some
                | EnumerableValue x ->
                    x |> Array.mapi (fun ix x -> tryMapFileVariable ($"%s{name}.%i{ix}", x))
                      |> Array.collect (Option.defaultValue [||])
                      |> Some
                | _ -> None
            request.Variables |> Array.collect (tryMapFileVariable >> (Option.defaultValue [||]))
        let operationContent =
            let variables =
                match request.Variables with
                | null | [||] -> JsonValue.Null
                | _ -> request.Variables |> Map.ofArray |> Serialization.toJsonValue
            let operationName =
                match request.OperationName with
                | Some x -> JsonValue.String x
                | None -> JsonValue.Null
            let json =
                [| "operationName", operationName
                   "query", JsonValue.String request.Query
                   "variables", variables |]
                |> JsonValue.Record
            let content = new StringContent(json.ToString(JsonSaveOptions.DisableFormatting))
            content.Headers.Add("Content-Disposition", "form-data; name=\"operations\"")
            content
        content.Add(operationContent)
        let mapContent =
            let files =
                files
                |> Array.mapi (fun ix (name, _) -> ix.ToString(), JsonValue.Array [| JsonValue.String ("variables." + name) |])
                |> JsonValue.Record
            let content = new StringContent(files.ToString(JsonSaveOptions.DisableFormatting))
            content.Headers.Add("Content-Disposition", "form-data; name=\"map\"")
            content
        content.Add(mapContent)
        let fileContents =
            files
            |> Array.mapi (fun ix (_, value) ->
                let content = new StreamContent(value.Stream)
                content.Headers.Add("Content-Disposition", $"form-data; name=\"%i{ix}\"; filename=\"%s{value.FileName}\"")
                content.Headers.Add("Content-Type", value.ContentType)
                content)
        fileContents |> Array.iter content.Add
        let! result = postAsync ct invoker request.ServerUrl request.HttpHeaders content
        return result
    }

    /// Executes a multipart request to a GraphQL server.
    let sendMultipartRequest connection request =
        (sendMultipartRequestAsync CancellationToken.None connection request).Result
