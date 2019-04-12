/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
namespace FSharp.Data.GraphQL.Client

open System
open System.Net
open FSharp.Data
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types

type GraphQLRequest  =
    { ServerUrl : string
      CustomHeaders: (string * string) []
      OperationName : string option
      Query : string
      Variables : (string * obj) [] }

module GraphQLClient =
    let private buildClient (headers : (string * string) []) =
        let client = new WebClient()
        client.Headers.Set("content-type", "application/json")
        if not (isNull headers)
        then headers |> Array.iter (fun (n, v) -> client.Headers.Set(n, v))
        client

    let sendRequestAsync (request : GraphQLRequest) =
        async {
            use client = buildClient request.CustomHeaders
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
            return!
                client.UploadStringTaskAsync(request.ServerUrl, requestJson.ToString())
                |> Async.AwaitTask
        }
       
    let sendIntrospectionRequestAsync (serverUrl : string) customHeaders =
        let sendGet () =
            async {
                use client = buildClient customHeaders
                return!
                    client.DownloadStringTaskAsync(serverUrl)
                    |> Async.AwaitTask
            }
        async {
            try return! sendGet ()
            with getex ->
                let request =
                    { ServerUrl = serverUrl
                      CustomHeaders = customHeaders
                      OperationName = None
                      Query = Introspection.IntrospectionQuery
                      Variables = [||] }
                try return! sendRequestAsync request
                with postex -> return (raise (AggregateException("Failure requesting server schema via both GET and POST methods.", [|getex; postex|])))
        }

    let sendIntrospectionRequest serverUrl customHeaders = 
        sendIntrospectionRequestAsync serverUrl customHeaders
        |> Async.RunSynchronously

    let sendRequest = sendRequestAsync >> Async.RunSynchronously