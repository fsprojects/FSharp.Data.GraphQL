/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Client

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
            try return! client.UploadStringTaskAsync(request.ServerUrl, requestJson.ToString()) |> Async.AwaitTask
            with _ -> return failwith "Could not connect to the server."
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
            with _ ->
                let request =
                    { ServerUrl = serverUrl
                      CustomHeaders = customHeaders
                      OperationName = None
                      Query = Introspection.IntrospectionQuery
                      Variables = [||] }
                try return! sendRequestAsync request
                with _ -> return failwith "Could not connect to the server to get the introspection schema."
        }

    let sendIntrospectionRequest serverUrl customHeaders = 
        sendIntrospectionRequestAsync serverUrl customHeaders
        |> Async.RunSynchronously

    let sendRequest = sendRequestAsync >> Async.RunSynchronously