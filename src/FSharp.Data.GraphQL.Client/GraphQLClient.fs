/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Client

open System.Net
open FSharp.Data
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types

type GraphQLRequest  =
    { ServerUrl : string
      CustomHeaders: (string * string) [] option
      OperationName : string option
      Query : string
      Variables : (string * obj) [] option }

module GraphQLClient =
    let private send (method : string) (request : GraphQLRequest) =
        async {
            use client = new WebClient()
            client.Headers.Set("content-type", "application/json")
            request.CustomHeaders |> Option.iter (Seq.iter (fun (n, v) -> client.Headers.Set(n, v)))
            let variables =
                match request.Variables with
                | Some x -> Map.ofSeq x |> Serialization.toJsonValue
                | None -> JsonValue.Null
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
                client.UploadStringTaskAsync(request.ServerUrl, method, requestJson.ToString())
                |> Async.AwaitTask
        }
       
    let sendIntrospectionRequestAsync serverUrl =
        let request =
            { ServerUrl = serverUrl
              CustomHeaders = None
              OperationName = None
              Query = Introspection.introspectionQuery
              Variables = None }
        async {
            try return! send "GET" request
            with _ -> return! send "POST" request
        }

    let sendIntrospectionRequest = sendIntrospectionRequestAsync >> Async.RunSynchronously
    
    let sendRequestAsync = send "POST"

    let sendRequest = sendRequestAsync >> Async.RunSynchronously