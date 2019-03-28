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
    let sendRequestAsync (request : GraphQLRequest) =
        async {
            use client = new WebClient()
            client.Headers.Set("content-type", "application/json")
            if not (isNull request.CustomHeaders)
            then request.CustomHeaders |> Array.iter (fun (n, v) -> client.Headers.Set(n, v))
            let variables = 
                match request.Variables with
                | null | [||] -> JsonValue.Null
                | _ -> Map.ofSeq request.Variables |> Serialization.toJsonValue
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
       
    let sendIntrospectionRequestAsync serverUrl =
        let request =
            { ServerUrl = serverUrl
              CustomHeaders = [||]
              OperationName = None
              Query = Introspection.IntrospectionQuery
              Variables = [||] }
        sendRequestAsync request

    let sendIntrospectionRequest = sendIntrospectionRequestAsync >> Async.RunSynchronously

    let sendRequest = sendRequestAsync >> Async.RunSynchronously