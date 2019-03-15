/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Client

open System.Net
open FSharp.Data
open System

type GraphQLRequest  =
    { ServerUrl : string
      CustomHeaders: (string * string) seq option
      OperationName : string option
      Query : string
      Variables : (string * obj) seq option }

type QueryDefinitions =
    { Variables : (string * Type) []
      OperationNames : string [] }

module GraphQLClient =
    let sendRequestAsync (request : GraphQLRequest) =
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
                client.UploadStringTaskAsync(request.ServerUrl, requestJson.ToString())
                |> Async.AwaitTask
        }
    let sendRequest = sendRequestAsync >> Async.RunSynchronously