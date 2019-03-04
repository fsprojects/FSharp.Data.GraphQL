/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Client

open System.Net
open FSharp.Data
open System

module GraphQLServer =
    let makeRequest (url : string) (headers : (string * string) seq option) (query : string) (variables : (string * obj) seq option) =
        let getOperationName (query : string) =
            let normalized = query.Trim()
            if Seq.head normalized = '{' then None
            else
                let endIndex = normalized.IndexOf('{')
                let operation = normalized.Substring(0, endIndex).Split([|" "|], StringSplitOptions.RemoveEmptyEntries)
                match operation with
                | [|_; name|] -> Some name
                | _ -> None
        async {
            use client = new WebClient()
            client.Headers.Set("content-type", "application/json")
            headers |> Option.iter (Seq.iter (fun (n, v) -> client.Headers.Set(n, v)))
            let variables =
                match variables with
                | Some x -> Map.ofSeq x |> Serialization.toJsonValue
                | None -> JsonValue.Null
            let operationName =
                match getOperationName query with
                | Some x -> JsonValue.String x
                | None -> JsonValue.Null
            let requestJson =         
                [| "operationName", operationName
                   "query", JsonValue.String query
                   "variables", variables |]
                |> JsonValue.Record
            return!
                client.UploadStringTaskAsync(url, requestJson.ToString())
                |> Async.AwaitTask
        }