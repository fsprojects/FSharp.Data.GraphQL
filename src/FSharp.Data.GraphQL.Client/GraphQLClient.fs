/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open System.Net.Http
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Client
open System.Text

type GraphQLClientConnection() =
    let client = new HttpClient()
    member internal __.Client = client
    interface IDisposable with
        member __.Dispose() = client.Dispose()

type GraphQLRequest  =
    { ServerUrl : string
      HttpHeaders: seq<string * string>
      OperationName : string option
      Query : string
      Variables : (string * obj) [] }

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
    
    let sendRequest client request =
        sendRequestAsync client request
        |> Async.RunSynchronously

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

    let sendIntrospectionRequest client serverUrl httpHeaders = 
        sendIntrospectionRequestAsync client serverUrl httpHeaders
        |> Async.RunSynchronously