namespace FSharp.Data.GraphQL.IntegrationTests.Server

open System.Text
open Giraffe
open Microsoft.AspNetCore.Http
open Newtonsoft.Json
open FSharp.Data.GraphQL.Execution
open System.IO
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Control.Tasks
open Newtonsoft.Json.Linq

type HttpHandler = HttpFunc -> HttpContext -> HttpFuncResult

module HttpHandlers =
    let internalServerError : HttpHandler = setStatusCode 500

    let okWithStr str : HttpHandler = setStatusCode 200 >=> text str

    let setCorsHeaders : HttpHandler =
        setHttpHeader "Access-Control-Allow-Origin" "*"
        >=> setHttpHeader "Access-Control-Allow-Headers" "content-type"

    let setContentTypeAsJson : HttpHandler =
        setHttpHeader "Content-Type" "application/json"

    let private graphQL (next : HttpFunc) (ctx : HttpContext) = task {
        let jsonSettings = jsonSerializerSettings [| OptionConverter() |]
        let serialize d = JsonConvert.SerializeObject(d, jsonSettings)
        let json =
            function
            | Direct (data, _) ->
                JsonConvert.SerializeObject(data, jsonSettings)
            | Deferred (data, _, deferred) ->
                deferred |> Observable.add(fun d -> printfn "Deferred: %s" (serialize d))
                JsonConvert.SerializeObject(data, jsonSettings)
            | Stream data ->  
                data |> Observable.add(fun d -> printfn "Subscription data: %s" (serialize d))
                "{}"
        let tryParse fieldName (data : byte []) =
            let raw = Encoding.UTF8.GetString data
            if System.String.IsNullOrWhiteSpace(raw) |> not
            then
                let map = JsonConvert.DeserializeObject<Map<string,string>>(raw)
                match Map.tryFind fieldName map with
                | Some s when System.String.IsNullOrWhiteSpace(s) -> None
                | s -> s
            else None
        let mapString (s : string option) =
            let deserialize = JsonConvert.DeserializeObject<Map<string, obj>>
            let mapper _ (x : obj) =
                match x with
                | :? JObject as x -> box (x.ToObject<Input>(jsonSerializer [| OptionConverter() |]))
                | _ -> x
            s |> Option.map (deserialize >> Map.map mapper)
        let removeWhitespacesAndLineBreaks (str : string) = 
            str.Trim().Replace("\r\n", " ")
        let readStream (s : Stream) =
            use ms = new MemoryStream(4096)
            s.CopyTo(ms)
            ms.ToArray()
        let body = readStream ctx.Request.Body
        let query = body |> tryParse "query"
        let variables = body |> tryParse "variables" |> mapString
        match query, variables  with
        | Some query, Some variables ->
            printfn "Received query: %s" query
            printfn "Received variables: %A" variables
            let query = removeWhitespacesAndLineBreaks query
            let root = { RequestId = System.Guid.NewGuid().ToString() }
            let result = Schema.executor.AsyncExecute(query, root, variables) |> Async.RunSynchronously
            printfn "Result metadata: %A" result.Metadata
            return! okWithStr (json result) next ctx
        | Some query, None ->
            printfn "Received query: %s" query
            let query = removeWhitespacesAndLineBreaks query
            let result = Schema.executor.AsyncExecute(query) |> Async.RunSynchronously
            printfn "Result metadata: %A" result.Metadata
            return! okWithStr (json result) next ctx
        | None, _ ->
            let result = Schema.executor.AsyncExecute(Introspection.IntrospectionQuery) |> Async.RunSynchronously
            printfn "Result metadata: %A" result.Metadata
            return! okWithStr (json result) next ctx
    }

    let webApp : HttpHandler = 
        setCorsHeaders
        >=> graphQL 
        >=> setContentTypeAsJson
