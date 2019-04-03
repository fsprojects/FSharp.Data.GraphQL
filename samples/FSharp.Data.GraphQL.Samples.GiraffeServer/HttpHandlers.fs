namespace FSharp.Data.GraphQL.Samples.GiraffeServer

open System.Text
open Giraffe
open Microsoft.AspNetCore.Http
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open FSharp.Data.GraphQL.Execution
open System.IO
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Control.Tasks

type HttpHandler = HttpFunc -> HttpContext -> HttpFuncResult

module HttpHandlers =
    let internalServerError : HttpHandler = setStatusCode 500

    let okWithStr str : HttpHandler = setStatusCode 200 >=> text str

    let setCorsHeaders : HttpHandler =
        setHttpHeader "Access-Control-Allow-Origin" "*"
        >=> setHttpHeader "Access-Control-Allow-Headers" "content-type"

    let setContentTypeAsJson : HttpHandler =
        setHttpHeader "Content-Type" "application/json"

    let private graphiQL (next : HttpFunc) (ctx : HttpContext) = task {
        printfn "Request headers:"
        ctx.Request.Headers
        |> Seq.map (|KeyValue|)
        |> Seq.iter (fun (name, value) -> value.ToArray() |> Array.iter (fun value -> printfn "%s: %s" name value))
        let jsonSettings =
            JsonSerializerSettings()
            |> tee (fun s ->
                s.Converters <- [| OptionConverter() :> JsonConverter |]
                s.ContractResolver <- Newtonsoft.Json.Serialization.CamelCasePropertyNamesContractResolver())
        let json =
            function
            | Direct (data, _) ->
                JsonConvert.SerializeObject(data, jsonSettings)
            | Deferred (data, _, deferred) ->
                deferred |> Observable.add(fun d -> printfn "Deferred: %s" (JsonConvert.SerializeObject(d, jsonSettings)))
                JsonConvert.SerializeObject(data, jsonSettings)
            | Stream _ ->
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
            let mapper (v : obj) =
                match v with
                | null -> null
                | :? JObject as jobj -> box (jobj.ToObject<ThingFilter>())
                | _ -> v
            Option.map (JsonConvert.DeserializeObject<Map<string, obj>> >> (Map.map (fun _ v -> mapper v))) s
        let removeSpacesAndNewLines (str : string) = 
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
            let query = query |> removeSpacesAndNewLines
            let result = Schema.executor.AsyncExecute(query, variables = variables, data = Schema.root) |> Async.RunSynchronously
            printfn "Result metadata: %A" result.Metadata
            return! okWithStr (json result) next ctx
        | Some query, None ->
            printfn "Received query: %s" query
            let query = query |> removeSpacesAndNewLines
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
        >=> graphiQL 
        >=> setContentTypeAsJson
