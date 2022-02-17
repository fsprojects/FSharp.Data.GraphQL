namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open System.Text
open Giraffe
open Microsoft.AspNetCore.Http
open Newtonsoft.Json
open FSharp.Data.GraphQL.Execution
open System.IO
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Control.Tasks

type HttpHandler = HttpFunc -> HttpContext -> HttpFuncResult

module HttpHandlers =
    let private converters : JsonConverter [] = [| OptionConverter() |]
    let private jsonSettings = jsonSerializerSettings converters

    let internalServerError : HttpHandler = setStatusCode 500

    let okWithStr str : HttpHandler = setStatusCode 200 >=> text str

    let setCorsHeaders : HttpHandler =
        setHttpHeader "Access-Control-Allow-Origin" "*"
        >=> setHttpHeader "Access-Control-Allow-Headers" "content-type"

    let setContentTypeAsJson : HttpHandler =
        setHttpHeader "Content-Type" "application/json"

    let private graphQL (next : HttpFunc) (ctx : HttpContext) = task {
        let serialize d = JsonConvert.SerializeObject(d, jsonSettings)

        let json =
            function
            | Direct (data, _) ->
                printfn "%A" data
                JsonConvert.SerializeObject(data, jsonSettings)
            | Deferred (data, _, deferred) ->
                deferred |> Observable.add(fun d -> printfn "Deferred: %s" (serialize d))
                JsonConvert.SerializeObject(data, jsonSettings)
            | Stream data ->
                data |> Observable.add(fun d -> printfn "Subscription data: %s" (serialize d))
                "{}"

        let removeWhitespacesAndLineBreaks (str : string) = str.Trim().Replace("\r\n", " ")

        let readStream (s : Stream) =
            use ms = new MemoryStream(4096)
            s.CopyTo(ms)
            ms.ToArray()

        let data = Encoding.UTF8.GetString(readStream ctx.Request.Body) |> Deserialize.deserializeQueryAndVariables

        let query = data |> Option.map fst
        let variables = data |> Option.bind snd

        match query, variables  with
        | Some query, Some variables ->
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
