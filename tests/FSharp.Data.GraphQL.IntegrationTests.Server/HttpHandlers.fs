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
open Giraffe.HttpStatusCodeHandlers.RequestErrors
open Giraffe.HttpStatusCodeHandlers.Successful
open Microsoft.AspNetCore.WebUtilities

type HttpHandler = HttpFunc -> HttpContext -> HttpFuncResult

module HttpHandlers =
    let private converters : JsonConverter [] = [| OptionConverter(); IDictionaryConverter() |]
    let private jsonSettings = jsonSerializerSettings converters
    let private jsonSerializer = jsonSerializer converters

    let internalServerError : HttpHandler = setStatusCode 500

    let okWithStr str : HttpHandler = setStatusCode 200 >=> text str

    let setCorsHeaders : HttpHandler =
        setHttpHeader "Access-Control-Allow-Origin" "*"
        >=> setHttpHeader "Access-Control-Allow-Headers" "content-type"

    let setContentTypeAsJson : HttpHandler =
        setHttpHeader "Content-Type" "application/json"

    let isMultipartRequest (req : HttpRequest) =
        not (System.String.IsNullOrEmpty(req.ContentType)) && req.ContentType.Contains("multipart/form-data")

    let getMultipartRequestBoundary (req : HttpRequest) =
        req.Headers.GetCommaSeparatedValues("Content-Type")
        |> Seq.map (fun v -> v.TrimStart())
        |> Seq.tryFind (fun v -> v.Contains("boundary"))
        |> Option.map (fun v -> v.Remove(0, v.IndexOf('=') + 1))
        |> Option.map (fun v -> v.Trim('"'))

    let private graphQL (next : HttpFunc) (ctx : HttpContext) = task {
        let serialize d = JsonConvert.SerializeObject(d, jsonSettings)

        let rec parseVariables (variables : obj) =
            match variables with
            | :? string as x -> JsonConvert.DeserializeObject<Map<string, obj>>(x, jsonSettings)
            | _ -> failwithf "Failure deserializing variables. Unexpected variables object format."

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
        
        let removeWhitespacesAndLineBreaks (str : string) = str.Trim().Replace("\r\n", " ")

        let readStream (s : Stream) =
            use ms = new MemoryStream(4096)
            s.CopyTo(ms)
            ms.ToArray()
        
        let root = { RequestId = System.Guid.NewGuid().ToString() }

        let addRequestType (requestType : string) (response : GQLResponse) =
            let mapper (content : GQLResponseContent) =
                let dataMapper (data : Output) : Output = 
                    let data = data |> Seq.map (|KeyValue|) |> Map.ofSeq
                    upcast data.Add("requestType", requestType)
                match content with
                | GQLResponseContent.Direct (data, errors) -> Direct (dataMapper data, errors)
                | GQLResponseContent.Deferred (data, errors, deferred) -> Deferred (dataMapper data, errors, deferred)
                | _ -> content
            { Content = mapper response.Content; Metadata = response.Metadata }

        if isMultipartRequest ctx.Request
        then
            let copyBodyToMemory (req : HttpRequest) =
                let ms = new MemoryStream(4096)
                req.Body.CopyTo(ms)
                ms.Position <- 0L
                ms
            match getMultipartRequestBoundary ctx.Request with
            | Some boundary ->
                use ms = copyBodyToMemory(ctx.Request)
                let reader = MultipartReader(boundary, ms)
                let request = MultipartRequest.read(reader, jsonSerializer) |> Async.AwaitTask |> Async.RunSynchronously
                let results = 
                    request.Operations
                    |> Seq.map (fun op -> 
                        Schema.executor.AsyncExecute(op.Query, variables = op.Variables, data = root)
                        |> Async.RunSynchronously 
                        |> addRequestType "Multipart")
                    |> Seq.map json
                    |> List.ofSeq
                match results with
                | [ result ] -> 
                    return! OK result next ctx
                | results -> 
                    let result = JArray.FromObject(results)
                    return! OK result next ctx
            | None -> 
                return! badRequest (text "Invalid multipart request header: missing boundary value.") next ctx
        else
            let data = 
                let raw = Encoding.UTF8.GetString(readStream ctx.Request.Body)
                if System.String.IsNullOrWhiteSpace(raw)
                then None
                else Some (JsonConvert.DeserializeObject<Map<string, obj>>(raw, jsonSettings))
            let query =
                data |> Option.bind (fun data ->
                    if data.ContainsKey("query")
                    then
                        match data.["query"] with
                        | :? string as x -> Some x
                        | _ -> failwith "Failure deserializing repsonse. Could not read query - it is not stringified in request."
                    else None)
            let variables =
                data |> Option.bind (fun data ->
                    if data.ContainsKey("variables")
                    then parseVariables data.["variables"] |> Some
                    else None)
            match query, variables  with
            | Some query, Some variables ->
                printfn "Received query: %s" query
                printfn "Received variables: %A" variables
                let query = removeWhitespacesAndLineBreaks query
                let result = Schema.executor.AsyncExecute(query, root, variables) |> Async.RunSynchronously |> addRequestType "Classic"
                printfn "Result metadata: %A" result.Metadata
                return! okWithStr (json result) next ctx
            | Some query, None ->
                printfn "Received query: %s" query
                let query = removeWhitespacesAndLineBreaks query
                let result = Schema.executor.AsyncExecute(query) |> Async.RunSynchronously |> addRequestType "Classic"
                printfn "Result metadata: %A" result.Metadata
                return! okWithStr (json result) next ctx
            | None, _ ->
                let result = Schema.executor.AsyncExecute(Introspection.IntrospectionQuery) |> Async.RunSynchronously |> addRequestType "Classic"
                printfn "Result metadata: %A" result.Metadata
                return! okWithStr (json result) next ctx
    }

    let webApp : HttpHandler = 
        setCorsHeaders
        >=> graphQL 
        >=> setContentTypeAsJson
