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
open Microsoft.AspNetCore.WebUtilities
open FSharp.Data.GraphQL.Ast

type HttpHandler = HttpFunc -> HttpContext -> HttpFuncResult

module HttpHandlers =
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

        let rec parseVariables (schema : ISchema) (defs : VariableDefinition list) (variables : obj) =
            let casted =
                match variables with
                | :? Map<string, obj> as x -> x
                | :? JToken as x -> x.ToObject<Map<string, obj>>(jsonSerializer)
                | :? string as x -> JsonConvert.DeserializeObject<Map<string, obj>>(x, jsonSettings)
                | _ -> failwithf "Failure deserializing variables. Unexpected variables object format."
            Variables.read schema defs casted

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

        let parseVariableDefinitions (query : string) =
            let ast = Parser.parse query
            ast.Definitions
            |> List.choose (function OperationDefinition def -> Some def.VariableDefinitions | _ -> None)
            |> List.collect id

        let getVariables (vardefs : VariableDefinition list) (data : Map<string, obj>) =
            match data.TryFind("variables") with
            | Some null -> None
            | Some variables -> parseVariables Schema.schema vardefs variables |> Some
            | _ -> None

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
                    |> List.map (fun op ->
                        let result =
                            match op.Variables with
                            | Some variables ->
                                let variables = parseVariables Schema.schema (parseVariableDefinitions op.Query) variables
                                Schema.executor.AsyncExecute(op.Query, variables = variables, data = root)
                            | None -> Schema.executor.AsyncExecute(op.Query, data = root)
                        result |> Async.RunSynchronously |> addRequestType "Multipart")
                match results with
                | [ result ] -> 
                    return! okWithStr (json result) next ctx
                | results -> 
                    let result = JArray.FromObject(List.map json results).ToString()
                    return! okWithStr result next ctx
            | None -> 
                return! badRequest (text "Invalid multipart request header: missing boundary value.") next ctx
        else
            let request =
                let data = 
                    let raw = Encoding.UTF8.GetString(readStream ctx.Request.Body)
                    if System.String.IsNullOrWhiteSpace(raw)
                    then None
                    else Some (JsonConvert.DeserializeObject<Map<string, obj>>(raw, jsonSettings))
                data |> Option.bind (fun data ->
                    if data.ContainsKey("query")
                    then
                        match data.["query"] with
                        | :? string as query -> Some (query, getVariables (parseVariableDefinitions query) data)
                        | _ -> failwith "Failure deserializing repsonse. Could not read query - it is not stringified in request."
                    else None)
            match request  with
            | Some (query, Some variables) ->
                printfn "Received query: %s" query
                printfn "Received variables: %A" variables
                let query = removeWhitespacesAndLineBreaks query
                let result = Schema.executor.AsyncExecute(query, root, variables) |> Async.RunSynchronously |> addRequestType "Classic"
                printfn "Result metadata: %A" result.Metadata
                return! okWithStr (json result) next ctx
            | Some (query, None) ->
                printfn "Received query: %s" query
                let query = removeWhitespacesAndLineBreaks query
                let result = Schema.executor.AsyncExecute(query) |> Async.RunSynchronously |> addRequestType "Classic"
                printfn "Result metadata: %A" result.Metadata
                return! okWithStr (json result) next ctx
            | None ->
                let result = Schema.executor.AsyncExecute(Introspection.IntrospectionQuery) |> Async.RunSynchronously |> addRequestType "Classic"
                printfn "Result metadata: %A" result.Metadata
                return! okWithStr (json result) next ctx
    }

    let webApp : HttpHandler = 
        setCorsHeaders
        >=> graphQL 
        >=> setContentTypeAsJson
