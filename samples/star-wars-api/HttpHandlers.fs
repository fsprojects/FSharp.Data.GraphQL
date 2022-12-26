namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open System.Collections.Immutable
open System.IO
open System.Text
open System.Text.Json
open System.Threading.Tasks
open Giraffe
open Microsoft.AspNetCore.Http
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types

type HttpHandler = HttpFunc -> HttpContext -> HttpFuncResult

type private GQLRequestContent =
    { Query : string
      // TODO: Add OperationName handling
      OperationName : string voption
      Variables : ImmutableDictionary<string, JsonElement> voption }

module HttpHandlers =

    let ofIResult ctx (res: IResult) : HttpFuncResult = task {
            do! res.ExecuteAsync(ctx)
            return Some ctx
        }

    let ofTaskIResult ctx (taskRes: Task<IResult>) : HttpFuncResult = task {
        let! res = taskRes
        do! res.ExecuteAsync(ctx)
        return Some ctx
    }

    let setCorsHeaders : HttpHandler =
        setHttpHeader "Access-Control-Allow-Origin" "*"
        >=> setHttpHeader "Access-Control-Allow-Headers" "content-type"

    let private graphQL (next : HttpFunc) (ctx : HttpContext) =
        task {

            // TODO: validate the result
            let toResponse documentId =
                function
                | Direct   (data, errs) ->
                    { DocumentId = documentId
                      Data = data
                      Errors = errs }
                | Deferred (data, errs, deferred) ->
                    // TODO: Print to logger
                    deferred |> Observable.add (fun d -> printfn "Deferred: %s" (JsonSerializer.Serialize(d, Json.serializerOptions)))
                    { DocumentId = documentId
                      Data = data
                      Errors = errs }
                | Stream data ->
                    // TODO: Print to logger
                    data |> Observable.add (fun d -> printfn "Subscription data: %s" (JsonSerializer.Serialize(d, Json.serializerOptions)))
                    { DocumentId = documentId
                      Data = null
                      Errors = [] }

            let removeWhitespacesAndLineBreaks (str : string) = str.Trim().Replace ("\r\n", " ")

            let hasData (context: HttpContext) =
                match context.Request.Body with
                | :? System.IO.Stream as stream ->
                    if stream.CanSeek then
                        stream.Length > 0L
                    else
                        let buffer = Array.zeroCreate 1
                        let valu = stream.Read(buffer, 0, 1)
                        valu > 0
                | _ -> false

            // TODO: Figure out how to check if body is empty
            // TODO: Return introspection on GET
            if (hasData ctx = false && ctx.Request.Method = "GET")
            then
                let! result = Schema.executor.AsyncExecute (Introspection.IntrospectionQuery)
                printfn "Result metadata: %A" result.Metadata
                let response = toResponse result.DocumentId result.Content
                return Results.Ok response
            else

            let! request = ctx.BindJsonAsync<GQLRequestContent>()
            let query = request.Query

            return!
                match request.Variables with
                | ValueSome variables -> task {
                        printfn "Received query: %s" query
                        printfn "Received variables: %A" variables
                        let query = removeWhitespacesAndLineBreaks query
                        let root = { RequestId = System.Guid.NewGuid().ToString () }
                        let! result = Schema.executor.AsyncExecute (query, root, variables)
                        printfn "Result metadata: %A" result.Metadata
                        let response = toResponse result.DocumentId result.Content
                        return Results.Ok response
                    }
                | ValueNone -> task {
                        printfn "Received query: %s" query
                        let query = removeWhitespacesAndLineBreaks query
                        let! result = Schema.executor.AsyncExecute (query)
                        printfn "Result metadata: %A" result.Metadata
                        let response = toResponse result.DocumentId result.Content
                        return Results.Ok response
                    }
        }
        |> ofTaskIResult ctx

    let webApp : HttpHandler = setCorsHeaders >=> choose [ POST; GET ] >=> graphQL
