namespace FSharp.Data.GraphQL.IntegrationTests.Server

open System
open System.Collections.Immutable
open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Json
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Options

open Giraffe
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types

type HttpHandler = HttpFunc -> HttpContext -> HttpFuncResult

type private GQLRequestContent =
    { Query : string
      OperationName : Skippable<string>
      Variables : Skippable<ImmutableDictionary<string, JsonElement>> }

[<Struct>]
type private OperationType =
    | IntrospectionQuery of introspection: string voption
    | OperationQuery of operation: GQLRequestContent

module HttpHandlers =

    let rec private moduleType = getModuleType <@ moduleType @>

    let ofTaskIResult ctx (taskRes: Task<IResult>) : HttpFuncResult = task {
        let! res = taskRes
        do! res.ExecuteAsync(ctx)

        return Some ctx
    }

    /// Set CORS to allow external servers (React samples) to call this API
    let setCorsHeaders : HttpHandler =
        setHttpHeader "Access-Control-Allow-Origin" "*"
        >=> setHttpHeader "Access-Control-Allow-Headers" "content-type"

    let private graphQL (next : HttpFunc) (ctx : HttpContext) =
        task {
            let logger = ctx.RequestServices.CreateLogger moduleType
            let jsonSerializerOptions = ctx.RequestServices.GetRequiredService<IOptions<JsonOptions>>().Value.SerializerOptions
            let request = ctx.Request

            let isGet = request.Method = HttpMethods.Get

            // TODO: validate the result
            /// Resolve response type and wrap it into an appropriate object
            let toResponse { DocumentId = documentId; Content = content; Metadata = metadata } =
                match content with
                | Direct (data, errs) ->
                    logger.LogInformation (
                        $"Produced direct GraphQL response with documentId = '{{documentId}}' and metadata:{Environment.NewLine}{{metadata}}",
                        documentId,
                        metadata
                    )

                    if logger.IsEnabled LogLevel.Trace then
                        logger.LogTrace (
                            $"GraphQL response data:{Environment.NewLine}:{{data}}",
                            JsonSerializer.Serialize (data, jsonSerializerOptions)
                        )

                    GQLResponse.Direct (documentId, data, errs)

                | Deferred (data, errs, deferred) ->
                    logger.LogInformation (
                        $"Produced deferred GraphQL response with documentId = '{{documentId}}' and metadata:{Environment.NewLine}{{metadata}}",
                        documentId,
                        metadata
                    )

                    if logger.IsEnabled LogLevel.Information then
                        deferred
                        |> Observable.add (function
                            | DeferredResult (data, path) ->
                                logger.LogInformation (
                                    "Produced GraphQL deferred result for path: {path}",
                                    path |> Seq.map string |> Seq.toArray |> Path.Join
                                )

                                if logger.IsEnabled LogLevel.Trace then
                                    logger.LogTrace (
                                        $"GraphQL deferred data:{Environment.NewLine}{{data}}",
                                        (JsonSerializer.Serialize (data, jsonSerializerOptions))
                                    )
                            | DeferredErrors (null, errors, path) ->
                                logger.LogInformation (
                                    "Produced GraphQL deferred errors for path: {path}",
                                    path |> Seq.map string |> Seq.toArray |> Path.Join
                                )

                                if logger.IsEnabled LogLevel.Trace then
                                    logger.LogTrace ($"GraphQL deferred errors:{Environment.NewLine}{{errors}}", errors)
                            | DeferredErrors (data, errors, path) ->
                                logger.LogInformation (
                                    "Produced GraphQL deferred result with errors for path: {path}",
                                    path |> Seq.map string |> Seq.toArray |> Path.Join
                                )

                                if logger.IsEnabled LogLevel.Trace then
                                    logger.LogTrace (
                                        $"GraphQL deferred errors:{Environment.NewLine}{{errors}}{Environment.NewLine}GraphQL deferred data:{Environment.NewLine}{{data}}",
                                        errors,
                                        (JsonSerializer.Serialize (data, jsonSerializerOptions))
                                    ))

                    GQLResponse.Direct (documentId, data, errs)

                | Stream stream ->
                    logger.LogInformation (
                        $"Produced stream GraphQL response with documentId = '{{documentId}}' and metadata:{Environment.NewLine}{{metadata}}",
                        documentId,
                        metadata
                    )

                    if logger.IsEnabled LogLevel.Information then
                        stream
                        |> Observable.add (function
                            | SubscriptionResult data ->
                                logger.LogInformation ("Produced GraphQL subscription result")

                                if logger.IsEnabled LogLevel.Trace then
                                    logger.LogTrace (
                                        $"GraphQL subscription data:{Environment.NewLine}{{data}}",
                                        (JsonSerializer.Serialize (data, jsonSerializerOptions))
                                    )
                            | SubscriptionErrors (null, errors) ->
                                logger.LogInformation ("Produced GraphQL subscription errors")

                                if logger.IsEnabled LogLevel.Trace then
                                    logger.LogTrace ($"GraphQL subscription errors:{Environment.NewLine}{{errors}}", errors)
                            | SubscriptionErrors (data, errors) ->
                                logger.LogInformation ("Produced GraphQL subscription result with errors")

                                if logger.IsEnabled LogLevel.Trace then
                                    logger.LogTrace (
                                        $"GraphQL subscription errors:{Environment.NewLine}{{errors}}{Environment.NewLine}GraphQL deferred data:{Environment.NewLine}{{data}}",
                                        errors,
                                        (JsonSerializer.Serialize (data, jsonSerializerOptions))
                                    ))

                    GQLResponse.Stream documentId

                | RequestError errs ->
                    logger.LogInformation(
                        $"Produced request error GraphQL response with documentId = '{{documentId}}' and metadata:{Environment.NewLine}{{metadata}}",
                        documentId,
                        metadata
                    )
                    GQLResponse.RequestError (documentId, errs)

            let removeWhitespacesAndLineBreaks (str : string) = str.Trim().Replace ("\r\n", " ")

            /// Check if the request contains body or not
            let checkIfHasBody () = task {
                match request.Body.CanSeek with
                | true -> return (request.Body.Length > 0L)
                | false ->
                    // EnableBuffering allows us to read the Body even if it's been read already somewhere else.
                    // See https://devblogs.microsoft.com/dotnet/re-reading-asp-net-core-request-bodies-with-enablebuffering/
                    request.EnableBuffering()
                    let body = request.Body
                    let buffer = Array.zeroCreate 1
                    let! bytesRead = body.ReadAsync(buffer, 0, 1)
                    body.Seek(0, SeekOrigin.Begin) |> ignore
                    return bytesRead > 0
            }

            let detectIntrospectionQuery () = task {
                /// Check for the conditions that would make this an introspection query
                if isGet then return IntrospectionQuery ValueNone
                else
                    let! hasBody = checkIfHasBody()
                    if not hasBody then return IntrospectionQuery ValueNone
                    else
                        let! request = ctx.BindJsonAsync<GQLRequestContent>()
                        if Introspection.IntrospectionQuery.Contains request.Query
                        then return ValueSome request.Query |> IntrospectionQuery
                        else return OperationQuery request
            }

            /// Execute default or custom introspection query
            let executeIntrospectionQuery (query : string voption) = task {
                let! result =
                    match query with
                    | ValueNone ->
                        logger.LogInformation ("Executing default GraphQL introspection query")
                        Schema.executor.AsyncExecute (Introspection.IntrospectionQuery)
                    | ValueSome query ->
                        logger.LogInformation ($"Executing GraphQL introspection query:{Environment.NewLine}{query}", query)
                        Schema.executor.AsyncExecute query

                let response = result |> toResponse
                return Results.Ok response
            }

            /// Execute the operation for given request
            let executeOperation request = task {
                // let! request = ctx.BindJsonAsync<GQLRequestContent>()
                let query = request.Query

                logger.LogTrace ($"Executing GraphQL query:{Environment.NewLine}{query}", query)
                let operationName = request.OperationName |> Skippable.toOption

                operationName
                |> Option.iter (fun on -> logger.LogTrace ($"GraphQL operation name: '{operationName}'", on))

                let variables = request.Variables |> Skippable.toOption

                variables
                |> Option.iter (fun vars -> logger.LogTrace ($"GraphQL variables:{Environment.NewLine}{variables}", variables))

                let root = { RequestId = System.Guid.NewGuid () |> string }
                let query = removeWhitespacesAndLineBreaks query
                let! result = Schema.executor.AsyncExecute (query, root, ?variables = variables, ?operationName = operationName)
                let response = result |> toResponse
                return Results.Ok response
            }

            match! detectIntrospectionQuery () with
            | IntrospectionQuery query -> return! executeIntrospectionQuery query
            | OperationQuery gqlRequestContent -> return! executeOperation gqlRequestContent
        } |> ofTaskIResult ctx

    let webApp : HttpHandler = setCorsHeaders >=> choose [ POST; GET ] >=> graphQL
