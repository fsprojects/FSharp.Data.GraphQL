namespace FSharp.Data.GraphQL.Server.AspNetCore.Giraffe

open System
open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Options

open FsToolkit.ErrorHandling
open Giraffe

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Server.AspNetCore

type HttpHandler = HttpFunc -> HttpContext -> HttpFuncResult

module HttpHandlers =

    let [<Literal>] internal IndentedOptionsName = "Indented"

    let rec private moduleType = getModuleType <@ moduleType @>

    let ofTaskIResult ctx (taskRes : Task<IResult>) : HttpFuncResult = task {
        let! res = taskRes
        do! res.ExecuteAsync (ctx)
        return Some ctx
    }

    let ofTaskIResult2 ctx (taskRes: Task<Result<IResult, IResult>>) : HttpFuncResult =
        taskRes
        |> TaskResult.defaultWith id
        |> ofTaskIResult ctx

    let private handleGraphQL<'Root> (next : HttpFunc) (ctx : HttpContext) =
        let sp = ctx.RequestServices

        let logger = sp.CreateLogger moduleType

        let options = sp.GetRequiredService<IOptionsMonitor<GraphQLOptions<'Root>>> ()

        let toResponse { DocumentId = documentId; Content = content; Metadata = metadata } =

            let serializeIndented value =
                let jsonSerializerOptions = options.Get(IndentedOptionsName).SerializerOptions
                JsonSerializer.Serialize (value, jsonSerializerOptions)

            match content with
            | Direct (data, errs) ->
                logger.LogDebug (
                    $"Produced direct GraphQL response with documentId = '{{documentId}}' and metadata:\n{{metadata}}",
                    documentId,
                    metadata
                )

                if logger.IsEnabled LogLevel.Trace then
                    logger.LogTrace ($"GraphQL response data:\n:{{data}}", serializeIndented data)

                GQLResponse.Direct (documentId, data, errs)
            | Deferred (data, errs, deferred) ->
                logger.LogDebug (
                    $"Produced deferred GraphQL response with documentId = '{{documentId}}' and metadata:\n{{metadata}}",
                    documentId,
                    metadata
                )

                if logger.IsEnabled LogLevel.Debug then
                    deferred
                    |> Observable.add (function
                        | DeferredResult (data, path) ->
                            logger.LogDebug ("Produced GraphQL deferred result for path: {path}", path |> Seq.map string |> Seq.toArray |> Path.Join)

                            if logger.IsEnabled LogLevel.Trace then
                                logger.LogTrace ($"GraphQL deferred data:\n{{data}}", serializeIndented data)
                        | DeferredErrors (null, errors, path) ->
                            logger.LogDebug ("Produced GraphQL deferred errors for path: {path}", path |> Seq.map string |> Seq.toArray |> Path.Join)

                            if logger.IsEnabled LogLevel.Trace then
                                logger.LogTrace ($"GraphQL deferred errors:\n{{errors}}", errors)
                        | DeferredErrors (data, errors, path) ->
                            logger.LogDebug (
                                "Produced GraphQL deferred result with errors for path: {path}",
                                path |> Seq.map string |> Seq.toArray |> Path.Join
                            )

                            if logger.IsEnabled LogLevel.Trace then
                                logger.LogTrace (
                                    $"GraphQL deferred errors:\n{{errors}}\nGraphQL deferred data:\n{{data}}",
                                    errors,
                                    serializeIndented data
                                ))

                GQLResponse.Direct (documentId, data, errs)
            | Stream stream ->
                logger.LogDebug (
                    $"Produced stream GraphQL response with documentId = '{{documentId}}' and metadata:\n{{metadata}}",
                    documentId,
                    metadata
                )

                if logger.IsEnabled LogLevel.Debug then
                    stream
                    |> Observable.add (function
                        | SubscriptionResult data ->
                            logger.LogDebug ("Produced GraphQL subscription result")

                            if logger.IsEnabled LogLevel.Trace then
                                logger.LogTrace ($"GraphQL subscription data:\n{{data}}", serializeIndented data)
                        | SubscriptionErrors (null, errors) ->
                            logger.LogDebug ("Produced GraphQL subscription errors")

                            if logger.IsEnabled LogLevel.Trace then
                                logger.LogTrace ($"GraphQL subscription errors:\n{{errors}}", errors)
                        | SubscriptionErrors (data, errors) ->
                            logger.LogDebug ("Produced GraphQL subscription result with errors")

                            if logger.IsEnabled LogLevel.Trace then
                                logger.LogTrace (
                                    $"GraphQL subscription errors:\n{{errors}}\nGraphQL deferred data:\n{{data}}",
                                    errors,
                                    serializeIndented data
                                ))

                GQLResponse.Stream documentId
            | RequestError errs ->
                if
                    errs
                    |> Seq.choose (fun x -> x.Exception |> Option.ofValueOption)
                    |> (not << Seq.isEmpty)
                then
                    errs
                    |> List.choose (fun x -> x.Exception |> Option.ofValueOption)
                    |> List.iter (fun ex ->
                        logger.LogError (ex, "Error while processing request that generated response with documentId '{documentId}'", documentId))
                else
                    logger.LogWarning (
                        ("Produced request error GraphQL response with:\n"
                         + "- documentId: '{documentId}'\n"
                         + "- error(s):\n    {requestError}\n"
                         + "- metadata:\n    {metadata}\n"),
                        documentId,
                        errs,
                        metadata
                    )

                GQLResponse.RequestError (documentId, errs)

        /// Checks if the request contains a body
        let checkIfHasBody (request : HttpRequest) = task {
            if request.Body.CanSeek then
                return (request.Body.Length > 0L)
            else
                request.EnableBuffering ()
                let body = request.Body
                let buffer = Array.zeroCreate 1
                let! bytesRead = body.ReadAsync (buffer, 0, 1)
                body.Seek (0, SeekOrigin.Begin) |> ignore
                return bytesRead > 0
        }

        /// <summary>Check if the request is an introspection query
        /// by first checking on such properties as `GET` method or `empty request body`
        /// and lastly by parsing document AST for introspection operation definition.
        /// </summary>
        /// <returns>Result of check of <see cref="OperationType"/></returns>
        let checkOperationType (ctx : HttpContext) = taskResult {

            let checkAnonymousFieldsOnly (ctx : HttpContext) = taskResult {
                let! gqlRequest = ctx.TryBindJsonAsync<GQLRequestContent> (GQLRequestContent.expectedJSON)
                let! ast = Parser.parseOrIResult ctx.Request.Path.Value gqlRequest.Query
                let operationName = gqlRequest.OperationName |> Skippable.toOption

                let createParsedContent () = {
                    Query = gqlRequest.Query
                    Ast = ast
                    OperationName = gqlRequest.OperationName
                    Variables = gqlRequest.Variables
                }
                if ast.IsEmpty then
                    logger.LogTrace ("Request is not GET, but 'query' field is an empty string. Must be an introspection query")
                    return IntrospectionQuery <| ValueNone
                else
                    match Ast.findOperationByName operationName ast with
                    | None ->
                        logger.LogTrace "Document has no operation"
                        return IntrospectionQuery <| ValueNone
                    | Some op ->
                        if not (op.OperationType = Ast.Query) then
                            logger.LogTrace "Document operation is not of type Query"
                            return createParsedContent () |> OperationQuery
                        else
                            let hasNonMetaFields =
                                Ast.containsFieldsBeyond
                                    Ast.metaTypeFields
                                    (fun x -> logger.LogTrace ($"Operation Selection in Field with name: {{fieldName}}", x.Name))
                                    (fun _ -> logger.LogTrace "Operation Selection is non-Field type")
                                    op

                            if hasNonMetaFields then
                                return createParsedContent () |> OperationQuery
                            else
                                return IntrospectionQuery <| ValueSome ast
            }

            let request = ctx.Request

            if HttpMethods.Get = request.Method then
                logger.LogTrace ("Request is GET. Must be an introspection query")
                return IntrospectionQuery <| ValueNone
            else
                let! hasBody = checkIfHasBody request

                if not hasBody then
                    logger.LogTrace ("Request is not GET, but has no body. Must be an introspection query")
                    return IntrospectionQuery <| ValueNone
                else
                    return! checkAnonymousFieldsOnly ctx
        }

        /// Execute default or custom introspection query
        let executeIntrospectionQuery (executor : Executor<_>) (ast : Ast.Document voption) = task {
            let! result =
                match ast with
                | ValueNone -> executor.AsyncExecute IntrospectionQuery.Definition
                | ValueSome ast -> executor.AsyncExecute ast

            let response = result |> toResponse
            return Results.Ok response
        }

        /// Execute the operation for given request
        let executeOperation (executor: Executor<_>) content = task {
            let operationName = content.OperationName |> Skippable.filter (not << isNull) |> Skippable.toOption
            let variables = content.Variables |> Skippable.filter (not << isNull) |> Skippable.toOption

            operationName
            |> Option.iter (fun on -> logger.LogTrace ("GraphQL operation name: '{operationName}'", on))

            logger.LogTrace ($"Executing GraphQL query:\n{{query}}", content.Query)

            variables
            |> Option.iter (fun v -> logger.LogTrace ($"GraphQL variables:\n{{variables}}", v))

            let root = options.CurrentValue.RootFactory ctx

            let! result =
                Async.StartAsTask (
                    executor.AsyncExecute (content.Ast, root, ?variables = variables, ?operationName = operationName),
                    cancellationToken = ctx.RequestAborted
                )

            let response = result |> toResponse
            return Results.Ok response
        }

        if ctx.RequestAborted.IsCancellationRequested then
            Task.FromResult None
        else
            taskResult {
                let executor = options.CurrentValue.SchemaExecutor
                match! checkOperationType ctx with
                | IntrospectionQuery optionalAstDocument -> return! executeIntrospectionQuery executor optionalAstDocument
                | OperationQuery content -> return! executeOperation executor content
            }
            |> ofTaskIResult2 ctx

    let graphQL<'Root> : HttpHandler = choose [ POST; GET ] >=> handleGraphQL<'Root>
