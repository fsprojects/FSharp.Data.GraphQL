namespace FSharp.Data.GraphQL.Samples.StarWarsApi

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

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Ast
open Giraffe

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
            let serializeWithOptions data = JsonSerializer.Serialize (data, jsonSerializerOptions)
            let request = ctx.Request

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
                            $"GraphQL response data:{Environment.NewLine}{{data}}",
                            serializeWithOptions data
                        )

                    GQLResponse.Direct (documentId, data, errs)

                | Deferred (data, errs, deferred) ->
                    logger.LogInformation (
                        $"Produced deferred GraphQL response with documentId = '{{documentId}}' and metadata:{Environment.NewLine}{{metadata}}",
                        documentId,
                        metadata
                    )
                    if errs.Length > 0 then
                        logger.LogTrace ($"{{number}} errors:{Environment.NewLine}{{errs}}", errs.Length, errs)

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
                                        serializeWithOptions data
                                    )
                            | DeferredErrors (null, errors, path) ->
                                logger.LogInformation (
                                    "Produced GraphQL deferred errors for path: {path}",
                                    path |> Seq.map string |> Seq.toArray |> Path.Join
                                )
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
                                        serializeWithOptions data
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
                                        serializeWithOptions data
                                    )
                            | SubscriptionErrors (null, errors) ->
                                logger.LogInformation ("Produced GraphQL subscription errors")
                                logger.LogTrace ($"GraphQL subscription errors:{Environment.NewLine}{{errors}}", errors)
                            | SubscriptionErrors (data, errors) ->
                                logger.LogInformation ("Produced GraphQL subscription result with errors")

                                if logger.IsEnabled LogLevel.Trace then
                                    logger.LogTrace (
                                        $"GraphQL subscription errors:{Environment.NewLine}{{errors}}{Environment.NewLine}GraphQL deferred data:{Environment.NewLine}{{data}}",
                                        errors,
                                        serializeWithOptions data
                                    ))

                    GQLResponse.Stream documentId

                | RequestError errs ->
                    logger.LogInformation (
                        $"Produced request error GraphQL response with documentId = '{{documentId}}' and metadata:{Environment.NewLine}{{metadata}}",
                        documentId,
                        metadata
                    )
                    logger.LogTrace ($"{{number}} errors:{Environment.NewLine}{{errs}}", errs.Length, errs)

                    GQLResponse.RequestError (documentId, errs)

            let removeWhitespacesAndLineBreaks (str : string) = str.Trim().Replace ("\r\n", " ")

            /// Check if the request contains a body
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

            /// Check if the request is an introspection query.
            let isRequestIntrospection () = task {
                if request.Method = HttpMethods.Get then
                    logger.LogTrace ("Request is GET. Must be an introspection query.")
                    return true
                else
                    let! hasBody = checkIfHasBody()
                    if not hasBody then
                        logger.LogTrace ("Request is not GET but has no body.  Must be an introspection query.")
                        return true
                    else
                        logger.LogTrace ("Request is not GET and has a body.")
                        return false
            }

            /// Check if the document is an introspection query
            let isDocumentIntrospection (ast : Ast.Document) (operationName : string option) : bool =

                let getOperation = function
                    | OperationDefinition odef -> Some odef
                    | _ -> None

                let findOperation doc opName =
                    match ast.Definitions |> List.choose getOperation, opName with
                    | [def], _ -> Some def
                    | defs, name ->
                        defs
                        |> List.tryFind (fun def -> def.Name = name)

                match findOperation ast operationName with
                | None ->
                    logger.LogTrace ("Document has no operation.")
                    false
                | Some operation ->
                    if not (operation.OperationType = Query) then
                        logger.LogTrace ("Document operation is not of type Query.")
                        false
                    else
                        let metaTypeFields =
                            seq { "__type"; "__schema"; "__typename" }
                            |> Set.ofSeq

                        let anyFieldIsNotMetaType =
                            // Run through the definitions, stopping and returning true if any name
                            // does not match the ones in metaTypeFields.
                            Seq.exists (fun definition ->
                                    match definition with
                                    | Field fd ->
                                        logger.LogTrace ($"Operation Selection is Field with name: {{n}}", fd.Name)
                                        not <| metaTypeFields.Contains(fd.Name)
                                    | _ ->
                                        logger.LogTrace ("Operation Selection is non-Field type")
                                        false
                            ) operation.SelectionSet
                        // If all of them passed the test, this is an introspection query.
                        not anyFieldIsNotMetaType
            /// Execute default or custom introspection query
            let executeIntrospectionQuery (ast : Ast.Document voption) = task {
                let! result =
                    match ast with
                    | ValueNone ->
                        Schema.executor.AsyncExecute (IntrospectionQuery.Definition)
                    | ValueSome ast ->
                        Schema.executor.AsyncExecute (ast)

                let response = result |> toResponse
                return Results.Ok response
            }

            /// Execute the operation for given request
            let executeOperation () = task {
                let! gqlRequest = ctx.BindJsonAsync<GQLRequestContent>()

                let operationName = gqlRequest.OperationName |> Skippable.toOption
                operationName |> Option.iter (fun on -> logger.LogTrace ($"GraphQL operation name: {{on}}", on))

                let query = gqlRequest.Query
                let ast = Parser.parse (removeWhitespacesAndLineBreaks query)

                if isDocumentIntrospection ast operationName then

                    if logger.IsEnabled LogLevel.Trace then
                        logger.LogTrace ($"Executing GraphQL introspection query:{Environment.NewLine}{{query}}", serializeWithOptions query)
                    return! executeIntrospectionQuery (ValueSome ast)

                else

                    if logger.IsEnabled LogLevel.Trace then
                        logger.LogTrace ($"Executing GraphQL query:{Environment.NewLine}{{query}}", serializeWithOptions query)

                    let variables = gqlRequest.Variables |> Skippable.toOption
                    variables |> Option.iter (fun v -> logger.LogTrace($"GraphQL variables:{Environment.NewLine}{{variables}}", v))

                    let root = { RequestId = System.Guid.NewGuid () |> string }
                    let executionPlan = Schema.executor.CreateExecutionPlan (ast, ?operationName = operationName)
                    let! result = Schema.executor.AsyncExecute (executionPlan, root, ?variables = variables)
                    let response = result |> toResponse
                    return Results.Ok response
            }

            let! requestIsIntrospection = isRequestIntrospection()
            if requestIsIntrospection then
                return! executeIntrospectionQuery ValueNone
            else
                return! executeOperation()
        } |> ofTaskIResult ctx

    let webApp : HttpHandler = setCorsHeaders >=> choose [ POST; GET ] >=> graphQL
