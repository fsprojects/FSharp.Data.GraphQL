namespace FSharp.Data.GraphQL.Server.AppInfrastructure.Giraffe

open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL
open Giraffe
open FSharp.Data.GraphQL.Server.AppInfrastructure
open FSharp.Data.GraphQL.Server.AppInfrastructure.Rop
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Logging
open System.Collections.Generic
open System.Text.Json
open System.Threading
open System.Threading.Tasks

type HttpHandler = HttpFunc -> HttpContext -> HttpFuncResult

module HttpHandlers =

    let private getRequiredService<'Service> (ctx : HttpContext) : 'Service =
        ctx.RequestServices.GetrequiredService<'Service>()

    let private httpOk (cancellationToken : CancellationToken) (serializerOptions : JsonSerializerOptions) payload : HttpHandler =
        setStatusCode 200
        >=> (setHttpHeader "Content-Type" "application/json")
        >=> (fun _ ctx ->
                JsonSerializer
                    .SerializeAsync(
                        ctx.Response.Body,
                        payload,
                        options = serializerOptions,
                        cancellationToken = cancellationToken
                    )
                    .ContinueWith(fun _ -> Some ctx) // what about when serialization fails? Maybe it will never at this stage anyway...
            )

    let private prepareGenericErrors (errorMessages : string list) =
        (NameValueLookup.ofList
            [ "errors",
                upcast
                    ( errorMessages
                        |> List.map
                            (fun msg ->
                                NameValueLookup.ofList ["message", upcast msg]
                            )
                    )
            ]
        )

    let private addToErrorsInData (theseNew: Error list) (data : IDictionary<string, obj>) =
        let toNameValueLookupList (errors : Error list) =
            errors
            |> List.map
                (fun (errMsg, path) ->
                    NameValueLookup.ofList ["message", upcast errMsg; "path", upcast path]
                )
        let result =
            data
            |> Seq.map(fun x -> (x.Key, x.Value))
            |> Map.ofSeq
        match data.TryGetValue("errors") with
        | (true, (:? list<NameValueLookup> as nameValueLookups)) ->
            result
            |> Map.change
                "errors"
                (fun _ -> Some <| upcast (nameValueLookups @ (theseNew |> toNameValueLookupList) |> List.distinct))
        | (true, _) ->
            result
        | (false, _) ->
            result
            |> Map.add
                "errors"
                (upcast (theseNew |> toNameValueLookupList))

    let handleGraphQL<'Root>
        (cancellationToken : CancellationToken)
        (logger : ILogger)
        (next : HttpFunc) (ctx : HttpContext) =
        task {
            let cancellationToken = CancellationTokenSource.CreateLinkedTokenSource(cancellationToken, ctx.RequestAborted).Token
            if cancellationToken.IsCancellationRequested then
                return (fun _ -> None) ctx
            else
                let options = ctx |> getRequiredService<GraphQLOptions<'Root>>
                let executor = options.SchemaExecutor
                let rootFactory = options.RootFactory
                let serializerOptions = options.SerializerOptions
                let deserializeGraphQLRequest () =
                    try
                        JsonSerializer.DeserializeAsync<GraphQLRequest>(
                            ctx.Request.Body,
                            serializerOptions
                        ).AsTask()
                         .ContinueWith<RopResult<GraphQLRequest, string>>(fun (x : Task<GraphQLRequest>) -> succeed x.Result)
                    with
                    | :? GraphQLException as ex ->
                        Task.FromResult(fail (sprintf "%s" (ex.Message)))

                let applyPlanExecutionResult (result : GQLResponse) =
                    task {
                        match result with
                        | Direct (data : IDictionary<string, obj>, errs) ->
                            let finalData = data |> addToErrorsInData errs
                            return! httpOk cancellationToken serializerOptions finalData next ctx
                        | other ->
                            let error =
                                prepareGenericErrors ["subscriptions are not supported here (use the websocket endpoint instead)."]
                            return! httpOk cancellationToken serializerOptions error next ctx
                    }
                if ctx.Request.Headers.ContentLength.GetValueOrDefault(0) = 0 then
                    let! result = executor.AsyncExecute (Introspection.IntrospectionQuery) |> Async.StartAsTask
                    if logger.IsEnabled(LogLevel.Debug) then
                        logger.LogDebug(sprintf "Result metadata: %A" result.Metadata)
                    else
                        ()
                    return! result |> applyPlanExecutionResult
                else
                    match! deserializeGraphQLRequest() with
                    | Failure errMsgs ->
                        return! httpOk cancellationToken serializerOptions (prepareGenericErrors errMsgs) next ctx
                    | Success (graphqlRequest, _) ->
                        match graphqlRequest.Query with
                        | None ->
                            let! result = executor.AsyncExecute (Introspection.IntrospectionQuery) |> Async.StartAsTask
                            if logger.IsEnabled(LogLevel.Debug) then
                                logger.LogDebug(sprintf "Result metadata: %A" result.Metadata)
                            else
                                ()
                            return! result |> applyPlanExecutionResult
                        | Some queryAsStr ->
                            let graphQLQueryDecodingResult =
                                queryAsStr
                                |> GraphQLQueryDecoding.decodeGraphQLQuery
                                    serializerOptions
                                    executor
                                    graphqlRequest.OperationName
                                    graphqlRequest.Variables
                            match graphQLQueryDecodingResult with
                            | Failure errMsgs ->
                                return!
                                    httpOk cancellationToken serializerOptions (prepareGenericErrors errMsgs) next ctx
                            | Success (query, _) ->
                                if logger.IsEnabled(LogLevel.Debug) then
                                    logger.LogDebug(sprintf "Received query: %A" query)
                                else
                                    ()
                                let root = rootFactory()
                                let! result =
                                    executor.AsyncExecute(
                                        query.ExecutionPlan,
                                        data = root,
                                        variables = query.Variables
                                    )|> Async.StartAsTask
                                if logger.IsEnabled(LogLevel.Debug) then
                                    logger.LogDebug(sprintf "Result metadata: %A" result.Metadata)
                                else
                                    ()
                                return! result |> applyPlanExecutionResult
        }