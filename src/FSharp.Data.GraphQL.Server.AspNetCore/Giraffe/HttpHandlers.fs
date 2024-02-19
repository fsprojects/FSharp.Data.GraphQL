namespace FSharp.Data.GraphQL.Server.AspNetCore.Giraffe

open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL
open Giraffe
open FSharp.Data.GraphQL.Server.AspNetCore
open FSharp.Data.GraphQL.Server.AspNetCore.Rop
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open System.Collections.Generic
open System.Text.Json
open System.Threading
open System.Threading.Tasks

type HttpHandler = HttpFunc -> HttpContext -> HttpFuncResult

module HttpHandlers =
    open System.Collections.Immutable

    let private httpOk (cancellationToken : CancellationToken) (customHandler : HttpHandler) (serializerOptions : JsonSerializerOptions) payload : HttpHandler =
        setStatusCode 200
        >=> customHandler
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

    let private addToErrorsInData (theseNew: GQLProblemDetails list) (data : IDictionary<string, obj>) =
        let toNameValueLookupList (errors : GQLProblemDetails list) =
            errors
            |> List.map
                (fun (error) ->
                    NameValueLookup.ofList
                      ["message", upcast error.Message
                       "path", upcast error.Path]
                )
        let result =
            data
            |> Seq.map(fun x -> (x.Key, x.Value))
            |> Map.ofSeq

        if theseNew |> List.isEmpty then
            result // because we want to avoid having an "errors" property as an empty list (according to specification)
        else
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

    let handleGraphQLWithResponseInterception<'Root>
      (cancellationToken : CancellationToken)
      (logger : ILogger)
      (interceptor : HttpHandler)
      (next : HttpFunc) (ctx : HttpContext) =
      task {
            let cancellationToken = CancellationTokenSource.CreateLinkedTokenSource(cancellationToken, ctx.RequestAborted).Token
            if cancellationToken.IsCancellationRequested then
                return (fun _ -> None) ctx
            else
                let options = ctx.RequestServices.GetRequiredService<GraphQLOptions<'Root>>()
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

                let applyPlanExecutionResult (result : GQLExecutionResult) =
                    task {
                        let gqlResponse =
                          match result.Content with
                          | Direct (data, errs) ->
                              GQLResponse.Direct(result.DocumentId, data, errs)
                          | _ ->
                                GQLResponse.RequestError(
                                  result.DocumentId,
                                  [ GQLProblemDetails.Create(
                                      "subscriptions are not supported here (use the websocket endpoint instead)."
                                  )]
                                )
                        return! httpOk cancellationToken interceptor serializerOptions gqlResponse next ctx
                    }

                let handleDeserializedGraphQLRequest (graphqlRequest : GraphQLRequest) =
                    task {
                        match graphqlRequest.Query with
                            | None ->
                                let! result = executor.AsyncExecute (IntrospectionQuery.Definition) |> Async.StartAsTask
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
                                        httpOk cancellationToken interceptor serializerOptions (prepareGenericErrors errMsgs) next ctx
                                | Success (query, _) ->
                                    if logger.IsEnabled(LogLevel.Debug) then
                                        logger.LogDebug(sprintf "Received query: %A" query)
                                    else
                                        ()
                                    let root = rootFactory(ctx)
                                    let! result =
                                      let variables = ImmutableDictionary.CreateRange(
                                        query.Variables
                                        |> Map.map (fun _ value ->  value :?> JsonElement)
                                      )
                                      executor.AsyncExecute(
                                          query.ExecutionPlan,
                                          data = root,
                                          variables = variables
                                      )|> Async.StartAsTask
                                    if logger.IsEnabled(LogLevel.Debug) then
                                        logger.LogDebug(sprintf "Result metadata: %A" result.Metadata)
                                    else
                                        ()
                                    return! result |> applyPlanExecutionResult
                        }
                if ctx.Request.Headers.ContentLength.GetValueOrDefault(0) = 0 then
                    let! result = executor.AsyncExecute (IntrospectionQuery.Definition) |> Async.StartAsTask
                    if logger.IsEnabled(LogLevel.Debug) then
                        logger.LogDebug(sprintf "Result metadata: %A" result.Metadata)
                    else
                        ()
                    return! result |> applyPlanExecutionResult
                else
                    match! deserializeGraphQLRequest() with
                    | Failure errMsgs ->
                        return! httpOk cancellationToken interceptor serializerOptions (prepareGenericErrors errMsgs) next ctx
                    | Success (graphqlRequest, _) ->
                        return! handleDeserializedGraphQLRequest graphqlRequest
        }

    let handleGraphQL<'Root>
        (cancellationToken : CancellationToken)
        (logger : ILogger)
        (next : HttpFunc) (ctx : HttpContext) =
        handleGraphQLWithResponseInterception<'Root>
          cancellationToken
          logger
          id
          next
          ctx
