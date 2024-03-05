namespace FSharp.Data.GraphQL.Server.AspNetCore.Giraffe

open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL
open Giraffe
open FSharp.Data.GraphQL.Server.AspNetCore
open FsToolkit.ErrorHandling
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open System.Collections.Generic
open System.Text.Json
open System.Text.Json.Serialization
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

    /// HttpHandler for handling GraphQL requests with Giraffe.
    /// This one is for specifying an interceptor when you need to
    /// do a custom handling on the response. For example, when you want
    /// to add custom headers.
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
              task {
                try
                  let! deserialized =
                    JsonSerializer.DeserializeAsync<GraphQLRequest>(
                      ctx.Request.Body,
                      serializerOptions
                    )
                  return Ok deserialized
                with
                | :? GraphQLException as ex ->
                  logger.LogError(``exception`` = ex, message = "Error while deserializing request.")
                  return Result.Error [$"%s{ex.Message}\n%s{ex.ToString()}"]
              }

            let applyPlanExecutionResult (result : GQLExecutionResult) =
                task {
                    let gqlResponse =
                      match result.Content with
                      | Direct (data, errs) ->
                          GQLResponse.Direct(result.DocumentId, data, errs)
                      | RequestError (problemDetailsList) ->
                        GQLResponse.RequestError(
                            result.DocumentId,
                            problemDetailsList
                        )
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
                                logger.LogDebug($"Result metadata: %A{result.Metadata}")
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
                            | Result.Error struct (docId, probDetails) ->
                                return!
                                    httpOk cancellationToken interceptor serializerOptions (GQLResponse.RequestError (docId, probDetails)) next ctx
                            | Ok query ->
                                if logger.IsEnabled(LogLevel.Debug) then
                                    logger.LogDebug($"Received query: %A{query}")
                                else
                                    ()
                                let root = rootFactory(ctx)
                                let! result =
                                  let variables = ImmutableDictionary.CreateRange(
                                    query.Variables
                                    |> Map.map (fun _ value ->  JsonSerializer.SerializeToElement(value))
                                  )
                                  executor.AsyncExecute(
                                      query.ExecutionPlan,
                                      data = root,
                                      variables = variables
                                  )|> Async.StartAsTask
                                if logger.IsEnabled(LogLevel.Debug) then
                                    logger.LogDebug($"Result metadata: %A{result.Metadata}")
                                else
                                    ()
                                return! result |> applyPlanExecutionResult
                    }
            if ctx.Request.Headers.ContentLength.GetValueOrDefault(0) = 0 then
                let! result = executor.AsyncExecute (IntrospectionQuery.Definition) |> Async.StartAsTask
                if logger.IsEnabled(LogLevel.Debug) then
                    logger.LogDebug($"Result metadata: %A{result.Metadata}")
                else
                    ()
                return! result |> applyPlanExecutionResult
            else
              match! deserializeGraphQLRequest() with
              | Result.Error errMsgs ->
                let probDetails =
                  errMsgs
                  |> List.map (fun msg -> GQLProblemDetails.Create(msg, Skip))
                return! httpOk cancellationToken interceptor serializerOptions (GQLResponse.RequestError(-1, probDetails)) next ctx
              | Ok graphqlRequest ->
                return! handleDeserializedGraphQLRequest graphqlRequest
        }

    /// HttpHandler for handling GraphQL requests with Giraffe
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
