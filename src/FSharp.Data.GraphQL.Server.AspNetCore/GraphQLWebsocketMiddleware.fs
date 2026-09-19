namespace FSharp.Data.GraphQL.Server.AspNetCore

open System
open System.Buffers
open System.Collections.Generic
open System.Diagnostics
open System.Linq
open System.Net.WebSockets
open System.Text.Json
open System.Text.Json.Serialization
open System.Threading
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Options

open Collections.Pooled
open FsToolkit.ErrorHandling

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Shared.WebSockets

/// <summary>
/// Splits a batched deferred or streamed payload, addressed by a path whose last segment is the list of indices of
/// the items in the batch, into one independently addressed payload per item.
/// </summary>
/// <remarks>
/// <see cref="FSharp.Data.GraphQL.Execution.collectItems"/> groups several streamed items produced together (by the
/// field's batching policy or the query's <c>preferredBatchSize</c>) into a single deferred payload, addressed by a
/// path ending in the list of the batch's item indices, such as <c>["items"; [2; 1]]</c>. A <c>graphql-transport-ws</c>
/// client cannot merge that into the response tree: no single index identifies where the payload belongs. Splitting
/// it here, at the transport, keeps the engine's batching (still one buffered event, still one merge of concurrently
/// resolved items) while addressing every item the same way a field that streams one item at a time already does: a
/// one-element data array at a path ending in that item's own index.
/// </remarks>
module internal IncrementalPayloadSplitting =

    // Written as `obj list`, not the (internal, and here inaccessible) `FieldPath` abbreviation it stands for:
    // a type abbreviation is erased, so this is the exact same type and unifies fine with FieldPath-typed values.
    let pathStartsWith (prefix : obj list) (path : obj list) =
        let prefixLength = List.length prefix
        List.length path >= prefixLength
        && List.truncate prefixLength path = prefix

    let tryGetPathItemIndex (fieldPath : obj list) (path : obj list) =
        let fieldPathLength = List.length fieldPath

        if pathStartsWith fieldPath path then
            path |> List.vtryItem fieldPathLength
        else
            ValueNone

    /// Matches a path ending in a list of indices, such as the path of a batched deferred payload, returning the
    /// path of the batch's own field and the indices of its items.
    [<return : Struct>]
    let (|BatchPath|_|) (path : obj list) =
        match List.rev path with
        | (:? (obj list) as indices) :: fieldPathRev -> ValueSome (List.rev fieldPathRev, indices)
        | _ -> ValueNone

    /// Splits a batch's data (an array with one element per index, in the same order) and errors (each carrying the
    /// full path of the item it belongs to, since every error of a batch originates from resolving one specific
    /// item) into one <c>(data, errors, path)</c> triple per item, addressed at that item's own path.
    let splitBatch (fieldPath : obj list) (indices : obj list) (data : obj) (errors : GQLProblemDetails list) =
        let items = data :?> obj[]
        let errorsByItemIndex =
            errors
            |> Seq.vchoose (fun error ->
                error.Path
                |> Skippable.toValueOption
                |> ValueOption.bind (tryGetPathItemIndex fieldPath)
                |> ValueOption.map (fun index -> struct (index, error)))
            |> _.ToLookup((fun struct (index, _) -> index), (fun struct (_, error) -> error))

        (indices, List.ofArray items)
        ||> List.map2 (fun index item ->
            let itemPath = [ yield! fieldPath; yield index ]
            let itemErrors = errorsByItemIndex[index] |> Seq.toList
            box [| item |], itemErrors, itemPath)

module internal ObservableErrorHandling =

    [<Literal>]
    let UnexpectedObservableErrorMessage = "Unexpected error during subscription"

    let private deduplicationKey (problem : GQLProblemDetails) =
        let extensions =
            problem.Extensions
            |> Skippable.toValueOption
            |> ValueOption.map (
                Seq.sortBy _.Key
                >> Seq.map (fun kvp -> kvp.Key, kvp.Value)
                >> Seq.toList
            )

        problem.Message, problem.Path, problem.Locations, extensions

    let rec problemDetailsOfObservableError (ex : exn) =
        match ex with
        | :? AggregateException as aggregate ->
            let problemDetails =
                aggregate.Flatten().InnerExceptions
                |> Seq.collect problemDetailsOfObservableError
                |> Seq.distinctBy deduplicationKey
                |> Seq.toList

            match problemDetails with
            | [] -> [ GQLProblemDetails.Create UnexpectedObservableErrorMessage ]
            | _ -> problemDetails
        | _ ->
            match box ex with
            | :? IGQLError as error -> [ GQLProblemDetails.OfError error ]
            | _ -> [ GQLProblemDetails.Create UnexpectedObservableErrorMessage ]

    let sanitizeRequestError (problemDetails : GQLProblemDetails) =
        match
            problemDetails.Exception
            |> ValueOption.map box
            |> ValueOption.toObj
        with
        | :? IGQLError -> problemDetails
        | :? exn -> GQLProblemDetails.Create UnexpectedObservableErrorMessage
        | _ -> problemDetails

open IncrementalPayloadSplitting
open ObservableErrorHandling

type GraphQLWebSocketMiddleware<'Root>
    (
        next : RequestDelegate, // must be kept for middleware signature compatibility
        applicationLifetime : IHostApplicationLifetime,
        serviceProvider : IServiceProvider,
        logger : ILogger<GraphQLWebSocketMiddleware<'Root>>,
        options : IOptions<GraphQLOptions<'Root>>
    ) =

    let options = options.Value
    let serializerOptions = options.SerializerOptions
    let pingHandler = options.WebsocketOptions.CustomPingHandler
    let connectionInitTimeout = options.WebsocketOptions.ConnectionInitTimeout
    let gracefulCloseTimeout : TimeSpan = TimeSpan.FromSeconds 5.0

    let serializeServerMessage (jsonSerializerOptions : JsonSerializerOptions) (serverMessage : ServerMessage) = task {
        let raw =
            match serverMessage with
            | ConnectionAck -> { Id = ValueNone; Type = "connection_ack"; Payload = ValueNone }
            | ServerPing -> { Id = ValueNone; Type = "ping"; Payload = ValueNone }
            | ServerPong p -> { Id = ValueNone; Type = "pong"; Payload = p |> ValueOption.map CustomResponse }
            | Next (id, payload) -> {
                Id = ValueSome id
                Type = "next"
                Payload = ValueSome <| ExecutionResult payload
              }
            | Complete id -> { Id = ValueSome id; Type = "complete"; Payload = ValueNone }
            | Error (id, errMessages) -> {
                Id = ValueSome id
                Type = "error"
                Payload = ValueSome <| ErrorMessages errMessages
              }
        return JsonSerializer.Serialize (raw, jsonSerializerOptions)
    }

    static let invalidJsonInClientMessageError =
        Result.Error
        <| InvalidMessage (4400, "Invalid json in client message")

    let deserializeClientMessage (serializerOptions : JsonSerializerOptions) (msg : IReadOnlyPooledList<byte>) = taskResult {
        try
            return JsonSerializer.Deserialize<ClientMessage>(msg.Span, serializerOptions)
        with
        | :? InvalidWebsocketMessageException as ex ->
            logger.LogError (ex, "Invalid websocket message:\n{payload}", msg)
            return!
                Result.Error
                <| InvalidMessage (4400, ex.Message.ToString ())
        | :? JsonException as ex when logger.IsEnabled (LogLevel.Trace) ->
            logger.LogError (ex, "Cannot deserialize WebSocket message:\n{payload}", msg)
            return! invalidJsonInClientMessageError
        | :? JsonException as ex ->
            logger.LogError (ex, "Cannot deserialize WebSocket message")
            return! invalidJsonInClientMessageError
        | ex ->
            logger.LogError (ex, $"Unexpected exception '{ex.GetType().Name}' in GraphQLWebsocketMiddleware")
            return! invalidJsonInClientMessageError
    }

    let isSocketOpen (theSocket : WebSocket) =
        not (theSocket.State = WebSocketState.Aborted)
        && not (theSocket.State = WebSocketState.Closed)
        && not (theSocket.State = WebSocketState.CloseReceived)

    let canCloseSocket (theSocket : WebSocket) =
        not (theSocket.State = WebSocketState.Aborted)
        && not (theSocket.State = WebSocketState.Closed)

    let receiveMessageViaSocket (cancellationToken : CancellationToken) (serializerOptions : JsonSerializerOptions) (socket : WebSocket) = taskResult {
        let buffer = ArrayPool.Shared.Rent options.ReadBufferSize
        try
            let completeMessage = new PooledList<byte> ()
            let mutable segmentResponse : WebSocketReceiveResult = null
            while (not cancellationToken.IsCancellationRequested)
                  && socket |> isSocketOpen
                  && ((segmentResponse = null)
                      || (not segmentResponse.EndOfMessage)) do
                try
                    let! r = socket.ReceiveAsync (ArraySegment<byte>(buffer), cancellationToken)
                    segmentResponse <- r
                    completeMessage.AddRange (ArraySegment<byte>(buffer, 0, r.Count))
                with :? OperationCanceledException ->
                    ()

            if Debugger.IsAttached then
                let message =
                    completeMessage
                    |> Seq.filter (fun x -> x > 0uy)
                    |> Seq.toArray
                    |> System.Text.Encoding.UTF8.GetString
                logger.LogInformation ("-> Request: {request}", message)
            if completeMessage.All (fun b -> b = 0uy) then
                return ValueNone
            else
                let! result = deserializeClientMessage serializerOptions completeMessage
                return ValueSome result
        finally
            ArrayPool.Shared.Return buffer
    }

    let sendMessageViaSocket (sendGate : SemaphoreSlim) (jsonSerializerOptions) (socket : WebSocket) (message : ServerMessage) : Task = task {
        do! sendGate.WaitAsync ()

        try
            logger.LogTrace ("<- Response: {response}", message)

            if not (socket.State = WebSocketState.Open) then
                logger.LogTrace (
                    $"Ignoring message to be sent via socket, since its state is not '{nameof WebSocketState.Open}', but '{{state}}'",
                    socket.State
                )
            else
                // TODO: Allocate string only if a debugger is attached
                let! serializedMessage = message |> serializeServerMessage jsonSerializerOptions
                let segment = ArraySegment<byte>(System.Text.Encoding.UTF8.GetBytes serializedMessage)

                if not (socket.State = WebSocketState.Open) then
                    logger.LogTrace (
                        $"Ignoring message to be sent via socket, since its state is not '{nameof WebSocketState.Open}', but '{{state}}'",
                        socket.State
                    )
                else
                    do! socket.SendAsync (segment, WebSocketMessageType.Text, endOfMessage = true, cancellationToken = CancellationToken.None)
        finally
            sendGate.Release () |> ignore
    }

    let addClientSubscription
        (id : SubscriptionId)
        (howToSendDataOnNext : SubscriptionId -> 'ResponseContent -> Task)
        (subscriptions : SubscriptionsDict, streamSource : IObservable<'ResponseContent>, sendMsg : ServerMessage -> Task)
        =
        let sendTerminalError (ex : exn) = sendMsg (Error (id, problemDetailsOfObservableError ex))

        let observer =
            new Reactive.AnonymousObserver<'ResponseContent> (
                onNext =
                    (fun theOutput ->
                        try
                            (howToSendDataOnNext id theOutput).Wait()
                        with _ ->
                            subscriptions
                            |> GraphQLSubscriptionsManagement.removeSubscription id
                            reraise ()),
                onError =
                    (fun ex ->
                        logger.LogError (ex, "Error on subscription with Id = '{id}'", id)
                        try
                            (sendTerminalError ex).Wait()
                        finally
                            subscriptions
                            |> GraphQLSubscriptionsManagement.removeSubscription (id)),
                onCompleted =
                    (fun () ->
                        try
                            (sendMsg (Complete id)).Wait()
                        finally
                            subscriptions
                            |> GraphQLSubscriptionsManagement.removeSubscription id)
            )

        // Registered before subscribing, so a stream that completes synchronously (from inside Subscribe) still
        // finds the id when its onCompleted callback above runs; only then is it safe to remove and dispose it.
        // Assigning Disposable on an already-disposed SingleAssignmentDisposable disposes the assigned value too.
        let placeholder = new System.Reactive.Disposables.SingleAssignmentDisposable ()

        subscriptions
        |> GraphQLSubscriptionsManagement.addSubscription (id, placeholder, (fun _ -> ()))

        try
            placeholder.Disposable <- streamSource.Subscribe (observer)
        with _ ->
            // Nothing will ever complete this subscription now, so the id is freed here instead; a no-op if the
            // synchronous completion above already removed it. Rethrown for the caller to report the failure.
            subscriptions
            |> GraphQLSubscriptionsManagement.removeSubscription id
            reraise ()

    let tryToGracefullyCloseSocket (sendGate : SemaphoreSlim) (cancellationToken : CancellationToken) (code, message) (theSocket : WebSocket) : Task =
        task {
            do! sendGate.WaitAsync ()

            try
                if theSocket |> canCloseSocket then
                    use closeCancellationTokenSource = CancellationTokenSource.CreateLinkedTokenSource cancellationToken
                    closeCancellationTokenSource.CancelAfter gracefulCloseTimeout

                    try
                        do! theSocket.CloseAsync (code, message, closeCancellationTokenSource.Token)
                    with :? OperationCanceledException ->
                        logger.LogWarning (
                            "Aborting WebSocket after graceful close did not complete before cancellation. State = '{state}'",
                            theSocket.State
                        )
                        theSocket.Abort ()
                else
                    logger.LogTrace (
                        $"Ignoring socket close request, since its state is neither writable nor closeable, but '{{state}}'",
                        theSocket.State
                    )
            finally
                sendGate.Release () |> ignore
        }

    let tryToGracefullyCloseSocketWithDefaultBehavior sendGate cancellationToken =
        tryToGracefullyCloseSocket sendGate cancellationToken (WebSocketCloseStatus.NormalClosure, "Normal Closure")

    let handleMessages (sendGate : SemaphoreSlim) (cancellationToken : CancellationToken) (httpContext : HttpContext) (socket : WebSocket) : Task =
        let subscriptions = Dictionary<SubscriptionId, SubscriptionUnsubscriber * OnUnsubscribeAction>()
        // ---------->
        // Helpers -->
        // ---------->
        let rcvMsgViaSocket = receiveMessageViaSocket (CancellationToken.None)

        let sendMsg = sendMessageViaSocket sendGate serializerOptions socket
        let rcv () = socket |> rcvMsgViaSocket serializerOptions

        let sendOutput id (output : SubscriptionExecutionResult) = sendMsg (Next (id, output))

        let sendSubscriptionResponseOutput id subscriptionResult =
            match subscriptionResult with
            | SubscriptionResult output ->
                SubscriptionExecutionResult.Create (output, [])
                |> sendOutput id
            | SubscriptionErrors (output, errors) ->
                // TODO: Use StringBuilder
                logger.LogWarning ("Subscription errors: {subscriptionErrors}", (String.Join ('\n', errors |> Seq.map (fun x -> $"- %s{x.Message}"))))
                // The executor may still have resolved partial data alongside the field errors; forward it as-is
                match output with
                | ValueNone ->
                    SubscriptionExecutionResult.CreateErrors errors
                    |> sendOutput id
                | ValueSome output ->
                    SubscriptionExecutionResult.Create (output, errors)
                    |> sendOutput id

        // Incremental payloads are sent as soon as they are produced, with their path inside the initial result,
        // so a client can merge them. The completion marker becomes a final payload with hasNext set to false.
        // A batched payload (path ending in a list of indices) is split into one payload per item first, since a
        // client cannot merge a payload that isn't addressed by a single index.
        let sendDeferredResponseOutput id deferredResult : Task = task {
            match deferredResult with
            | ValueSome (DeferredResult (data, BatchPath (fieldPath, indices))) ->
                for itemData, _, itemPath in splitBatch fieldPath indices data [] do
                    do!
                        SubscriptionExecutionResult.CreateIncremental (itemData, [], itemPath)
                        |> sendOutput id
            | ValueSome (DeferredResult (data, path)) ->
                do!
                    SubscriptionExecutionResult.CreateIncremental (data, [], path)
                    |> sendOutput id
            | ValueSome (DeferredErrors (ValueSome data, errors, BatchPath (fieldPath, indices))) ->
                logger.LogWarning (
                    "Deferred response errors: {deferredErrors}",
                    // TODO: Use StringBuilder
                    (String.Join ('\n', errors |> Seq.map (fun x -> $"- %s{x.Message}")))
                )
                for itemData, itemErrors, itemPath in splitBatch fieldPath indices data errors do
                    do!
                        SubscriptionExecutionResult.CreateIncremental (itemData, itemErrors, itemPath)
                        |> sendOutput id
            | ValueSome (DeferredErrors (data, errors, path)) ->
                logger.LogWarning (
                    "Deferred response errors: {deferredErrors}",
                    // TODO: Use StringBuilder
                    (String.Join ('\n', errors |> Seq.map (fun x -> $"- %s{x.Message}")))
                )
                do!
                    SubscriptionExecutionResult.CreateIncremental (data |> ValueOption.toObj, errors, path)
                    |> sendOutput id
            | ValueNone ->
                do!
                    SubscriptionExecutionResult.CreateCompleted ()
                    |> sendOutput id
        }

        let applyPlanExecutionResult (id : SubscriptionId) (socket) (executionResult : GQLExecutionResult) : Task = task {
            match executionResult with
            | Stream observableOutput ->
                (subscriptions, observableOutput, sendMsg)
                |> addClientSubscription id sendSubscriptionResponseOutput
            | Deferred (data, errors, observableOutput) ->
                do!
                    SubscriptionExecutionResult.CreateInitial (data, errors)
                    |> sendOutput id
                (subscriptions, observableOutput |> Observable.withCompletionMarker, sendMsg)
                |> addClientSubscription id sendDeferredResponseOutput
            | Direct (data, errors) ->
                // An execution result, whose data is null when a non-null root field failed during execution;
                // still a result, so it is sent as Next + Complete like any other, not as the terminal Error
                // message below
                if not errors.IsEmpty then
                    logger.LogWarning ("Execution errors:\n{errors}", errors)
                do!
                    SubscriptionExecutionResult.Create (data |> ValueOption.toObj, errors)
                    |> sendOutput id
                // The graphql-transport-ws protocol requires Complete after the single Next of a query or mutation
                do! sendMsg (Complete id)
            | RequestError problemDetails ->
                let sanitizedProblemDetails = problemDetails |> List.map sanitizeRequestError
                logger.LogWarning ("Request errors:\n{errors}", problemDetails)
                // The request was rejected before execution, so it is not a result: the protocol requires it to be
                // sent as the terminal Error message instead of a Next followed by Complete, or a client would
                // read it as a successful result with null data
                do! sendMsg (Error (id, sanitizedProblemDetails))
        }

        let logMsgReceivedWithOptionalPayload optionalPayload (msgAsStr : string) =
            match optionalPayload with
            | ValueSome payload -> logger.LogTrace ($"{msgAsStr} with payload\n{{messageAddendum}}", (payload : 'Payload))
            | ValueNone -> logger.LogTrace (msgAsStr)

        let logMsgWithIdReceived (id : string) (msgAsStr : string) = logger.LogTrace ($"{msgAsStr}. Id = '{{messageId}}'", id)

        // <--------------
        // <-- Helpers --|
        // <--------------

        // ------->
        // Main -->
        // ------->
        task {
            try
                try
                    while not cancellationToken.IsCancellationRequested
                          && socket |> isSocketOpen do
                        let! receivedMessage = rcv ()
                        match receivedMessage with
                        | Result.Error failureMessages ->
                            nameof InvalidMessage
                            |> logMsgReceivedWithOptionalPayload ValueNone
                            match failureMessages with
                            | InvalidMessage (code, explanation) ->
                                do!
                                    socket
                                    |> tryToGracefullyCloseSocket sendGate cancellationToken (enum code, explanation)
                        | Ok ValueNone -> logger.LogTrace ("WebSocket received empty message! State = '{socketState}'", socket.State)
                        | Ok (ValueSome msg) ->
                            match msg with
                            | ConnectionInit p ->
                                nameof ConnectionInit |> logMsgReceivedWithOptionalPayload p
                                do!
                                    socket
                                    |> tryToGracefullyCloseSocket
                                        sendGate
                                        cancellationToken
                                        (enum CustomWebSocketStatus.TooManyInitializationRequests, "Too many initialization requests")
                            | ClientPing p ->
                                nameof ClientPing |> logMsgReceivedWithOptionalPayload p
                                match pingHandler with
                                | ValueSome func ->
                                    let! customP = p |> func serviceProvider
                                    do! ServerPong customP |> sendMsg
                                | ValueNone -> do! ServerPong p |> sendMsg
                            | ClientPong p -> nameof ClientPong |> logMsgReceivedWithOptionalPayload p
                            | Subscribe (id, query) ->
                                try
                                    nameof Subscribe |> logMsgWithIdReceived id
                                    if subscriptions |> GraphQLSubscriptionsManagement.isIdTaken id then
                                        do!
                                            let warningMsg : FormattableString = $"Subscriber for Id = '{id}' already exists"
                                            logger.LogWarning (String.Format (warningMsg.Format, "id"), id)
                                            socket
                                            |> tryToGracefullyCloseSocket
                                                sendGate
                                                cancellationToken
                                                (enum CustomWebSocketStatus.SubscriberAlreadyExists, warningMsg.ToString ())
                                    else
                                        let variables = query.Variables |> Skippable.toValueOption
                                        let getInputContext () = httpContext.RequestServices.GetRequiredService<IInputExecutionContext>()
                                        let! planExecutionResult =
                                            let root = options.RootFactory httpContext
                                            options.SchemaExecutor.AsyncExecute (query.Query, getInputContext, root, ?variables = variables)
                                        do! planExecutionResult |> applyPlanExecutionResult id socket
                                with ex ->
                                    logger.LogError (ex, "Unexpected error during subscription with id '{id}'", id)
                                    do! sendMsg (Error (id, [ GQLProblemDetails.Create UnexpectedObservableErrorMessage ]))
                            | ClientComplete id ->
                                "ClientComplete" |> logMsgWithIdReceived id
                                subscriptions
                                |> GraphQLSubscriptionsManagement.removeSubscription (id)
                    logger.LogTrace "Leaving the 'graphql-ws' connection loop..."
                    do!
                        socket
                        |> tryToGracefullyCloseSocketWithDefaultBehavior sendGate cancellationToken
                with ex ->
                    logger.LogError (ex, "Cannot handle a message; dropping a websocket connection")
                    // At this point, only something really weird must have happened.
                    // In order to avoid faulty state scenarios and unimagined damages,
                    // just close the socket without further ado.
                    do!
                        socket
                        |> tryToGracefullyCloseSocketWithDefaultBehavior sendGate cancellationToken
            finally
                subscriptions
                |> GraphQLSubscriptionsManagement.removeAllSubscriptions
        }

    // <--------
    // <-- Main
    // <--------

    let waitForConnectionInitAndRespondToClient
        (sendGate : SemaphoreSlim)
        (cancellationToken : CancellationToken)
        (socket : WebSocket)
        : TaskResult<unit, string> = task {
        let timerTokenSource = new CancellationTokenSource ()
        timerTokenSource.CancelAfter connectionInitTimeout
        let detonationRegistration =
            timerTokenSource.Token.Register (fun _ ->
                (socket
                 |> tryToGracefullyCloseSocket
                     sendGate
                     cancellationToken
                     (enum CustomWebSocketStatus.ConnectionTimeout, "Connection initialization timeout"))
                    .Wait())

        let! connectionInitSucceeded =
            TaskResult.Run<bool>(
                (fun _ -> task {
                    logger.LogDebug ($"Waiting for {nameof ConnectionInit}...")
                    let! receivedMessage = receiveMessageViaSocket CancellationToken.None serializerOptions socket
                    match receivedMessage with
                    | Ok (ValueSome (ConnectionInit _)) ->
                        logger.LogDebug ($"Valid {nameof ConnectionInit} received! Responding with ACK!")
                        detonationRegistration.Unregister () |> ignore
                        do!
                            ConnectionAck
                            |> sendMessageViaSocket sendGate serializerOptions socket
                        return true
                    | Ok (ValueSome (Subscribe _)) ->
                        do!
                            socket
                            |> tryToGracefullyCloseSocket sendGate cancellationToken (enum CustomWebSocketStatus.Unauthorized, "Unauthorized")
                        return false
                    | Result.Error (InvalidMessage (code, explanation)) ->
                        do!
                            socket
                            |> tryToGracefullyCloseSocket sendGate cancellationToken (enum code, explanation)
                        return false
                    | _ ->
                        do!
                            socket
                            |> tryToGracefullyCloseSocketWithDefaultBehavior sendGate cancellationToken
                        return false
                }),
                timerTokenSource.Token
            )
        if (not timerTokenSource.Token.IsCancellationRequested) then
            if connectionInitSucceeded then
                return Ok ()
            else
                return Result.Error ($"{nameof ConnectionInit} failed (not because of timeout)")
        else
            return Result.Error <| "{nameof ConnectionInit} timeout"
    }

    member _.InvokeAsync (ctx : HttpContext) : Task =
        if ctx.WebSockets.IsWebSocketRequest then
            task {
                use! socket = ctx.WebSockets.AcceptWebSocketAsync ("graphql-transport-ws")
                let sendGate = new SemaphoreSlim (1, 1)
                use connectionLifetimeCancellationTokenSource =
                    CancellationTokenSource.CreateLinkedTokenSource (ctx.RequestAborted, applicationLifetime.ApplicationStopping)
                let connectionLifetimeCancellationToken = connectionLifetimeCancellationTokenSource.Token
                let! connectionInitResult =
                    socket
                    |> waitForConnectionInitAndRespondToClient sendGate connectionLifetimeCancellationToken
                match connectionInitResult with
                | Result.Error errMsg -> logger.LogWarning errMsg
                | Ok _ ->
                    connectionLifetimeCancellationToken.Register (fun _ ->
                        (socket
                         |> tryToGracefullyCloseSocketWithDefaultBehavior sendGate connectionLifetimeCancellationToken)
                            .Wait())
                    |> ignore
                    try
                        do!
                            socket
                            |> handleMessages sendGate connectionLifetimeCancellationToken ctx
                    with ex ->
                        logger.LogError (ex, "Cannot handle WebSocket message.")
            }
        else
            TypedResults.Problem (
                title = "WebSocket connection expected.",
                detail = $"'{options.WebsocketOptions.EndpointUrl}' endpoint only accepts WebSocket connections.",
                statusCode = StatusCodes.Status400BadRequest
            )
            :> IResult
            |> _.ExecuteAsync(ctx)
