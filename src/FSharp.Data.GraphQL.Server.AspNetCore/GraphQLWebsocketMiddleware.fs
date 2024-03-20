namespace FSharp.Data.GraphQL.Server.AspNetCore

open System
open System.Collections.Generic
open System.Net.WebSockets
open System.Text.Json
open System.Text.Json.Serialization
open System.Threading
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Options
open FsToolkit.ErrorHandling

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Server.AspNetCore.WebSockets

type GraphQLWebSocketMiddleware<'Root>
    (
        next : RequestDelegate,
        applicationLifetime : IHostApplicationLifetime,
        serviceProvider : IServiceProvider,
        logger : ILogger<GraphQLWebSocketMiddleware<'Root>>,
        options : IOptions<GraphQLOptions<'Root>>
    ) =

    let options = options.Value
    let serializerOptions = options.SerializerOptions
    let pingHandler = options.WebsocketOptions.CustomPingHandler
    let endpointUrl = PathString options.WebsocketOptions.EndpointUrl
    let connectionInitTimeout = options.WebsocketOptions.ConnectionInitTimeoutInMs

    let serializeServerMessage (jsonSerializerOptions : JsonSerializerOptions) (serverMessage : ServerMessage) = task {
        let raw =
            match serverMessage with
            | ConnectionAck -> { Id = ValueNone; Type = "connection_ack"; Payload = ValueNone }
            | ServerPing -> { Id = ValueNone; Type = "ping"; Payload = ValueNone }
            | ServerPong p -> { Id = ValueNone; Type = "pong"; Payload = p |> ValueOption.map CustomResponse }
            | Next (id, payload) -> { Id = ValueSome id; Type = "next"; Payload = ValueSome <| ExecutionResult payload }
            | Complete id -> { Id = ValueSome id; Type = "complete"; Payload = ValueNone }
            | Error (id, errMsgs) -> { Id = ValueSome id; Type = "error"; Payload = ValueSome <| ErrorMessages errMsgs }
        return JsonSerializer.Serialize (raw, jsonSerializerOptions)
    }

    let deserializeClientMessage (serializerOptions : JsonSerializerOptions) (msg : string) = taskResult {
        let invalidJsonInClientMessageError() =
            Result.Error <| InvalidMessage (4400, "invalid json in client message")
        try
            return JsonSerializer.Deserialize<ClientMessage> (msg, serializerOptions)
        with
        | :? InvalidWebsocketMessageException as ex ->
            logger.LogError(ex, $"Invalid websocket message:{Environment.NewLine}{{payload}}", msg)
            return! Result.Error <| InvalidMessage (4400, ex.Message.ToString ())
        | :? JsonException as ex when logger.IsEnabled(LogLevel.Trace) ->
            logger.LogError(ex, $"Cannot deserialize WebSocket message:{Environment.NewLine}{{payload}}", msg)
            return! invalidJsonInClientMessageError()
        | :? JsonException as ex ->
            logger.LogError(ex, "Cannot deserialize WebSocket message")
            return! invalidJsonInClientMessageError()
        | ex ->
            logger.LogError(ex, "Unexpected exception \"{exceptionname}\" in GraphQLWebsocketMiddleware.", (ex.GetType().Name))
            return! invalidJsonInClientMessageError()
    }

    let isSocketOpen (theSocket : WebSocket) =
        not (theSocket.State = WebSocketState.Aborted)
        && not (theSocket.State = WebSocketState.Closed)
        && not (theSocket.State = WebSocketState.CloseReceived)

    let canCloseSocket (theSocket : WebSocket) =
        not (theSocket.State = WebSocketState.Aborted)
        && not (theSocket.State = WebSocketState.Closed)

    let receiveMessageViaSocket (cancellationToken : CancellationToken) (serializerOptions : JsonSerializerOptions) (socket : WebSocket) = taskResult {
        let buffer = Array.zeroCreate 4096
        let completeMessage = new List<byte> ()
        let mutable segmentResponse : WebSocketReceiveResult = null
        while (not cancellationToken.IsCancellationRequested)
              && socket |> isSocketOpen
              && ((segmentResponse = null)
                  || (not segmentResponse.EndOfMessage)) do
            try
                let! r = socket.ReceiveAsync (new ArraySegment<byte> (buffer), cancellationToken)
                segmentResponse <- r
                completeMessage.AddRange (new ArraySegment<byte> (buffer, 0, r.Count))
            with :? OperationCanceledException ->
                ()

        // TODO: Allocate string only if a debugger is attached
        let message =
            completeMessage
            |> Seq.filter (fun x -> x > 0uy)
            |> Array.ofSeq
            |> System.Text.Encoding.UTF8.GetString
        if String.IsNullOrWhiteSpace message then
            return ValueNone
        else
            let! result = message |> deserializeClientMessage serializerOptions
            return ValueSome result
    }

    let sendMessageViaSocket (jsonSerializerOptions) (socket : WebSocket) (message : ServerMessage) : Task = task {
        if not (socket.State = WebSocketState.Open) then
            logger.LogTrace ($"Ignoring message to be sent via socket, since its state is not '{nameof WebSocketState.Open}', but '{{state}}'", socket.State)
        else
            // TODO: Allocate string only if a debugger is attached
            let! serializedMessage = message |> serializeServerMessage jsonSerializerOptions
            let segment = new ArraySegment<byte> (System.Text.Encoding.UTF8.GetBytes (serializedMessage))
            if not (socket.State = WebSocketState.Open) then
                logger.LogTrace ($"Ignoring message to be sent via socket, since its state is not '{nameof WebSocketState.Open}', but '{{state}}'", socket.State)
            else
                do! socket.SendAsync (segment, WebSocketMessageType.Text, endOfMessage = true, cancellationToken = CancellationToken.None)

            logger.LogTrace ("<- Response: {response}", message)
    }

    let addClientSubscription
        (id : SubscriptionId)
        (howToSendDataOnNext : SubscriptionId -> 'ResponseContent -> Task)
        (subscriptions : SubscriptionsDict,
         socket : WebSocket,
         streamSource : IObservable<'ResponseContent>,
         jsonSerializerOptions : JsonSerializerOptions)
        =
        let observer =
            new Reactive.AnonymousObserver<'ResponseContent> (
                onNext = (fun theOutput -> (howToSendDataOnNext id theOutput).Wait ()),
                onError = (fun ex -> logger.LogError (ex, "Error on subscription with Id = '{id}'", id)),
                onCompleted =
                    (fun () ->
                        (sendMessageViaSocket jsonSerializerOptions socket (Complete id)).Wait ()
                        subscriptions
                        |> GraphQLSubscriptionsManagement.removeSubscription (id))
            )

        let unsubscriber = streamSource.Subscribe (observer)

        subscriptions
        |> GraphQLSubscriptionsManagement.addSubscription (id, unsubscriber, (fun _ -> ()))

    let tryToGracefullyCloseSocket (code, message) theSocket =
        if theSocket |> canCloseSocket then
            theSocket.CloseAsync (code, message, CancellationToken.None)
        else
            Task.CompletedTask

    let tryToGracefullyCloseSocketWithDefaultBehavior =
        tryToGracefullyCloseSocket (WebSocketCloseStatus.NormalClosure, "Normal Closure")

    let handleMessages (cancellationToken : CancellationToken) (httpContext : HttpContext) (socket : WebSocket) : Task =
        let subscriptions = new Dictionary<SubscriptionId, SubscriptionUnsubscriber * OnUnsubscribeAction> ()
        // ---------->
        // Helpers -->
        // ---------->
        let rcvMsgViaSocket = receiveMessageViaSocket (CancellationToken.None)

        let sendMsg = sendMessageViaSocket serializerOptions socket
        let rcv () = socket |> rcvMsgViaSocket serializerOptions

        let sendOutput id (output : Output) =
            match output.TryGetValue "errors" with
            | true, theValue ->
                // The specification says: "This message terminates the operation and no further messages will be sent."
                subscriptions
                |> GraphQLSubscriptionsManagement.removeSubscription (id)
                sendMsg (Error (id, unbox theValue))
            | false, _ -> sendMsg (Next (id, output))

        let sendSubscriptionResponseOutput id subscriptionResult =
            match subscriptionResult with
            | SubscriptionResult output -> output |> sendOutput id
            | SubscriptionErrors (output, errors) ->
                logger.LogWarning ("Subscription errors: {subscriptionErrors}", (String.Join ('\n', errors |> Seq.map (fun x -> $"- %s{x.Message}"))))
                Task.FromResult ()

        let sendDeferredResponseOutput id deferredResult =
            match deferredResult with
            | DeferredResult (obj, path) ->
                let output = obj :?> Dictionary<string, obj>
                output |> sendOutput id
            | DeferredErrors (obj, errors, _) ->
                logger.LogWarning (
                    "Deferred response errors: {deferredErrors}",
                    (String.Join ('\n', errors |> Seq.map (fun x -> $"- %s{x.Message}")))
                )
                Task.FromResult ()

        let sendDeferredResultDelayedBy (ct : CancellationToken) (ms : int) id deferredResult : Task = task {
            do! Task.Delay (ms, ct)
            do! deferredResult |> sendDeferredResponseOutput id
        }

        let applyPlanExecutionResult (id : SubscriptionId) (socket) (executionResult : GQLExecutionResult) : Task = task {
            match executionResult with
            | Stream observableOutput ->
                (subscriptions, socket, observableOutput, serializerOptions)
                |> addClientSubscription id sendSubscriptionResponseOutput
            | Deferred (data, errors, observableOutput) ->
                do! data |> sendOutput id
                if errors.IsEmpty then
                    (subscriptions, socket, observableOutput, serializerOptions)
                    |> addClientSubscription id (sendDeferredResultDelayedBy cancellationToken 5000)
                else
                    ()
            | Direct (data, _) -> do! data |> sendOutput id
            | RequestError problemDetails ->
                logger.LogWarning("Request errors:\n{errors}", problemDetails)

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
                while not cancellationToken.IsCancellationRequested
                      && socket |> isSocketOpen do
                    let! receivedMessage = rcv ()
                    match receivedMessage with
                    | Result.Error failureMsgs ->
                        nameof InvalidMessage
                        |> logMsgReceivedWithOptionalPayload ValueNone
                        match failureMsgs with
                        | InvalidMessage (code, explanation) -> do! socket.CloseAsync (enum code, explanation, CancellationToken.None)
                    | Ok ValueNone -> logger.LogTrace ("WebSocket received empty message! State = '{socketState}'", socket.State)
                    | Ok (ValueSome msg) ->
                        match msg with
                        | ConnectionInit p ->
                            nameof ConnectionInit |> logMsgReceivedWithOptionalPayload p
                            do!
                                socket.CloseAsync (
                                    enum CustomWebSocketStatus.TooManyInitializationRequests,
                                    "Too many initialization requests",
                                    CancellationToken.None
                                )
                        | ClientPing p ->
                            nameof ClientPing |> logMsgReceivedWithOptionalPayload p
                            match pingHandler with
                            | ValueSome func ->
                                let! customP = p |> func serviceProvider
                                do! ServerPong customP |> sendMsg
                            | ValueNone -> do! ServerPong p |> sendMsg
                        | ClientPong p -> nameof ClientPong |> logMsgReceivedWithOptionalPayload p
                        | Subscribe (id, query) ->
                            nameof Subscribe |> logMsgWithIdReceived id
                            if subscriptions |> GraphQLSubscriptionsManagement.isIdTaken id then
                                do!
                                    let warningMsg : FormattableString = $"Subscriber for Id = '{id}' already exists"
                                    logger.LogWarning (String.Format (warningMsg.Format, "id"), id)
                                    socket.CloseAsync (
                                        enum CustomWebSocketStatus.SubscriberAlreadyExists,
                                        warningMsg.ToString (),
                                        CancellationToken.None
                                    )
                            else
                                let variables = query.Variables |> Skippable.toOption
                                let! planExecutionResult =
                                    let root = options.RootFactory httpContext
                                    options.SchemaExecutor.AsyncExecute (query.Query, root, ?variables = variables)
                                do! planExecutionResult |> applyPlanExecutionResult id socket
                        | ClientComplete id ->
                            "ClientComplete" |> logMsgWithIdReceived id
                            subscriptions
                            |> GraphQLSubscriptionsManagement.removeSubscription (id)
                logger.LogTrace "Leaving the 'graphql-ws' connection loop..."
                do! socket |> tryToGracefullyCloseSocketWithDefaultBehavior
            with ex ->
                logger.LogError (ex, "Cannot handle a message; dropping a websocket connection")
                // at this point, only something really weird must have happened.
                // In order to avoid faulty state scenarios and unimagined damages,
                // just close the socket without further ado.
                do! socket |> tryToGracefullyCloseSocketWithDefaultBehavior
        }

    // <--------
    // <-- Main
    // <--------

    let waitForConnectionInitAndRespondToClient (socket : WebSocket) : TaskResult<unit, string> = task {
        let timerTokenSource = new CancellationTokenSource ()
        timerTokenSource.CancelAfter connectionInitTimeout
        let detonationRegistration =
            timerTokenSource.Token.Register (fun _ ->
                (socket
                 |> tryToGracefullyCloseSocket (enum CustomWebSocketStatus.ConnectionTimeout, "Connection initialization timeout"))
                    .Wait ())

        let! connectionInitSucceeded =
            TaskResult.Run<bool> (
                (fun _ -> task {
                    logger.LogDebug ($"Waiting for {nameof ConnectionInit}...")
                    let! receivedMessage = receiveMessageViaSocket CancellationToken.None serializerOptions socket
                    match receivedMessage with
                    | Ok (ValueSome (ConnectionInit _)) ->
                        logger.LogDebug ($"Valid {nameof ConnectionInit} received! Responding with ACK!")
                        detonationRegistration.Unregister () |> ignore
                        do!
                            ConnectionAck
                            |> sendMessageViaSocket serializerOptions socket
                        return true
                    | Ok (ValueSome (Subscribe _)) ->
                        do!
                            socket
                            |> tryToGracefullyCloseSocket (enum CustomWebSocketStatus.Unauthorized, "Unauthorized")
                        return false
                    | Result.Error (InvalidMessage (code, explanation)) ->
                        do!
                            socket
                            |> tryToGracefullyCloseSocket (enum code, explanation)
                        return false
                    | _ ->
                        do! socket |> tryToGracefullyCloseSocketWithDefaultBehavior
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

    member __.InvokeAsync (ctx : HttpContext) = task {
        if not (ctx.Request.Path = endpointUrl) then
            do! next.Invoke (ctx)
        else if ctx.WebSockets.IsWebSocketRequest then
            use! socket = ctx.WebSockets.AcceptWebSocketAsync ("graphql-transport-ws")
            let! connectionInitResult = socket |> waitForConnectionInitAndRespondToClient
            match connectionInitResult with
            | Result.Error errMsg -> logger.LogWarning errMsg
            | Ok _ ->
                let longRunningCancellationToken =
                    (CancellationTokenSource
                        .CreateLinkedTokenSource(ctx.RequestAborted, applicationLifetime.ApplicationStopping)
                        .Token)
                longRunningCancellationToken.Register (fun _ -> (socket |> tryToGracefullyCloseSocketWithDefaultBehavior).Wait ())
                |> ignore
                try
                    do! socket |> handleMessages longRunningCancellationToken ctx
                with ex ->
                    logger.LogError (ex, "Cannot handle WebSocket message.")
        else
            do! next.Invoke (ctx)
    }
