namespace FSharp.Data.GraphQL.Server.AspNetCore

open FSharp.Data.GraphQL
open Microsoft.AspNetCore.Http
open System
open System.Collections.Generic
open System.Net.WebSockets
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open FSharp.Data.GraphQL.Execution
open FsToolkit.ErrorHandling
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open System.Collections.Immutable

type GraphQLWebSocketMiddleware<'Root>(next : RequestDelegate, applicationLifetime : IHostApplicationLifetime, serviceProvider : IServiceProvider, logger : ILogger<GraphQLWebSocketMiddleware<'Root>>, options : GraphQLOptions<'Root>) =

  let serializeServerMessage (jsonSerializerOptions: JsonSerializerOptions) (serverMessage : ServerMessage) =
      task {
        let raw =
          match serverMessage with
          | ConnectionAck ->
            { Id = None
              Type = "connection_ack"
              Payload = None }
          | ServerPing ->
            { Id = None
              Type = "ping"
              Payload = None }
          | ServerPong p ->
            { Id = None
              Type = "pong"
              Payload = p |> Option.map CustomResponse }
          | Next (id, payload) ->
            { Id = Some id
              Type = "next"
              Payload = Some <| ExecutionResult payload }
          | Complete id ->
            { Id = Some id
              Type = "complete"
              Payload = None }
          | Error (id, errMsgs) ->
            { Id = Some id
              Type = "error"
              Payload = Some <| ErrorMessages errMsgs }
        return JsonSerializer.Serialize(raw, jsonSerializerOptions)
      }

  let deserializeClientMessage (serializerOptions : JsonSerializerOptions) (msg: string) =
    taskResult {
        try
          return JsonSerializer.Deserialize<ClientMessage>(msg, serializerOptions)
        with
        | :? InvalidMessageException as e ->
          return! Result.Error <|
            InvalidMessage(4400, e.Message.ToString())
        | :? JsonException as e ->
          if logger.IsEnabled(LogLevel.Debug) then
            logger.LogDebug(e.ToString())
          else
            ()
          return! Result.Error <|
            InvalidMessage(4400, "invalid json in client message")
    }

  let isSocketOpen (theSocket : WebSocket) =
    not (theSocket.State = WebSocketState.Aborted) &&
    not (theSocket.State = WebSocketState.Closed) &&
    not (theSocket.State = WebSocketState.CloseReceived)

  let canCloseSocket (theSocket : WebSocket) =
    not (theSocket.State = WebSocketState.Aborted) &&
    not (theSocket.State = WebSocketState.Closed)

  let receiveMessageViaSocket (cancellationToken : CancellationToken) (serializerOptions: JsonSerializerOptions) (socket : WebSocket) =
    taskResult {
      let buffer = Array.zeroCreate 4096
      let completeMessage = new List<byte>()
      let mutable segmentResponse : WebSocketReceiveResult = null
      while (not cancellationToken.IsCancellationRequested) &&
            socket |> isSocketOpen &&
            ((segmentResponse = null) || (not segmentResponse.EndOfMessage)) do
        try
          let! r = socket.ReceiveAsync(new ArraySegment<byte>(buffer), cancellationToken)
          segmentResponse <- r
          completeMessage.AddRange(new ArraySegment<byte>(buffer, 0, r.Count))
        with :? OperationCanceledException ->
          ()

      // TODO: Allocate string only if a debugger is attached
      let message =
        completeMessage
        |> Seq.filter (fun x -> x > 0uy)
        |> Array.ofSeq
        |> System.Text.Encoding.UTF8.GetString
      if String.IsNullOrWhiteSpace message then
        return None
      else
        let! result =
          message
          |> deserializeClientMessage serializerOptions
        return Some result
    }

  let sendMessageViaSocket (jsonSerializerOptions) (socket : WebSocket) (message : ServerMessage) =
    task {
      if not (socket.State = WebSocketState.Open) then
        logger.LogTrace("Ignoring message to be sent via socket, since its state is not 'Open', but '{state}'", socket.State)
      else
        // TODO: Allocate string only if a debugger is attached
        let! serializedMessage = message |> serializeServerMessage jsonSerializerOptions
        let segment =
          new ArraySegment<byte>(
            System.Text.Encoding.UTF8.GetBytes(serializedMessage)
          )
        if not (socket.State = WebSocketState.Open) then
          logger.LogTrace("ignoring message to be sent via socket, since its state is not 'Open', but '{state}'", socket.State)
        else
          do! socket.SendAsync(segment, WebSocketMessageType.Text, endOfMessage = true, cancellationToken = CancellationToken.None)

        logger.LogTrace("<- Response: {response}", message)
    }

  let addClientSubscription
      (id : SubscriptionId)
      (howToSendDataOnNext: SubscriptionId -> 'ResponseContent -> Task<unit>)
      ( subscriptions : SubscriptionsDict,
        socket : WebSocket,
        streamSource: IObservable<'ResponseContent>,
        jsonSerializerOptions : JsonSerializerOptions )  =
    let observer = new Reactive.AnonymousObserver<'ResponseContent>(
      onNext =
        (fun theOutput ->
          theOutput
          |> howToSendDataOnNext id
          |> Task.WaitAll
        ),
      onError =
        (fun ex ->
          logger.LogError(ex, "Error on subscription with id='{id}'", id)
        ),
      onCompleted =
        (fun () ->
          Complete id
          |> sendMessageViaSocket jsonSerializerOptions (socket)
          |> Async.AwaitTask
          |> Async.RunSynchronously
          subscriptions
          |> GraphQLSubscriptionsManagement.removeSubscription(id)
        )
    )

    let unsubscriber = streamSource.Subscribe(observer)

    subscriptions
    |> GraphQLSubscriptionsManagement.addSubscription(id, unsubscriber, (fun _ -> ()))

  let tryToGracefullyCloseSocket (code, message) theSocket =
    if theSocket |> canCloseSocket
    then
      theSocket.CloseAsync(code, message, CancellationToken.None)
    else
      Task.CompletedTask

  let tryToGracefullyCloseSocketWithDefaultBehavior =
    tryToGracefullyCloseSocket (WebSocketCloseStatus.NormalClosure, "Normal Closure")

  let handleMessages (cancellationToken: CancellationToken) (httpContext: HttpContext) (serializerOptions: JsonSerializerOptions) (executor : Executor<'Root>) (root: HttpContext -> 'Root) (pingHandler : PingHandler option) (socket : WebSocket) =
    let subscriptions = new Dictionary<SubscriptionId, SubscriptionUnsubscriber * OnUnsubscribeAction>()
    // ---------->
    // Helpers -->
    // ---------->
    let rcvMsgViaSocket = receiveMessageViaSocket (CancellationToken.None)

    let sendMsg = sendMessageViaSocket serializerOptions socket
    let rcv() =
      socket
      |> rcvMsgViaSocket serializerOptions

    let sendOutput id (output : Output) =
      match output.TryGetValue("errors") with
      | true, theValue ->
        // The specification says: "This message terminates the operation and no further messages will be sent."
        subscriptions
        |> GraphQLSubscriptionsManagement.removeSubscription(id)
        sendMsg (Error (id, unbox theValue))
      | false, _ ->
        sendMsg (Next (id, output))

    let sendSubscriptionResponseOutput id subscriptionResult =
      match subscriptionResult with
      | SubscriptionResult output ->
        output
        |> sendOutput id
      | SubscriptionErrors (output, errors) ->
        printfn "Subscription errors: %s" (String.Join('\n', errors |> Seq.map (fun x -> sprintf "- %s" x.Message)))
        Task.FromResult(())

    let sendDeferredResponseOutput id deferredResult =
      match deferredResult with
      | DeferredResult (obj, path) ->
        let output = obj :?> Dictionary<string, obj>
        output
        |> sendOutput id
      | DeferredErrors (obj, errors, _) ->
        printfn "Deferred response errors: %s" (String.Join('\n', errors |> Seq.map (fun x -> sprintf "- %s" x.Message)))
        Task.FromResult(())

    let sendDeferredResultDelayedBy (cancToken: CancellationToken) (ms: int) id deferredResult =
      task {
            do! Async.StartAsTask(Async.Sleep ms, cancellationToken = cancToken)
            do! deferredResult
                |> sendDeferredResponseOutput id
        }
    let sendQueryOutputDelayedBy = sendDeferredResultDelayedBy cancellationToken

    let applyPlanExecutionResult (id: SubscriptionId) (socket) (executionResult: GQLExecutionResult)  =
      task {
            match executionResult with
            | Stream observableOutput ->
                (subscriptions, socket, observableOutput, serializerOptions)
                |> addClientSubscription id sendSubscriptionResponseOutput
            | Deferred (data, errors, observableOutput) ->
                do! data
                    |> sendOutput id
                if errors.IsEmpty then
                  (subscriptions, socket, observableOutput, serializerOptions)
                  |> addClientSubscription id (sendQueryOutputDelayedBy 5000)
                else
                  ()
            | Direct (data, _) ->
                do! data
                    |> sendOutput id
            | RequestError problemDetails ->
                printfn "Request error: %s" (String.Join('\n', problemDetails |> Seq.map (fun x -> sprintf "- %s" x.Message)))
        }

    let getStrAddendumOfOptionalPayload optionalPayload =
        optionalPayload
        |> Option.map (fun payloadStr -> sprintf " with payload: %A" payloadStr)
        |> Option.defaultWith (fun () -> "")

    let logMsgReceivedWithOptionalPayload optionalPayload (msgAsStr : string) =
      logger.LogTrace ("{message}{messageaddendum}", msgAsStr, (optionalPayload |> getStrAddendumOfOptionalPayload))

    let logMsgWithIdReceived (id : string) (msgAsStr : string) =
      logger.LogTrace("{message} (id: {messageid})", msgAsStr, id)

    // <--------------
    // <-- Helpers --|
    // <--------------

    // ------->
    // Main -->
    // ------->
    task {
      try
        while not cancellationToken.IsCancellationRequested && socket |> isSocketOpen do
            let! receivedMessage = rcv()
            match receivedMessage with
            | Result.Error failureMsgs ->
              "InvalidMessage" |> logMsgReceivedWithOptionalPayload None
              match failureMsgs with
              | InvalidMessage (code, explanation) ->
                do! socket.CloseAsync(enum code, explanation, CancellationToken.None)
            | Ok maybeMsg ->
              match maybeMsg with
              | None ->
                logger.LogTrace("Websocket socket received empty message! (socket state = {socketstate})", socket.State)
              | Some msg ->
                  match msg with
                  | ConnectionInit p ->
                      "ConnectionInit" |> logMsgReceivedWithOptionalPayload p
                      do! socket.CloseAsync(
                        enum CustomWebSocketStatus.tooManyInitializationRequests,
                        "too many initialization requests",
                        CancellationToken.None)
                  | ClientPing p ->
                      "ClientPing" |> logMsgReceivedWithOptionalPayload p
                      match pingHandler with
                      | Some func ->
                        let! customP = p |> func serviceProvider
                        do! ServerPong customP |> sendMsg
                      | None ->
                        do! ServerPong p |> sendMsg
                  | ClientPong p ->
                      "ClientPong" |> logMsgReceivedWithOptionalPayload p
                  | Subscribe (id, query) ->
                      "Subscribe" |> logMsgWithIdReceived id
                      if subscriptions |> GraphQLSubscriptionsManagement.isIdTaken id then
                        do! socket.CloseAsync(
                          enum CustomWebSocketStatus.subscriberAlreadyExists,
                          sprintf "Subscriber for %s already exists" id,
                          CancellationToken.None)
                      else
                        let variables = ImmutableDictionary.CreateRange(query.Variables |> Map.map (fun _ value -> value :?> JsonElement))
                        let! planExecutionResult =
                          executor.AsyncExecute(query.ExecutionPlan, root(httpContext), variables)
                          |> Async.StartAsTask
                        do! planExecutionResult
                            |> applyPlanExecutionResult id socket
                  | ClientComplete id ->
                      "ClientComplete" |> logMsgWithIdReceived id
                      subscriptions |> GraphQLSubscriptionsManagement.removeSubscription (id)
        logger.LogTrace "Leaving graphql-ws connection loop..."
        do! socket |> tryToGracefullyCloseSocketWithDefaultBehavior
      with
      | ex ->
        logger.LogError(ex, "Cannot handle a message; dropping a websocket connection")
        // at this point, only something really weird must have happened.
        // In order to avoid faulty state scenarios and unimagined damages,
        // just close the socket without further ado.
        do! socket |> tryToGracefullyCloseSocketWithDefaultBehavior
    }

    // <--------
    // <-- Main
    // <--------

  let waitForConnectionInitAndRespondToClient (serializerOptions : JsonSerializerOptions) (connectionInitTimeoutInMs : int) (socket : WebSocket) : TaskResult<unit, string> =
    taskResult {
      let timerTokenSource = new CancellationTokenSource()
      timerTokenSource.CancelAfter(connectionInitTimeoutInMs)
      let detonationRegistration = timerTokenSource.Token.Register(fun _ ->
        socket
        |> tryToGracefullyCloseSocket (enum CustomWebSocketStatus.connectionTimeout, "Connection initialization timeout")
        |> Task.WaitAll
      )

      let! connectionInitSucceeded = TaskResult.Run<bool>((fun _ ->
        task {
          logger.LogDebug("Waiting for ConnectionInit...")
          let! receivedMessage = receiveMessageViaSocket (CancellationToken.None) serializerOptions socket
          match receivedMessage with
          | Ok (Some (ConnectionInit _)) ->
            logger.LogDebug("Valid connection_init received! Responding with ACK!")
            detonationRegistration.Unregister() |> ignore
            do! ConnectionAck |> sendMessageViaSocket serializerOptions socket
            return true
          | Ok (Some (Subscribe _)) ->
            do!
              socket
              |> tryToGracefullyCloseSocket (enum CustomWebSocketStatus.unauthorized, "Unauthorized")
            return false
          | Result.Error (InvalidMessage (code, explanation)) ->
            do!
              socket
              |> tryToGracefullyCloseSocket (enum code, explanation)
            return false
          | _ ->
            do!
              socket
              |> tryToGracefullyCloseSocketWithDefaultBehavior
            return false
        }), timerTokenSource.Token)
      if (not timerTokenSource.Token.IsCancellationRequested) then
        if connectionInitSucceeded then
          return ()
        else
          return! Result.Error <| "ConnectionInit failed (not because of timeout)"
      else
        return! Result.Error <| "ConnectionInit timeout"
    }

  member __.InvokeAsync(ctx : HttpContext) =
    task {
      if not (ctx.Request.Path = PathString (options.WebsocketOptions.EndpointUrl)) then
        do! next.Invoke(ctx)
      else
        if ctx.WebSockets.IsWebSocketRequest then
          use! socket = ctx.WebSockets.AcceptWebSocketAsync("graphql-transport-ws")
          let! connectionInitResult =
            socket
            |> waitForConnectionInitAndRespondToClient options.SerializerOptions options.WebsocketOptions.ConnectionInitTimeoutInMs
          match connectionInitResult with
          | Result.Error errMsg ->
            logger.LogWarning("{warningmsg}", (sprintf "%A" errMsg))
          | Ok _ ->
            let longRunningCancellationToken =
              (CancellationTokenSource.CreateLinkedTokenSource(ctx.RequestAborted, applicationLifetime.ApplicationStopping).Token)
            longRunningCancellationToken.Register(fun _ ->
                socket
                |> tryToGracefullyCloseSocketWithDefaultBehavior
                |> Async.AwaitTask
                |> Async.RunSynchronously
            ) |> ignore
            let safe_HandleMessages = handleMessages longRunningCancellationToken
            try
              do! socket
                |> safe_HandleMessages ctx options.SerializerOptions options.SchemaExecutor options.RootFactory options.WebsocketOptions.CustomPingHandler
            with
              | ex ->
                logger.LogError(ex, "Cannot handle Websocket message.")
        else
          do! next.Invoke(ctx)
    }