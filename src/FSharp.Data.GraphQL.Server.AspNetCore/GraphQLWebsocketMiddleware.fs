namespace FSharp.Data.GraphQL.Server.AspNetCore

open FSharp.Data.GraphQL
open Microsoft.AspNetCore.Http
open FSharp.Data.GraphQL.Server.AspNetCore.Rop
open System
open System.Collections.Generic
open System.Net.WebSockets
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open FSharp.Data.GraphQL.Execution
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open System.Text.Json.Serialization

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
    task {
        try
          return
            JsonSerializer.Deserialize<ClientMessage>(msg, serializerOptions)
            |> succeed
        with
        | :? InvalidMessageException as e ->
          return
            fail <| InvalidMessage(4400, e.Message.ToString())
        | :? JsonException as e ->
          if logger.IsEnabled(LogLevel.Debug) then
            logger.LogDebug(e.ToString())
          else
            ()
          return
            fail <| InvalidMessage(4400, "invalid json in client message")
    }

  let isSocketOpen (theSocket : WebSocket) =
    not (theSocket.State = WebSocketState.Aborted) &&
    not (theSocket.State = WebSocketState.Closed) &&
    not (theSocket.State = WebSocketState.CloseReceived)

  let canCloseSocket (theSocket : WebSocket) =
    not (theSocket.State = WebSocketState.Aborted) &&
    not (theSocket.State = WebSocketState.Closed)

  let receiveMessageViaSocket (cancellationToken : CancellationToken) (serializerOptions: JsonSerializerOptions) (executor : Executor<'Root>) (socket : WebSocket) : Task<RopResult<ClientMessage, ClientMessageProtocolFailure> option> =
    task {
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
        let! serializedMessage = message |> serializeServerMessage jsonSerializerOptions
        let segment =
          new ArraySegment<byte>(
            System.Text.Encoding.UTF8.GetBytes(serializedMessage)
          )
        if not (socket.State = WebSocketState.Open) then
          logger.LogTrace("ignoring message to be sent via socket, since its state is not 'Open', but '{state}'", socket.State)
        else
          do! socket.SendAsync(segment, WebSocketMessageType.Text, endOfMessage = true, cancellationToken = new CancellationToken())

        logger.LogTrace("<- Response: {response}", message)
    }

  let addClientSubscription (id : SubscriptionId) (jsonSerializerOptions) (socket) (howToSendDataOnNext: SubscriptionId -> Output -> Task<unit>) (streamSource: IObservable<Output>) (subscriptions : SubscriptionsDict)  =
    let observer = new Reactive.AnonymousObserver<Output>(
      onNext =
        (fun theOutput ->
          theOutput
          |> howToSendDataOnNext id
          |> Task.WaitAll
        ),
      onError =
        (fun ex ->
          logger.LogError("[Error on subscription {id}]: {exceptionstr}", id, ex)
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
    task {
        if theSocket |> canCloseSocket
        then
          do! theSocket.CloseAsync(code, message, new CancellationToken())
        else
          ()
    }

  let tryToGracefullyCloseSocketWithDefaultBehavior =
    tryToGracefullyCloseSocket (WebSocketCloseStatus.NormalClosure, "Normal Closure")

  let handleMessages (cancellationToken: CancellationToken) (serializerOptions: JsonSerializerOptions) (executor : Executor<'Root>) (root: HttpContext -> 'Root) (pingHandler : PingHandler option) (socket : WebSocket) =
    let subscriptions = new Dictionary<SubscriptionId, SubscriptionUnsubscriber * OnUnsubscribeAction>()
    // ---------->
    // Helpers -->
    // ---------->
    let safe_ReceiveMessageViaSocket = receiveMessageViaSocket (new CancellationToken())

    let safe_Send = sendMessageViaSocket serializerOptions socket
    let safe_Receive() =
      socket
      |> safe_ReceiveMessageViaSocket serializerOptions executor

    let safe_SendQueryOutput id output =
      let outputAsDict = output :> IDictionary<string, obj>
      match outputAsDict.TryGetValue("errors") with
      | true, theValue ->
        // The specification says: "This message terminates the operation and no further messages will be sent."
        subscriptions
        |> GraphQLSubscriptionsManagement.removeSubscription(id)
        safe_Send (Error (id, unbox theValue))
      | false, _ ->
        safe_Send (Next (id, output))

    let sendQueryOutputDelayedBy (cancToken: CancellationToken) (ms: int) id output =
      task {
            do! Async.StartAsTask(Async.Sleep ms, cancellationToken = cancToken)
            do! output
                |> safe_SendQueryOutput id
        }
    let safe_SendQueryOutputDelayedBy = sendQueryOutputDelayedBy cancellationToken

    let safe_ApplyPlanExecutionResult (id: SubscriptionId) (socket) (executionResult: GQLExecutionResult)  =
      task {
            match executionResult with
            // TODO: Add error handling
            //| RequestError errs ->
            | Stream observableOutput ->
                subscriptions
                |> addClientSubscription id serializerOptions socket safe_SendQueryOutput observableOutput
            | Deferred (data, errors, observableOutput) ->
                do! data
                    |> safe_SendQueryOutput id
                if errors.IsEmpty then
                  subscriptions
                  |> addClientSubscription id serializerOptions socket (safe_SendQueryOutputDelayedBy 5000) observableOutput
                else
                  ()
            | Direct (data, _) ->
                do! data
                    |> safe_SendQueryOutput id
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
            let! receivedMessage = safe_Receive()
            match receivedMessage with
            | None ->
              logger.LogTrace("Websocket socket received empty message! (socket state = {socketstate})", socket.State)
            | Some msg ->
                match msg with
                | Failure failureMsgs ->
                    "InvalidMessage" |> logMsgReceivedWithOptionalPayload None
                    match failureMsgs |> List.head with
                    | InvalidMessage (code, explanation) ->
                      do! socket.CloseAsync(enum code, explanation, new CancellationToken())
                | Success (ConnectionInit p, _) ->
                    "ConnectionInit" |> logMsgReceivedWithOptionalPayload p
                    do! socket.CloseAsync(
                      enum CustomWebSocketStatus.tooManyInitializationRequests,
                      "too many initialization requests",
                      new CancellationToken())
                | Success (ClientPing p, _) ->
                    "ClientPing" |> logMsgReceivedWithOptionalPayload p
                    match pingHandler with
                    | Some func ->
                      let! customP = p |> func serviceProvider
                      do! ServerPong customP |> safe_Send
                    | None ->
                      do! ServerPong p |> safe_Send
                | Success (ClientPong p, _) ->
                    "ClientPong" |> logMsgReceivedWithOptionalPayload p
                | Success (Subscribe (id, query), _) ->
                    "Subscribe" |> logMsgWithIdReceived id
                    if subscriptions |> GraphQLSubscriptionsManagement.isIdTaken id then
                      do! socket.CloseAsync(
                        enum CustomWebSocketStatus.subscriberAlreadyExists,
                        sprintf "Subscriber for %s already exists" id,
                        new CancellationToken())
                    else
                      let operationName = query.OperationName |> Skippable.filter (not << isNull) |> Skippable.toOption
                      let variables = query.Variables |> Skippable.filter (not << isNull) |> Skippable.toOption
                      let! planExecutionResult =
                        executor.AsyncExecute(query.Query, (root ctx), ?variables = variables, ?operationName = operationName)
                      do! planExecutionResult
                          |> safe_ApplyPlanExecutionResult id socket
                | Success (ClientComplete id, _) ->
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

  let waitForConnectionInitAndRespondToClient (serializerOptions : JsonSerializerOptions) (schemaExecutor : Executor<'Root>) (connectionInitTimeoutInMs : int) (socket : WebSocket) : Task<RopResult<unit, string>> =
    task {
      let timerTokenSource = new CancellationTokenSource()
      timerTokenSource.CancelAfter(connectionInitTimeoutInMs)
      let detonationRegistration = timerTokenSource.Token.Register(fun _ ->
        socket
        |> tryToGracefullyCloseSocket (enum CustomWebSocketStatus.connectionTimeout, "Connection initialization timeout")
        |> Task.WaitAll
      )
      let! connectionInitSucceeded = Task.Run<bool>((fun _ ->
        task {
          logger.LogDebug("Waiting for ConnectionInit...")
          let! receivedMessage = receiveMessageViaSocket (new CancellationToken()) serializerOptions schemaExecutor socket
          match receivedMessage with
          | Some (Success (ConnectionInit payload, _)) ->
            logger.LogDebug("Valid connection_init received! Responding with ACK!")
            detonationRegistration.Unregister() |> ignore
            do! ConnectionAck |> sendMessageViaSocket serializerOptions socket
            return true
          | Some (Success (Subscribe _, _)) ->
            do!
              socket
              |> tryToGracefullyCloseSocket (enum CustomWebSocketStatus.unauthorized, "Unauthorized")
            return false
          | Some (Failure [InvalidMessage (code, explanation)]) ->
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
          return (succeed ())
        else
          return (fail "ConnectionInit failed (not because of timeout)")
      else
        return (fail "ConnectionInit timeout")
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
            |> waitForConnectionInitAndRespondToClient options.SerializerOptions options.SchemaExecutor options.WebsocketOptions.ConnectionInitTimeoutInMs
          match connectionInitResult with
          | Failure errMsg ->
            logger.LogWarning("{warningmsg}", (sprintf "%A" errMsg))
          | Success _ ->
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
                |> safe_HandleMessages options.SerializerOptions options.SchemaExecutor options.RootFactory options.WebsocketOptions.CustomPingHandler
            with
              | ex ->
                logger.LogError("Unexpected exception \"{exceptionname}\" in GraphQLWebsocketMiddleware. More:\n{exceptionstr}", (ex.GetType().Name), ex)
        else
          do! next.Invoke(ctx)
    }
