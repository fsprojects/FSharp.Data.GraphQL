namespace FSharp.Data.GraphQL.Server.AspNetCore

open System
open System.Collections.Generic
open System.Net.WebSockets
open System.Text.Json.Serialization
open System.Threading
open System.Threading.Channels
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging

open FsToolkit.ErrorHandling

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Shared
open FSharp.Data.GraphQL.Shared.WebSockets
open FSharp.Data.GraphQL.Server.AspNetCore.ObservableErrorHandling
open FSharp.Data.GraphQL.Server.AspNetCore.ClientMessagePatterns

/// <summary>
/// One <c>graphql-transport-ws</c> connection: the connection handshake, then three loops that each own one piece of state and talk through channels.
/// </summary>
/// <remarks>
/// <list type="bullet">
/// <item><description>The reader loop is the only reader of the socket; it queues every client message for the control loop.</description></item>
/// <item><description>The control loop is the only owner of the subscription registry; it executes requests, starts and cancels subscription workers, and queues messages for the sender.</description></item>
/// <item><description>The sender loop is the only writer of the socket, including its close.</description></item>
/// </list>
/// Every producer only ever writes into a channel, so no lock is needed anywhere, and no thread ever blocks on a send.
/// </remarks>
type internal GraphQLWebSocketConnection<'Root>
    /// <param name="httpContext">The HTTP context of the request the socket was accepted from.</param>
    /// <param name="socket">The accepted socket.</param>
    /// <param name="options">The GraphQL options of the application.</param>
    /// <param name="serviceProvider">The services of the application, for the custom ping handler.</param>
    /// <param name="logger">The logger of the middleware.</param>
    /// <param name="connectionToken">Cancelled when the request is aborted or the application stops.</param>
    (
        httpContext : HttpContext,
        socket : WebSocket,
        options : GraphQLOptions<'Root>,
        serviceProvider : IServiceProvider,
        logger : ILogger,
        connectionToken : CancellationToken
    ) =

    static let gracefulCloseTimeout = TimeSpan.FromSeconds 5.0

    static let channelOptions () =
        UnboundedChannelOptions (SingleReader = true, SingleWriter = false, AllowSynchronousContinuations = false)

    let inbox = Channel.CreateUnbounded<ConnectionEvent>(channelOptions ())
    let outbound = Channel.CreateUnbounded<OutboundMessage>(channelOptions ())
    let reader = WebSocketMessageReader (socket, options.SerializerOptions, options.ReadBufferSize, logger)
    let sender = WebSocketMessageSender (socket, options.SerializerOptions, gracefulCloseTimeout, logger)

    // Owned by the control loop alone: the running workers by generation, and the id each client-visible
    // subscription currently maps to. A generation outlives its id's registration when the client completes the
    // subscription and reuses the id before the cancelled worker has ended.
    let workers = Dictionary<int, SubscriptionHandle>()
    let active = Dictionary<SubscriptionId, int>()
    let mutable nextGeneration = 0

    let send (message : ServerMessage) = outbound.Writer.TryWrite (Send message) |> ignore
    let close (status : WebSocketCloseStatus) (description : string) =
        outbound.Writer.TryWrite (Close (status, description))
        |> ignore
    let closeWith (code : int) (description : string) = close (enum<WebSocketCloseStatus> code) description

    let logMessageReceived (optionalPayload : 'Payload voption) (messageName : string) =
        if logger.IsEnabled LogLevel.Trace then
            match optionalPayload with
            | ValueSome payload -> logger.LogTrace ($"{messageName} with payload\n{{messageAddendum}}", payload)
            | ValueNone -> logger.LogTrace messageName

    let getInputContext () = httpContext.RequestServices.GetRequiredService<IInputExecutionContext>()

    /// Starts a worker for a subscription whose id is free, registering it under a fresh generation.
    let startWorker (id : SubscriptionId) (source : IObservable<'T>) (payloads : ISubscriptionPayloads<'T>) =
        let generation = nextGeneration
        nextGeneration <- nextGeneration + 1
        let cancellation = CancellationTokenSource.CreateLinkedTokenSource connectionToken
        let worker =
            SubscriptionWorker<'T>(id, generation, source, payloads, outbound.Writer, inbox.Writer, logger)
        active[id] <- generation
        // Registered in the same synchronous stretch as the start, so a worker that ends synchronously only queues
        // its end: the control loop processes it after this registration
        workers[generation] <- {
            Id = id
            Generation = generation
            Cancellation = cancellation
            Worker = worker.RunAsync cancellation.Token
        }

    let subscribe (id : SubscriptionId) (query : GQLRequestContent) : Task<bool> = task {
        logger.LogTrace ($"{nameof Subscribe}. Id = '{{messageId}}'", id)
        if active.ContainsKey id then
            logger.LogWarning ("Subscriber for Id = '{id}' already exists", id)
            closeWith CustomWebSocketStatus.SubscriberAlreadyExists $"Subscriber for Id = '{id}' already exists"
            return false
        else
            try
                let variables = query.Variables |> Skippable.toValueOption
                let root = options.RootFactory httpContext
                let! executionResult =
                    options.SchemaExecutor.AsyncExecute (query.Query, getInputContext, root, ?variables = variables)
                match executionResult.Content with
                | Direct (data, errors) ->
                    // An execution result, whose data is null when a non-null root field failed during execution;
                    // still a result, so it is sent as Next + Complete like any other, not as the terminal Error
                    if not errors.IsEmpty then
                        logger.LogWarning ("Execution errors:\n{errors}", errors)
                    send (Next (id, SubscriptionExecutionResult.Create (data, errors)))
                    // The graphql-transport-ws protocol requires Complete after the single Next of a query or mutation
                    send (Complete id)
                | RequestError problemDetails ->
                    logger.LogWarning ("Request errors:\n{errors}", problemDetails)
                    // The request was rejected before execution, so it is not a result: the protocol requires it to be
                    // sent as the terminal Error message instead of a Next followed by Complete, or a client would
                    // read it as a successful result with null data
                    send (ServerError (id, problemDetails |> List.map sanitizeRequestError))
                | Deferred (data, errors, events) -> startWorker id events (DeferredPayloads (logger, data, errors))
                | Stream stream -> startWorker id stream (StreamPayloads logger)
            with ex ->
                logger.LogError (ex, "Unexpected error during subscription with id '{id}'", id)
                send (ServerError (id, [ GQLProblemDetails.Create UnexpectedObservableErrorMessage ]))
            return true
    }

    /// Handles one client message; returns whether the connection keeps running.
    let handleClientMessage (message : ClientMessage) : Task<bool> = task {
        match message with
        | ConnectionInit payload ->
            nameof ConnectionInit |> logMessageReceived payload
            closeWith CustomWebSocketStatus.TooManyInitializationRequests "Too many initialization requests"
            return false
        | ClientPing payload ->
            nameof ClientPing |> logMessageReceived payload
            match options.WebsocketOptions.CustomPingHandler with
            | ValueSome handler ->
                let! customPayload = handler serviceProvider payload
                send (ServerPong customPayload)
            | ValueNone -> send (ServerPong payload)
            return true
        | ClientPong payload ->
            nameof ClientPong |> logMessageReceived payload
            return true
        | Subscribe (id, query) -> return! subscribe id query
        | ClientComplete id ->
            logger.LogTrace ($"{nameof ClientComplete}. Id = '{{messageId}}'", id)
            match active.TryGetValue id with
            | true, generation ->
                // The worker sends nothing further, unsubscribes, and reports its end; the id is free right away
                active.Remove id |> ignore
                workers[generation].Cancellation.Cancel()
            | false, _ -> ()
            return true
    }

    let waitForEvent () : Task<bool> = task {
        try
            let! available = inbox.Reader.WaitToReadAsync connectionToken
            return available
        with :? OperationCanceledException ->
            return false
    }

    /// Processes connection events until the client leaves, a protocol failure closes the connection, or the
    /// connection is cancelled.
    let controlLoop () : Task = task {
        let mutable running = true
        while running do
            let! available = waitForEvent ()
            if not available then
                running <- false
            else
                match inbox.Reader.TryRead () with
                | true, MessageReceived message ->
                    let! keepRunning = handleClientMessage message
                    running <- keepRunning
                | true, ProtocolFailure (code, explanation) ->
                    nameof InvalidMessage |> logMessageReceived ValueNone
                    closeWith code explanation
                    running <- false
                | true, SubscriptionEnded (id, generation) ->
                    match workers.TryGetValue generation with
                    | true, handle ->
                        workers.Remove generation |> ignore
                        handle.Cancellation.Dispose ()
                    | false, _ -> ()
                    match active.TryGetValue id with
                    | true, current when current = generation -> active.Remove id |> ignore
                    | _ -> () // The id was already re-used by a newer subscription
                | false, _ -> ()
    }

    /// Reads client messages into the inbox until the socket can deliver no more.
    let readLoop () : Task = backgroundTask {
        try
            try
                while socket |> WebSocketStates.isOpen do
                    let! receivedMessage = reader.ReceiveAsync ()
                    match receivedMessage with
                    | InvalidReceivedMessage (code, explanation) ->
                        inbox.Writer.TryWrite (ProtocolFailure (code, explanation))
                        |> ignore
                    | EmptyReceivedMessage -> logger.LogTrace ("WebSocket received empty message! State = '{socketState}'", socket.State)
                    | ReceivedClientMessage message -> inbox.Writer.TryWrite (MessageReceived message) |> ignore
            with ex ->
                logger.LogDebug (ex, "Receiving from the WebSocket ended")
        finally
            inbox.Writer.TryComplete () |> ignore
    }

    /// Awaits the pending receive after a close was queued: it returns once the close handshake completed, or fails
    /// once the socket was aborted.
    let awaitReceiveAfterClose (receiveTask : Task<Result<ClientMessage voption, ClientMessageProtocolFailure>>) : Task = task {
        try
            let! _ = receiveTask
            ()
        with ex ->
            logger.LogDebug (ex, "Receiving ended after the connection was closed")
    }

    /// The connection handshake: the client must send ConnectionInit within the configured timeout.
    let initialize () : Task<bool> = task {
        logger.LogDebug ($"Waiting for {nameof ConnectionInit}...")
        let receiveTask = reader.ReceiveAsync ()
        use delayCancellation = CancellationTokenSource.CreateLinkedTokenSource connectionToken
        let timeout = Task.Delay (options.WebsocketOptions.ConnectionInitTimeout, delayCancellation.Token)
        let! completed = Task.WhenAny (receiveTask, timeout)
        if obj.ReferenceEquals (completed, timeout) then
            if not connectionToken.IsCancellationRequested then
                closeWith CustomWebSocketStatus.ConnectionTimeout "Connection initialization timeout"
            do! awaitReceiveAfterClose receiveTask
            return false
        else
            // A cancelled delay is not an unobserved fault
            delayCancellation.Cancel ()
            let! receivedMessage = receiveTask
            match receivedMessage with
            | ConnectionInitReceived ->
                logger.LogDebug ($"Valid {nameof ConnectionInit} received! Responding with ACK!")
                send ConnectionAck
                return true
            | SubscribeBeforeConnectionInit ->
                closeWith CustomWebSocketStatus.Unauthorized "Unauthorized"
                return false
            | InvalidConnectionInitMessage (code, explanation) ->
                closeWith code explanation
                return false
            | UnexpectedConnectionInitMessage ->
                close WebSocketCloseStatus.NormalClosure "Normal Closure"
                return false
    }

    /// Cancels every running subscription worker and waits for all of them to report their end.
    let shutdownWorkers () : Task = task {
        for handle in workers.Values do
            handle.Cancellation.Cancel ()
        try
            do! Task.WhenAll (workers.Values |> Seq.map _.Worker)
        with ex ->
            logger.LogError (ex, "A subscription worker of the connection did not stop cleanly")
        for handle in workers.Values do
            handle.Cancellation.Dispose ()
        workers.Clear ()
        active.Clear ()
    }

    /// Runs the connection until the client leaves, the connection is cancelled, or a protocol failure ends it.
    member _.RunAsync () : Task = task {
        // Started first: every close, including one during the handshake, goes through the sender
        let senderTask = sender.RunAsync outbound.Reader
        let mutable readerTask = Task.CompletedTask
        try
            let! initialized = initialize ()
            if initialized then
                readerTask <- readLoop ()
                do! controlLoop ()
                logger.LogTrace "Leaving the 'graphql-ws' connection loop..."
        with ex ->
            // At this point, only something really weird must have happened. In order to avoid faulty state
            // scenarios and unimagined damages, the socket is closed without further ado.
            logger.LogError (ex, "Cannot handle a message; dropping a websocket connection")
        // Workers are stopped before the sender queue is completed, so none of them can find it closed
        do! shutdownWorkers ()
        // Ignored by the sender when a protocol close already went out
        close WebSocketCloseStatus.NormalClosure "Normal Closure"
        outbound.Writer.TryComplete () |> ignore
        do! senderTask
        // Cannot hang: the sender closed or aborted the socket, which ends the pending receive
        do! readerTask
    }
