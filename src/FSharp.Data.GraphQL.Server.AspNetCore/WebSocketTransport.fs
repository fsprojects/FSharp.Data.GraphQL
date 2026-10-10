namespace FSharp.Data.GraphQL.Server.AspNetCore

open System
open System.Buffers
open System.Diagnostics
open System.Linq
open System.Net.WebSockets
open System.Text
open System.Text.Json
open System.Threading
open System.Threading.Channels
open System.Threading.Tasks
open Microsoft.Extensions.Logging

open Collections.Pooled
open FsToolkit.ErrorHandling

open FSharp.Data.GraphQL.Shared.WebSockets
open FSharp.Data.GraphQL.Server.AspNetCore.ServerMessageSerialization

/// The socket states relevant to a connection's loops.
module internal WebSocketStates =

    /// Whether the socket can still deliver client messages.
    let isOpen (socket : WebSocket) =
        socket.State <> WebSocketState.Aborted
        && socket.State <> WebSocketState.Closed
        && socket.State <> WebSocketState.CloseReceived

    /// Whether a close handshake can still be started or completed on the socket.
    let canClose (socket : WebSocket) =
        socket.State <> WebSocketState.Aborted
        && socket.State <> WebSocketState.Closed

/// <summary>
/// Reads whole client messages from a socket, and deserializes them into protocol messages.
/// </summary>
/// <remarks>
/// <para>
/// A receive is never cancelled: the managed socket aborts on a cancelled receive and the client would never see a close code. A pending receive ends
/// when the client sends its next message or its close frame, when the sender loop completes a close handshake, or when the socket is aborted.
/// </para>
/// <para>
/// A message is buffered only up to the maximum size: a message exceeding it is rejected with
/// <see cref="WebSocketCloseStatus.MessageTooBig"/> as soon as one byte beyond the limit is received, and no more of it is buffered; closing the
/// socket afterwards may still read and discard the rest of it.
/// </para>
/// </remarks>
type internal WebSocketMessageReader
    /// <param name="socket">The socket to read from.</param>
    /// <param name="serializerOptions">The options client messages are deserialized with.</param>
    /// <param name="readBufferSize">The size of the buffer rented for every receive.</param>
    /// <param name="maxMessageSize">The maximum size in bytes of a message; must be positive.</param>
    /// <param name="logger">The logger of the connection.</param>
    (socket : WebSocket, serializerOptions : JsonSerializerOptions, readBufferSize : int, maxMessageSize : int, logger : ILogger) =

    static let invalidJsonInClientMessageError =
        Error (InvalidMessage (CustomWebSocketStatus.InvalidMessage, "Invalid json in client message"))

    do ArgumentOutOfRangeException.ThrowIfNegativeOrZero (maxMessageSize, nameof maxMessageSize)

    let messageTooBigError =
        Error (InvalidMessage (int WebSocketCloseStatus.MessageTooBig, $"Message exceeds the maximum size of %d{maxMessageSize} bytes"))

    /// <summary>
    /// Deserializes the JSON of a client message into a protocol message, or the protocol failure the message is rejected with.
    /// </summary>
    member _.Deserialize (json : byte array) : Result<ClientMessage, ClientMessageProtocolFailure> =
        try
            Ok (JsonSerializer.Deserialize<ClientMessage>(ReadOnlySpan<byte> json, serializerOptions))
        with
        | :? InvalidWebsocketMessageException as ex ->
            logger.LogError (ex, "Invalid websocket message")
            Error (InvalidMessage (CustomWebSocketStatus.InvalidMessage, ex.Message.ToString ()))
        | :? JsonException as ex when logger.IsEnabled (LogLevel.Trace) ->
            logger.LogError (ex, "Cannot deserialize WebSocket message:\n{payload}", Encoding.UTF8.GetString json)
            invalidJsonInClientMessageError
        | :? JsonException as ex ->
            logger.LogError (ex, "Cannot deserialize WebSocket message")
            invalidJsonInClientMessageError
        | ex ->
            logger.LogError (ex, $"Unexpected exception '{ex.GetType().Name}' in GraphQLWebsocketMiddleware")
            invalidJsonInClientMessageError

    /// <summary>
    /// Receives the next message and deserializes it, as the connection handshake does with the single message it reads.
    /// </summary>
    member this.ReceiveMessageAsync () : Task<Result<ClientMessage voption, ClientMessageProtocolFailure>> = taskResult {
        match! this.ReceiveAsync () with
        | ValueNone -> return ValueNone
        | ValueSome json ->
            let! message = this.Deserialize json
            return ValueSome message
    }

    /// <summary>
    /// Receives the next message as its JSON: <see cref="ValueNone"/> for an empty message (such as the client's close frame), or the
    /// protocol failure of a message over the maximum size. The message is deserialized by <see cref="Deserialize"/> when it is handled,
    /// so that a message waiting to be handled holds no more memory than its bytes.
    /// </summary>
    member _.ReceiveAsync () : Task<Result<byte array voption, ClientMessageProtocolFailure>> = taskResult {
        let buffer = ArrayPool.Shared.Rent readBufferSize
        try
            use completeMessage = new PooledList<byte> ()
            let mutable segmentResponse : WebSocketReceiveResult | null = null
            let mutable tooBig = false
            while not tooBig
                  && socket |> WebSocketStates.isOpen
                  && (match segmentResponse with
                      | null -> true
                      | segment -> not segment.EndOfMessage) do
                // Never more than one byte beyond the limit is requested, so that a message over it is detected without reading,
                // let alone buffering, the rest of it. Never negative, since no more than the limit is ever buffered.
                let remaining = maxMessageSize - completeMessage.Count
                let count = if remaining < buffer.Length then remaining + 1 else buffer.Length
                let! received = socket.ReceiveAsync (ArraySegment<byte>(buffer, 0, count), CancellationToken.None)
                segmentResponse <- received
                if received.Count > remaining then
                    tooBig <- true
                else
                    completeMessage.AddRange (ArraySegment<byte>(buffer, 0, received.Count))

            if tooBig then
                logger.LogWarning ("Rejecting a client message exceeding the maximum size of {maxMessageSize} bytes", maxMessageSize)
                return! messageTooBigError
            else
                if Debugger.IsAttached then
                    let message =
                        completeMessage
                        |> Seq.filter (fun x -> x > 0uy)
                        |> Seq.toArray
                        |> Encoding.UTF8.GetString
                    logger.LogInformation ("-> Request: {request}", message)
                if completeMessage.All (fun b -> b = 0uy) then
                    return ValueNone
                else
                    return ValueSome (completeMessage.Span.ToArray ())
        finally
            ArrayPool.Shared.Return buffer
    }

/// <summary>
/// The queue of the sender loop of a connection: every message is serialized when it is queued, so that what waits for the client is counted
/// in bytes, and a message that would make more than the limit wait is refused.
/// </summary>
/// <remarks>
/// <para>
/// A client that does not read what the server sends would otherwise make the connection hold every message for it, such as the pong of every
/// ping it keeps sending. The producers of a connection never wait for room, so a refused message means that the client does not keep up: the
/// queue reports it, and the connection aborts the socket.
/// </para>
/// <para>
/// A message is accepted whatever its size while nothing else waits, so that a result larger than the limit still reaches a client that reads.
/// </para>
/// </remarks>
[<Sealed>]
type internal SenderQueue
    /// <param name="serializerOptions">The options server messages are serialized with.</param>
    /// <param name="byteLimit">The most bytes that may wait for the client.</param>
    /// <param name="onOverflow">Called, possibly more than once, when a message is refused because too much waits for the client.</param>
    /// <param name="logger">The logger of the connection.</param>
    (serializerOptions : JsonSerializerOptions, byteLimit : int64, onOverflow : unit -> unit, logger : ILogger) =
    inherit ChannelWriter<OutboundMessage> ()

    let queue =
        Channel.CreateUnbounded<QueuedMessage> (UnboundedChannelOptions (SingleReader = true, SingleWriter = false, AllowSynchronousContinuations = false))

    // The bytes of the messages queued and not sent yet, counted by the producers and released by the sender loop
    let mutable queuedBytes = 0L

    /// The messages for the sender loop
    member _.Reader = queue.Reader

    /// Releases the bytes of a message the sender loop sent or dropped
    member _.Release (json : byte array) = Interlocked.Add (&queuedBytes, -int64 json.Length) |> ignore

    /// <inheritdoc />
    override _.TryWrite (message : OutboundMessage) =
        match message with
        | Close (status, description) -> queue.Writer.TryWrite (QueuedClose (status, description))
        | Send serverMessage ->
            logger.LogTrace ("<- Response: {response}", serverMessage)
            let json = Encoding.UTF8.GetBytes (serializeServerMessage serializerOptions serverMessage)
            let size = int64 json.Length
            let total = Interlocked.Add (&queuedBytes, size)
            if total > byteLimit && total > size then
                Interlocked.Add (&queuedBytes, -size) |> ignore
                onOverflow ()
                false
            elif queue.Writer.TryWrite (QueuedSend json) then
                true
            else
                Interlocked.Add (&queuedBytes, -size) |> ignore
                false

    /// <inheritdoc />
    override _.WaitToWriteAsync (cancellationToken : CancellationToken) = queue.Writer.WaitToWriteAsync cancellationToken

    /// <inheritdoc />
    override _.TryComplete (error : exn | null) = queue.Writer.TryComplete error

/// <summary>
/// The sender loop of a connection: the sole caller of <see cref="WebSocket.SendAsync"/>, <see cref="WebSocket.CloseAsync"/> and
/// <see cref="WebSocket.Abort"/> on its socket, so nothing else needs to serialize access to it.
/// </summary>
/// <remarks>
/// Messages are sent in the order they were queued. The first <see cref="QueuedMessage.QueuedClose"/> closes the socket gracefully, aborting it when the
/// handshake does not complete within the timeout; whatever is queued after it is dropped. A failed send also marks the connection closed, since the
/// socket is gone.
/// </remarks>
type internal WebSocketMessageSender
    /// <param name="socket">The socket to write to.</param>
    /// <param name="gracefulCloseTimeout">How long a close handshake may take before the socket is aborted.</param>
    /// <param name="logger">The logger of the connection.</param>
    (socket : WebSocket, gracefulCloseTimeout : TimeSpan, logger : ILogger) =

    let closeSocket (status : WebSocketCloseStatus) (description : string) : Task = task {
        if socket |> WebSocketStates.canClose then
            // Bounded by a timeout of its own, not by the connection's token: a close requested because that token
            // was cancelled must still complete the handshake instead of aborting at once
            use timeout = new CancellationTokenSource (gracefulCloseTimeout)
            try
                do! socket.CloseAsync (status, description, timeout.Token)
            with
            | :? OperationCanceledException ->
                logger.LogWarning ("Aborting WebSocket after graceful close did not complete in time. State = '{state}'", socket.State)
                socket.Abort ()
            | ex ->
                logger.LogWarning (ex, "Aborting WebSocket after graceful close failed. State = '{state}'", socket.State)
                socket.Abort ()
        else
            logger.LogTrace ("Ignoring socket close request, since its state is neither writable nor closeable, but '{state}'", socket.State)
    }

    /// <summary>
    /// Sends every queued message until the queue is completed, closing the socket at the first close request.
    /// </summary>
    /// <param name="queue">The messages to send.</param>
    /// <param name="sendCancellation">
    /// Cancelled when the client does not read what waits for it: a pending send then fails and aborts the socket.
    /// </param>
    member _.RunAsync (queue : SenderQueue, sendCancellation : CancellationToken) : Task = backgroundTask {
        let outbound = queue.Reader
        let mutable closed = false
        let mutable more = true
        while more do
            let! canRead = outbound.WaitToReadAsync ()
            if not canRead then
                more <- false
            else
                let mutable draining = true
                while draining do
                    match outbound.TryRead () with
                    | true, QueuedSend json when closed ->
                        queue.Release json
                        logger.LogTrace "Ignoring a message to be sent after the connection was closed"
                    | true, QueuedSend json when socket.State <> WebSocketState.Open ->
                        queue.Release json
                        logger.LogTrace (
                            $"Ignoring message to be sent via socket, since its state is not '{nameof WebSocketState.Open}', but '{{state}}'",
                            socket.State
                        )
                    | true, QueuedSend json ->
                        try
                            try
                                do! socket.SendAsync (ArraySegment<byte> json, WebSocketMessageType.Text, endOfMessage = true, cancellationToken = sendCancellation)
                            with ex ->
                                logger.LogWarning (ex, "Sending a message failed; the connection is treated as closed")
                                closed <- true
                        finally
                            // Counted until it is sent, so that a send the client does not take keeps counting against the limit
                            queue.Release json
                    | true, QueuedClose _ when closed -> ()
                    | true, QueuedClose (status, description) ->
                        closed <- true
                        do! closeSocket status description
                    | false, _ -> draining <- false
    }
