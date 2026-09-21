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
/// Reads whole client messages from a socket and deserializes them into protocol messages.
/// </summary>
/// <remarks>
/// A receive is never cancelled: the managed socket aborts on a cancelled receive and the client would never see a close code. A pending receive ends
/// when the client sends its next message or its close frame, when the sender loop completes a close handshake, or when the socket is aborted.
/// </remarks>
type internal WebSocketMessageReader
    /// <param name="socket">The socket to read from.</param>
    /// <param name="serializerOptions">The options client messages are deserialized with.</param>
    /// <param name="readBufferSize">The size of the buffer rented for every receive.</param>
    /// <param name="logger">The logger of the connection.</param>
    (socket : WebSocket, serializerOptions : JsonSerializerOptions, readBufferSize : int, logger : ILogger) =

    static let invalidJsonInClientMessageError =
        Error (InvalidMessage (CustomWebSocketStatus.InvalidMessage, "Invalid json in client message"))

    let deserializeClientMessage (message : IReadOnlyPooledList<byte>) = taskResult {
        try
            return JsonSerializer.Deserialize<ClientMessage>(message.Span, serializerOptions)
        with
        | :? InvalidWebsocketMessageException as ex ->
            logger.LogError (ex, "Invalid websocket message:\n{payload}", message)
            return! Error (InvalidMessage (CustomWebSocketStatus.InvalidMessage, ex.Message.ToString ()))
        | :? JsonException as ex when logger.IsEnabled (LogLevel.Trace) ->
            logger.LogError (ex, "Cannot deserialize WebSocket message:\n{payload}", message)
            return! invalidJsonInClientMessageError
        | :? JsonException as ex ->
            logger.LogError (ex, "Cannot deserialize WebSocket message")
            return! invalidJsonInClientMessageError
        | ex ->
            logger.LogError (ex, $"Unexpected exception '{ex.GetType().Name}' in GraphQLWebsocketMiddleware")
            return! invalidJsonInClientMessageError
    }

    /// <summary>
    /// Receives the next message: a protocol message, <see cref="ValueNone"/> for an empty message (such as the client's close frame), or the
    /// protocol failure the message is rejected with.
    /// </summary>
    member _.ReceiveAsync () : Task<Result<ClientMessage voption, ClientMessageProtocolFailure>> = taskResult {
        let buffer = ArrayPool.Shared.Rent readBufferSize
        try
            use completeMessage = new PooledList<byte> ()
            let mutable segmentResponse : WebSocketReceiveResult | null = null
            while socket |> WebSocketStates.isOpen
                  && (match segmentResponse with
                      | null -> true
                      | segment -> not segment.EndOfMessage) do
                let! received = socket.ReceiveAsync (ArraySegment<byte> buffer, CancellationToken.None)
                segmentResponse <- received
                completeMessage.AddRange (ArraySegment<byte>(buffer, 0, received.Count))

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
                let! result = deserializeClientMessage completeMessage
                return ValueSome result
        finally
            ArrayPool.Shared.Return buffer
    }

/// <summary>
/// The sender loop of a connection: the sole caller of <see cref="WebSocket.SendAsync"/>, <see cref="WebSocket.CloseAsync"/> and
/// <see cref="WebSocket.Abort"/> on its socket, so nothing else needs to serialize access to it.
/// </summary>
/// <remarks>
/// Messages are sent in the order they were queued. The first <see cref="OutboundMessage.Close"/> closes the socket gracefully, aborting it when the
/// handshake does not complete within the timeout; whatever is queued after it is dropped. A failed send also marks the connection closed, since the
/// socket is gone.
/// </remarks>
type internal WebSocketMessageSender
    /// <param name="socket">The socket to write to.</param>
    /// <param name="serializerOptions">The options server messages are serialized with.</param>
    /// <param name="gracefulCloseTimeout">How long a close handshake may take before the socket is aborted.</param>
    /// <param name="logger">The logger of the connection.</param>
    (socket : WebSocket, serializerOptions : JsonSerializerOptions, gracefulCloseTimeout : TimeSpan, logger : ILogger) =

    let sendMessage (message : ServerMessage) : Task = task {
        logger.LogTrace ("<- Response: {response}", message)
        let serialized = serializeServerMessage serializerOptions message
        let segment = ArraySegment<byte>(Encoding.UTF8.GetBytes serialized)
        do! socket.SendAsync (segment, WebSocketMessageType.Text, endOfMessage = true, cancellationToken = CancellationToken.None)
    }

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

    /// Sends every queued message until the queue is completed, closing the socket at the first close request.
    member _.RunAsync (outbound : ChannelReader<OutboundMessage>) : Task = backgroundTask {
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
                    | true, Send message when closed ->
                        logger.LogTrace ("Ignoring message to be sent after the connection was closed: {response}", message)
                    | true, Send message when socket.State <> WebSocketState.Open ->
                        logger.LogTrace (
                            $"Ignoring message to be sent via socket, since its state is not '{nameof WebSocketState.Open}', but '{{state}}'",
                            socket.State
                        )
                    | true, Send message ->
                        try
                            do! sendMessage message
                        with ex ->
                            logger.LogWarning (ex, "Sending a message failed; the connection is treated as closed")
                            closed <- true
                    | true, Close _ when closed -> ()
                    | true, Close (status, description) ->
                        closed <- true
                        do! closeSocket status description
                    | false, _ -> draining <- false
    }
