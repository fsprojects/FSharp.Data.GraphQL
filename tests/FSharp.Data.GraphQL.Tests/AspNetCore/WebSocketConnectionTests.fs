module FSharp.Data.GraphQL.Tests.AspNetCore.WebSocketConnectionTests

open System
open System.Net.WebSockets
open System.Text
open System.Text.Json
open System.Threading
open System.Threading.Channels
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging.Abstractions
open Microsoft.Extensions.Options
open Xunit

open FSharp.Data.GraphQL.Server.AspNetCore
open FSharp.Data.GraphQL.Shared.WebSockets

// Drives GraphQLWebSocketConnection over a fake socket: the protocol handshake, close codes, queries, request
// errors, subscriptions, client-side completion, incremental delivery, the message size limit and the bounded
// inbox, without a hosted server.

let private timeout = TimeSpan.FromSeconds 10.0

/// <summary>
/// A socket whose client side is scripted by the test: text frames and a close frame are queued for the server to
/// receive, every frame the server sends is recorded, and a server-initiated close is answered by the client the way
/// a managed socket would, by completing the pending receive with a close frame.
/// </summary>
type private FakeWebSocket () =
    inherit WebSocket ()

    let incoming = Channel.CreateUnbounded<byte[] voption> ()
    let sent = Channel.CreateUnbounded<string> ()
    let mutable state = WebSocketState.Open
    let mutable serverCloseStatus = ValueNone
    let mutable remainder = ReadOnlyMemory<byte>.Empty
    // Written by the server's reader, read by the test
    let mutable deliveredBytes = 0L
    let mutable deliveredMessages = 0
    let mutable blockSends = false

    member _.EnqueueText (json : string) = incoming.Writer.TryWrite (ValueSome (Encoding.UTF8.GetBytes json)) |> ignore
    /// Makes every later send wait until it is cancelled, as for a client that does not read; a cancelled send aborts the
    /// socket, as a managed one does
    member _.BlockSends () = blockSends <- true
    member _.EnqueueClose () = incoming.Writer.TryWrite ValueNone |> ignore
    member _.Sent = sent.Reader
    member _.ServerCloseStatus : WebSocketCloseStatus voption = serverCloseStatus
    /// How many bytes of text frames the server has received so far
    member _.DeliveredBytes = Interlocked.Read &deliveredBytes
    /// How many whole text messages the server has received so far
    member _.DeliveredMessages = Volatile.Read &deliveredMessages

    override _.CloseStatus = serverCloseStatus |> ValueOption.toNullable
    override _.CloseStatusDescription = null
    override _.State = state
    override _.SubProtocol = GraphQLTransportWS.SubProtocol

    override _.Abort () =
        state <- WebSocketState.Aborted
        incoming.Writer.TryComplete () |> ignore

    override _.CloseAsync (status, _, _) =
        serverCloseStatus <- ValueSome status
        match state with
        | WebSocketState.CloseReceived -> state <- WebSocketState.Closed
        | _ ->
            state <- WebSocketState.Closed
            // The client answers the close handshake
            incoming.Writer.TryWrite ValueNone |> ignore
        Task.CompletedTask

    override _.CloseOutputAsync (status, _, _) =
        serverCloseStatus <- ValueSome status
        state <- WebSocketState.CloseSent
        Task.CompletedTask

    override _.Dispose () = ()

    override _.ReceiveAsync (buffer : ArraySegment<byte>, _ : CancellationToken) : Task<WebSocketReceiveResult> = task {
        let deliver (bytes : ReadOnlyMemory<byte>) =
            let count = min bytes.Length buffer.Count
            bytes.Slice(0, count).CopyTo (Memory<byte> (buffer.Array, buffer.Offset, count))
            remainder <- bytes.Slice count
            Interlocked.Add (&deliveredBytes, int64 count) |> ignore
            if remainder.IsEmpty then
                Interlocked.Increment &deliveredMessages |> ignore
            WebSocketReceiveResult (count, WebSocketMessageType.Text, remainder.IsEmpty)

        if not remainder.IsEmpty then
            return deliver remainder
        else
            // Throws once the socket was aborted, as a real receive does
            match! incoming.Reader.ReadAsync () with
            | ValueNone ->
                if state = WebSocketState.Open then
                    state <- WebSocketState.CloseReceived
                return WebSocketReceiveResult (0, WebSocketMessageType.Close, true, Nullable WebSocketCloseStatus.NormalClosure, "closed")
            | ValueSome bytes -> return deliver (ReadOnlyMemory bytes)
    }

    override this.SendAsync (buffer : ArraySegment<byte>, _ : WebSocketMessageType, _ : bool, cancellationToken : CancellationToken) : Task =
        if blockSends then
            task {
                let cancelled = TaskCompletionSource TaskCreationOptions.RunContinuationsAsynchronously
                use _ = cancellationToken.Register (fun () -> cancelled.TrySetResult () |> ignore)
                do! cancelled.Task
                this.Abort ()
                cancellationToken.ThrowIfCancellationRequested ()
            }
            :> Task
        else
            sent.Writer.TryWrite (Encoding.UTF8.GetString (buffer.Array, buffer.Offset, buffer.Count)) |> ignore
            Task.CompletedTask

type private Session = {
    Socket : FakeWebSocket
    Options : GraphQLOptions<Root>
    Run : Task
    Scope : IDisposable
}

let private startConnectionWith (executor : FSharp.Data.GraphQL.Executor<Root>) (configure : GraphQLOptions<Root> -> GraphQLOptions<Root>) =
    let services = ServiceCollection ()
    services.AddLogging () |> ignore
    services.AddGraphQL<Root> (executor, (fun _ -> { RequestId = "test" })) |> ignore
    let scope = services.BuildServiceProvider().CreateScope()
    let serviceProvider = scope.ServiceProvider
    let httpContext = DefaultHttpContext (RequestServices = serviceProvider)
    serviceProvider.GetRequiredService<IHttpContextAccessor>().HttpContext <- httpContext
    let options = serviceProvider.GetRequiredService<IOptions<GraphQLOptions<Root>>>().Value |> configure
    let socket = new FakeWebSocket ()
    let connection = GraphQLWebSocketConnection<Root> (httpContext, socket, options, serviceProvider, NullLogger.Instance, CancellationToken.None)
    { Socket = socket; Options = options; Run = connection.RunAsync (); Scope = scope }

let private startConnection (configure : GraphQLOptions<Root> -> GraphQLOptions<Root>) = startConnectionWith TestSchema.executor configure

let private start () = startConnection id

let private receive (session : Session) : Task<JsonDocument> = task {
    use cancellation = new CancellationTokenSource (timeout)
    let! json = session.Socket.Sent.ReadAsync cancellation.Token
    return JsonDocument.Parse json
}

let private typeOf (message : JsonDocument) = message.RootElement.GetProperty("type").GetString ()
let private idOf (message : JsonDocument) = message.RootElement.GetProperty("id").GetString ()
let private payloadOf (message : JsonDocument) = message.RootElement.GetProperty "payload"

let private hasProperty (name : string) (element : JsonElement) =
    let mutable ignored = Unchecked.defaultof<JsonElement>
    element.TryGetProperty (name, &ignored)

let private subscribe (session : Session) (id : string) (query : string) =
    session.Socket.EnqueueText (JsonSerializer.Serialize {| id = id; ``type`` = "subscribe"; payload = {| query = query |} |})

let private initialize (session : Session) : Task = task {
    session.Socket.EnqueueText """{"type":"connection_init"}"""
    use! ack = receive session
    typeOf ack |> equals "connection_ack"
}

let private closeFromClient (session : Session) : Task = task {
    session.Socket.EnqueueClose ()
    do! waitForTask timeout "The connection did not end after the client closed" session.Run
    do! session.Run
}

/// Receives the messages of one subscription up to and including its complete or error message
let private receiveUntilTerminal (session : Session) (id : string) : Task<JsonDocument list> = task {
    let received = ResizeArray<JsonDocument> ()
    let mutable terminal = false
    while not terminal do
        let! message = receive session
        idOf message |> equals id
        received.Add message
        match typeOf message with
        | "complete"
        | "error" -> terminal <- true
        | _ -> ()
    return List.ofSeq received
}

/// A message of the given type and of exactly the given size in bytes, padded by its payload
let private paddedMessage (messageType : string) (size : int) =
    let envelope = $$$"""{"type":"{{{messageType}}}","payload":{"pad":""}}"""
    // Pads between the quotes of the empty "pad" string
    envelope.Insert (envelope.Length - 3, String ('x', size - envelope.Length))

/// Polls the condition until it holds, failing with the message once the timeout has passed
let private waitUntil (message : unit -> string) (condition : unit -> bool) : Task = task {
    let started = Diagnostics.Stopwatch.StartNew ()
    while not (condition ()) do
        if started.Elapsed > timeout then
            fail (message ())
        do! Task.Delay 10
}

/// A ping handler that answers no ping until the returned gate is opened, keeping the control loop busy
let private gatedPingHandler () =
    // Opening the gate must not run the control loop on the test's thread
    let gate = TaskCompletionSource TaskCreationOptions.RunContinuationsAsynchronously
    let handler : PingHandler = fun _ payload -> task {
        do! gate.Task
        return payload
    }
    struct (gate, handler)

[<Fact>]
let ``Connection is closed with 4408 when connection_init does not arrive in time`` () : Task = task {
    let session =
        startConnection (fun options -> {
            options with
                WebsocketOptions = { options.WebsocketOptions with ConnectionInitTimeout = TimeSpan.FromMilliseconds 100.0 }
        })
    use _ = session.Scope
    do! waitForTask timeout "The connection did not end after the initialization timeout" session.Run
    do! session.Run
    session.Socket.ServerCloseStatus |> equals (ValueSome (enum<WebSocketCloseStatus> 4408))
}

[<Fact>]
let ``Subscribe before connection_init closes the connection with 4401`` () : Task = task {
    let session = start ()
    use _ = session.Scope
    subscribe session "1" """{ hero(id: "1000") { name } }"""
    do! waitForTask timeout "The connection did not end after the unauthorized subscribe" session.Run
    do! session.Run
    session.Socket.ServerCloseStatus |> equals (ValueSome (enum<WebSocketCloseStatus> 4401))
}

[<Fact>]
let ``A second connection_init closes the connection with 4429`` () : Task = task {
    let session = start ()
    use _ = session.Scope
    do! initialize session
    session.Socket.EnqueueText """{"type":"connection_init"}"""
    do! waitForTask timeout "The connection did not end after the second connection_init" session.Run
    do! session.Run
    session.Socket.ServerCloseStatus |> equals (ValueSome (enum<WebSocketCloseStatus> 4429))
}

[<Fact>]
let ``Client close ends the connection with a normal closure`` () : Task = task {
    let session = start ()
    use _ = session.Scope
    do! initialize session
    do! closeFromClient session
    session.Socket.ServerCloseStatus |> equals (ValueSome WebSocketCloseStatus.NormalClosure)
}

[<Fact>]
let ``Ping is answered with pong`` () : Task = task {
    let session = start ()
    use _ = session.Scope
    do! initialize session
    session.Socket.EnqueueText """{"type":"ping"}"""
    use! pong = receive session
    typeOf pong |> equals "pong"
    do! closeFromClient session
}

[<Fact>]
let ``A query is answered with next and complete`` () : Task = task {
    let session = start ()
    use _ = session.Scope
    do! initialize session
    subscribe session "1" """{ hero(id: "1000") { name } }"""
    let! messages = receiveUntilTerminal session "1"
    messages |> List.map typeOf |> equals [ "next"; "complete" ]
    (payloadOf messages.Head).GetProperty("data").GetProperty("hero").GetProperty("name").GetString ()
    |> equals "Luke Skywalker"
    do! closeFromClient session
}

[<Fact>]
let ``A request error is sent as an error message`` () : Task = task {
    let session = start ()
    use _ = session.Scope
    do! initialize session
    subscribe session "1" """{ hero(id: "1000") { nope } }"""
    let! messages = receiveUntilTerminal session "1"
    let error = List.exactlyOne messages
    typeOf error |> equals "error"
    let payload = payloadOf error
    payload.ValueKind |> equals JsonValueKind.Array
    Assert.Contains ("nope", payload[0].GetProperty("message").GetString ())
    do! closeFromClient session
}

[<Fact>]
let ``A duplicate subscription id closes the connection with 4409`` () : Task = task {
    let session = start ()
    use _ = session.Scope
    do! initialize session
    subscribe session "1" """subscription { watchMoon(id: "1") { id isMoon } }"""
    subscribe session "1" """subscription { watchMoon(id: "2") { id isMoon } }"""
    do! waitForTask timeout "The connection did not end after the duplicate subscription id" session.Run
    do! session.Run
    session.Socket.ServerCloseStatus |> equals (ValueSome (enum<WebSocketCloseStatus> 4409))
}

[<Fact>]
let ``Client complete cancels the subscription and frees its id`` () : Task = task {
    let session = start ()
    use _ = session.Scope
    do! initialize session
    subscribe session "1" """subscription { watchMoon(id: "1") { id isMoon } }"""
    session.Socket.EnqueueText """{"id":"1","type":"complete"}"""
    subscribe session "1" """{ hero(id: "1000") { name } }"""
    let! messages = receiveUntilTerminal session "1"
    messages |> List.map typeOf |> equals [ "next"; "complete" ]
    do! closeFromClient session
}

[<Fact>]
let ``A query with defer delivers the incremental payloads then complete`` () : Task = task {
    let session = start ()
    use _ = session.Scope
    do! initialize session
    subscribe session "1" """{ hero(id: "1000") { name homePlanet @defer } }"""
    let! messages = receiveUntilTerminal session "1"
    messages |> List.map typeOf |> equals [ "next"; "next"; "next"; "next"; "complete" ]
    let payloads = messages |> List.filter (fun message -> typeOf message = "next") |> List.map payloadOf
    let initial = List.head payloads
    Assert.True (initial.GetProperty("hasNext").GetBoolean (), "The initial payload must have hasNext true")
    initial.GetProperty("data").GetProperty("hero").GetProperty("homePlanet").ValueKind |> equals JsonValueKind.Null
    let final = List.last payloads
    Assert.False (final.GetProperty("hasNext").GetBoolean (), "The final payload must have hasNext false")
    Assert.False (hasProperty "data" final, "The final payload must not carry data")
    let delivered = payloads |> List.find (hasProperty "incremental")
    Assert.Contains ("Tatooine", (delivered.GetProperty("incremental")[0]).GetProperty("data").GetRawText ())
    do! closeFromClient session
}

[<Fact>]
let ``Closing the connection cancels a running subscription`` () : Task = task {
    let session = start ()
    use _ = session.Scope
    do! initialize session
    subscribe session "1" """subscription { watchMoon(id: "1") { id isMoon } }"""
    do! closeFromClient session
    session.Socket.ServerCloseStatus |> equals (ValueSome WebSocketCloseStatus.NormalClosure)
}

/// Options with a size limit small enough to fall inside a message read in several frames
let private withSizeLimit (limit : int) (options : GraphQLOptions<Root>) = {
    options with
        ReadBufferSize = 256
        WebsocketOptions = { options.WebsocketOptions with MaxReceiveMessageSize = limit }
}

let private withPingHandler (handler : PingHandler) (options : GraphQLOptions<Root>) = {
    options with
        WebsocketOptions = { options.WebsocketOptions with CustomPingHandler = ValueSome handler }
}

let private smallSizeLimit = 1024

[<Theory>]
[<InlineData 1>]
[<InlineData 0>]
let ``A message up to the maximum size is processed`` (bytesBelowLimit : int) : Task = task {
    let session = startConnection (withSizeLimit smallSizeLimit)
    use _ = session.Scope
    do! initialize session
    let ping = paddedMessage "ping" (smallSizeLimit - bytesBelowLimit)
    Encoding.UTF8.GetByteCount ping |> equals (smallSizeLimit - bytesBelowLimit)
    session.Socket.EnqueueText ping
    use! pong = receive session
    typeOf pong |> equals "pong"
    use sentPing = JsonDocument.Parse ping
    (payloadOf pong).GetProperty("pad").GetString ()
    |> equals ((payloadOf sentPing).GetProperty("pad").GetString ())
    do! closeFromClient session
    session.Socket.ServerCloseStatus |> equals (ValueSome WebSocketCloseStatus.NormalClosure)
}

[<Fact>]
let ``A message over the maximum size closes the connection with 1009 without being read in full`` () : Task = task {
    let session = startConnection (withSizeLimit smallSizeLimit)
    use _ = session.Scope
    do! initialize session
    let deliveredBefore = session.Socket.DeliveredBytes
    let size = 64 * 1024
    session.Socket.EnqueueText (paddedMessage "ping" size)
    do! waitForTask timeout "The connection did not end after a message over the size limit" session.Run
    do! session.Run
    session.Socket.ServerCloseStatus |> equals (ValueSome WebSocketCloseStatus.MessageTooBig)
    let read = session.Socket.DeliveredBytes - deliveredBefore
    Assert.True (
        read <= int64 smallSizeLimit + 1L,
        $"The server read %d{read} of the %d{size} bytes of a message over the limit of %d{smallSizeLimit} bytes instead of stopping one byte past it"
    )
    match session.Socket.Sent.TryRead () with
    | true, answer -> fail $"The message over the size limit was answered with %s{answer}"
    | false, _ -> ()
}

[<Fact>]
let ``A message over the default maximum size of 4 MiB closes the connection with 1009 without being read in full`` () : Task = task {
    let limit = 4 * 1024 * 1024
    let session = start ()
    use _ = session.Scope
    session.Options.WebsocketOptions.MaxReceiveMessageSize |> equals limit
    do! initialize session
    let deliveredBefore = session.Socket.DeliveredBytes
    session.Socket.EnqueueText (paddedMessage "ping" (limit + 4096))
    do! waitForTask timeout "The connection did not end after a message over the size limit" session.Run
    do! session.Run
    session.Socket.ServerCloseStatus |> equals (ValueSome WebSocketCloseStatus.MessageTooBig)
    let read = session.Socket.DeliveredBytes - deliveredBefore
    Assert.True (read <= int64 limit + 1L, $"The server read %d{read} bytes of a message over the limit of %d{limit} bytes instead of stopping one byte past it")
}

[<Fact>]
let ``A connection_init over the maximum size closes the connection with 1009`` () : Task = task {
    let session = startConnection (withSizeLimit smallSizeLimit)
    use _ = session.Scope
    session.Socket.EnqueueText (paddedMessage "connection_init" (smallSizeLimit + 1))
    do! waitForTask timeout "The connection did not end after a connection_init over the size limit" session.Run
    do! session.Run
    session.Socket.ServerCloseStatus |> equals (ValueSome WebSocketCloseStatus.MessageTooBig)
}

[<Theory>]
[<InlineData 0>]
[<InlineData(-1)>]
let ``A maximum message size that is not positive is rejected`` (maxMessageSize : int) =
    use socket = new FakeWebSocket ()
    let ex =
        Assert.Throws<ArgumentOutOfRangeException> (fun () ->
            WebSocketMessageReader (socket, serializerOptions, GraphQLOptionsDefaults.ReadBufferSize, maxMessageSize, NullLogger.Instance)
            |> ignore
        )
    ex.ParamName |> equals "maxMessageSize"

[<Fact>]
let ``The reader stops reading from the socket while the inbox is full`` () : Task = task {
    let capacity = WebSocketConnectionLimits.IncomingMessageQueueCapacity
    let struct (gate, handler) = gatedPingHandler ()
    let session = startConnection (withPingHandler handler)
    use _ = session.Scope
    do! initialize session
    let sent = capacity * 4
    for _ in 1..sent do
        session.Socket.EnqueueText """{"type":"ping"}"""
    // connection_init, the ping the control loop is busy with, the pings filling the inbox, and the one the reader waits to queue
    let readerWaiting = 1 + 1 + capacity + 1
    do!
        (fun () -> session.Socket.DeliveredMessages >= readerWaiting)
        |> waitUntil (fun () -> $"The server read only %d{session.Socket.DeliveredMessages} messages, fewer than the %d{readerWaiting} it can hold")
    // Whatever more the reader would read, it reads in this time: every message the client sent is already in the socket
    do! Task.Delay 200
    let read = session.Socket.DeliveredMessages
    Assert.True (
        (read = readerWaiting),
        $"The server read %d{read} of the %d{sent + 1} messages the client sent while its control loop was busy, instead of stopping at %d{readerWaiting}"
    )
    // Once the control loop catches up, every message is read and answered: none was dropped
    gate.SetResult ()
    for _ in 1..sent do
        use! pong = receive session
        typeOf pong |> equals "pong"
    do! closeFromClient session
}

[<Fact>]
let ``The connection ends when its control loop stops while the reader waits for room in the inbox`` () : Task = task {
    let capacity = WebSocketConnectionLimits.IncomingMessageQueueCapacity
    let struct (gate, handler) = gatedPingHandler ()
    let session = startConnection (withPingHandler handler)
    use _ = session.Scope
    do! initialize session
    session.Socket.EnqueueText """{"type":"ping"}"""
    // Ends the control loop with 4429 once it gets to it, while the reader waits for room behind it
    session.Socket.EnqueueText """{"type":"connection_init"}"""
    for _ in 1 .. capacity * 2 do
        session.Socket.EnqueueText """{"type":"ping"}"""
    let readerWaiting = 1 + 1 + capacity + 1
    do!
        (fun () -> session.Socket.DeliveredMessages >= readerWaiting)
        |> waitUntil (fun () -> $"The server read only %d{session.Socket.DeliveredMessages} messages, fewer than the %d{readerWaiting} it can hold")
    gate.SetResult ()
    do! waitForTask timeout "The connection did not end after its control loop stopped while the reader was waiting for room in the inbox" session.Run
    do! session.Run
    session.Socket.ServerCloseStatus |> equals (ValueSome (enum<WebSocketCloseStatus> 4429))
}

/// A schema whose slow field resolves only once the test opens its gate
module private GatedTestSchema =

    open FSharp.Data.GraphQL
    open FSharp.Data.GraphQL.Types

    let slowGate = ref (TaskCompletionSource<string> TaskCreationOptions.RunContinuationsAsynchronously)

    let private slowValue () = async {
        let! value = Async.AwaitTask slowGate.Value.Task
        return Some value
    }

    let Query =
        Define.Object<Root> (
            name = "Query",
            fields = [
                Define.Field ("fast", StringType, (fun _ (_ : Root) -> "fast"))
                Define.AsyncField ("slow", Nullable StringType, (fun _ (_ : Root) -> slowValue ()))
            ]
        )

    let executor = Executor (Schema (Query) :> ISchema<Root>, [])

[<Fact>]
let ``The ids of subscriptions that ended while the inbox was full are free again`` () : Task = task {
    // The workers report their ends through a queue of their own, which the control loop reads before every client message:
    // reported through the full inbox, the ends would wait behind the messages of the client reusing the ids
    let count = 20
    let capacity = WebSocketConnectionLimits.IncomingMessageQueueCapacity
    let slow = TaskCompletionSource<string> TaskCreationOptions.RunContinuationsAsynchronously
    GatedTestSchema.slowGate.Value <- slow
    let struct (gate, handler) = gatedPingHandler ()
    let session = startConnectionWith GatedTestSchema.executor (withPingHandler handler)
    use _ = session.Scope
    do! initialize session
    for i in 1..count do
        subscribe session $"s{i}" "{ fast ... @defer { slow } }"
    for _ in 1..count do
        use! initial = receive session
        typeOf initial |> equals "next"
    // Blocks the control loop and fills its inbox
    for _ in 1 .. capacity + 2 do
        session.Socket.EnqueueText """{"type":"ping"}"""
    let readerWaiting = 1 + count + capacity + 2
    do!
        (fun () -> session.Socket.DeliveredMessages >= readerWaiting)
        |> waitUntil (fun () -> $"The server read only %d{session.Socket.DeliveredMessages} messages, fewer than the %d{readerWaiting} it can hold")
    // Every subscription ends while the control loop is blocked and its inbox is full
    slow.SetResult "slow"
    let mutable completed = 0
    while completed < count do
        use! message = receive session
        if typeOf message = "complete" then
            completed <- completed + 1
    gate.SetResult ()
    for _ in 1 .. capacity + 2 do
        use! pong = receive session
        typeOf pong |> equals "pong"
    // Every id the server completed can be used again
    for i in 1..count do
        subscribe session $"s{i}" "{ fast }"
    for i in 1..count do
        let! messages = receiveUntilTerminal session $"s{i}"
        messages |> List.map typeOf |> equals [ "next"; "complete" ]
    do! closeFromClient session
    session.Socket.ServerCloseStatus |> equals (ValueSome WebSocketCloseStatus.NormalClosure)
}

[<Fact>]
let ``A client that does not read what the server sends has its connection aborted`` () : Task = task {
    let session = start ()
    use _ = session.Scope
    do! initialize session
    session.Socket.BlockSends ()
    // Every ping is answered with a pong carrying its payload, so 20 pings of 1 MiB would make more than the limit wait
    for _ in 1..20 do
        session.Socket.EnqueueText (paddedMessage "ping" (1024 * 1024))
    do! session.Run.WaitAsync timeout
    session.Socket.State |> equals WebSocketState.Aborted
}

/// The options the application of these tests runs with
let private defaultOptions () =
    let services = ServiceCollection ()
    services.AddLogging () |> ignore
    services.AddGraphQL<Root> (TestSchema.executor, (fun _ -> { RequestId = "test" })) |> ignore
    let provider = services.BuildServiceProvider ()
    struct (provider, provider.GetRequiredService<IOptions<GraphQLOptions<Root>>>().Value)

[<Fact>]
let ``The sender queue accepts a message over its limit only while nothing else waits`` () =
    let struct (provider, options) = defaultOptions ()
    use _ = provider
    let overflows = ref 0
    // The JSON of a pong is longer than the limit of 10 bytes
    let queue = SenderQueue (options.SerializerOptions, 10L, (fun () -> overflows.Value <- overflows.Value + 1), NullLogger.Instance)
    Assert.True (queue.TryWrite (Send (ServerPong ValueNone)), "Expected a message to be accepted while nothing else waits")
    Assert.False (queue.TryWrite (Send (ServerPong ValueNone)), "Expected a message to be refused while another one waits over the limit")
    overflows.Value |> equals 1
    match queue.Reader.TryRead () with
    | true, QueuedSend json -> queue.Release json
    | _ -> fail "Expected the accepted message to be queued for the sender"
    Assert.True (queue.TryWrite (Send (ServerPong ValueNone)), "Expected a message to be accepted once the sender took the waiting one")

[<Fact>]
let ``A maximum message size that is not positive fails when the middleware is created`` () =
    let struct (provider, options) = defaultOptions ()
    use _ = provider
    let invalid = Options.Create { options with WebsocketOptions = { options.WebsocketOptions with MaxReceiveMessageSize = 0 } }
    let lifetime =
        { new Microsoft.Extensions.Hosting.IHostApplicationLifetime with
            member _.ApplicationStarted = CancellationToken.None
            member _.ApplicationStopping = CancellationToken.None
            member _.ApplicationStopped = CancellationToken.None
            member _.StopApplication () = () }
    let create () =
        GraphQLWebSocketMiddleware<Root> (
            RequestDelegate (fun _ -> Task.CompletedTask),
            lifetime,
            provider,
            NullLogger<GraphQLWebSocketMiddleware<Root>>.Instance,
            invalid
        )
        |> ignore
    let error = Assert.Throws<ArgumentOutOfRangeException> (fun () -> create ())
    Assert.Contains ("MaxReceiveMessageSize", error.Message, StringComparison.Ordinal)
