module FSharp.Data.GraphQL.Tests.AspNetCore.WebSocketConnectionTests

open System
open System.Collections.Concurrent
open System.Net.WebSockets
open System.Text
open System.Text.Json
open System.Threading
open System.Threading.Channels
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Logging.Abstractions
open Microsoft.Extensions.Options
open Xunit

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Server.AspNetCore
open FSharp.Data.GraphQL.Shared.WebSockets

// Drives GraphQLWebSocketConnection over a fake socket: the protocol handshake, close codes, queries, request
// errors, subscriptions, client-side completion and incremental delivery, without a hosted server.

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

    member _.EnqueueText (json : string) = incoming.Writer.TryWrite (ValueSome (Encoding.UTF8.GetBytes json)) |> ignore
    member _.EnqueueClose () = incoming.Writer.TryWrite ValueNone |> ignore
    member _.Sent = sent.Reader
    member _.ServerCloseStatus : WebSocketCloseStatus voption = serverCloseStatus

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

    override _.SendAsync (buffer : ArraySegment<byte>, _ : WebSocketMessageType, _ : bool, _ : CancellationToken) : Task =
        sent.Writer.TryWrite (Encoding.UTF8.GetString (buffer.Array, buffer.Offset, buffer.Count)) |> ignore
        Task.CompletedTask

type private Session = {
    Socket : FakeWebSocket
    Run : Task
    Scope : IDisposable
}

/// A logger that keeps the level and the message of every entry, so that a test can count them
type private RecordingLogger () =
    let entries = ConcurrentQueue<struct (LogLevel * string)> ()

    member _.Entries = List.ofSeq entries

    interface ILogger with
        member _.BeginScope<'TState> (_ : 'TState) : IDisposable = Unchecked.defaultof<IDisposable>
        member _.IsEnabled (_ : LogLevel) = true
        member _.Log<'TState> (level : LogLevel, _ : EventId, state : 'TState, ex : exn | null, formatter : Func<'TState, exn | null, string>) =
            entries.Enqueue (struct (level, formatter.Invoke (state, ex)))

let private startConnectionLogging (logger : ILogger) (configure : GraphQLOptions<Root> -> GraphQLOptions<Root>) =
    let services = ServiceCollection ()
    services.AddLogging () |> ignore
    services.AddGraphQL<Root> (TestSchema.executor, (fun _ -> { RequestId = "test" })) |> ignore
    let scope = services.BuildServiceProvider().CreateScope()
    let serviceProvider = scope.ServiceProvider
    let httpContext = DefaultHttpContext (RequestServices = serviceProvider)
    serviceProvider.GetRequiredService<IHttpContextAccessor>().HttpContext <- httpContext
    let options = serviceProvider.GetRequiredService<IOptions<GraphQLOptions<Root>>>().Value |> configure
    let socket = new FakeWebSocket ()
    let connection = GraphQLWebSocketConnection<Root> (httpContext, socket, options, serviceProvider, logger, CancellationToken.None)
    { Socket = socket; Run = connection.RunAsync (); Scope = scope }

let private startConnection (configure : GraphQLOptions<Root> -> GraphQLOptions<Root>) = startConnectionLogging NullLogger.Instance configure

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

let private errorsOf (payload : JsonElement) = payload.GetProperty("errors").EnumerateArray () |> List.ofSeq

let private pathOf (error : JsonElement) =
    error.GetProperty("path").EnumerateArray ()
    |> Seq.map _.GetString()
    |> List.ofSeq

let private messageOf (error : JsonElement) = error.GetProperty("message").GetString ()

[<Fact>]
let ``An unexpected exception of a resolver reaches a WebSocket client as a generic error at its path`` () : Task = task {
    let session = start ()
    use _ = session.Scope
    do! initialize session
    subscribe session "1" """{ hero(id: "1000") { name unexpectedFailure } }"""
    let! messages = receiveUntilTerminal session "1"
    messages |> List.map typeOf |> equals [ "next"; "complete" ]
    let next = List.head messages
    Assert.DoesNotContain (TestSchema.SecretDetail, next.RootElement.GetRawText ())
    let error = errorsOf (payloadOf next) |> single
    messageOf error |> equals ErrorMasking.UnexpectedErrorMessage
    pathOf error |> equals [ "hero"; "unexpectedFailure" ]
    do! closeFromClient session
}

[<Fact>]
let ``An unexpected exception of a resolver keeps its message over WebSocket when masking is disabled`` () : Task = task {
    let session = startConnection (fun options -> { options with MaskUnexpectedErrors = false })
    use _ = session.Scope
    do! initialize session
    subscribe session "1" """{ hero(id: "1000") { name unexpectedFailure } }"""
    let! messages = receiveUntilTerminal session "1"
    let error = errorsOf (payloadOf (List.head messages)) |> single
    messageOf error |> equals TestSchema.SecretDetail
    pathOf error |> equals [ "hero"; "unexpectedFailure" ]
    do! closeFromClient session
}

[<Fact>]
let ``An unexpected exception of a deferred field reaches a WebSocket client as a generic error at its path in the incremental payload`` () : Task = task {
    let session = start ()
    use _ = session.Scope
    do! initialize session
    // The field is resolved inside the deferred object, so its failure is delivered with the deferred payload; a
    // deferred field's own resolver runs before the field is deferred, putting its failure in the initial payload
    subscribe session "1" """{ hero(id: "1000") @defer { name unexpectedFailure } }"""
    let! messages = receiveUntilTerminal session "1"
    let delivered =
        messages
        |> List.filter (fun message -> typeOf message = "next")
        |> List.map payloadOf
        |> List.tryFind (hasProperty "incremental")
        |> Option.defaultWith (fun () ->
            let received = messages |> List.map _.RootElement.GetRawText() |> String.concat "\n"
            failwith $"Expected a payload with an incremental entry, but received:\n{received}")
    Assert.DoesNotContain (TestSchema.SecretDetail, delivered.GetRawText ())
    let error = errorsOf (delivered.GetProperty("incremental")[0]) |> single
    messageOf error |> equals ErrorMasking.UnexpectedErrorMessage
    pathOf error |> equals [ "hero"; "unexpectedFailure" ]
    do! closeFromClient session
}

[<Fact>]
let ``A GraphQL error a resolver raises on purpose keeps its message over WebSocket`` () : Task = task {
    let session = start ()
    use _ = session.Scope
    do! initialize session
    subscribe session "1" """{ hero(id: "1000") { name deliberateFailure } }"""
    let! messages = receiveUntilTerminal session "1"
    let error = errorsOf (payloadOf (List.head messages)) |> single
    messageOf error |> equals TestSchema.DeliberateFailureMessage
    pathOf error |> equals [ "hero"; "deliberateFailure" ]
    do! closeFromClient session
}

[<Fact>]
let ``A request error caused by an unexpected exception reaches a WebSocket client as a generic error`` () : Task = task {
    let session = startConnection (fun options -> { options with SchemaExecutor = TestSchema.requestFailureExecutor })
    use _ = session.Scope
    do! initialize session
    subscribe session "1" """{ hero(id: "1000") { name } }"""
    let! messages = receiveUntilTerminal session "1"
    let error = List.exactlyOne messages
    typeOf error |> equals "error"
    Assert.DoesNotContain (TestSchema.SecretDetail, error.RootElement.GetRawText ())
    (payloadOf error).EnumerateArray () |> single |> messageOf |> equals ErrorMasking.UnexpectedErrorMessage
    do! closeFromClient session
}

[<Fact>]
let ``The unexpected exceptions masked in one message are logged once at the error level`` () : Task = task {
    // A client controls how many errors a message holds, so an error entry per masked error would let it flood the log
    let logger = RecordingLogger ()
    let session = startConnectionLogging logger id
    use _ = session.Scope
    do! initialize session
    let failures = String.Join (" ", Seq.init 20 (fun i -> $"f%d{i}: unexpectedFailure"))
    subscribe session "1" $"""{{ hero(id: "1000") {{ %s{failures} }} }}"""
    let! messages = receiveUntilTerminal session "1"
    errorsOf (payloadOf (List.head messages)) |> List.length |> equals 20
    let entriesAbout (level : LogLevel) =
        logger.Entries
        |> List.filter (fun struct (entryLevel, message) -> entryLevel = level && message.Contains ("Masked", StringComparison.Ordinal))
    let struct (_, errorEntry) = entriesAbout LogLevel.Error |> single
    Assert.Contains ("Masked 20 unexpected error(s)", errorEntry, StringComparison.Ordinal)
    entriesAbout LogLevel.Debug |> List.length |> equals 20
    do! closeFromClient session
}

[<Fact>]
let ``A syntax error of a query reaches a WebSocket client with the message of the parser`` () : Task = task {
    let query = """{ hero(id: "1000") { name """
    let expected =
        match FSharp.Data.GraphQL.Parser.tryParse query with
        | Error message -> message
        | Ok _ -> failwith "The query of the test must not parse"
    let session = start ()
    use _ = session.Scope
    do! initialize session
    subscribe session "1" query
    let! messages = receiveUntilTerminal session "1"
    let error = List.exactlyOne messages
    typeOf error |> equals "error"
    (payloadOf error).EnumerateArray () |> single |> messageOf |> equals expected
    do! closeFromClient session
}

[<Fact>]
let ``A GraphQL error of an exception declaring another message for clients reaches a WebSocket client with that message`` () : Task = task {
    let session = start ()
    use _ = session.Scope
    do! initialize session
    subscribe session "1" """{ hero(id: "1000") { name divergentFailure } }"""
    let! messages = receiveUntilTerminal session "1"
    let next = List.head messages
    Assert.DoesNotContain (TestSchema.SecretDetail, next.RootElement.GetRawText ())
    let error = errorsOf (payloadOf next) |> single
    messageOf error |> equals TestSchema.DivergentClientMessage
    pathOf error |> equals [ "hero"; "divergentFailure" ]
    do! closeFromClient session
}

[<Fact>]
let ``A GraphQL error thrown while an operation starts keeps its message over WebSocket`` () : Task = task {
    let session =
        startConnection (fun options -> { options with RootFactory = fun _ -> raise (GQLMessageException "The root is closed") })
    use _ = session.Scope
    do! initialize session
    subscribe session "1" """{ hero(id: "1000") { name } }"""
    let! messages = receiveUntilTerminal session "1"
    let error = List.exactlyOne messages
    typeOf error |> equals "error"
    (payloadOf error).EnumerateArray () |> single |> messageOf |> equals "The root is closed"
    do! closeFromClient session
}

[<Fact>]
let ``An unexpected exception completing a deferred fragment reaches a WebSocket client as a generic error`` () : Task = task {
    let session = start ()
    use _ = session.Scope
    do! initialize session
    // The field is non-null, so its failure propagates up to the fragment, which completes with the error and no data
    subscribe session "1" """{ hero(id: "1000") { name ... @defer { unexpectedNonNullFailure } } }"""
    let! messages = receiveUntilTerminal session "1"
    let received = messages |> List.map _.RootElement.GetRawText() |> String.concat "\n"
    Assert.DoesNotContain (TestSchema.SecretDetail, received)
    let completedErrors =
        messages
        |> List.filter (fun message -> typeOf message = "next")
        |> List.map payloadOf
        |> List.filter (hasProperty "completed")
        |> List.collect (fun payload -> payload.GetProperty("completed").EnumerateArray () |> List.ofSeq)
        |> List.filter (hasProperty "errors")
        |> List.collect errorsOf
    match completedErrors with
    | [ error ] -> messageOf error |> equals ErrorMasking.UnexpectedErrorMessage
    | _ -> fail $"Expected a single error in the completed entries, but received:\n{received}"
    do! closeFromClient session
}
