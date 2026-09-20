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
    override _.SubProtocol = "graphql-transport-ws"

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

let private startConnection (configure : GraphQLOptions<Root> -> GraphQLOptions<Root>) =
    let services = ServiceCollection ()
    services.AddLogging () |> ignore
    services.AddGraphQL<Root> (TestSchema.executor, (fun _ -> { RequestId = "test" })) |> ignore
    let scope = services.BuildServiceProvider().CreateScope()
    let serviceProvider = scope.ServiceProvider
    let httpContext = DefaultHttpContext (RequestServices = serviceProvider)
    serviceProvider.GetRequiredService<IHttpContextAccessor>().HttpContext <- httpContext
    let options = serviceProvider.GetRequiredService<IOptions<GraphQLOptions<Root>>>().Value |> configure
    let socket = new FakeWebSocket ()
    let connection = GraphQLWebSocketConnection<Root> (httpContext, socket, options, serviceProvider, NullLogger.Instance, CancellationToken.None)
    { Socket = socket; Run = connection.RunAsync (); Scope = scope }

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
