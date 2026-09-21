module FSharp.Data.GraphQL.IntegrationTests.WebSocketTests

open System
open System.Collections.Generic
open System.IO
open System.Net.WebSockets
open System.Text
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open Xunit
open Helpers

// Drives the graphql-transport-ws middleware of the Star Wars host through a real WebSocket: connection handshake,
// subscribe, the sequence of next messages of an incremental delivery, complete, error, and client-side complete.

let private receiveTimeout = TimeSpan.FromSeconds 30.0

/// Receives one whole message and parses it as JSON; fails if the server closes the socket instead
let private receive (socket : WebSocket) = task {
    use cancellation = new CancellationTokenSource (receiveTimeout)
    let buffer = Array.zeroCreate<byte> 4096
    use message = new MemoryStream ()
    let mutable endOfMessage = false
    while not endOfMessage do
        let! result = socket.ReceiveAsync (ArraySegment buffer, cancellation.Token)
        if result.MessageType = WebSocketMessageType.Close then
            failwith $"The server closed the socket: {result.CloseStatus} {result.CloseStatusDescription}"
        message.Write (buffer, 0, result.Count)
        endOfMessage <- result.EndOfMessage
    return JsonDocument.Parse (message.ToArray ())
}

let private send (socket : WebSocket) (json : string) : Task =
    socket.SendAsync (ArraySegment (Encoding.UTF8.GetBytes json), WebSocketMessageType.Text, true, CancellationToken.None)

let private typeOf (message : JsonDocument) = message.RootElement.GetProperty("type").GetString ()
let private idOf (message : JsonDocument) = message.RootElement.GetProperty("id").GetString ()
let private payloadOf (message : JsonDocument) = message.RootElement.GetProperty "payload"

let private hasProperty (name : string) (element : JsonElement) =
    let mutable ignored = Unchecked.defaultof<JsonElement>
    element.TryGetProperty (name, &ignored)

let private entries (name : string) (payload : JsonElement) =
    if hasProperty name payload then
        payload.GetProperty(name).EnumerateArray () |> Seq.toList
    else
        []

/// Opens a connection to the Star Wars host and completes the connection_init handshake
let private connect () = task {
    let client = TestHosts.createStarWarsWebSocketClient ()
    let! socket = client.ConnectAsync (Uri "ws://localhost/ws", CancellationToken.None)
    do! send socket """{"type":"connection_init"}"""
    use! ack = receive socket
    typeOf ack |> equals "connection_ack"
    return socket
}

let private subscribe (socket : WebSocket) (id : string) (query : string) =
    JsonSerializer.Serialize {| id = id; ``type`` = "subscribe"; payload = {| query = query |} |}
    |> send socket

let private close (socket : WebSocket) = task {
    use cancellation = new CancellationTokenSource (receiveTimeout)
    do! socket.CloseAsync (WebSocketCloseStatus.NormalClosure, "done", cancellation.Token)
}

/// Receives every message of the subscription until its terminal complete or error message
let private receiveUntilTerminal (socket : WebSocket) (id : string) = task {
    let received = ResizeArray<JsonDocument> ()
    let mutable terminal = false
    while not terminal do
        let! message = receive socket
        idOf message |> equals id
        received.Add message
        match typeOf message with
        | "complete"
        | "error" -> terminal <- true
        | _ -> ()
    return List.ofSeq received
}

/// The invariants every incremental delivery must satisfy on the wire: the first payload carries data with
/// hasNext true, only the last one has hasNext false and carries no data, every id is announced once and completed
/// once, and no entry refers to an id before its announcement or after its completion
let private assertWellFormed (payloads : JsonElement list) =
    match payloads with
    | [] -> failwith "Expected at least the initial payload"
    | initial :: _ ->
        Assert.True (hasProperty "data" initial, "The initial payload must carry data")
        Assert.True (initial.GetProperty("hasNext").GetBoolean (), "The initial payload must have hasNext true")
        let final = List.last payloads
        Assert.False (final.GetProperty("hasNext").GetBoolean (), "The last payload must have hasNext false")
        Assert.False (hasProperty "data" final, "The last payload must not carry data")
        let announced = HashSet<string> ()
        let completed = HashSet<string> ()
        for payload in payloads do
            for pending in entries "pending" payload do
                let id = pending.GetProperty("id").GetString ()
                Assert.True (announced.Add id, $"Pending id '{id}' was announced twice")
            for entry in entries "incremental" payload do
                let id = entry.GetProperty("id").GetString ()
                Assert.True (announced.Contains id, $"An incremental entry refers to id '{id}' before its announcement")
                Assert.False (completed.Contains id, $"An incremental entry refers to id '{id}' after its completion")
            for entry in entries "completed" payload do
                let id = entry.GetProperty("id").GetString ()
                Assert.True (announced.Contains id, $"A completed entry refers to id '{id}' before its announcement")
                Assert.True (completed.Add id, $"Id '{id}' was completed twice")
        for id in announced do
            Assert.True (completed.Contains id, $"Announced id '{id}' was never completed")

/// Asserts that the subscription ended with complete, reporting the server's error payload otherwise
let private expectComplete (messages : JsonDocument list) =
    let last = List.last messages
    match typeOf last with
    | "complete" -> ()
    | "error" -> failwith $"The subscription ended with an error: {(payloadOf last).GetRawText ()}"
    | other -> failwith $"The subscription ended with an unexpected '{other}' message"

let private pathOf (pending : JsonElement) =
    pending.GetProperty("path").EnumerateArray ()
    |> Seq.map (fun segment -> segment.ToString ())
    |> String.concat "."

[<Fact>]
let ``Query with defer and stream over a WebSocket delivers well-formed incremental payloads then complete`` () : Task = task {
    let! socket = connect ()
    do!
        subscribe
            socket
            "1"
            """{ hero(id: "1000") { name homePlanet @defer friendsStream @stream { ... on Human { name } ... on Droid { name } } } }"""
    let! messages = receiveUntilTerminal socket "1"
    expectComplete messages
    let payloads =
        messages
        |> List.filter (fun message -> typeOf message = "next")
        |> List.map payloadOf
    assertWellFormed payloads
    let hero = (List.head payloads).GetProperty("data").GetProperty "hero"
    hero.GetProperty("name").GetString () |> equals "Luke Skywalker"
    hero.GetProperty("homePlanet").ValueKind |> equals JsonValueKind.Null
    hero.GetProperty("friendsStream").GetArrayLength () |> equals 0
    // The stream is announced with the initial data that exposes its empty list
    (List.head payloads |> entries "pending" |> List.map pathOf) |> equals [ "hero.friendsStream" ]
    let incremental = payloads |> List.collect (entries "incremental")
    let streamedNames =
        incremental
        |> List.collect (entries "items")
        |> List.map (fun item -> item.GetProperty("name").GetString ())
        |> List.sort
    streamedNames |> equals [ "C-3PO"; "Han Solo"; "Leia Organa"; "R2-D2" ]
    let deferred = incremental |> List.filter (hasProperty "data") |> List.exactlyOne
    Assert.Contains ("Tatooine", deferred.GetProperty("data").GetRawText ())
    do! close socket
}

[<Fact>]
let ``Client complete frees the subscription id for a new subscription`` () : Task = task {
    let! socket = connect ()
    do! subscribe socket "2" """subscription { watchMoon(id: "1") { id isMoon } }"""
    do! send socket """{"id":"2","type":"complete"}"""
    do! subscribe socket "2" """{ hero(id: "1000") { name } }"""
    let! messages = receiveUntilTerminal socket "2"
    expectComplete messages
    messages |> List.map typeOf |> equals [ "next"; "complete" ]
    (payloadOf messages.Head).GetProperty("data").GetProperty("hero").GetProperty("name").GetString ()
    |> equals "Luke Skywalker"
    socket.State |> equals WebSocketState.Open
    do! close socket
}

[<Fact>]
let ``A request error is sent as an error message`` () : Task = task {
    let! socket = connect ()
    do! subscribe socket "3" """{ hero(id: "1000") { nope } }"""
    let! messages = receiveUntilTerminal socket "3"
    let error = List.exactlyOne messages
    typeOf error |> equals "error"
    let payload = payloadOf error
    payload.ValueKind |> equals JsonValueKind.Array
    Assert.Contains ("nope", payload[0].GetProperty("message").GetString ())
    do! close socket
}
