namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open System.Net.WebSockets
open System.Text.Json
open System.Text.Json.Serialization
open System.Threading
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open FSharp.Control.Reactive

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Execution

type GraphQLWebSocket(innerSocket : WebSocket) =
    inherit WebSocket()

    let subscriptions = ConcurrentDictionary<string, IDisposable>() :> IDictionary<string, IDisposable>
    let id = System.Guid.NewGuid()

    override _.CloseStatus = innerSocket.CloseStatus

    override _.CloseStatusDescription = innerSocket.CloseStatusDescription

    override _.State = innerSocket.State

    override _.SubProtocol = innerSocket.SubProtocol

    override _.CloseAsync(status, description, ct) = innerSocket.CloseAsync(status, description, ct)

    override _.CloseOutputAsync(status, description, ct) = innerSocket.CloseOutputAsync(status, description, ct)

    override this.Dispose() =
        this.UnsubscribeAll()
        innerSocket.Dispose()

    override _.ReceiveAsync(buffer : ArraySegment<byte>, ct) = innerSocket.ReceiveAsync(buffer, ct)

    override _.SendAsync(buffer : ArraySegment<byte>, msgType, endOfMsg, ct) = innerSocket.SendAsync(buffer, msgType, endOfMsg, ct)

    override _.Abort() = innerSocket.Abort()

    member _.Subscribe(id : string, unsubscriber : IDisposable) =
        subscriptions.Add(id, unsubscriber)

    member _.Unsubscribe(id : string) =
        match subscriptions.ContainsKey(id) with
        | true ->
            subscriptions.[id].Dispose()
            subscriptions.Remove(id) |> ignore
        | false -> ()

    member _.UnsubscribeAll() =
        subscriptions
        |> Seq.iter (fun x -> x.Value.Dispose())
        subscriptions.Clear()

    member _.Id = id

module SocketManager =
    let private sockets = ConcurrentDictionary<Guid, GraphQLWebSocket>() :> IDictionary<Guid, GraphQLWebSocket>

    let private disposeSocket (socket : GraphQLWebSocket) =
        sockets.Remove(socket.Id) |> ignore
        socket.Dispose()

    let private sendMessage (socket : GraphQLWebSocket) cancellationToken (message : WebSocketServerMessage) = task {
        let options =
            WebSocketServerMessageConverter() :> JsonConverter
            |> Seq.singleton
            |> Json.getSerializerOptions
        use ms = new MemoryStream()
        do! JsonSerializer.SerializeAsync(ms, message, options, cancellationToken)
        ms.Seek(0L, SeekOrigin.Begin) |> ignore
        let segment = new ArraySegment<byte>(ms.ToArray())
        if socket.State = WebSocketState.Open then
            do! socket.SendAsync(segment, WebSocketMessageType.Text, true, cancellationToken)
        else
            disposeSocket socket
    }

    let private receiveMessage (executor : Executor<'Root>) (replacements : Map<string, obj>) cancellationToken (socket : WebSocket) = task {
        use ms = new MemoryStream(4096)
        let segment = ArraySegment<byte>(ms.ToArray())
        let! result = socket.ReceiveAsync(segment, cancellationToken)
        if result.Count = 0
        then
            return ValueNone
        else
            let options =
                WebSocketClientMessageConverter(executor, replacements) :> JsonConverter
                |> Seq.singleton
                |> Json.getSerializerOptions
            return JsonSerializer.Deserialize<WebSocketClientMessage>(ms, options) |> ValueSome
    }

    let private handleMessages (executor : Executor<'Root>) (root : unit -> 'Root) cancellationToken (socket : GraphQLWebSocket) = task {

        let send id output = Data (id, output) |> sendMessage socket cancellationToken

        let sendMessage = sendMessage socket cancellationToken

        let sendDelayed message = task {
            do! Task.Delay 5000
            do! sendMessage message
        }

        let handleGQLResponseContent id =
            function
            | Stream output -> task {
                    let unsubscriber = output |> Observable.subscribe (fun o -> sendMessage (WebSocketServerMessage.OfResponseContent(id, o)) |> Task.WaitAll)
                    socket.Subscribe(id, unsubscriber)
                }
            | Deferred (data, _, output) -> task {
                    do! send id data
                    let unsubscriber = output |> Observable.subscribe (fun o -> sendDelayed (WebSocketServerMessage.OfResponseContent(id, o)) |> Task.WaitAll)
                    socket.Subscribe(id, unsubscriber)
                }

            | RequestError errs ->
                task { Task.Delay 1 |> ignore }    // TODO GBirkel: Placeholder to make build succeed. Replace!

            | Direct (data, _) ->
                send id data
        try
            let mutable loop = true
            while loop do
                let! message = socket |> receiveMessage executor Map.empty cancellationToken
                match message with
                | ValueSome ConnectionInit ->
                    do! sendMessage ConnectionAck
                | ValueSome (Start (id, payload)) ->
                    let! result = executor.AsyncExecute(payload.ExecutionPlan, root(), payload.Variables)
                    do! handleGQLResponseContent id result
                    do! Data (id, Dictionary<string, obj>()) |> sendMessage
                | ValueSome ConnectionTerminate ->
                    do! socket.CloseAsync(WebSocketCloseStatus.NormalClosure, "", CancellationToken.None) |> Async.AwaitTask
                    disposeSocket socket
                    loop <- false
                | ValueSome (ParseError (id, _)) ->
                    do! Error (id, "Invalid message type!") |> sendMessage
                | ValueSome (Stop id) ->
                    socket.Unsubscribe(id)
                    do! Complete id |> sendMessage
                | ValueNone -> ()
        with
        | _ -> disposeSocket socket
    }

    let startSocket (socket : GraphQLWebSocket) (executor : Executor<'Root>) (root : unit -> 'Root) cancellationToken =
        sockets.Add(socket.Id, socket)
        handleMessages executor root cancellationToken socket

type GraphQLWebSocketMiddleware<'Root>(next : RequestDelegate, executor : Executor<'Root>, root : unit -> 'Root) =
    member _.Invoke(ctx : HttpContext) =
        task {
            match ctx.WebSockets.IsWebSocketRequest with
            | true ->
                let! socket = ctx.WebSockets.AcceptWebSocketAsync("graphql-ws")
                use socket = new GraphQLWebSocket(socket)
                do! SocketManager.startSocket socket executor root CancellationToken.None //ctx.RequestAborted
            | false ->
                next.Invoke(ctx) |> ignore
        }
