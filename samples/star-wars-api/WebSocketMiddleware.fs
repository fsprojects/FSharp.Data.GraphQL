namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open System
open System.Threading
open System.Threading.Tasks
open System.Net.WebSockets
open Microsoft.AspNetCore.Http
open Newtonsoft.Json
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Execution
open System.Collections.Generic
open System.Collections.Concurrent

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

    let private sendMessage (socket : GraphQLWebSocket) (message : WebSocketServerMessage) = async {
        let settings =
            WebSocketServerMessageConverter() :> JsonConverter
            |> Seq.singleton
            |> jsonSerializerSettings
        let json = JsonConvert.SerializeObject(message, settings)
        let buffer = utf8Bytes json
        let segment = new ArraySegment<byte>(buffer)
        if socket.State = WebSocketState.Open then
            do! socket.SendAsync(segment, WebSocketMessageType.Text, true, CancellationToken.None) |> Async.AwaitTask
        else
            disposeSocket socket
    }

    let private receiveMessage (executor : Executor<'Root>) (replacements : Map<string, obj>) (socket : WebSocket) = async {
        let buffer = Array.zeroCreate 4096
        let segment = ArraySegment<byte>(buffer)
        do! socket.ReceiveAsync(segment, CancellationToken.None)
            |> Async.AwaitTask
            |> Async.Ignore
        let message = utf8String buffer
        if isNullOrWhiteSpace message
        then
            return None
        else
            let settings =
                WebSocketClientMessageConverter(executor, replacements) :> JsonConverter
                |> Seq.singleton
                |> jsonSerializerSettings
            return JsonConvert.DeserializeObject<WebSocketClientMessage>(message, settings) |> Some
    }

    let private handleMessages (executor : Executor<'Root>) (root : unit -> 'Root) (socket : GraphQLWebSocket) = async {
        let send id output =
            Data (id, output)
            |> sendMessage socket
            |> Async.RunSynchronously
        let sendDelayed id output =
            Thread.Sleep(5000)
            send id output
        let handle id =
            function
            | Stream output ->
                let unsubscriber = output |> Observable.subscribe (fun o -> send id o)
                socket.Subscribe(id, unsubscriber)
            | Deferred (data, _, output) ->
                send id data
                let unsubscriber = output |> Observable.subscribe (fun o -> sendDelayed id o)
                socket.Subscribe(id, unsubscriber)
            | Direct (data, _) ->
                send id data
        try
            let mutable loop = true
            while loop do
                let! message = socket |> receiveMessage executor Map.empty
                match message with
                | Some ConnectionInit ->
                    do! sendMessage socket ConnectionAck
                | Some (Start (id, payload)) ->
                    executor.AsyncExecute(payload.ExecutionPlan, root(), payload.Variables)
                    |> Async.RunSynchronously
                    |> handle id
                    do! Data (id, Dictionary<string, obj>()) |> sendMessage socket
                | Some ConnectionTerminate ->
                    do! socket.CloseAsync(WebSocketCloseStatus.NormalClosure, "", CancellationToken.None) |> Async.AwaitTask
                    disposeSocket socket
                    loop <- false
                | Some (ParseError (id, _)) ->
                    do! Error (id, "Invalid message type!") |> sendMessage socket
                | Some (Stop id) ->
                    socket.Unsubscribe(id)
                    do! Complete id |> sendMessage socket
                | None -> ()
        with
        | _ -> disposeSocket socket
    }

    let startSocket (socket : GraphQLWebSocket) (executor : Executor<'Root>) (root : unit -> 'Root) =
        sockets.Add(socket.Id, socket)
        handleMessages executor root socket |> Async.RunSynchronously

type GraphQLWebSocketMiddleware<'Root>(next : RequestDelegate, executor : Executor<'Root>, root : unit -> 'Root) =
    member _.Invoke(ctx : HttpContext) =
        async {
            match ctx.WebSockets.IsWebSocketRequest with
            | true ->
                let! socket = ctx.WebSockets.AcceptWebSocketAsync("graphql-ws") |> Async.AwaitTask
                use socket = new GraphQLWebSocket(socket)
                SocketManager.startSocket socket executor root
            | false ->
                next.Invoke(ctx) |> ignore
        } |> Async.StartAsTask :> Task
