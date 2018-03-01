namespace FSharp.Data.GraphQL.Samples.GiraffeServer

open System
open System.Threading
open System.Threading.Tasks
open System.Net.WebSockets
open Microsoft.AspNetCore.Http
open Newtonsoft.Json
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Execution
open System.Collections.Generic

type GraphQLWebSocket(innerSocket : WebSocket) =
    inherit WebSocket()
    let mutable subscriptions : Map<string, IDisposable> = Map<string, IDisposable> Seq.empty
    override __.CloseStatus = innerSocket.CloseStatus
    override __.CloseStatusDescription = innerSocket.CloseStatusDescription
    override __.State = innerSocket.State
    override __.SubProtocol = innerSocket.SubProtocol
    override __.CloseAsync(status, description, ct) = innerSocket.CloseAsync(status, description, ct)
    override __.CloseOutputAsync(status, description, ct) = innerSocket.CloseOutputAsync(status, description, ct)
    override this.Dispose() =
        this.UnsubscribeAll()
        innerSocket.Dispose()
    override __.ReceiveAsync(buffer, ct) = innerSocket.ReceiveAsync(buffer, ct)
    override __.SendAsync(buffer, msgType, endOfMsg, ct) = innerSocket.SendAsync(buffer, msgType, endOfMsg, ct)
    override __.Abort() = innerSocket.Abort()
    member __.Subscribe(id, unsubscriber) =
        subscriptions <- subscriptions.Add (id, unsubscriber)
    member __.Unsubscribe(id) =
        match subscriptions.TryFind id with
        | Some unsubscriber ->
            unsubscriber.Dispose()
            subscriptions <- subscriptions.Remove id
        | None -> ()
    member __.UnsubscribeAll() =
        subscriptions
        |> Map.toSeq
        |> Seq.iter (fun (_, unsubscriber) -> unsubscriber.Dispose())
        subscriptions <- Map<string, IDisposable> Seq.empty

module SocketManager =
    let mutable private sockets = list<GraphQLWebSocket>.Empty

    let private disposeSocket socket =
        sockets <- sockets |> List.choose (fun s -> if s <> socket then Some s else None)
        socket.Dispose()

    let private sendMessage (socket : GraphQLWebSocket) (message : SubscriptionServerMessage) = async {
        let settings = 
            SubscriptionServerMessageConverter() :> JsonConverter
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
                SubscriptionClientMessageConverter(executor, replacements) :> JsonConverter
                |> Seq.singleton
                |> jsonSerializerSettings
            return JsonConvert.DeserializeObject<SubscriptionClientMessage>(message, settings) |> Some
    }

    let private handleMessages (executor : Executor<'Root>) (root : 'Root) (socket : GraphQLWebSocket) = async {
        let send id output =
            Data (id, output)
            |> sendMessage socket
            |> Async.RunSynchronously
        let handle id =
            function
            | Stream output -> 
                let unsubscriber = output |> Observable.subscribe (fun o -> send id o)
                socket.Subscribe(id, unsubscriber)
            | _ -> ()
        try
            let mutable loop = true
            while loop do
                let! message = socket |> receiveMessage executor Map.empty
                match message with
                | Some ConnectionInit ->
                    do! sendMessage socket ConnectionAck
                | Some (Start (id, payload)) ->
                    executor.AsyncExecute(payload.ExecutionPlan, root, payload.Variables)
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

    let startSocket socket (executor : Executor<'Root>) (root : 'Root) = 
        sockets <- socket :: sockets
        handleMessages executor root socket |> Async.RunSynchronously

type GraphQLSubscriptionsMiddleware<'Root>(next : RequestDelegate, executor : Executor<'Root>, root : 'Root) =
    member __.Invoke(ctx : HttpContext) =
        async {
            if ctx.Request.Path = PathString("/subscriptions") then
                match ctx.WebSockets.IsWebSocketRequest with
                | true ->
                    let! socket = ctx.WebSockets.AcceptWebSocketAsync("graphql-ws") |> Async.AwaitTask
                    use socket = new GraphQLWebSocket(socket)
                    SocketManager.startSocket socket executor root
                | false ->
                    ctx.Response.StatusCode <- 400
            else
                next.Invoke(ctx) |> ignore
        } |> Async.StartAsTask :> Task