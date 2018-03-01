namespace FSharp.Data.GraphQL.Samples.GiraffeServer

open System
open System.Threading
open System.Threading.Tasks
open System.Net.WebSockets
open Microsoft.AspNetCore.Http
open FSharp.Control.Tasks.ContextInsensitive
open Newtonsoft.Json
open FSharp.Data.GraphQL.Execution
open System.Collections
open System.Collections.Generic

module WebSockets =
    let mutable sockets = list<WebSocket>.Empty

    let private addSocket sockets socket = socket :: sockets

    let private removeSocket sockets socket =
        sockets |> List.choose (fun s -> if s <> socket then Some s else None)

    let private sendMessage (socket : WebSocket) (message : SubscriptionSocketServerMessage) = async {
        let settings = jsonSerializerSettings [ SubscriptionSocketServerMessageConverter() :> JsonConverter ]
        let json = JsonConvert.SerializeObject(message, settings)
        let buffer = utf8Bytes json
        let segment = new ArraySegment<byte>(buffer)
        if socket.State = WebSocketState.Open then
            do! socket.SendAsync(segment, WebSocketMessageType.Text, true, CancellationToken.None) |> Async.AwaitTask
            printf "Socket message sent:\n%s\n" json
        else
            sockets <- removeSocket sockets socket
    }

    let private receiveMessage (socket : WebSocket) = async {
        let buffer = Array.zeroCreate 4096
        let segment = ArraySegment<byte>(buffer)
        do! socket.ReceiveAsync(segment, CancellationToken.None)
            |> Async.AwaitTask
            |> Async.Ignore
        let message = utf8String buffer
        if String.IsNullOrWhiteSpace(message)
        then
            return None
        else
            let settings =
                SubscriptionSocketClientMessageConverter(Schema.executor, Map.empty) :> JsonConverter
                |> Seq.singleton
                |> jsonSerializerSettings
            return JsonConvert.DeserializeObject<SubscriptionSocketClientMessage>(message, settings) |> Some
    }

    let broadcast msg = async {
        for socket in sockets do
            try
                do! sendMessage socket msg
            with
            | _ -> sockets <- removeSocket sockets socket
    }

    let private handleMessages (socket : WebSocket) = async {
        let handle id =
            function
            | Stream data ->
                data 
                |> Observable.add (fun d -> Data (id, d) |> broadcast |> Async.RunSynchronously)
            | _ -> ()
        try
            let mutable loop = true
            while loop do
                let! message = receiveMessage socket
                match message with
                | Some ConnectionInit ->
                    do! sendMessage socket ConnectionAck
                | Some (Start (id, payload)) ->
                    Schema.executor.AsyncExecute(payload.ExecutionPlan, { ClientId = "5" }, payload.Variables)
                    |> Async.RunSynchronously
                    |> handle id
                    do! Data (id, Dictionary<string,obj>()) |> sendMessage socket
                | Some ConnectionTerminate ->
                    do! socket.CloseAsync(WebSocketCloseStatus.NormalClosure, "", CancellationToken.None) |> Async.AwaitTask
                    sockets <- removeSocket sockets socket
                    loop <- false
                | Some (ParseError (id, _)) ->
                    do! Error (id, "Invalid message type!") |> sendMessage socket
                | Some (Stop id) ->
                    do! Complete id |> sendMessage socket
                | None -> ()
        with
        | _ -> sockets <- removeSocket sockets socket
    }

    type Middleware(next : RequestDelegate) =
        member __.Invoke(ctx : HttpContext) =
            async {
                if ctx.Request.Path = PathString("/subscriptions") then
                    match ctx.WebSockets.IsWebSocketRequest with
                    | true ->
                        let! socket = ctx.WebSockets.AcceptWebSocketAsync("graphql-ws") |> Async.AwaitTask
                        sockets <- addSocket sockets socket
                        do! handleMessages socket
                    | false ->
                        ctx.Response.StatusCode <- 400
                else
                    next.Invoke(ctx) |> ignore
            } |> Async.StartAsTask :> Task