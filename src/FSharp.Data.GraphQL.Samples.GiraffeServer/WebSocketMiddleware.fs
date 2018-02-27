namespace FSharp.Data.GraphQL.Samples.GiraffeServer

open System
open System.Text
open System.Threading
open System.Threading.Tasks
open System.Net.WebSockets
open Microsoft.AspNetCore.Http
open FSharp.Control.Tasks.ContextInsensitive

module WebSockets =
    let mutable sockets = list<WebSocket>.Empty

    let private addSocket sockets socket = socket :: sockets

    let private removeSocket sockets socket =
        sockets |> List.choose (fun s -> if s <> socket then Some s else None)

    let private sendMessage (socket : WebSocket) (message : string) = task {
            let buffer = Encoding.UTF8.GetBytes(message)
            let segment = new ArraySegment<byte>(buffer)
            if socket.State = WebSocketState.Open then
                do! socket.SendAsync(segment, WebSocketMessageType.Text, true, CancellationToken.None)
            else
                sockets <- removeSocket sockets socket
    }


    let private receiveMessages (socket : WebSocket) = async {
        let buffer = Array.zeroCreate 4096
        let segment = ArraySegment<byte>(buffer)
        let! ct = Async.CancellationToken
        try
            let mutable result = socket.ReceiveAsync(segment, ct).Result
            while not (result.CloseStatus.HasValue) do
                result <- socket.ReceiveAsync(segment, ct).Result
            socket.CloseAsync(result.CloseStatus.Value, result.CloseStatusDescription, ct) |> Async.AwaitTask |> ignore
            sockets <- removeSocket sockets socket
        with
        | _ -> sockets <- removeSocket sockets socket
    }

    let sendMessageToSockets message = task {
        for socket in sockets do
            try
                do! sendMessage socket message
            with
            | _ -> sockets <- removeSocket sockets socket
    }

    type Middleware(next : RequestDelegate) =
        member __.Invoke(ctx : HttpContext) =
            async {
                if ctx.Request.Path = PathString("/subscriptions") then
                    match ctx.WebSockets.IsWebSocketRequest with
                    | true ->
                        let! socket = ctx.WebSockets.AcceptWebSocketAsync() |> Async.AwaitTask
                        sockets <- addSocket sockets socket
                        do! receiveMessages socket
                    | false ->
                        ctx.Response.StatusCode <- 400
                else
                    next.Invoke(ctx) |> ignore
            } |> Async.StartAsTask :> Task