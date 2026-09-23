module internal FSharp.Data.GraphQL.Server.Suave.GraphQLWebSocketHandler

open System
open System.Collections.Generic
open System.Text
open System.Text.Json
open System.Text.Json.Serialization

open Suave
open Suave.Http
open Suave.Logging
open Suave.Sockets
open Suave.WebSocket

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Shared
open FSharp.Data.GraphQL.Shared.WebSockets

/// The <c>graphql-transport-ws</c> subprotocol, as defined by
/// <see href="https://github.com/enisdenjo/graphql-ws/blob/master/PROTOCOL.md">the graphql-ws protocol specification</see>.
[<Literal>]
let private Subprotocol = "graphql-transport-ws"

let private serializeServerMessage (serializerOptions : JsonSerializerOptions) (serverMessage : ServerMessage) : string =
    let raw : RawServerMessage =
        match serverMessage with
        | ConnectionAck -> { Id = ValueNone; Type = "connection_ack"; Payload = ValueNone }
        | ServerPing -> { Id = ValueNone; Type = "ping"; Payload = ValueNone }
        | ServerPong p -> { Id = ValueNone; Type = "pong"; Payload = p |> ValueOption.map CustomResponse }
        | Next (id, payload) -> {
            Id = ValueSome id
            Type = "next"
            Payload = ValueSome (ExecutionResult payload)
          }
        | Complete id -> { Id = ValueSome id; Type = "complete"; Payload = ValueNone }
        | Error (id, errMessages) -> {
            Id = ValueSome id
            Type = "error"
            Payload = ValueSome (ErrorMessages errMessages)
          }

    JsonSerializer.Serialize (raw, serializerOptions)

let private invalidJsonInClientMessageError = InvalidMessage (4400, "Invalid json in client message")

let private deserializeClientMessage
    (serializerOptions : JsonSerializerOptions)
    (bytes : byte[])
    : Result<ClientMessage, ClientMessageProtocolFailure> =
    try
        Ok (JsonSerializer.Deserialize<ClientMessage>(bytes, serializerOptions))
    with
    | :? InvalidWebsocketMessageException as ex -> Result.Error (InvalidMessage (4400, ex.Message))
    | :? JsonException -> Result.Error invalidJsonInClientMessageError

let private sendMessage (serializerOptions : JsonSerializerOptions) (webSocket : WebSocket) (message : ServerMessage) : Async<unit> = async {
    let bytes =
        message
        |> serializeServerMessage serializerOptions
        |> Encoding.UTF8.GetBytes
    let! _ = webSocket.send Text (ArraySegment bytes) true
    ()
}

let private closeSocket (webSocket : WebSocket) (code : int) (reason : string) : Async<unit> = async {
    let codeBytes =
        let bytes = BitConverter.GetBytes (uint16 code)
        if BitConverter.IsLittleEndian then
            Array.rev bytes
        else
            bytes

    let payload = Array.append codeBytes (Encoding.UTF8.GetBytes reason)
    let! _ = webSocket.send Close (ArraySegment payload) true
    ()
}

let private closeSocketNormally (webSocket : WebSocket) = closeSocket webSocket CloseCode.CLOSE_NORMAL.code "Normal Closure"

/// Awaits the given computation, giving up (and returning <see langword="None"/>) after the given timeout has elapsed.
let private withTimeout (timeout : TimeSpan) (computation : Async<'T>) : Async<'T option> = async {
    let! child = Async.StartChild (computation, int timeout.TotalMilliseconds)

    try
        let! result = child
        return Some result
    with :? TimeoutException ->
        return None
}

[<Struct>]
type private RawFrame =
    | FrameData of byte[]
    | FrameClosed

let rec private readFullFrame (webSocket : WebSocket) (acc : byte[] list) : Async<RawFrame> = async {
    let! frame = webSocket.read ()

    match frame with
    | Choice1Of2 (Close, _, _) -> return FrameClosed
    | Choice1Of2 (Ping, _, _)
    | Choice1Of2 (Pong, _, _) -> return! readFullFrame webSocket acc
    | Choice1Of2 (_, data, fin) ->
        let acc = data :: acc
        if fin then
            return FrameData (acc |> List.rev |> Array.concat)
        else
            return! readFullFrame webSocket acc
    | Choice2Of2 _error -> return FrameClosed
}

type private ReceivedMessage =
    | ClientMsg of ClientMessage
    | ProtocolFailure of ClientMessageProtocolFailure
    | NoOp
    | SocketClosed

let private receiveMessage (serializerOptions : JsonSerializerOptions) (webSocket : WebSocket) : Async<ReceivedMessage> = async {
    let! frame = readFullFrame webSocket []

    match frame with
    | FrameClosed -> return SocketClosed
    | FrameData bytes when bytes.Length = 0 -> return NoOp
    | FrameData bytes ->
        match deserializeClientMessage serializerOptions bytes with
        | Ok msg -> return ClientMsg msg
        | Result.Error failure -> return ProtocolFailure failure
}

/// Waits for a <c>connection_init</c> message and acknowledges it, per the <c>graphql-transport-ws</c> protocol.
let private waitForConnectionInitAndRespondToClient (options : GraphQLOptions<'Root>) (webSocket : WebSocket) : Async<Result<unit, string>> = async {
    let! received =
        withTimeout options.WebsocketOptions.ConnectionInitTimeout (receiveMessage options.SerializerOptions webSocket)

    match received with
    | None ->
        do! closeSocket webSocket CustomWebSocketStatus.ConnectionTimeout "Connection initialization timeout"
        return Result.Error $"{nameof ConnectionInit} timeout"
    | Some (ClientMsg (ConnectionInit _)) ->
        do! sendMessage options.SerializerOptions webSocket ConnectionAck
        return Ok ()
    | Some (ClientMsg (Subscribe _)) ->
        do! closeSocket webSocket CustomWebSocketStatus.Unauthorized "Unauthorized"
        return Result.Error "Unauthorized"
    | Some (ProtocolFailure (InvalidMessage (code, explanation))) ->
        do! closeSocket webSocket code explanation
        return Result.Error explanation
    | Some _ ->
        do! closeSocketNormally webSocket
        return Result.Error $"{nameof ConnectionInit} failed (not because of timeout)"
}

/// Runs the <c>graphql-transport-ws</c> message loop for an already-initialized connection.
let private handleMessages (options : GraphQLOptions<'Root>) (ctx : HttpContext) (webSocket : WebSocket) : Async<unit> =
    let serializerOptions = options.SerializerOptions
    let subscriptions = Dictionary<SubscriptionId, SubscriptionUnsubscriber * OnUnsubscribeAction>()
    let sendMsg msg = sendMessage serializerOptions webSocket msg

    let sendOutput id (output : SubscriptionExecutionResult) = sendMsg (Next (id, output))

    let sendSubscriptionResponseOutput id subscriptionResult =
        match subscriptionResult with
        | SubscriptionResult output -> sendOutput id { Data = ValueSome output; Errors = [] }
        | SubscriptionErrors (_, errors) -> sendOutput id { Data = ValueNone; Errors = errors }

    let sendDeferredResponseOutput id deferredResult =
        match deferredResult with
        | DeferredResult (data, _path) -> sendOutput id { Data = ValueSome (data :?> Output); Errors = [] }
        | DeferredErrors (_, errors, _path) -> sendOutput id { Data = ValueNone; Errors = errors }

    let sendDeferredResultDelayedBy (ms : int) id deferredResult = async {
        do! Async.Sleep ms
        do! sendDeferredResponseOutput id deferredResult
    }

    let addClientSubscription
        (id : SubscriptionId)
        (howToSendDataOnNext : SubscriptionId -> 'ResponseContent -> Async<unit>)
        (streamSource : IObservable<'ResponseContent>)
        =
        let observer = {
            new IObserver<'ResponseContent> with
                member _.OnNext value = howToSendDataOnNext id value |> Async.RunSynchronously
                member _.OnError ex =
                    ctx.runtime.logger.info (Suave.Logging.Message.eventX $"Error on subscription with Id = '{id}': {ex}")
                member _.OnCompleted () =
                    sendMsg (Complete id) |> Async.RunSynchronously
                    subscriptions
                    |> GraphQLSubscriptionsManagement.removeSubscription id
        }

        let unsubscriber = streamSource.Subscribe observer
        subscriptions
        |> GraphQLSubscriptionsManagement.addSubscription (id, unsubscriber, ignore)

    let applyPlanExecutionResult (id : SubscriptionId) (executionResult : GQLExecutionResult) = async {
        match executionResult with
        | Stream observableOutput ->
            observableOutput
            |> addClientSubscription id sendSubscriptionResponseOutput
        | Deferred (data, errors, observableOutput) ->
            do! sendOutput id { Data = ValueSome data; Errors = [] }
            if errors.IsEmpty then
                observableOutput
                |> addClientSubscription id (sendDeferredResultDelayedBy 5000)
        | Direct (data, _) -> do! sendOutput id { Data = ValueSome data; Errors = [] }
        | RequestError problemDetails -> do! sendOutput id { Data = ValueNone; Errors = problemDetails }
    }

    let handleSubscribe (id : SubscriptionId) (query : GQLRequestContent) = async {
        if subscriptions |> GraphQLSubscriptionsManagement.isIdTaken id then
            do! closeSocket webSocket CustomWebSocketStatus.SubscriberAlreadyExists $"Subscriber for Id = '{id}' already exists"
        else
            try
                let variables = query.Variables |> Skippable.toValueOption
                let getInputContext () = SuaveInputExecutionContext (ctx.request) :> IInputExecutionContext
                let root = options.RootFactory ctx
                let! planExecutionResult =
                    options.SchemaExecutor.AsyncExecute (query.Query, getInputContext, root, ?variables = variables)
                do! applyPlanExecutionResult id planExecutionResult
            with ex ->
                ctx.runtime.logger.info (Suave.Logging.Message.eventX $"Unexpected error during subscription with id '{id}': {ex}")
                do! sendMsg (Error (id, [ NameValueLookup ([ ("subscription", "Unexpected error during subscription" :> obj) ]) ]))
    }

    let rec loop () = async {
        let! received = receiveMessage serializerOptions webSocket

        match received with
        | SocketClosed -> return ()
        | NoOp -> return! loop ()
        | ProtocolFailure (InvalidMessage (code, explanation)) ->
            do! closeSocket webSocket code explanation
            return ()
        | ClientMsg (ConnectionInit _) ->
            do! closeSocket webSocket CustomWebSocketStatus.TooManyInitializationRequests "Too many initialization requests"
            return ()
        | ClientMsg (ClientPing p) ->
            match options.WebsocketOptions.CustomPingHandler with
            | ValueSome handler ->
                let! customP = handler p
                do! sendMsg (ServerPong customP)
            | ValueNone -> do! sendMsg (ServerPong p)
            return! loop ()
        | ClientMsg (ClientPong _) -> return! loop ()
        | ClientMsg (Subscribe (id, query)) ->
            do! handleSubscribe id query
            return! loop ()
        | ClientMsg (ClientComplete id) ->
            subscriptions
            |> GraphQLSubscriptionsManagement.removeSubscription id
            return! loop ()
    }

    async {
        try
            do! loop ()
        finally
            subscriptions
            |> GraphQLSubscriptionsManagement.removeAllSubscriptions
    }

/// A <see cref="Suave.Http.WebPart"/> that accepts <c>graphql-transport-ws</c> WebSocket connections and handles
/// GraphQL subscriptions (as well as queries and mutations) over them.
let handleGraphQLWebSocket (options : GraphQLOptions<'Root>) : WebPart =
    fun (ctx : HttpContext) ->
        let continuation (webSocket : WebSocket) (ctx : HttpContext) : SocketOp<unit> =
            SocketOp.ofAsync (
                async {
                    let! initResult = waitForConnectionInitAndRespondToClient options webSocket

                    match initResult with
                    | Result.Error _ -> ()
                    | Ok () -> do! handleMessages options ctx webSocket
                }
            )

        handShakeWithSubprotocol (chooseSubprotocol Subprotocol) continuation ctx
