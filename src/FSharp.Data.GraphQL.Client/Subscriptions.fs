/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Client

open System
open System.Text
open System.Net.WebSockets
open FSharp.Data.GraphQL
open System.Threading

/// A subscription request, containing essential data for requesting a subscription query on the server.
type GraphQLSubscriptionRequest  =
      /// Contains the name of the operation, or None in case the operation is not named.
    { OperationName : string option
      /// Contains the GraphQL query string which will be parsed by the server.
      Query : string
      /// Contains variables used by the query.
      Variables : (string * obj) [] }

type internal WebSocketClientMessage =
    | ConnectionInit of connectionParams : (string * obj) []
    | ConnectionTerminate
    | Start of id : string * payload : GraphQLSubscriptionRequest
    | Stop of id : string
    member x.ToJsonValue() =
        match x with
        | ConnectionInit [||] -> [| "type", JsonValue.String "connection_init" |] |> JsonValue.Record
        | ConnectionInit connectionParams ->
            [| "payload", Map.ofArray connectionParams |> Serialization.toJsonValue
               "type", JsonValue.String "connection_init" |]
            |> JsonValue.Record
        | ConnectionTerminate -> [| "type", JsonValue.String "connection_terminate" |] |> JsonValue.Record
        | Start (id, payload) ->
            let payloadJson =
                let operationName =
                    match payload.OperationName with
                    | Some x -> JsonValue.String x
                    | None -> JsonValue.Null
                let query = JsonValue.String payload.Query
                let variables =
                    match payload.Variables with
                    | null | [||] -> JsonValue.Null
                    | _ -> Map.ofArray payload.Variables |> Serialization.toJsonValue
                [| "query", query
                   "operationName", operationName
                   "variables", variables |]
                |> JsonValue.Record
            [| "id", JsonValue.String id
               "payload", payloadJson
               "type", JsonValue.String "start" |]
            |> JsonValue.Record
        | Stop id ->
            [| "id", JsonValue.String id
               "type", JsonValue.String "stop" |]
            |> JsonValue.Record
    override x.ToString() = x.ToJsonValue().ToString()

type internal WebSocketServerMessage =
    | ConnectionAck
    | KeepAlive
    | ConnectionError of payload : JsonValue
    | Data of id : string * payload : JsonValue
    | Error of id : string * payload : JsonValue
    | Complete of id : string
    static member FromString(s : string) =
        let getPayload (r : (string * JsonValue) []) =
            match r |> Array.tryFind (fun (name, _) -> name.ToLowerInvariant() = "payload") with
            | Some (_, x) -> x
            | _ -> JsonValue.Null
        let getType (r : (string * JsonValue) []) =
            match r |> Array.tryFind (fun (name, _) -> name.ToLowerInvariant() = "type") with
            | Some (_, JsonValue.String t) -> t.ToLowerInvariant()
            | _ -> failwith "Expected server to return a valid message, but message type field is missing in response, or the type field is not a string field."
        let getId (r : (string * JsonValue) []) =
            match r |> Array.tryFind (fun (name, _) -> name.ToLowerInvariant() = "id") with
            | Some (_, JsonValue.String id) -> id.ToLowerInvariant()
            | _ -> failwith "Expected server to return a valid message, but message ID field is missing in response, or the ID field is not a string field."
        match JsonValue.Parse(s) with
        | JsonValue.Record r ->
            match getType r with
            | "connection_ack" -> ConnectionAck
            | "keep_alive" -> KeepAlive
            | "connection_error" -> ConnectionError (getPayload r)
            | "data" -> Data (getId r, getPayload r)
            | "error" -> Error (getId r, getPayload r)
            | "complete" -> Complete (getId r)
            | other -> failwithf "Unexpected message type received from server. Can not treat \"%s\" message type." other
        | other -> failwithf "Unexpected response JSON type. Expected a Record Json, but got %A." other

/// A subscription response message, following the protocol of the handler that will use it.
type IGraphQLSubscriptionResponse =
    inherit IDisposable
    /// When implemented, returns the immediate data for the operation.
    abstract Data : JsonValue option
    /// When implemented, returns the deferred data (@defer, @stream and subscriptions) for the operation.
    abstract Deferred : IObservable<JsonValue>
    /// When implemented, unsubscribe from the operation using the handler of the protocol.
    abstract Unsubscribe : unit -> unit
    /// When implemented, asynchronously unsubscribe from the operation using the handler of the protocol.
    abstract AsyncUnsubscribe : unit -> Async<unit>

type internal SubscriptionResponseMessage =
    | Incoming of payload : JsonValue
    | Finished

/// The default GraphQL subscription response implementation that uses GraphQL Over Web Socket protocol.
/// See https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md for information.
type GraphQLSubscriptionResponse internal (source : IObservable<SubscriptionResponseMessage>, unsubscriber : unit -> Async<unit>) =
    let tryPickFirstAndKeepRemaining (source : IObservable<'T>) =
        let sourceSeq = Observable.toSeq source
        let first = Seq.tryHead sourceSeq
        match first with
        | Some first -> Some (first, Seq.skip 1 sourceSeq)
        | None -> None

    let data, deferred =
        source
        |> tryPickFirstAndKeepRemaining
        |> Option.map (fun (first, remaining) ->
            let deferred = {
                new IObservable<JsonValue> with
                    member __.Subscribe(observer) =
                        remaining
                        |> Seq.iter (function
                            | Incoming payload -> observer.OnNext(payload)
                            | Finished -> observer.OnCompleted())
                        { new IDisposable with member __.Dispose() = () } }
            match first with
            | Incoming payload -> Some payload, deferred
            | Finished -> None, deferred)
        |> Option.defaultValue (None, Observable.empty)

    interface IGraphQLSubscriptionResponse with
        member __.Data = data
        member __.Deferred = deferred
        member __.Unsubscribe() = unsubscriber () |> Async.RunSynchronously
        member __.AsyncUnsubscribe() = unsubscriber ()

    interface IDisposable with
        member __.Dispose() = unsubscriber () |> Async.RunSynchronously

/// An interface for implementing subscription handlers for GraphQLProvider.
type IGraphQLSubscriptionHandler =
    inherit IDisposable
    /// Wnen implemented, starts a connection asynchronously to a GraphQL server which supports subscriptions accordingly to the implementation protocol.
    abstract member AsyncConnect : string * (string * obj) [] * bool -> Async<unit>
    /// When implemented, start a subscription asynchronously to the GraphQL using the implementation protocol.
    abstract member AsyncSubscribe : GraphQLSubscriptionRequest -> Async<IGraphQLSubscriptionResponse>
    /// Wnen implemented, starts a connection to a GraphQL server which supports subscriptions accordingly to the implementation protocol.
    abstract member Connect : string * (string * obj) [] * bool -> unit
    /// When implemented, start a subscription to the GraphQL using the implementation protocol.
    abstract member Subscribe : GraphQLSubscriptionRequest -> IGraphQLSubscriptionResponse
    /// When implemented, closes the subscription connection to the GraphQL server using the implementation protocol.
    abstract member Close : unit -> unit
    /// When implemented, asynchronously closes the subscription connection to the GraphQL server using the implementation protocol.
    abstract member AsyncClose : unit -> Async<unit>

/// A GraphQL subscription handler which uses GraphQL Over Web Socket protocol.
/// See https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md for information.
type GraphQLOverWebSocketSubscriptionHandler() =
    let socket = new ClientWebSocket()

    let send requestMessage = 
        let requestSegment = 
            requestMessage.ToString()
            |> Encoding.UTF8.GetBytes
            |> ArraySegment<byte>
        socket.SendAsync(requestSegment, WebSocketMessageType.Text, true, CancellationToken.None)
        |> Async.AwaitTask

    let receive () =
        async {
            let buffer = Array.zeroCreate 4096
            let responseSegment = ArraySegment<byte>(buffer)
            do! socket.ReceiveAsync(responseSegment, CancellationToken.None) |> Async.AwaitTask |> Async.Ignore
            return Encoding.UTF8.GetString(buffer) |> WebSocketServerMessage.FromString
        }
    
    let throwUnexpectedResponseException (expected : WebSocketServerMessage) (actual : WebSocketServerMessage) =
        failwithf "Expected server response to be \"%A\", but got \"%A\"." expected actual

    let throwNotConnectedException () =
        failwith "Socket is not connected. A connection must be done first before using Connect method."

    let throwConnectedException () =
        failwith "Socket is already connected."
    
    let connectionLockSource = obj()
    let mutable subscriptionSource : IObservable<WebSocketServerMessage> option = None
        
    let disconnect () =
        lock connectionLockSource (fun () ->
            async {
                match subscriptionSource with
                | Some _ ->
                    do! send ConnectionTerminate
                    do! socket.CloseAsync(WebSocketCloseStatus.NormalClosure, "Terminating connection.", CancellationToken.None) |> Async.AwaitTask
                    subscriptionSource <- None
                | None -> return () })

    let connect serverUrl connectionParams keepAlive =
        if Option.isSome subscriptionSource then throwConnectedException ()
        lock connectionLockSource (fun () ->
            async {
                do! socket.ConnectAsync(Uri(serverUrl), CancellationToken.None) |> Async.AwaitTask
                do! send (ConnectionInit connectionParams)
                let! responseMessage = receive()
                if responseMessage <> ConnectionAck 
                then throwUnexpectedResponseException ConnectionAck responseMessage
                if keepAlive then
                    let! responseMessage = receive()
                    if responseMessage <> KeepAlive 
                    then throwUnexpectedResponseException KeepAlive responseMessage
                subscriptionSource <-
                    seq { while socket.State = WebSocketState.Open do yield receive () }
                    |> Observable.ofAsyncSeq
                    |> Some })
                

    let subscribe request =
        async {
            match subscriptionSource with
            | Some source ->
                let id = Guid.NewGuid().ToString()
                let requestMessage = Start (id, request)
                do! send requestMessage
                let responseSource =
                    source
                    |> Observable.choose (function
                        | (Data (incomingId, payload) | Error (incomingId, payload)) when incomingId = id -> Some (Incoming payload)
                        | Complete incomingId when incomingId = id -> Some Finished
                        | _ -> None)
                let unsubscriber = fun () -> send (Stop id)
                return new GraphQLSubscriptionResponse(responseSource, unsubscriber) :> IGraphQLSubscriptionResponse
            | None -> return throwNotConnectedException () }

    interface IGraphQLSubscriptionHandler with
        member __.AsyncConnect(serverUrl, connectionParams, keepAlive) = connect serverUrl connectionParams keepAlive
        member __.Connect(serverUrl, connectionParams, keepAlive) = connect serverUrl connectionParams keepAlive |> Async.RunSynchronously
        member __.AsyncSubscribe(request) = subscribe request
        member __.Subscribe(request) = subscribe request |> Async.RunSynchronously
        member __.Close() = disconnect () |> Async.RunSynchronously
        member __.AsyncClose() = disconnect ()

    interface IDisposable with
        member __.Dispose() = disconnect () |> Async.RunSynchronously
