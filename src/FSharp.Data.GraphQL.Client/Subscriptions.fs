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

/// A subscription response, containing both immediate and deferred results for a subscription request.
type GraphQLSubscriptionResponse =
      /// Contains immediate result for the subscription request.
    { Data : string
      /// Contains the delayed results for the subscription request, which needs to be subscribed to.
      Deferred : IObservable<string> }

/// An interface for implementing subscription handlers for GraphQLProvider.
type IGraphQLSubscriptionHandler =
    inherit IDisposable
    /// Wnen implemented, starts a connection asynchronously to a GraphQL server which supports subscriptions accordingly to the implementation protocol.
    abstract member AsyncConnect : string * (string * obj) [] * bool -> Async<unit>
    /// When implemented, start a subscription asynchronously to the GraphQL using the implementation protocol.
    abstract member AsyncSubscribe : GraphQLSubscriptionRequest -> Async<GraphQLSubscriptionResponse>
    /// Wnen implemented, starts a connection to a GraphQL server which supports subscriptions accordingly to the implementation protocol.
    abstract member Connect : string * (string * obj) [] * bool -> unit
    /// When implemented, start a subscription to the GraphQL using the implementation protocol.
    abstract member Subscribe : GraphQLSubscriptionRequest -> GraphQLSubscriptionResponse

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
    | ConnectionError of payload : (string * obj) []
    | Data of id : string * payload : (string * obj) []
    | Error of id : string * payload : (string * obj) []
    | Complete of id : string
    static member FromString(s : string) =
        let getPayload (r : (string * JsonValue) []) =
            match r |> Array.tryFind (fun (name, _) -> name.ToLowerInvariant() = "payload") with
            | Some (_, JsonValue.Record p) -> Serialization.deserializeMap p |> Map.toArray
            | _ -> [||]
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

/// A GraphQL subscription handler which uses GraphQL Over Web Socket interface.
/// See https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md for information.
type GraphQLOverWebSocketSubscriptionHandler() =
    let socket = new ClientWebSocket()

    let receive() =
        async {
            let buffer = Array.zeroCreate 4096
            let responseSegment = ArraySegment<byte>(buffer)
            do! socket.ReceiveAsync(responseSegment, CancellationToken.None) |> Async.AwaitTask |> Async.Ignore
            return Encoding.UTF8.GetString(buffer) |> WebSocketServerMessage.FromString
        }
    
    let fail (expected : WebSocketServerMessage) (actual : WebSocketServerMessage) =
        failwithf "Expected server response to be \"%A\", but got \"%A\"." expected actual
    
    let mutable subscriptionSource = Unchecked.defaultof<IObservable<WebSocketServerMessage>>
        
    let connect serverUrl connectionParams keepAlive =
        async {
            if socket.State = WebSocketState.Open || socket.State = WebSocketState.Connecting
            then return ()
            else
                do! socket.ConnectAsync(Uri(serverUrl), CancellationToken.None) |> Async.AwaitTask
                let requestMessage = ConnectionInit connectionParams
                let requestSegment = Encoding.UTF8.GetBytes(requestMessage.ToString()) |> ArraySegment<byte>
                do! socket.SendAsync(requestSegment, WebSocketMessageType.Text, true, CancellationToken.None) |> Async.AwaitTask
                let! responseMessage = receive()
                if responseMessage <> ConnectionAck then fail ConnectionAck responseMessage
                if keepAlive then
                    let! responseMessage = receive()
                    if responseMessage <> KeepAlive then fail KeepAlive responseMessage
                subscriptionSource <- 
                    seq { while true do yield {  } }
        }

    interface IGraphQLSubscriptionHandler with
        member __.AsyncConnect(serverUrl, connectionParams, keepAlive) = connect serverUrl connectionParams keepAlive
        member __.Connect(serverUrl, connectionParams, keepAlive) = connect serverUrl connectionParams keepAlive |> Async.RunSynchronously
        member __.AsyncSubscribe(request) =
            async {
                if socket.State <> WebSocketState.Open && socket.State <> WebSocketState.Connecting
                then return failwithf "Socket is not connected. A connection must be done first before using Connection method."
                else
                    let id = Guid.NewGuid().ToString()
                    let requestMessage = Start (id, request)
                    let requestSegment = Encoding.UTF8.GetBytes(requestMessage.ToString()) |> ArraySegment<byte>
                    do! socket.SendAsync(requestSegment, WebSocketMessageType.Text, true, CancellationToken.None) |> Async.AwaitTask

            }
