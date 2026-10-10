namespace FSharp.Data.GraphQL.Server.AspNetCore

open System.Net.WebSockets
open System.Text.Json

open FSharp.Data.GraphQL.Shared.WebSockets

/// A message for the sender loop of a connection, the only writer of its socket.
type internal OutboundMessage =
    /// A protocol message to send.
    | Send of message : ServerMessage
    /// Close the socket with the given status; every message queued before it is sent first, every one after it is dropped.
    | Close of status : WebSocketCloseStatus * description : string

/// A message waiting for the sender loop of a connection, serialized when it was queued.
type internal QueuedMessage =
    /// The JSON of a protocol message to send.
    | QueuedSend of json : byte array
    /// Close the socket with the given status; every message queued before it is sent first, every one after it is dropped.
    | QueuedClose of status : WebSocketCloseStatus * description : string

/// An event for the control loop of a connection, the only owner of its subscription registry.
type internal ConnectionEvent =
    /// <summary>
    /// The client sent a message, as its JSON: the control loop deserializes it into a <see cref="ClientMessage"/>, so that
    /// a message waiting for it holds no more memory than its bytes.
    /// </summary>
    | MessageReceived of json : byte array
    /// The client sent something that is not a valid protocol message.
    | ProtocolFailure of code : int * explanation : string
    /// A subscription worker ended, whether by completing, failing, or being cancelled.
    | SubscriptionEnded of id : SubscriptionId * generation : int

/// Serialization of protocol messages sent to the client.
module internal ServerMessageSerialization =

    /// The JSON of a server message in the graphql-transport-ws wire format.
    let serializeServerMessage (jsonSerializerOptions : JsonSerializerOptions) (serverMessage : ServerMessage) =
        let raw =
            match serverMessage with
            | ConnectionAck -> { Id = ValueNone; Type = "connection_ack"; Payload = ValueNone }
            | ServerPing -> { Id = ValueNone; Type = "ping"; Payload = ValueNone }
            | ServerPong p -> { Id = ValueNone; Type = "pong"; Payload = p |> ValueOption.map CustomResponse }
            | Next (id, payload) -> {
                Id = ValueSome id
                Type = "next"
                Payload = ValueSome <| ExecutionResult payload
              }
            | Complete id -> { Id = ValueSome id; Type = "complete"; Payload = ValueNone }
            | ServerError (id, errMessages) -> {
                Id = ValueSome id
                Type = "error"
                Payload = ValueSome <| ErrorMessages errMessages
              }
        JsonSerializer.Serialize (raw, jsonSerializerOptions)

/// Patterns over a received client message, as the reader produces it.
module internal ClientMessagePatterns =

    let (|InvalidReceivedMessage|EmptyReceivedMessage|ReceivedClientMessage|) receivedMessage =
        match receivedMessage with
        | Error (InvalidMessage (code, explanation)) -> InvalidReceivedMessage (code, explanation)
        | Ok ValueNone -> EmptyReceivedMessage
        | Ok (ValueSome message) -> ReceivedClientMessage message

    let (|ConnectionInitReceived|SubscribeBeforeConnectionInit|InvalidConnectionInitMessage|UnexpectedConnectionInitMessage|) receivedMessage =
        match receivedMessage with
        | Ok (ValueSome (ConnectionInit _)) -> ConnectionInitReceived
        | Ok (ValueSome (Subscribe _)) -> SubscribeBeforeConnectionInit
        | Error (InvalidMessage (code, explanation)) -> InvalidConnectionInitMessage (code, explanation)
        | _ -> UnexpectedConnectionInitMessage
