namespace FSharp.Data.GraphQL.Shared.WebSockets

open System
open System.Collections.Generic
open System.Text.Json
open System.Text.Json.Serialization
open FSharp.Data.GraphQL

type InvalidWebsocketMessageException (explanation : string) =
    inherit System.Exception (explanation)

type SubscriptionId = string
type SubscriptionUnsubscriber = IDisposable
type OnUnsubscribeAction = SubscriptionId -> unit
type SubscriptionsDict = IDictionary<SubscriptionId, SubscriptionUnsubscriber * OnUnsubscribeAction>

[<JsonFSharpConverter(unionTagName = "type", SkippableOptionFields = SkippableOptionFields.Always)>]
type ClientMessage =
    | [<JsonName "connection_init">] ConnectionInit of Payload : JsonDocument voption
    | [<JsonName "ping">] ClientPing of Payload : JsonDocument voption
    | [<JsonName "pong">] ClientPong of Payload : JsonDocument voption
    | [<JsonName "subscribe">] Subscribe of Id : string * Payload : GQLRequestContent
    | [<JsonName "complete">] ClientComplete of Id : string

type ClientMessageProtocolFailure = InvalidMessage of Code : int * Explanation : string

[<JsonFSharpConverter(unionTagName = "type", SkippableOptionFields = SkippableOptionFields.Always)>]
type ServerMessage =
    | [<JsonName "connection_ack">] ConnectionAck
    | [<JsonName "ping">] ServerPing
    | [<JsonName "pong">] ServerPong of JsonDocument voption
    | [<JsonName "next">] Next of Id : string * Payload : GQLWebSocketResponse
    | [<JsonName "error">] Error of Id : string * Err : GQLProblemDetails list
    | [<JsonName "complete">] Complete of Id : string

module CustomWebSocketStatus =

    let InvalidMessage = 4400
    let Unauthorized = 4401
    let ConnectionTimeout = 4408
    let SubscriberAlreadyExists = 4409
    let TooManyInitializationRequests = 4429
