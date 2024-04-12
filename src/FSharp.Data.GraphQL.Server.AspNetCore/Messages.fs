namespace FSharp.Data.GraphQL.Server.AspNetCore.WebSockets

open System
open System.Collections.Generic
open System.Text.Json
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Server.AspNetCore

type SubscriptionId = string
type SubscriptionUnsubscriber = IDisposable
type OnUnsubscribeAction = SubscriptionId -> unit
type SubscriptionsDict = IDictionary<SubscriptionId, SubscriptionUnsubscriber * OnUnsubscribeAction>

type RawMessage = { Id : string voption; Type : string; Payload : JsonDocument voption }

type ServerRawPayload =
    | ExecutionResult of Output
    | ErrorMessages of NameValueLookup list
    | CustomResponse of JsonDocument

type RawServerMessage = { Id : string voption; Type : string; Payload : ServerRawPayload voption }

type ClientMessage =
    | ConnectionInit of payload : JsonDocument voption
    | ClientPing of payload : JsonDocument voption
    | ClientPong of payload : JsonDocument voption
    | Subscribe of id : string * query : GQLRequestContent
    | ClientComplete of id : string

type ClientMessageProtocolFailure = InvalidMessage of code : int * explanation : string

type ServerMessage =
    | ConnectionAck
    | ServerPing
    | ServerPong of JsonDocument voption
    | Next of id : string * payload : Output
    | Error of id : string * err : NameValueLookup list
    | Complete of id : string

module CustomWebSocketStatus =

    let InvalidMessage = 4400
    let Unauthorized = 4401
    let ConnectionTimeout = 4408
    let SubscriberAlreadyExists = 4409
    let TooManyInitializationRequests = 4429
