namespace FSharp.Data.GraphQL.Server.AspNetCore

open System
open System.Collections.Generic
open System.Text.Json
open FSharp.Data.GraphQL.Execution

type SubscriptionId = string
type SubscriptionUnsubscriber = IDisposable
type OnUnsubscribeAction = SubscriptionId -> unit
type SubscriptionsDict = IDictionary<SubscriptionId, SubscriptionUnsubscriber * OnUnsubscribeAction>

type RawMessage = { Id : string option; Type : string; Payload : JsonDocument option }

type ServerRawPayload =
    | ExecutionResult of Output
    | ErrorMessages of NameValueLookup list
    | CustomResponse of JsonDocument

type RawServerMessage = { Id : string option; Type : string; Payload : ServerRawPayload option }

type ClientMessage =
    | ConnectionInit of payload : JsonDocument option
    | ClientPing of payload : JsonDocument option
    | ClientPong of payload : JsonDocument option
    | Subscribe of id : string * query : GQLRequestContent
    | ClientComplete of id : string

type ClientMessageProtocolFailure = InvalidMessage of code : int * explanation : string

type ServerMessage =
    | ConnectionAck
    | ServerPing
    | ServerPong of JsonDocument option
    | Next of id : string * payload : Output
    | Error of id : string * err : NameValueLookup list
    | Complete of id : string

module CustomWebSocketStatus =

    let InvalidMessage = 4400
    let Unauthorized = 4401
    let ConnectionTimeout = 4408
    let SubscriberAlreadyExists = 4409
    let TooManyInitializationRequests = 4429
