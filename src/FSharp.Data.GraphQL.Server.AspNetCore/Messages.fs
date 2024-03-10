namespace FSharp.Data.GraphQL.Server.AspNetCore

open FSharp.Data.GraphQL.Execution
open System
open System.Text.Json
open System.Collections.Generic

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

    let invalidMessage = 4400
    let unauthorized = 4401
    let connectionTimeout = 4408
    let subscriberAlreadyExists = 4409
    let tooManyInitializationRequests = 4429
