namespace FSharp.Data.GraphQL.Shared.WebSockets

open System
open System.Collections.Generic
open System.Text.Json
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Shared

type InvalidWebsocketMessageException (explanation : string) =
    inherit System.Exception (explanation)

type SubscriptionId = string
type SubscriptionUnsubscriber = IDisposable
type OnUnsubscribeAction = SubscriptionId -> unit
type SubscriptionsDict = IDictionary<SubscriptionId, SubscriptionUnsubscriber * OnUnsubscribeAction>

type RawMessage = { Id : string voption; Type : string; Payload : JsonDocument voption }

type SubscriptionExecutionResult = { Data : Output voption; Errors : GQLProblemDetails list }

type ServerRawPayload =
    | ExecutionResult of SubscriptionExecutionResult
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
    | Next of id : string * payload : SubscriptionExecutionResult
    | Error of id : string * err : NameValueLookup list
    | Complete of id : string

module CustomWebSocketStatus =

    let InvalidMessage = 4400
    let Unauthorized = 4401
    let ConnectionTimeout = 4408
    let SubscriberAlreadyExists = 4409
    let TooManyInitializationRequests = 4429
