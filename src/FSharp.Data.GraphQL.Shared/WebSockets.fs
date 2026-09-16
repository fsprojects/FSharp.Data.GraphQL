namespace FSharp.Data.GraphQL.Shared.WebSockets

open System
open System.Collections.Generic
open System.Text.Json
open System.Text.Json.Serialization
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Shared

type InvalidWebsocketMessageException (explanation : string) =
    inherit System.Exception (explanation)

type SubscriptionId = string
type SubscriptionUnsubscriber = IDisposable
type OnUnsubscribeAction = SubscriptionId -> unit
type SubscriptionsDict = IDictionary<SubscriptionId, SubscriptionUnsubscriber * OnUnsubscribeAction>

type RawMessage = { Id : string voption; Type : string; Payload : JsonDocument voption }

/// <summary>
/// Announces a deferred or streamed field for the first time, identifying it by a short id used in every
/// subsequent <see cref="IncrementalResult"/> or <see cref="CompletedResult"/> for the same field.
/// </summary>
type PendingResult = { Id : string; Path : FieldPath }

/// <summary>
/// One incremental delivery of a deferred or streamed field, identified by the id from its
/// <see cref="PendingResult"/>.
/// </summary>
/// <remarks>
/// <see cref="Data"/> carries a <c>@defer</c> field's own value (or a stream failure reported before any item was
/// produced); <see cref="Items"/> carries one or more of a <c>@stream</c> field's items, in list order.
/// </remarks>
type IncrementalResult = {
    Id : string
    Data : objnull Skippable
    Items : objnull[] Skippable
    Errors : GQLProblemDetails list Skippable
}

/// <summary>
/// Reports that the deferred or streamed field identified by the id from its <see cref="PendingResult"/> has
/// delivered everything it is going to.
/// </summary>
type CompletedResult = { Id : string; Errors : GQLProblemDetails list Skippable }

/// <summary>
/// Payload of a <c>next</c> message of the <c>graphql-transport-ws</c> protocol.
/// </summary>
/// <remarks>
/// <see cref="Pending"/>, <see cref="Incremental"/>, <see cref="Completed"/> and <see cref="HasNext"/> are present
/// only in payloads of incremental delivery, produced by the <c>@defer</c> and <c>@stream</c> directives, using the
/// <c>pending</c>/<c>incremental</c>/<c>completed</c>/<c>hasNext</c> format used by graphql-js 17 and Apollo
/// Client's <c>GraphQL17Alpha9Handler</c>.
/// </remarks>
type SubscriptionExecutionResult = {
    /// Result data: an object for a complete or initial payload. Always <see cref="Skip"/> for a subsequent
    /// payload, whose deltas are carried by <see cref="Incremental"/> and <see cref="Completed"/> instead.
    Data : obj Skippable
    /// Errors raised while producing the payload. Always <see cref="Skip"/> for a subsequent payload.
    Errors : GQLProblemDetails list Skippable
    /// Fields newly announced by this payload.
    Pending : PendingResult list Skippable
    /// Deltas of already-announced fields delivered by this payload.
    Incremental : IncrementalResult list Skippable
    /// Fields that finished delivering as of this payload.
    Completed : CompletedResult list Skippable
    /// Tells whether more incremental payloads follow.
    HasNext : bool Skippable
} with

    /// Creates a payload of a complete execution result.
    static member Create (data : Output | null, errors : GQLProblemDetails list) = {
        Data = Include (box data)
        Errors = Include errors
        Pending = Skip
        Incremental = Skip
        Completed = Skip
        HasNext = Skip
    }

    /// Creates a payload that carries only errors.
    static member CreateErrors (errors : GQLProblemDetails list) = {
        Data = Include null
        Errors = Include errors
        Pending = Skip
        Incremental = Skip
        Completed = Skip
        HasNext = Skip
    }

    /// Creates the initial payload of an incremental delivery, which is always followed by subsequent payloads.
    static member CreateInitial (data : Output | null, errors : GQLProblemDetails list, pending : PendingResult list) = {
        Data = Include (box data)
        Errors = Include errors
        Pending = (if pending.IsEmpty then Skip else Include pending)
        Incremental = Skip
        Completed = Skip
        HasNext = Include true
    }

    /// <summary>
    /// Creates a subsequent payload of an incremental delivery, carrying the fields it newly announces, the
    /// deltas it delivers for already-announced fields, and the fields it completes.
    /// </summary>
    static member CreateSubsequent
        (pending : PendingResult list, incremental : IncrementalResult list, completed : CompletedResult list, hasNext : bool)
        = {
        Data = Skip
        Errors = Skip
        Pending = (if pending.IsEmpty then Skip else Include pending)
        Incremental = (if incremental.IsEmpty then Skip else Include incremental)
        Completed = (if completed.IsEmpty then Skip else Include completed)
        HasNext = Include hasNext
    }

type ServerRawPayload =
    | ExecutionResult of SubscriptionExecutionResult
    | ErrorMessages of GQLProblemDetails list
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
    | Error of id : string * err : GQLProblemDetails list
    | Complete of id : string

module CustomWebSocketStatus =

    let InvalidMessage = 4400
    let Unauthorized = 4401
    let ConnectionTimeout = 4408
    let SubscriberAlreadyExists = 4409
    let TooManyInitializationRequests = 4429
