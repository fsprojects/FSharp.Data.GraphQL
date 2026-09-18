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
/// Payload of a <c>next</c> message of the <c>graphql-transport-ws</c> protocol.
/// </summary>
/// <remarks>
/// <see cref="SubscriptionExecutionResult.Path"/> and <see cref="SubscriptionExecutionResult.HasNext"/> are present
/// only in payloads of incremental delivery, which is produced by the <c>@defer</c> and <c>@stream</c> directives.
/// </remarks>
type SubscriptionExecutionResult = {
    /// Result data: an object for complete and initial payloads, or a deferred or streamed value for incremental payloads.
    /// It is omitted from the final payload of an incremental delivery.
    Data : objnull Skippable
    /// Errors raised while producing the payload.
    Errors : GQLProblemDetails list
    /// Path of a deferred or streamed value inside the initial result.
    Path : FieldPath Skippable
    /// Tells whether more incremental payloads follow.
    HasNext : bool Skippable
} with

    /// Creates a payload of a complete execution result.
    static member Create (data : Output | null, errors : GQLProblemDetails list) = {
        Data = Include (box data)
        Errors = errors
        Path = Skip
        HasNext = Skip
    }

    /// Creates a payload that carries only errors.
    static member CreateErrors (errors : GQLProblemDetails list) = { Data = Include null; Errors = errors; Path = Skip; HasNext = Skip }

    /// Creates the initial payload of an incremental delivery, which is always followed by incremental payloads.
    static member CreateInitial (data : Output | null, errors : GQLProblemDetails list) = {
        Data = Include (box data)
        Errors = errors
        Path = Skip
        HasNext = Include true
    }

    /// <summary>
    /// Creates an incremental payload with a deferred or streamed value located at the path.
    /// More payloads may follow, so <see cref="SubscriptionExecutionResult.HasNext"/> is <see langword="true"/>.
    /// </summary>
    static member CreateIncremental (data : objnull, errors : GQLProblemDetails list, path : FieldPath) = {
        Data = Include data
        Errors = errors
        Path = Include path
        HasNext = Include true
    }

    /// <summary>
    /// Creates the final payload of an incremental delivery, which only reports that no more payloads follow.
    /// </summary>
    /// <remarks>
    /// Payloads are sent as soon as they are produced, and whether a payload is the last one becomes known
    /// only when the deferred results complete, so the end of the delivery is reported separately.
    /// </remarks>
    static member CreateCompleted () = { Data = Skip; Errors = []; Path = Skip; HasNext = Include false }

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
