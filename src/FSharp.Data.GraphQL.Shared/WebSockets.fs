namespace FSharp.Data.GraphQL.Shared.WebSockets

open System
open System.Collections.Generic
open System.Text.Json
open System.Text.Json.Serialization
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Shared

/// <summary>
/// Represents an invalid WebSocket protocol message.
/// </summary>
/// <param name="explanation">The validation failure explanation.</param>
type InvalidWebsocketMessageException (explanation : string) =
    inherit System.Exception (explanation)

/// <summary>
/// Identifies a GraphQL WebSocket subscription.
/// </summary>
type SubscriptionId = string

/// <summary>
/// Represents a disposable handle for an active subscription.
/// </summary>
type SubscriptionUnsubscriber = IDisposable

/// <summary>
/// Represents a callback invoked when a subscription is removed.
/// </summary>
type OnUnsubscribeAction = SubscriptionId -> unit

/// <summary>
/// Stores active subscriptions keyed by their identifier.
/// </summary>
type SubscriptionsDict = IDictionary<SubscriptionId, SubscriptionUnsubscriber * OnUnsubscribeAction>

/// <summary>
/// Represents a raw WebSocket message before it is mapped to protocol-specific client messages.
/// </summary>
type RawMessage = {
    /// <summary>
    /// Gets the message id, when the message is operation-scoped.
    /// </summary>
    Id : string voption
    /// <summary>
    /// Gets the protocol message type.
    /// </summary>
    Type : string
    /// <summary>
    /// Gets the raw JSON payload.
    /// </summary>
    Payload : JsonDocument voption
}

/// <summary>
/// Announces a deferred or streamed field for the first time, identifying it by a short id used in every
/// subsequent <see cref="IncrementalResult"/> or <see cref="CompletedResult"/> for the same field.
/// </summary>
type PendingResult = {
    /// <summary>
    /// Gets the short id assigned to the announced field.
    /// </summary>
    Id : string
    /// <summary>
    /// Gets the response path of the announced field.
    /// </summary>
    Path : FieldPath
}

/// <summary>
/// One incremental delivery of a deferred or streamed field, identified by the id from its
/// <see cref="PendingResult"/>.
/// </summary>
/// <remarks>
/// <see cref="Data"/> carries a <c>@defer</c> field's own value; <see cref="Items"/> carries one or more of a
/// <c>@stream</c> field's items, in list order.
/// </remarks>
type IncrementalResult = {
    /// <summary>
    /// Gets the id of the deferred or streamed field this payload belongs to.
    /// </summary>
    Id : string
    /// <summary>
    /// Gets the deferred field data, when the payload carries deferred data.
    /// </summary>
    Data : objnull Skippable
    /// <summary>
    /// Gets the streamed items, when the payload carries streamed data.
    /// </summary>
    Items : objnull[] Skippable
    /// <summary>
    /// Gets the execution errors associated with the payload.
    /// </summary>
    Errors : GQLProblemDetails list Skippable
}

/// <summary>
/// Reports that the deferred or streamed field identified by the id from its <see cref="PendingResult"/> has
/// delivered everything it is going to.
/// </summary>
[<Struct>]
type CompletedResult = {
    /// <summary>
    /// Gets the id of the deferred or streamed field that completed.
    /// </summary>
    Id : string
    /// <summary>
    /// Gets any completion errors associated with the field.
    /// </summary>
    Errors : GQLProblemDetails list Skippable
}

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
    /// <summary>
    /// Gets the result data.
    /// </summary>
    /// <remarks>
    /// This is an object for a complete or initial payload. It is always <see cref="Skip" /> for a subsequent
    /// payload, whose deltas are carried by <see cref="Incremental" /> and <see cref="Completed" /> instead.
    /// </remarks>
    Data : obj Skippable
    /// <summary>
    /// Gets the errors raised while producing the payload.
    /// </summary>
    /// <remarks>
    /// This is always <see cref="Skip" /> for a subsequent payload.
    /// </remarks>
    Errors : GQLProblemDetails list Skippable
    /// <summary>
    /// Gets the fields newly announced by this payload.
    /// </summary>
    Pending : PendingResult list Skippable
    /// <summary>
    /// Gets the deltas of already-announced fields delivered by this payload.
    /// </summary>
    Incremental : IncrementalResult list Skippable
    /// <summary>
    /// Gets the fields that finished delivering as of this payload.
    /// </summary>
    Completed : CompletedResult list Skippable
    /// <summary>
    /// Gets a value indicating whether more incremental payloads follow.
    /// </summary>
    HasNext : bool Skippable
} with

    /// <summary>
    /// Creates a payload of a complete execution result.
    /// </summary>
    static member Create (data : Output | null, errors : GQLProblemDetails list) = {
        Data = Include (box data)
        Errors = Include errors
        Pending = Skip
        Incremental = Skip
        Completed = Skip
        HasNext = Skip
    }

    /// <summary>
    /// Creates a payload that carries only errors.
    /// </summary>
    static member CreateErrors (errors : GQLProblemDetails list) = {
        Data = Include null
        Errors = Include errors
        Pending = Skip
        Incremental = Skip
        Completed = Skip
        HasNext = Skip
    }

    /// <summary>
    /// Creates the initial payload of an incremental delivery, which is always followed by subsequent payloads.
    /// </summary>
    static member CreateInitial (data : Output | null, errors : GQLProblemDetails list, pending : PendingResult list) = {
        Data = Include (box data)
        Errors = Include errors
        Pending = (if pending.IsEmpty then Skip else Include pending)
        Incremental = Skip
        Completed = Skip
        HasNext = Include true
    }

    /// <summary>
    /// Creates a subsequent payload of an incremental delivery.
    /// </summary>
    /// <remarks>
    /// It carries the fields it newly announces, the deltas it delivers for already-announced fields, and the
    /// fields it completes.
    /// </remarks>
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

/// <summary>
/// Represents the raw payload of a server WebSocket message.
/// </summary>
type ServerRawPayload =
    /// <summary>
    /// Contains a GraphQL execution result payload.
    /// </summary>
    | ExecutionResult of SubscriptionExecutionResult
    /// <summary>
    /// Contains one or more GraphQL error payloads.
    /// </summary>
    | ErrorMessages of GQLProblemDetails list
    /// <summary>
    /// Contains a custom JSON payload.
    /// </summary>
    | CustomResponse of JsonDocument

/// <summary>
/// Represents a raw server WebSocket message.
/// </summary>
type RawServerMessage = {
    /// <summary>
    /// Gets the message id, when the message is operation-scoped.
    /// </summary>
    Id : string voption
    /// <summary>
    /// Gets the protocol message type.
    /// </summary>
    Type : string
    /// <summary>
    /// Gets the raw server payload.
    /// </summary>
    Payload : ServerRawPayload voption
}

/// <summary>
/// Represents a parsed client WebSocket protocol message.
/// </summary>
type ClientMessage =
    /// <summary>
    /// Initializes a protocol connection.
    /// </summary>
    | ConnectionInit of payload : JsonDocument voption
    /// <summary>
    /// Sends a client ping frame.
    /// </summary>
    | ClientPing of payload : JsonDocument voption
    /// <summary>
    /// Sends a client pong frame.
    /// </summary>
    | ClientPong of payload : JsonDocument voption
    /// <summary>
    /// Starts a GraphQL subscription or operation.
    /// </summary>
    | Subscribe of id : string * query : GQLRequestContent
    /// <summary>
    /// Completes a client-side operation.
    /// </summary>
    | ClientComplete of id : string

/// <summary>
/// Represents a protocol-level validation failure for an incoming client message.
/// </summary>
type ClientMessageProtocolFailure =
    /// <summary>
    /// Indicates that the client message failed protocol validation.
    /// </summary>
    | InvalidMessage of code : int * explanation : string

/// <summary>
/// Represents a server WebSocket protocol message.
/// </summary>
type ServerMessage =
    /// <summary>
    /// Acknowledges a successful connection initialization.
    /// </summary>
    | ConnectionAck
    /// <summary>
    /// Sends a server ping frame.
    /// </summary>
    | ServerPing
    /// <summary>
    /// Sends a server pong frame.
    /// </summary>
    | ServerPong of JsonDocument voption
    /// <summary>
    /// Sends a GraphQL execution payload.
    /// </summary>
    | Next of id : string * payload : SubscriptionExecutionResult
    /// <summary>
    /// Sends protocol errors for an operation.
    /// </summary>
    | Error of id : string * err : GQLProblemDetails list
    /// <summary>
    /// Marks an operation as complete.
    /// </summary>
    | Complete of id : string

/// <summary>
/// Defines application-specific GraphQL WebSocket close codes.
/// </summary>
module CustomWebSocketStatus =

    /// <summary>
    /// The client sent an invalid message.
    /// </summary>
    let InvalidMessage = 4400

    /// <summary>
    /// The client is not authorized.
    /// </summary>
    let Unauthorized = 4401

    /// <summary>
    /// The client did not initialize the connection in time.
    /// </summary>
    let ConnectionTimeout = 4408

    /// <summary>
    /// The requested subscription identifier is already in use.
    /// </summary>
    let SubscriberAlreadyExists = 4409

    /// <summary>
    /// The client sent too many initialization requests.
    /// </summary>
    let TooManyInitializationRequests = 4429
