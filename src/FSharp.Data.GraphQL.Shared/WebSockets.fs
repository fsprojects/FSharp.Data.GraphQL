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

/// Identifies a GraphQL WebSocket subscription.
type SubscriptionId = string

/// Represents a disposable handle for an active subscription.
type SubscriptionUnsubscriber = IDisposable

/// Represents a callback invoked when a subscription is removed.
type OnUnsubscribeAction = SubscriptionId -> unit

/// Stores active subscriptions keyed by their identifier.
type SubscriptionsDict = IDictionary<SubscriptionId, SubscriptionUnsubscriber * OnUnsubscribeAction>

/// Represents a raw WebSocket message before it is mapped to protocol-specific client messages.
type RawMessage = {
    /// Gets the message id, when the message is operation-scoped.
    Id : string voption
    /// Gets the protocol message type.
    Type : string
    /// Gets the raw JSON payload.
    Payload : JsonDocument voption
}

/// <summary>
/// Announces a deferred or streamed field for the first time, identifying it by a short id used in every
/// subsequent <see cref="IncrementalResult"/> or <see cref="CompletedResult"/> for the same field.
/// </summary>
type PendingResult = {
    /// Gets the short id assigned to the announced field.
    Id : string
    /// Gets the response path of the announced field.
    Path : FieldPath
    /// Gets the optional label of the announced deferred field.
    Label : string Skippable
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
    /// Gets the id of the deferred or streamed field this payload belongs to.
    Id : string
    /// Gets the deferred field data, when the payload carries deferred data.
    Data : objnull Skippable
    /// Gets the streamed items, when the payload carries streamed data.
    Items : objnull[] Skippable
    /// Gets the execution errors associated with the payload.
    Errors : GQLProblemDetails list Skippable
}

/// <summary>
/// Reports that the deferred or streamed field identified by the id from its <see cref="PendingResult"/> has
/// delivered everything it is going to.
/// </summary>
[<Struct>]
type CompletedResult = {
    /// Gets the id of the deferred or streamed field that completed.
    Id : string
    /// Gets any completion errors associated with the field.
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
    Data : objnull Skippable
    /// <summary>Gets the errors raised while producing the payload.</summary>
    /// <remarks>This is always <see cref="Skip" /> for a subsequent payload.</remarks>
    Errors : GQLProblemDetails list Skippable
    /// Gets the fields newly announced by this payload.
    Pending : PendingResult list Skippable
    /// Gets the deltas of already-announced fields delivered by this payload.
    Incremental : IncrementalResult list Skippable
    /// Gets the fields that finished delivering as of this payload.
    Completed : CompletedResult list Skippable
    /// Gets a value indicating whether more incremental payloads follow.
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

    /// Creates a payload that carries only errors, omitting the top-level <c>data</c> property.
    static member CreateErrors (errors : GQLProblemDetails list) = {
        Data = Skip
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
    /// Creates a subsequent payload of an incremental delivery.
    /// </summary>
    /// <remarks>
    /// It carries the fields it newly announces, the deltas it delivers for already-announced fields, and the
    /// fields it completes.
    /// </remarks>
    static member CreateSubsequent
        (pending : PendingResult list, incremental : IncrementalResult list, completed : CompletedResult list, hasNext : bool)
        =
        {
            Data = Skip
            Errors = Skip
            Pending = (if pending.IsEmpty then Skip else Include pending)
            Incremental = (if incremental.IsEmpty then Skip else Include incremental)
            Completed = (if completed.IsEmpty then Skip else Include completed)
            HasNext = Include hasNext
        }

/// Represents the raw payload of a server WebSocket message.
type ServerRawPayload =
    /// Contains a GraphQL execution result payload.
    | ExecutionResult of SubscriptionExecutionResult
    /// Contains one or more GraphQL error payloads.
    | ErrorMessages of GQLProblemDetails list
    /// Contains a custom JSON payload.
    | CustomResponse of JsonDocument

/// Represents a raw server WebSocket message.
type RawServerMessage = {
    /// Gets the message id, when the message is operation-scoped.
    Id : string voption
    /// Gets the protocol message type.
    Type : string
    /// Gets the raw server payload.
    Payload : ServerRawPayload voption
}

/// Represents a parsed client WebSocket protocol message.
type ClientMessage =
    /// Initializes a protocol connection.
    | ConnectionInit of payload : JsonDocument voption
    /// Sends a client ping frame.
    | ClientPing of payload : JsonDocument voption
    /// Sends a client pong frame.
    | ClientPong of payload : JsonDocument voption
    /// Starts a GraphQL subscription or operation.
    | Subscribe of id : string * query : GQLRequestContent
    /// Completes a client-side operation.
    | ClientComplete of id : string

/// Represents a protocol-level validation failure for an incoming client message.
[<Struct>]
type ClientMessageProtocolFailure =
    /// Indicates that the client message failed protocol validation.
    | InvalidMessage of code : int * explanation : string

/// Represents a server WebSocket protocol message.
type ServerMessage =
    /// Acknowledges a successful connection initialization.
    | ConnectionAck
    /// Sends a server ping frame.
    | ServerPing
    /// Sends a server pong frame.
    | ServerPong of JsonDocument voption
    /// Sends a GraphQL execution payload.
    | Next of id : string * payload : SubscriptionExecutionResult
    /// Sends protocol errors for an operation.
    | ServerError of id : string * err : GQLProblemDetails list
    /// Marks an operation as complete.
    | Complete of id : string

/// Defines application-specific GraphQL WebSocket close codes.
module CustomWebSocketStatus =

    /// The client sent an invalid message.
    let InvalidMessage = 4400

    /// The client is not authorized.
    let Unauthorized = 4401

    /// The client did not initialize the connection in time.
    let ConnectionTimeout = 4408

    /// The requested subscription identifier is already in use.
    let SubscriberAlreadyExists = 4409

    /// The client sent too many initialization requests.
    let TooManyInitializationRequests = 4429
