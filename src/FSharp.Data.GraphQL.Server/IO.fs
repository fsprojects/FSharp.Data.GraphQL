namespace FSharp.Data.GraphQL

open System
open System.Collections.Generic
open System.Text.Json.Serialization
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Extensions
open FSharp.Data.GraphQL.Types

/// <summary>
/// Represents a GraphQL response object keyed by field name.
/// </summary>
type Output = IDictionary<string, obj>

/// <summary>
/// Represents the serialized shape of a GraphQL response document.
/// </summary>
type GQLResponse = {
    /// <summary>
    /// Gets the identifier of the executed document inside the request batch.
    /// </summary>
    DocumentId : int
    /// <summary>
    /// Gets the response data.
    /// </summary>
    Data : Skippable<Output voption>
    /// <summary>
    /// Gets the response errors.
    /// </summary>
    Errors : Skippable<GQLProblemDetails list>
} with

    /// <summary>
    /// Creates a response for a successfully executed operation.
    /// </summary>
    /// <param name="documentId">The identifier of the executed document inside the request batch.</param>
    /// <param name="data">The response data.</param>
    /// <param name="errors">The response errors.</param>
    static member Direct (documentId, data : Output | null, errors) = {
        DocumentId = documentId
        Data = Include (data |> ValueOption.ofObj)
        Errors = Skippable.ofList errors
    }

    /// <summary>
    /// Creates a response placeholder for a streaming GraphQL operation.
    /// </summary>
    /// <param name="documentId">The identifier of the executed document inside the request batch.</param>
    static member Stream (documentId) = { DocumentId = documentId; Data = Include ValueNone; Errors = Skip }

    /// <summary>
    /// Creates a response for a request rejected before execution.
    /// </summary>
    /// <param name="documentId">The identifier of the rejected document inside the request batch.</param>
    /// <param name="errors">The request errors.</param>
    static member RequestError (documentId, errors) = { DocumentId = documentId; Data = Skip; Errors = Include errors }

/// <summary>
/// Represents the executor output together with request metadata.
/// </summary>
type GQLExecutionResult = {
    /// <summary>
    /// Gets the identifier of the executed document inside the request batch.
    /// </summary>
    DocumentId : int
    /// <summary>
    /// Gets the execution content.
    /// </summary>
    Content : GQLResponseContent
    /// <summary>
    /// Gets the execution metadata.
    /// </summary>
    Metadata : Metadata
} with

    /// <summary>
    /// Creates a direct execution result.
    /// </summary>
    /// <param name="documentId">The identifier of the executed document inside the request batch.</param>
    /// <param name="data">The execution data.</param>
    /// <param name="errors">The execution errors.</param>
    /// <param name="meta">The execution metadata.</param>
    static member Direct (documentId, data : Output | null, errors, meta) = {
        DocumentId = documentId
        Content = Direct (data |> ValueOption.ofObj, errors)
        Metadata = meta
    }

    /// <summary>
    /// Creates a deferred execution result.
    /// </summary>
    /// <param name="documentId">The identifier of the executed document inside the request batch.</param>
    /// <param name="data">The initial execution data.</param>
    /// <param name="errors">The initial execution errors.</param>
    /// <param name="deferred">The follow-up deferred payload stream.</param>
    /// <param name="meta">The execution metadata.</param>
    static member Deferred (documentId, data, errors, deferred, meta) = {
        DocumentId = documentId
        Content = Deferred (data, errors, deferred)
        Metadata = meta
    }

    /// <summary>
    /// Creates a subscription execution result.
    /// </summary>
    /// <param name="documentId">The identifier of the executed document inside the request batch.</param>
    /// <param name="data">The subscription payload stream.</param>
    /// <param name="meta">The execution metadata.</param>
    static member Stream (documentId, data, meta) = { DocumentId = documentId; Content = Stream data; Metadata = meta }

    /// <summary>
    /// Creates an execution result for a request rejected before execution.
    /// </summary>
    /// <param name="documentId">The identifier of the rejected document inside the request batch.</param>
    /// <param name="errors">The request errors.</param>
    /// <param name="meta">The execution metadata.</param>
    static member RequestError (documentId, errors, meta) = { DocumentId = documentId; Content = RequestError errors; Metadata = meta }

    /// <summary>
    /// Creates an empty direct execution result.
    /// </summary>
    /// <param name="documentId">The identifier of the executed document inside the request batch.</param>
    /// <param name="meta">The execution metadata.</param>
    static member Empty (documentId, meta) = GQLExecutionResult.Direct (documentId, Map.empty, [], meta)

    /// <summary>
    /// Creates a request-error execution result from problem details.
    /// </summary>
    /// <param name="documentId">The identifier of the rejected document inside the request batch.</param>
    /// <param name="errors">The request errors.</param>
    /// <param name="meta">The execution metadata.</param>
    static member Error (documentId, errors, meta) = GQLExecutionResult.RequestError (documentId, errors, meta)

    /// <summary>
    /// Creates a request-error execution result from a single problem detail.
    /// </summary>
    /// <param name="documentId">The identifier of the rejected document inside the request batch.</param>
    /// <param name="error">The request error.</param>
    /// <param name="meta">The execution metadata.</param>
    static member Error (documentId, error, meta) = GQLExecutionResult.RequestError (documentId, [ error ], meta)

    /// <summary>
    /// Creates a request-error execution result from a single GraphQL error.
    /// </summary>
    /// <param name="documentId">The identifier of the rejected document inside the request batch.</param>
    /// <param name="error">The GraphQL error.</param>
    /// <param name="meta">The execution metadata.</param>
    static member Error (documentId, error, meta) =
        GQLExecutionResult.RequestError (documentId, [ GQLProblemDetails.OfError error ], meta)

    /// <summary>
    /// Creates a request-error execution result from GraphQL errors.
    /// </summary>
    /// <param name="documentId">The identifier of the rejected document inside the request batch.</param>
    /// <param name="errors">The GraphQL errors.</param>
    /// <param name="meta">The execution metadata.</param>
    static member Error (documentId, errors, meta) =
        GQLExecutionResult.RequestError (documentId, errors |> List.map GQLProblemDetails.OfError, meta)

    /// <summary>
    /// Creates a request-error execution result from an error message.
    /// </summary>
    /// <param name="documentId">The identifier of the rejected document inside the request batch.</param>
    /// <param name="msg">The error message.</param>
    /// <param name="meta">The execution metadata.</param>
    static member Error (documentId, msg, meta) =
        GQLExecutionResult.RequestError (documentId, [ GQLProblemDetails.Create msg ], meta)

    /// <summary>
    /// Creates a request-error execution result from an exception.
    /// </summary>
    /// <param name="documentId">The identifier of the rejected document inside the request batch.</param>
    /// <param name="ex">The exception that caused the failure.</param>
    /// <param name="meta">The execution metadata.</param>
    static member ErrorFromException (documentId : int, ex : Exception, meta : Metadata) =
        GQLExecutionResult.RequestError (documentId, [ GQLProblemDetails.Create (ex.Message, ex) ], meta)

    /// <summary>
    /// Creates an invalid-request execution result.
    /// </summary>
    /// <param name="documentId">The identifier of the rejected document inside the request batch.</param>
    /// <param name="errors">The validation or request errors.</param>
    /// <param name="meta">The execution metadata.</param>
    static member Invalid (documentId, errors, meta) = GQLExecutionResult.RequestError (documentId, errors, meta)

    /// <summary>
    /// Creates an asynchronous request-error execution result from an error message.
    /// </summary>
    /// <param name="documentId">The identifier of the rejected document inside the request batch.</param>
    /// <param name="msg">The error message.</param>
    /// <param name="meta">The execution metadata.</param>
    static member ErrorAsync (documentId, msg : string, meta) = AsyncVal.wrap (GQLExecutionResult.Error (documentId, msg, meta))

    /// <summary>
    /// Creates an asynchronous request-error execution result from a single GraphQL error.
    /// </summary>
    /// <param name="documentId">The identifier of the rejected document inside the request batch.</param>
    /// <param name="error">The GraphQL error.</param>
    /// <param name="meta">The execution metadata.</param>
    static member ErrorAsync (documentId, error : IGQLError, meta) = AsyncVal.wrap (GQLExecutionResult.Error (documentId, error, meta))

/// <summary>
/// Represents the different execution-content shapes produced by the executor.
/// </summary>
and GQLResponseContent =
    /// <summary>
    /// The request was rejected before execution started.
    /// </summary>
    /// <remarks>
    /// There is no data, unlike <see cref="Direct"/>, whose data may legitimately be <see langword="null" /> after
    /// execution.
    /// </remarks>
    | RequestError of Errors : GQLProblemDetails list
    /// <summary>
    /// An execution result.
    /// </summary>
    /// <remarks>
    /// Data is <see langword="null" /> when a non-null root field failed during execution and the error propagated to
    /// the root, exactly as it would for a non-null nested field, rather than being rejected as
    /// <see cref="RequestError" />.
    /// </remarks>
    | Direct of Data : Output voption * Errors : GQLProblemDetails list
    /// <summary>
    /// An execution result with deferred follow-up payloads.
    /// </summary>
    | Deferred of Data : Output * Errors : GQLProblemDetails list * Defer : IObservable<GQLDeferredResponseContent>
    /// <summary>
    /// A subscription result stream.
    /// </summary>
    | Stream of Stream : IObservable<GQLSubscriptionResponseContent>

/// <summary>
/// One event of a <c>@defer</c> or <c>@stream</c> field's own delivery.
/// </summary>
/// <remarks>
/// <see cref="DeferredCompleted"/> fires once after a <c>@defer</c> field's own payload, and once after all of a
/// <c>@stream</c> field's items - whether they all succeeded or the source failed partway through - but never for
/// a <c>@live</c> field, which has no end of its own.
/// </remarks>
and GQLDeferredResponseContent =
    /// <summary>
    /// Announces a streamed field before any of its items are delivered.
    /// </summary>
    | DeferredPending of Path : FieldPath
    /// <summary>
    /// Delivers the data of a deferred field or one or more streamed items at the given path.
    /// </summary>
    | DeferredResult of Data : obj * Path : FieldPath
    /// <summary>
    /// Delivers partial data together with execution errors at the given path.
    /// </summary>
    | DeferredErrors of Data : obj * Errors : GQLProblemDetails list * Path : FieldPath
    /// <summary>
    /// Marks a deferred or streamed field as fully delivered.
    /// </summary>
    | DeferredCompleted of Path : FieldPath

/// <summary>
/// Represents events emitted by a live GraphQL subscription.
/// </summary>
and GQLSubscriptionResponseContent =
    /// <summary>
    /// Delivers a subscription data payload.
    /// </summary>
    | SubscriptionResult of Data : Output
    /// <summary>
    /// Delivers a subscription payload together with execution errors.
    /// </summary>
    | SubscriptionErrors of Data : Output voption * Errors : GQLProblemDetails list
