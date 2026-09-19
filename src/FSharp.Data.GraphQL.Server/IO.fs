namespace FSharp.Data.GraphQL

open System
open System.Collections.Generic
open System.Text.Json.Serialization
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Extensions
open FSharp.Data.GraphQL.Types

/// Represents a GraphQL response object keyed by field name.
type Output = IDictionary<string, obj>

/// Represents the serialized shape of a GraphQL response document.
type GQLResponse = {
    /// Gets the identifier of the executed document inside the request batch.
    DocumentId : int
    /// Gets the response data.
    Data : Skippable<Output voption>
    /// Gets the response errors.
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

/// Represents the executor output together with request metadata.
type GQLExecutionResult = {
    /// Gets the identifier of the executed document inside the request batch.
    DocumentId : int
    /// Gets the execution content.
    Content : GQLResponseContent
    /// Gets the execution metadata.
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

/// Represents the different execution-content shapes produced by the executor.
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
    /// An execution result with deferred follow-up payloads.
    | Deferred of Data : Output * Errors : GQLProblemDetails list * Defer : IObservable<GQLDeferredResponseContent>
    /// A subscription result stream.
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
    /// Announces a deferred or streamed field before later payloads need to refer to it.
    | DeferredPending of Path : FieldPath * Label : string voption * IsStream : bool
    /// Delivers the data of a deferred field or one or more streamed items at the given path.
    | DeferredResult of Data : obj * Path : FieldPath
    /// Delivers partial data together with execution errors at the given path.
    | DeferredErrors of Data : obj * Errors : GQLProblemDetails list * Path : FieldPath
    /// Marks a deferred or streamed field as fully delivered.
    | DeferredCompleted of Path : FieldPath

/// Represents events emitted by a live GraphQL subscription.
and GQLSubscriptionResponseContent =
    /// Delivers a subscription data payload.
    | SubscriptionResult of Data : Output
    /// Delivers a subscription payload together with execution errors.
    | SubscriptionErrors of Data : Output voption * Errors : GQLProblemDetails list
