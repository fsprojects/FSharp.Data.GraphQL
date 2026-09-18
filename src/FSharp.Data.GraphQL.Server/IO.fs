namespace FSharp.Data.GraphQL

open System
open System.Collections.Generic
open System.Text.Json.Serialization
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Extensions
open FSharp.Data.GraphQL.Types

type Output = IDictionary<string, obj>

type GQLResponse = {
    DocumentId : int
    Data : Output voption Skippable
    Errors : GQLProblemDetails list Skippable
} with

    static member Direct (documentId, data : Output | null, errors) = {
        DocumentId = documentId
        Data = Include (Option.ofObj data |> ValueOption.ofOption)
        Errors = Skippable.ofList errors
    }
    static member Stream (documentId) = { DocumentId = documentId; Data = Include ValueNone; Errors = Skip }
    static member RequestError (documentId, errors) = { DocumentId = documentId; Data = Skip; Errors = Include errors }

type GQLExecutionResult = {
    DocumentId : int
    Content : GQLResponseContent
    Metadata : Metadata
} with

    static member Direct (documentId, data : Output | null, errors, meta) = {
        DocumentId = documentId
        Content = Direct (Option.ofObj data |> ValueOption.ofOption, errors)
        Metadata = meta
    }
    static member Deferred (documentId, data, errors, deferred, meta) = {
        DocumentId = documentId
        Content = Deferred (data, errors, deferred)
        Metadata = meta
    }
    static member Stream (documentId, data, meta) = { DocumentId = documentId; Content = Stream data; Metadata = meta }
    static member RequestError (documentId, errors, meta) = { DocumentId = documentId; Content = RequestError errors; Metadata = meta }
    static member Empty (documentId, meta) = GQLExecutionResult.Direct (documentId, Map.empty, [], meta)
    static member Error (documentId, errors, meta) = GQLExecutionResult.RequestError (documentId, errors, meta)
    static member Error (documentId, error, meta) = GQLExecutionResult.RequestError (documentId, [ error ], meta)
    static member Error (documentId, error, meta) =
        GQLExecutionResult.RequestError (documentId, [ GQLProblemDetails.OfError error ], meta)
    static member Error (documentId, errors, meta) =
        GQLExecutionResult.RequestError (documentId, errors |> List.map GQLProblemDetails.OfError, meta)
    static member Error (documentId, msg, meta) =
        GQLExecutionResult.RequestError (documentId, [ GQLProblemDetails.Create msg ], meta)

    static member ErrorFromException (documentId : int, ex : Exception, meta : Metadata) =
        GQLExecutionResult.RequestError (documentId, [ GQLProblemDetails.Create (ex.Message, ex) ], meta)

    static member Invalid (documentId, errors, meta) = GQLExecutionResult.RequestError (documentId, errors, meta)
    static member ErrorAsync (documentId, msg : string, meta) = AsyncVal.wrap (GQLExecutionResult.Error (documentId, msg, meta))
    static member ErrorAsync (documentId, error : IGQLError, meta) = AsyncVal.wrap (GQLExecutionResult.Error (documentId, error, meta))

// TODO: Rename to PascalCase
and GQLResponseContent =
    /// The request was rejected before execution started: validation, planning, variable or inline argument
    /// coercion, a middleware, or the executor itself failing. There is no data, unlike a Direct result whose
    /// data happens to be null.
    | RequestError of Errors : GQLProblemDetails list
    /// An execution result. Data is null when a non-null root field failed during execution and the error
    /// propagated to the root, exactly as it would for a non-null nested field, rather than being rejected as a
    /// RequestError.
    | Direct of Data : Output voption * Errors : GQLProblemDetails list
    | Deferred of Data : Output * Errors : GQLProblemDetails list * Defer : IObservable<GQLDeferredResponseContent>
    | Stream of Stream : IObservable<GQLSubscriptionResponseContent>

and GQLDeferredResponseContent =
    | DeferredResult of Data : obj * Path : FieldPath
    | DeferredErrors of Data : obj voption * Errors : GQLProblemDetails list * Path : FieldPath

and GQLSubscriptionResponseContent =
    | SubscriptionResult of Data : Output
    | SubscriptionErrors of Data : Output voption * Errors : GQLProblemDetails list
