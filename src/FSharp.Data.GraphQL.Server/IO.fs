namespace FSharp.Data.GraphQL

open System
open System.Collections.Generic
open System.Text.Json.Serialization
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Extensions
open FSharp.Data.GraphQL.Types
open R3

type Output = IDictionary<string, obj>

type GQLResponse =
    { DocumentId: int
      Data : Output Skippable
      Errors : GQLProblemDetails list Skippable }
    static member Direct(documentId, data, errors) =
        { DocumentId = documentId
          Data = Include data
          Errors = Skippable.ofList errors }
    static member Stream(documentId) =
        { DocumentId = documentId
          Data = Include null
          Errors = Skip }
    static member RequestError(documentId, errors) =
        { DocumentId = documentId
          Data = Skip
          Errors = Include errors }

type GQLExecutionResult =
    { DocumentId: int
      Content : GQLResponseContent
      Metadata : Metadata }
    static member Direct(documentId, data, errors, meta) =
        { DocumentId = documentId
          Content = Direct (data, errors)
          Metadata = meta }
    static member Deferred(documentId, data, errors, deferred, meta) =
        { DocumentId = documentId
          Content = Deferred (data, errors, deferred)
          Metadata = meta }
    static member Stream(documentId, data, meta) =
        { DocumentId = documentId
          Content = Stream data
          Metadata = meta }
    static member RequestError(documentId, errors, meta) =
        { DocumentId = documentId
          Content = RequestError errors
          Metadata = meta }
    static member Empty(documentId, meta) =
        GQLExecutionResult.Direct(documentId, Map.empty, [], meta)
    static member Error(documentId, errors, meta) =
        GQLExecutionResult.RequestError(documentId, errors, meta)
    static member Error(documentId, error, meta) =
        GQLExecutionResult.RequestError(documentId, [ error ], meta)
    static member Error(documentId, error, meta) =
        GQLExecutionResult.RequestError(documentId, [ GQLProblemDetails.OfError error ], meta)
    static member Error(documentId, errors, meta) =
        GQLExecutionResult.RequestError(documentId, errors |> List.map GQLProblemDetails.OfError, meta)
    static member Error(documentId, msg, meta) =
        GQLExecutionResult.RequestError(documentId, [ GQLProblemDetails.Create msg ], meta)

    static member ErrorFromException(documentId : int, ex : Exception, meta : Metadata) =
        GQLExecutionResult.RequestError(documentId, [ GQLProblemDetails.Create (ex.Message, ex) ], meta)

    static member Invalid(documentId, errors, meta) =
        GQLExecutionResult.RequestError(documentId, errors, meta)
    static member ErrorAsync(documentId, msg : string, meta) =
        AsyncVal.wrap (GQLExecutionResult.Error (documentId, msg, meta))
    static member ErrorAsync(documentId, error : IGQLError, meta) =
        AsyncVal.wrap (GQLExecutionResult.Error (documentId, error, meta))

// TODO: Rename to PascalCase
and GQLResponseContent =
    | RequestError of Errors: GQLProblemDetails list
    | Direct of Data : Output * Errors: GQLProblemDetails list
    | Deferred of Data : Output * Errors : GQLProblemDetails list * Defer : Observable<GQLDeferredResponseContent>
    | Stream of Stream : Observable<GQLSubscriptionResponseContent>

and GQLDeferredResponseContent =
    | DeferredResult of Data : obj * Path : FieldPath
    | DeferredErrors of Data : obj * Errors: GQLProblemDetails list * Path : FieldPath

and GQLSubscriptionResponseContent =
    | SubscriptionResult of Data : Output
    | SubscriptionErrors of Data : Output * Errors: GQLProblemDetails list
