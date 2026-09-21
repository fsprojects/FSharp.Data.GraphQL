/// <summary>
/// Maps the failures of a subscription's source, and request errors, to the problem details a client is allowed to see: a GraphQL-facing error keeps
/// its message, anything else is replaced by a generic one so that backend exception messages never leak over the wire.
/// </summary>
module internal FSharp.Data.GraphQL.Server.AspNetCore.ObservableErrorHandling

open System
open System.Text.Json.Serialization

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Shared

[<Literal>]
let UnexpectedObservableErrorMessage = "Unexpected error during subscription"

let private deduplicationKey (problem : GQLProblemDetails) =
    let extensions =
        problem.Extensions
        |> Skippable.toValueOption
        |> ValueOption.map (
            Seq.sortBy _.Key
            >> Seq.map (fun kvp -> kvp.Key, kvp.Value)
            >> Seq.toList
        )

    struct (problem.Message, problem.Path, problem.Locations, extensions)

/// The problem details to report for a failure of a subscription's source, flattening aggregates and
/// deduplicating repeated errors.
let rec problemDetailsOfObservableError (ex : exn) =
    match ex with
    | :? AggregateException as aggregate ->
        let problemDetails =
            aggregate.Flatten().InnerExceptions
            |> Seq.collect problemDetailsOfObservableError
            |> Seq.distinctBy deduplicationKey
            |> Seq.toList

        match problemDetails with
        | [] -> [ GQLProblemDetails.Create UnexpectedObservableErrorMessage ]
        | _ -> problemDetails
    | _ ->
        match box ex with
        | :? IGQLError as error -> [ GQLProblemDetails.OfError error ]
        | _ -> [ GQLProblemDetails.Create UnexpectedObservableErrorMessage ]

/// A request error as reported to the client: unchanged when it is a GraphQL-facing error, replaced by the
/// generic message when it wraps a backend exception.
let sanitizeRequestError (problemDetails : GQLProblemDetails) =
    match
        problemDetails.Exception
        |> ValueOption.map box
        |> ValueOption.toObj
    with
    | :? IGQLError -> problemDetails
    | :? exn -> GQLProblemDetails.Create UnexpectedObservableErrorMessage
    | _ -> problemDetails
