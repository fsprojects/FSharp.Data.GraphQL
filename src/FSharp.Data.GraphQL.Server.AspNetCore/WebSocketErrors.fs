/// <summary>
/// Maps the failures of a subscription's source to the problem details a client is allowed to see: a GraphQL-facing error keeps
/// its message, anything else is replaced by a generic one while errors are masked, so that backend exception messages never leak over the wire.
/// </summary>
module internal FSharp.Data.GraphQL.Server.AspNetCore.ObservableErrorHandling

open System
open System.Text.Json.Serialization

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Shared
open FSharp.Data.GraphQL.Server.AspNetCore.ErrorMasking

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
/// deduplicating repeated errors. An exception that is not GraphQL-facing keeps its message only when
/// maskUnexpectedErrors is false; the caller logs it either way, so the problem details do not carry it.
let rec problemDetailsOfObservableError (maskUnexpectedErrors : bool) (ex : exn) =
    match ex with
    | :? AggregateException as aggregate ->
        let problemDetails =
            aggregate.Flatten().InnerExceptions
            |> Seq.collect (problemDetailsOfObservableError maskUnexpectedErrors)
            |> Seq.distinctBy deduplicationKey
            |> Seq.toList

        match problemDetails with
        | [] -> [ GQLProblemDetails.Create UnexpectedObservableErrorMessage ]
        | _ -> problemDetails
    | _ ->
        match box ex with
        | :? IGQLError as error -> [ GQLProblemDetails.OfError error ]
        | _ when maskUnexpectedErrors && not (isGraphQLFacingException ex) -> [ GQLProblemDetails.Create UnexpectedObservableErrorMessage ]
        | _ -> [ GQLProblemDetails.Create ex.Message ]
