/// <summary>
/// Masks the errors that unexpected exceptions caused before they reach a client, so that backend exception messages
/// never leak over HTTP or WebSocket, while the errors a schema reports on purpose keep their messages.
/// </summary>
/// <remarks>
/// See <see cref="GraphQLOptions{Root}.MaskUnexpectedErrors"/> for which errors count as unexpected.
/// </remarks>
module internal FSharp.Data.GraphQL.Server.AspNetCore.ErrorMasking

open System
open System.Collections.Generic
open System.Text.Json.Serialization
open Microsoft.Extensions.Logging

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Shared.WebSockets

/// The message a masked error is reported with.
[<Literal>]
let UnexpectedErrorMessage = "Unexpected error"

/// Whether the exception reports a GraphQL error on purpose, so that its message is meant for the client.
let rec isGraphQLFacingException (ex : exn) =
    match ex with
    | :? GraphQLException -> true
    | :? AggregateException as aggregate ->
        // The message of an aggregate embeds the messages of its inner exceptions, so it is only as safe as all of them
        let innerExceptions = aggregate.Flatten().InnerExceptions
        innerExceptions.Count > 0
        && innerExceptions |> Seq.forall isGraphQLFacingException
    | _ ->
        match box ex with
        | :? IGQLError -> true
        | _ -> false

/// Matches an error that an unexpected exception caused, giving that exception.
[<return : Struct>]
let (|CausedByUnexpectedException|_|) (problem : GQLProblemDetails) =
    match problem.Exception with
    | ValueSome ex when not (isGraphQLFacingException ex) -> ValueSome ex
    | _ -> ValueNone

let private isCausedByUnexpectedException (problem : GQLProblemDetails) =
    match problem with
    | CausedByUnexpectedException _ -> true
    | _ -> false

/// The extensions a masked error keeps: only the kind of error the engine attributed it to, which tells nothing about
/// the exception, unlike the extensions that came with the exception itself.
let private maskExtensions (extensions : IReadOnlyDictionary<string, obj> Skippable) =
    match extensions with
    | Include extensions ->
        match extensions.TryGetValue CustomErrorFields.Kind with
        | true, (:? ErrorKind as kind) -> Include (GQLProblemDetails.SetErrorKind kind (Dictionary<string, obj>(1, StringComparer.Ordinal)))
        | _ -> Skip
    | Skip -> Skip

/// <summary>
/// The error as a client may see it: unchanged, unless an unexpected exception caused it. Such an error gets
/// <see cref="UnexpectedErrorMessage"/>, keeps its path, its locations and its kind, and its exception is logged instead.
/// </summary>
let maskProblemDetails (logger : ILogger) (problem : GQLProblemDetails) =
    match problem with
    | CausedByUnexpectedException ex ->
        match problem.Path with
        | Include path ->
            logger.LogError (ex, "Masked an unexpected error at path '{path}' before sending it to the client", String.Join<obj>("/", path))
        | Skip -> logger.LogError (ex, "Masked an unexpected error before sending it to the client")
        {
            Message = UnexpectedErrorMessage
            // Dropped, so that a masked error passed through masking again is neither changed nor logged a second time
            Exception = ValueNone
            Path = problem.Path
            Locations = problem.Locations
            Extensions = maskExtensions problem.Extensions
        }
    | _ -> problem

/// <summary>The errors as a client may see them; see <see cref="maskProblemDetails"/>.</summary>
let maskErrors (logger : ILogger) (errors : GQLProblemDetails list) =
    // Nothing to mask in the common case: the list is kept instead of copied
    if errors |> List.exists isCausedByUnexpectedException then
        errors |> List.map (maskProblemDetails logger)
    else
        errors

let private maskSkippableErrors (logger : ILogger) (errors : GQLProblemDetails list Skippable) = errors |> Skippable.map (maskErrors logger)

/// <summary>The <c>next</c> payload as a client may see it: its own errors and those of its incremental and completed entries masked.</summary>
let maskExecutionResult (logger : ILogger) (payload : SubscriptionExecutionResult) = {
    payload with
        Errors = payload.Errors |> maskSkippableErrors logger
        Incremental =
            payload.Incremental
            |> Skippable.map (List.map (fun entry -> { entry with Errors = entry.Errors |> maskSkippableErrors logger }))
        Completed =
            payload.Completed
            |> Skippable.map (List.map (fun entry -> { entry with Errors = entry.Errors |> maskSkippableErrors logger }))
}

/// <summary>The <c>graphql-transport-ws</c> message as a client may see it: the errors of a <c>next</c> or <c>error</c> message masked.</summary>
let maskServerMessage (logger : ILogger) (message : ServerMessage) =
    match message with
    | Next (id, payload) -> Next (id, maskExecutionResult logger payload)
    | ServerError (id, errors) -> ServerError (id, maskErrors logger errors)
    | ConnectionAck
    | ServerPing
    | ServerPong _
    | Complete _ -> message
