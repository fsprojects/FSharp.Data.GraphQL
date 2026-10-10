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

/// The most paths of masked errors that the log entry of a response or message lists
[<Literal>]
let private MaxLoggedPaths = 10

/// <summary>
/// The message meant for the client of an exception that reports a GraphQL error on purpose, or nothing for an unexpected
/// exception.
/// </summary>
/// <remarks>
/// An <see cref="IGQLError"/> declares its message for clients, which can differ from the message of the exception. The
/// message of an aggregate embeds the messages of its inner exceptions, so it is meant for the client only when each of
/// them is, as it is.
/// </remarks>
let rec private clientMessageOf (ex : exn) : string voption =
    match ex with
    | :? AggregateException as aggregate ->
        let innerExceptions = aggregate.Flatten().InnerExceptions
        let isMeantForClient (inner : exn) =
            match clientMessageOf inner with
            | ValueSome message -> String.Equals (message, inner.Message, StringComparison.Ordinal)
            | ValueNone -> false
        if innerExceptions.Count > 0 && innerExceptions |> Seq.forall isMeantForClient then
            ValueSome aggregate.Message
        else
            ValueNone
    | :? GraphQLException -> ValueSome ex.Message
    | _ ->
        match box ex with
        | :? IGQLError as error -> ValueSome error.Message
        | _ -> ValueNone

/// Whether the exception reports a GraphQL error on purpose, so that a message is meant for the client.
let isGraphQLFacingException (ex : exn) = (clientMessageOf ex).IsSome

/// <summary>
/// Whether the error is reported by the message of its exception while the exception declares another one for clients,
/// which is what the default <c>ParseError</c> of a schema does with an <see cref="IGQLError"/> exception.
/// </summary>
let private isReportedByExceptionMessage (problem : GQLProblemDetails) (ex : exn) (clientMessage : string) =
    String.Equals (problem.Message, ex.Message, StringComparison.Ordinal)
    && not (String.Equals (clientMessage, ex.Message, StringComparison.Ordinal))

/// Whether masking changes the error
let private needsMasking (problem : GQLProblemDetails) =
    match problem.Exception with
    | ValueNone -> false
    | ValueSome ex ->
        match clientMessageOf ex with
        | ValueSome clientMessage -> isReportedByExceptionMessage problem ex clientMessage
        | ValueNone -> true

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
/// The unexpected exceptions masked in one response or message, logged together once it is masked.
/// </summary>
/// <remarks>
/// A client controls how many errors a response holds, through aliases or a list whose items fail, so an entry per error
/// would let it multiply the error entries of the log at will. A response is therefore logged once at the error level,
/// with the number of masked errors, their first paths and the first exception; each error is logged at the debug level.
/// </remarks>
[<Sealed>]
type private MaskedExceptions
    /// <param name="logger">The logger of the response or message.</param>
    (logger : ILogger) =
    let paths = ResizeArray<string> ()
    let mutable count = 0
    let mutable first : exn voption = ValueNone

    /// Records the exception masked in the error at the path
    member _.Add (ex : exn, path : obj list Skippable) =
        let path =
            match path with
            | Include path -> String.Join<obj> ("/", path)
            | Skip -> "(none)"
        count <- count + 1
        if first.IsValueNone then
            first <- ValueSome ex
        if paths.Count < MaxLoggedPaths then
            paths.Add path
        logger.LogDebug (ex, "Masked an unexpected error at path '{path}' before sending it to the client", path)

    /// Logs the exceptions masked in the response or message, if any
    member _.Log () =
        match first with
        | ValueNone -> ()
        | ValueSome first ->
            let more = if count > paths.Count then ", ..." else ""
            logger.LogError (
                first,
                "Masked {count} unexpected error(s) before sending them to the client, at paths {paths}{more}; the first exception follows",
                count,
                String.Join (", ", paths),
                more
            )

/// <summary>
/// The error as a client may see it: unchanged, unless an unexpected exception caused it. Such an error gets
/// <see cref="UnexpectedErrorMessage"/>, keeps its path, its locations and its kind, and its exception is recorded for the
/// log instead. An error of an <see cref="IGQLError"/> exception reported by the message of the exception gets the message
/// the exception declares for clients, as when the exception fails the source of a subscription.
/// </summary>
let private maskProblemDetailsInto (masked : MaskedExceptions) (problem : GQLProblemDetails) =
    match problem.Exception with
    | ValueNone -> problem
    | ValueSome ex ->
        match clientMessageOf ex with
        | ValueNone ->
            masked.Add (ex, problem.Path)
            {
                Message = UnexpectedErrorMessage
                // Dropped, so that a masked error passed through masking again is neither changed nor logged a second time
                Exception = ValueNone
                Path = problem.Path
                Locations = problem.Locations
                Extensions = maskExtensions problem.Extensions
            }
        | ValueSome clientMessage when isReportedByExceptionMessage problem ex clientMessage -> { problem with Message = clientMessage }
        | ValueSome _ -> problem

/// <summary>The error as a client may see it, its masked exception logged; see <see cref="maskProblemDetailsInto"/>.</summary>
let maskProblemDetails (logger : ILogger) (problem : GQLProblemDetails) =
    let masked = MaskedExceptions logger
    let problem = maskProblemDetailsInto masked problem
    masked.Log ()
    problem

let private maskErrorsInto (masked : MaskedExceptions) (errors : GQLProblemDetails list) =
    // Nothing to mask in the common case: the list is kept instead of copied
    if errors |> List.exists needsMasking then
        errors |> List.map (maskProblemDetailsInto masked)
    else
        errors

let private maskSkippableErrorsInto (masked : MaskedExceptions) (errors : GQLProblemDetails list Skippable) =
    errors |> Skippable.map (maskErrorsInto masked)

let private hasErrorsToMask (errors : GQLProblemDetails list Skippable) =
    match errors with
    | Include errors -> errors |> List.exists needsMasking
    | Skip -> false

/// <summary>
/// The errors as a client may see them, the exceptions masked in them logged together; see
/// <see cref="maskProblemDetailsInto"/>.
/// </summary>
let maskErrors (logger : ILogger) (errors : GQLProblemDetails list) =
    if errors |> List.exists needsMasking then
        let masked = MaskedExceptions logger
        let errors = maskErrorsInto masked errors
        masked.Log ()
        errors
    else
        errors

/// <summary>
/// The <c>next</c> payload as a client may see it: its own errors and those of its incremental and completed entries
/// masked, the exceptions masked in them logged together.
/// </summary>
let maskExecutionResult (logger : ILogger) (payload : SubscriptionExecutionResult) =
    let entriesHaveErrorsToMask (entries : 'Entry list Skippable) (errorsOf : 'Entry -> GQLProblemDetails list Skippable) =
        match entries with
        | Include entries -> entries |> List.exists (errorsOf >> hasErrorsToMask)
        | Skip -> false
    // Nothing to mask in the common case: the payload is kept instead of copied
    if
        hasErrorsToMask payload.Errors
        || entriesHaveErrorsToMask payload.Incremental _.Errors
        || entriesHaveErrorsToMask payload.Completed _.Errors
    then
        let masked = MaskedExceptions logger
        let payload = {
            payload with
                Errors = payload.Errors |> maskSkippableErrorsInto masked
                Incremental =
                    payload.Incremental
                    |> Skippable.map (List.map (fun entry -> { entry with Errors = entry.Errors |> maskSkippableErrorsInto masked }))
                Completed =
                    payload.Completed
                    |> Skippable.map (List.map (fun entry -> { entry with Errors = entry.Errors |> maskSkippableErrorsInto masked }))
        }
        masked.Log ()
        payload
    else
        payload

/// <summary>The <c>graphql-transport-ws</c> message as a client may see it: the errors of a <c>next</c> or <c>error</c> message masked.</summary>
let maskServerMessage (logger : ILogger) (message : ServerMessage) =
    match message with
    | Next (id, payload) -> Next (id, maskExecutionResult logger payload)
    | ServerError (id, errors) -> ServerError (id, maskErrors logger errors)
    | ConnectionAck
    | ServerPing
    | ServerPong _
    | Complete _ -> message
