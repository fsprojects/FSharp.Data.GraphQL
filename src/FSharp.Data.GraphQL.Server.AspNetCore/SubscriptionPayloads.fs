namespace FSharp.Data.GraphQL.Server.AspNetCore

open System.Text
open Microsoft.Extensions.Logging

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Shared.WebSockets

/// An event of a subscription's source, as its observer queues it for the subscription worker.
[<Struct>]
type internal SubscriptionEvent<'T> =
    /// The source produced a value.
    | Item of item : 'T
    /// The source failed.
    | Faulted of failure : exn
    /// The source completed.
    | Completed

/// <summary>
/// Translates the events of one subscription's source into the payloads of its <c>next</c> messages. Called only by that subscription's worker, one
/// event at a time, so an implementation needs no synchronization.
/// </summary>
type internal ISubscriptionPayloads<'T> =
    /// <summary>
    /// Absorbs an event that may precede the initial payload without producing a message, returning <see langword="false"/> when the event is not
    /// such an event.
    /// </summary>
    abstract TryAbsorbBeforeInitial : event : 'T -> bool

    /// The initial payload, sent once before the first event that could not be absorbed; none for a source whose
    /// events are complete results of their own.
    abstract Initial : unit -> SubscriptionExecutionResult voption

    /// The payload the event produces, if any.
    abstract Translate : event : 'T -> SubscriptionExecutionResult voption

    /// <summary>
    /// The payload sent once the source completed, right before the <c>complete</c> message, if any.
    /// </summary>
    abstract Final : unit -> SubscriptionExecutionResult voption

module private ErrorFormatting =

    /// One line per error, as a bulleted list for the log
    let formatErrors (errors : GQLProblemDetails list) =
        let builder = StringBuilder ()

        for error in errors do
            builder
            |> _.Append("- ")
            |> _.Append(error.Message)
            |> _.Append('\n')
            |> ignore

        if builder.Length > 0 then
            builder.Length <- builder.Length - 1 // Remove the last newline

        builder.ToString ()

/// <summary>
/// The payloads of a deferred result: the initial payload with the announcements visible in its data, then every deferred or streamed delivery in the
/// incremental wire format, and finally the payload that reports <c>hasNext: false</c>.
/// </summary>
type internal DeferredPayloads
    /// <param name="logger">The logger of the connection.</param>
    /// <param name="data">The data of the initial payload.</param>
    /// <param name="errors">The errors of the initial payload.</param>
    (logger : ILogger, data : Output, errors : GQLProblemDetails list) =

    let delivery = IncrementalDelivery ()

    interface ISubscriptionPayloads<GQLDeferredResponseContent> with

        /// <inheritdoc />
        member _.TryAbsorbBeforeInitial event =
            match event with
            | DeferredPending _
            | DeferredFragmentPending _ ->
                // Announced before the initial payload, so it can be part of its pending entries
                delivery.Apply event |> ignore
                true
            | _ -> false

        /// <inheritdoc />
        member _.Initial () =
            ValueSome (SubscriptionExecutionResult.CreateInitial (data, errors, delivery.TakePendingVisibleIn data))

        /// <inheritdoc />
        member _.Translate event =
            match event with
            | DeferredErrors (_, errors, _) -> logger.LogWarning ("Deferred response errors: {deferredErrors}", ErrorFormatting.formatErrors errors)
            | _ -> ()

            delivery.Apply event

        /// <inheritdoc />
        member _.Final () = ValueSome (delivery.Finish ())

/// <summary>
/// The payloads of a subscription stream: every event is a complete result of its own.
/// </summary>
type internal StreamPayloads
    /// <param name="logger">The logger of the connection.</param>
    (logger : ILogger) =

    interface ISubscriptionPayloads<GQLSubscriptionResponseContent> with

        /// <inheritdoc />
        member _.TryAbsorbBeforeInitial _ = false

        /// <inheritdoc />
        member _.Initial () = ValueNone

        /// <inheritdoc />
        member _.Translate event =
            match event with
            | SubscriptionResult output -> ValueSome (SubscriptionExecutionResult.Create (ValueSome output, []))
            | SubscriptionErrors (output, errors) ->
                logger.LogWarning ("Subscription errors: {subscriptionErrors}", ErrorFormatting.formatErrors errors)
                // The executor may still have resolved partial data alongside the field errors; it is forwarded as-is
                match output with
                | ValueNone -> ValueSome (SubscriptionExecutionResult.CreateErrors errors)
                | ValueSome output -> ValueSome (SubscriptionExecutionResult.Create (ValueSome output, errors))

        /// <inheritdoc />
        member _.Final () = ValueNone
