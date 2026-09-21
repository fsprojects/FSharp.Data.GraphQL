namespace FSharp.Data.GraphQL.Server.AspNetCore

open System
open System.Reactive
open System.Reactive.Disposables
open System.Threading
open System.Threading.Channels
open System.Threading.Tasks
open Microsoft.Extensions.Logging

open FSharp.Data.GraphQL.Shared.WebSockets
open FSharp.Data.GraphQL.Server.AspNetCore.ObservableErrorHandling

/// A running subscription of a connection, as its control loop tracks it.
type internal SubscriptionHandle = {
    /// The id the client gave the subscription.
    Id : SubscriptionId
    /// Distinguishes this subscription from an earlier or later one the client gave the same id.
    Generation : int
    /// Cancels the worker: on a client `complete`, or when the connection ends.
    Cancellation : CancellationTokenSource
    /// The worker's run.
    Worker : Task
}

/// <summary>
/// Delivers one subscription: subscribes to its source, translates every event into the subscription's messages and queues them for the connection's
/// sender, then reports its end to the connection's control loop.
/// </summary>
/// <remarks>
/// <para>
/// The source's observer only queues events into a channel this worker is the single reader of, so the observer never blocks, and the
/// translation state is touched by one loop only. The initial payload goes out lazily: after every announcement the source produces synchronously on
/// subscription has been absorbed, right before the first event that is not one, or as soon as the queue is empty. A synchronous completion or
/// failure of the source is therefore still preceded by the initial payload.
/// </para>
/// <para>
/// Cancellation ends the worker silently: nothing further is sent, the source is unsubscribed, and the end is reported. The connection's sender
/// queue is never completed before every worker has ended, so a queued message is never lost.
/// </para>
/// </remarks>
type internal SubscriptionWorker<'T>
    /// <param name="id">The id the client gave the subscription.</param>
    /// <param name="generation">Distinguishes this subscription from an earlier or later one the client gave the same id.</param>
    /// <param name="source">The events of the subscription, as the executor produces them.</param>
    /// <param name="payloads">Translates the events into the payloads of the subscription's <c>next</c> messages.</param>
    /// <param name="outbound">The connection's sender queue, where every message of the subscription is written.</param>
    /// <param name="inbox">The connection's control loop queue, where the end of the subscription is reported.</param>
    /// <param name="logger">The logger of the connection.</param>
    (
        id : SubscriptionId,
        generation : int,
        source : IObservable<'T>,
        payloads : ISubscriptionPayloads<'T>,
        outbound : ChannelWriter<OutboundMessage>,
        inbox : ChannelWriter<ConnectionEvent>,
        logger : ILogger
    ) =

    /// Runs the subscription until its source completes or fails, or the token is cancelled.
    member _.RunAsync (cancellationToken : CancellationToken) : Task = backgroundTask {
        let events =
            Channel.CreateUnbounded<SubscriptionEvent<'T>>(
                UnboundedChannelOptions (SingleReader = true, SingleWriter = false, AllowSynchronousContinuations = false)
            )
        let post (event : SubscriptionEvent<'T>) = events.Writer.TryWrite event |> ignore
        let observer =
            new AnonymousObserver<'T> (
                onNext = (fun item -> post (Item item)),
                onError = (fun ex -> post (Faulted ex)),
                onCompleted = (fun () -> post Completed)
            )
        let sendMessage (message : ServerMessage) = outbound.TryWrite (Send message) |> ignore
        let sendPayload (payload : SubscriptionExecutionResult) = sendMessage (Next (id, payload))
        let mutable initialSent = false
        let mutable finished = false
        let sendInitialOnce () =
            if not initialSent then
                initialSent <- true
                payloads.Initial () |> ValueOption.iter sendPayload
        let handle event =
            match event with
            | Item item when not initialSent && payloads.TryAbsorbBeforeInitial item -> ()
            | Item item ->
                sendInitialOnce ()
                payloads.Translate item |> ValueOption.iter sendPayload
            | Completed ->
                sendInitialOnce ()
                payloads.Final () |> ValueOption.iter sendPayload
                sendMessage (Complete id)
                finished <- true
            | Faulted ex ->
                sendInitialOnce ()
                logger.LogError (ex, "Error on subscription with Id = '{id}'", id)
                sendMessage (ServerError (id, problemDetailsOfObservableError ex))
                finished <- true
        use subscription = new SingleAssignmentDisposable ()
        try
            try
                // Whatever the source produces synchronously while subscribing lands in the queue before the loop starts
                subscription.Disposable <- source.Subscribe observer
                while not finished do
                    match events.Reader.TryRead () with
                    | true, event -> handle event
                    | false, _ ->
                        sendInitialOnce ()
                        let! _ = events.Reader.WaitToReadAsync cancellationToken
                        ()
            with
            | :? OperationCanceledException when cancellationToken.IsCancellationRequested -> ()
            | ex ->
                // Subscribing threw, or a translation did: reported as the subscription's terminal error
                logger.LogError (ex, "Error on subscription with Id = '{id}'", id)
                sendMessage (ServerError (id, problemDetailsOfObservableError ex))
        finally
            // Unsubscribed before the end is reported, so the id is freed only once the source has stopped
            try
                subscription.Dispose ()
            with ex ->
                logger.LogError (ex, "Disposing the source of subscription with Id = '{id}' failed", id)
            inbox.TryWrite (SubscriptionEnded (id, generation))
            |> ignore
    }
