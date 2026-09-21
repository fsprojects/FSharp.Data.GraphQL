namespace FSharp.Data.GraphQL

open System
open System.Collections.Generic
open System.Reactive.Linq
open System.Runtime.ExceptionServices
open System.Threading
open System.Threading.Channels
open System.Threading.Tasks
open FSharp.Control.Reactive.Observable

/// <summary>
/// An outcome of the resolution loop of <see cref="ofAsyncEnumerableResolved"/>, consumed by its single emitter.
/// </summary>
[<Struct>]
type internal ResolutionEvent<'Result> =
    /// A resolution produced its result.
    | Resolved of result : 'Result
    /// A resolution threw instead of producing a result.
    | ResolutionFailed of failure : exn
    /// The pull loop ended: how many resolutions it started in total, immediate results included, and the failures of
    /// the enumeration itself and of disposing the enumerator, if any.
    | EnumerationEnded of started : int * enumerationFailure : exn voption * disposalFailure : exn voption

/// Extension methods to observable, used in place of FSharp.Control.Observable
module internal Observable =

    let ofAsyncVal x = x |> AsyncVal.toAsync |> ofAsync

    let toSeq (o : IObservable<'T>) : 'T seq = Observable.ToEnumerable (o)

    /// Projects each element of an observable sequence into consecutive non-overlapping buffers
    /// which are produced based on timing information.
    let bufferMilliseconds (ms : int) x =
        let span = TimeSpan.FromMilliseconds (float ms)
        Observable.Buffer (x, span)

    /// Projects each element of an observable sequence into consecutive non-overlapping buffers
    /// which are produced based on timing and element count information.
    let bufferMillisecondsCount (ms : int) (count : int) x =
        let span = TimeSpan.FromMilliseconds (float ms)
        Observable.Buffer (x, span, count)

    let ofAsyncSeq (items : Async<'Item> seq) = items |> Seq.map ofAsync |> Observable.Merge

    let ofAsyncValSeq (items : AsyncVal<'Item> seq) = items |> Seq.map ofAsyncVal |> Observable.Merge

    let singleton (value : 'T) = {
        new IObservable<'T> with
            member _.Subscribe (observer) =
                observer.OnNext value
                observer.OnCompleted ()
                {
                    new IDisposable with
                        member _.Dispose () = ()
                }
    }

    /// <summary>
    /// Disposes the enumerator, if one was acquired, and returns the failure to report: the one captured while
    /// enumerating, or the one raised by the disposal itself when there was none before.
    /// </summary>
    let internal disposeEnumerator (enumerator : IAsyncEnumerator<'T> voption) (failure : exn voption) : Task<exn voption> = task {
        match enumerator with
        | ValueNone -> return failure
        | ValueSome enumerator ->
            try
                do! enumerator.DisposeAsync ()
                return failure
            with ex ->
                // An enumeration failure is the more useful one to report, the disposal failure is likely its consequence
                return failure |> ValueOption.orElse (ValueSome ex)
    }

    /// <summary>
    /// Creates a cold observable, which enumerates the asynchronous sequence for every subscription.
    /// </summary>
    /// <remarks>
    /// Disposing the subscription cancels the enumeration and disposes the enumerator.
    /// An exception raised by the sequence, when acquiring or disposing its enumerator as well as while enumerating,
    /// is delivered through <see cref="IObserver{T}.OnError"/>.
    /// </remarks>
    let ofAsyncEnumerable (source : IAsyncEnumerable<'T>) : IObservable<'T> =
        // backgroundTask, not task: the enumeration is started by Observable.Create on the subscriber's thread, and
        // a subscriber's synchronization context must neither be captured by the loop nor be needed to pump it
        let enumerate (observer : IObserver<'T>) (cancellationToken : CancellationToken) : Task = backgroundTask {
            let mutable enumerator = ValueNone
            let mutable enumerationFailure = ValueNone
            try
                // Acquired inside the try, because a source may throw when asked for its enumerator
                let acquired = source.GetAsyncEnumerator cancellationToken
                enumerator <- ValueSome acquired
                let mutable hasNext = true
                // The token is checked explicitly, because a sequence is not obliged to observe the token it was given
                while hasNext && not cancellationToken.IsCancellationRequested do
                    let! moved = acquired.MoveNextAsync ()
                    if moved then
                        observer.OnNext acquired.Current
                    else
                        hasNext <- false
            with ex ->
                enumerationFailure <- ValueSome ex
            let! failure = disposeEnumerator enumerator enumerationFailure
            match failure with
            // A failure caused by disposing the subscription has no observer left to be delivered to
            | ValueSome ex when not cancellationToken.IsCancellationRequested -> observer.OnError ex
            | _ -> ()
        }
        Observable.Create<'T>(Func<IObserver<'T>, CancellationToken, Task>(fun observer cancellationToken -> enumerate observer cancellationToken))

    /// <summary>
    /// Enumerates the sequence, resolving each item into a result with <paramref name="resolve"/>. At most
    /// <paramref name="maxConcurrency"/> items are pulled from the source and resolved at the same time: once that
    /// many resolutions are in flight, pulling the next item waits for one of them to be emitted.
    /// </summary>
    /// <remarks>
    /// <para>
    /// A result produced synchronously is emitted immediately, keeping it in the order it was pulled.
    /// </para>
    /// <para>
    /// An exception raised by the source, when acquiring or disposing its enumerator as well as while enumerating,
    /// is turned into a result with <paramref name="onFailure"/> and emitted only after every item pulled before it,
    /// so it can never overtake a result that is still being resolved. A resolution whose computation throws — as
    /// opposed to returning a result that merely carries errors, which <paramref name="resolve"/> is free to keep
    /// resolving items after — stops the enumeration the same way and is delivered through
    /// <paramref name="onFailure"/> once every resolution already started has settled; no item pulled after such a
    /// failure is resolved. Only the resolutions in flight are tracked, so a long-running source does not retain
    /// what it already delivered.
    /// </para>
    /// <para>
    /// A concurrency slot is held from the moment an item is pulled until its result has been delivered to the
    /// observer, so a slow observer bounds the enumeration, and a <see cref="SemaphoreSlim"/> is the only
    /// synchronization primitive involved: results reach the observer through a channel with a single reader.
    /// </para>
    /// <para>
    /// An observer whose <see cref="IObserver{T}.OnNext"/> throws while a result is delivered always has its
    /// concurrency slot released, so the enumeration never deadlocks over it, but nothing further is delivered to
    /// it: per the observable contract, the subscription is torn down by the caller as soon as <c>OnNext</c>
    /// throws, same as for any other observer.
    /// </para>
    /// <para>
    /// Disposing the subscription cancels the enumeration; resolutions already started are still awaited and, if
    /// still relevant, emitted, but no further item is pulled.
    /// </para>
    /// </remarks>
    let ofAsyncEnumerableResolved
        (maxConcurrency : int)
        (resolve : int -> 'T -> AsyncVal<'Result>)
        (onFailure : exn -> 'Result)
        (source : IAsyncEnumerable<'T>)
        : IObservable<'Result> =
        // Two loops, no lock: the pull loop enumerates the source and starts the resolutions, which settle on arbitrary
        // threads and only ever write their outcome into a channel; the emitter is the channel's single reader and the
        // only caller of the observer, so observer calls are serialized by construction. A concurrency slot is held
        // from the moment an item is pulled until the emitter has delivered its result, so a slow observer bounds the
        // pull loop exactly as before and the channel never holds more than maxConcurrency events.
        // backgroundTask, not task: the loops await slots and the resolutions still draining on the thread pool - a
        // subscriber's synchronization context that has to be pumped for those continuations would be a deadlock
        // waiting to happen
        let run (observer : IObserver<'Result>) (cancellationToken : CancellationToken) : Task = backgroundTask {
            use enumerationCancellation = CancellationTokenSource.CreateLinkedTokenSource cancellationToken
            use slots = new SemaphoreSlim (maxConcurrency, maxConcurrency)
            let events =
                Channel.CreateUnbounded<ResolutionEvent<'Result>> (
                    UnboundedChannelOptions (SingleReader = true, SingleWriter = false, AllowSynchronousContinuations = false)
                )
            // The writer is never completed: a resolution may settle after the pull loop has ended, and the emitter
            // stops by counting the settled resolutions against the started ones instead
            let post (event : ResolutionEvent<'Result>) = events.Writer.TryWrite event |> ignore
            let resolveInBackground (pendingResult : AsyncVal<'Result>) =
                // backgroundTask, not task: a resolution must never resume on a caller's synchronization context,
                // and its synchronous prefix must not run on the pull loop's thread, which is pulling the next item
                // in parallel
                backgroundTask {
                    try
                        let! result = pendingResult |> AsyncVal.toTask
                        post (Resolved result)
                    with ex ->
                        post (ResolutionFailed ex)
                }
                |> ignore
            let pull () : Task = backgroundTask {
                let mutable started = 0
                let mutable enumerator = ValueNone
                let mutable enumerationFailure = ValueNone
                try
                    // Acquired inside the try, because a source may throw when asked for its enumerator
                    let acquired = source.GetAsyncEnumerator enumerationCancellation.Token
                    enumerator <- ValueSome acquired
                    let mutable hasNext = true
                    // The token is checked explicitly, because a sequence is not obliged to observe the token it was given
                    while hasNext && not enumerationCancellation.IsCancellationRequested do
                        do! slots.WaitAsync enumerationCancellation.Token
                        // A resolution may have failed, or the subscription been disposed, while this waited for a slot
                        // or while the source was producing the next item; rechecked after each await so nothing pulled
                        // after that is resolved, let alone emitted (an item the source already produced is dropped:
                        // the failure ends the stream anyway)
                        if enumerationCancellation.IsCancellationRequested then
                            slots.Release () |> ignore
                            hasNext <- false
                        else
                            let! moved = acquired.MoveNextAsync ()
                            if not moved || enumerationCancellation.IsCancellationRequested then
                                slots.Release () |> ignore
                                hasNext <- false
                            else
                                let itemIndex = started
                                let item = acquired.Current
                                // The slot belongs to the started resolution from here on; a resolve function that
                                // throws before returning its AsyncVal has not started one, so the slot is given back
                                let resolution =
                                    try
                                        Ok (resolve itemIndex item)
                                    with ex ->
                                        slots.Release () |> ignore
                                        Error ex
                                match resolution with
                                | Error ex -> raise ex
                                | Ok resolution ->
                                    started <- started + 1
                                    match resolution with
                                    // Items resolved synchronously are posted immediately, which keeps them in the source order
                                    | Immediate result -> post (Resolved result)
                                    | pendingResult -> resolveInBackground pendingResult
                with ex ->
                    // An exception raised once the enumeration was cancelled, by the subscriber or by a failed
                    // resolution, is that cancellation's consequence, not a failure of the source
                    if not enumerationCancellation.IsCancellationRequested then
                        enumerationFailure <- ValueSome ex
                // Captured items no longer need the enumerator, so it is disposed before their resolutions settle
                let! failureAfterDispose = disposeEnumerator enumerator enumerationFailure
                let disposalFailure =
                    match enumerationFailure, failureAfterDispose with
                    | ValueNone, ValueSome ex -> ValueSome ex
                    | _ -> ValueNone
                post (EnumerationEnded (started, enumerationFailure, disposalFailure))
            }
            let pullTask = pull ()
            let mutable settled = 0
            let mutable ended = ValueNone
            let mutable resolutionFailureBeforeEnd = ValueNone
            let mutable resolutionFailureAfterEnd = ValueNone
            let mutable observerFailed = false
            let emit (result : 'Result) =
                // Per the observable contract the subscription is torn down as soon as OnNext throws, so nothing
                // further is delivered; the enumeration is stopped and drained so no slot or enumerator is leaked
                if not cancellationToken.IsCancellationRequested && not observerFailed then
                    try
                        observer.OnNext result
                    with _ ->
                        observerFailed <- true
                        enumerationCancellation.Cancel ()
            let finished () =
                match ended with
                | ValueSome struct (started, _, _) -> settled = started
                | ValueNone -> false
            while not (finished ()) do
                let! event = events.Reader.ReadAsync ()
                match event with
                | Resolved result ->
                    emit result
                    slots.Release () |> ignore
                    settled <- settled + 1
                | ResolutionFailed ex ->
                    // The first failure stops the enumeration; it is delivered once every started resolution has settled
                    if ended.IsNone then
                        if resolutionFailureBeforeEnd.IsNone then
                            resolutionFailureBeforeEnd <- ValueSome ex
                    elif resolutionFailureAfterEnd.IsNone then
                        resolutionFailureAfterEnd <- ValueSome ex
                    enumerationCancellation.Cancel ()
                    slots.Release () |> ignore
                    settled <- settled + 1
                | EnumerationEnded (started, enumerationFailure, disposalFailure) ->
                    ended <- ValueSome struct (started, enumerationFailure, disposalFailure)
            do! pullTask
            let failure =
                match ended with
                | ValueSome struct (_, enumerationFailure, disposalFailure) ->
                    // A resolution failure that stopped the enumeration outranks what the enumeration reported when it
                    // ended; a failure of the source itself outranks a resolution that failed only afterwards, and
                    // a disposal failure is reported only when there was nothing else
                    resolutionFailureBeforeEnd
                    |> ValueOption.orElse enumerationFailure
                    |> ValueOption.orElse resolutionFailureAfterEnd
                    |> ValueOption.orElse disposalFailure
                | ValueNone -> ValueNone
            match failure with
            // A failure caused by disposing the subscription has no observer left to be delivered to
            | ValueSome ex when not cancellationToken.IsCancellationRequested -> emit (onFailure ex)
            | _ -> ()
            if not cancellationToken.IsCancellationRequested && not observerFailed then
                observer.OnCompleted ()
        }
        Observable.Create<'Result>(Func<IObserver<'Result>, CancellationToken, Task>(fun observer cancellationToken -> run observer cancellationToken))

/// <summary>
/// Functions for consuming <see cref="IAsyncEnumerable{T}"/> from <see cref="Async"/> computations.
/// </summary>
module internal AsyncEnumerable =

    /// <summary>
    /// Enumerates the whole asynchronous sequence into an array using the cancellation token of the current computation.
    /// </summary>
    let toArrayAsync (source : IAsyncEnumerable<'T>) : Async<'T[]> = async {
        let! cancellationToken = Async.CancellationToken
        // backgroundTask, not task: the async workflow around it may have been started on a caller's synchronization
        // context, which the drain has no reason to capture
        let enumerate () : Task<Result<'T[], exn>> = backgroundTask {
            let items = ResizeArray<'T>()
            let mutable enumerator = ValueNone
            let mutable failure = ValueNone
            try
                // Acquired inside the try, because a source may throw when asked for its enumerator
                let acquired = source.GetAsyncEnumerator cancellationToken
                enumerator <- ValueSome acquired
                let mutable hasNext = true
                while hasNext do
                    cancellationToken.ThrowIfCancellationRequested ()
                    let! moved = acquired.MoveNextAsync ()
                    if moved then
                        items.Add acquired.Current
                    else
                        hasNext <- false
            with ex ->
                failure <- ValueSome ex

            match! Observable.disposeEnumerator enumerator failure with
            | ValueSome ex -> return Error ex
            | ValueNone -> return Ok (items.ToArray ())
        }
        // The failure is returned as a value and rethrown here, because awaiting a faulted task
        // would wrap the original exception into an AggregateException
        match! enumerate () |> Async.AwaitTask with
        | Ok items -> return items
        | Error ex ->
            ex.Reraise ()
            return Array.empty
    }
