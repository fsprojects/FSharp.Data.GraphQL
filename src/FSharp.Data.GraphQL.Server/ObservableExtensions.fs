namespace FSharp.Data.GraphQL

open System
open System.Collections.Generic
open System.Reactive.Linq
open System.Runtime.ExceptionServices
open System.Threading
open System.Threading.Tasks
open FSharp.Control.Reactive.Observable

/// Extension methods to observable, used in place of FSharp.Control.Observable
module internal Observable =

    let ofAsyncVal x = x |> AsyncVal.toAsync |> ofAsync

    let toSeq (o : IObservable<'T>) : 'T seq = Observable.ToEnumerable(o)

    /// Projects each element of an observable sequence into consecutive non-overlapping buffers
    /// which are produced based on timing information.
    let bufferMilliseconds (ms : int) x =
        let span = TimeSpan.FromMilliseconds(float ms)
        Observable.Buffer(x, span)

    /// Projects each element of an observable sequence into consecutive non-overlapping buffers
    /// which are produced based on timing and element count information.
    let bufferMillisecondsCount (ms : int) (count : int) x =
        let span = TimeSpan.FromMilliseconds(float ms)
        Observable.Buffer(x, span, count)

    let ofAsyncSeq (items : Async<'Item> seq) =
        items |> Seq.map ofAsync |> Observable.Merge

    let ofAsyncValSeq (items : AsyncVal<'Item> seq) =
        items |> Seq.map ofAsyncVal |> Observable.Merge

    let singleton (value : 'T) = {
        new IObservable<'T> with
            member _.Subscribe(observer) =
                observer.OnNext value
                observer.OnCompleted()
                { new IDisposable with member _.Dispose() = () }
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
    /// Matches an <see cref="OperationCanceledException"/> that was not requested on
    /// <paramref name="cancellationToken"/>.
    /// </summary>
    /// <remarks>
    /// Distinguishes a linked-token cancellation, typically caused by a resolution failure, from the
    /// subscription itself being disposed.
    /// </remarks>
    let (|CanceledIndependently|_|) (cancellationToken : CancellationToken) (ex : exn) =
        match ex with
        | :? OperationCanceledException when not cancellationToken.IsCancellationRequested -> Some ()
        | _ -> None

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
            let mutable failure = ValueNone
            try
                // Acquired inside the try, because a source may throw when asked for its enumerator
                let acquired = source.GetAsyncEnumerator cancellationToken
                enumerator <- ValueSome acquired
                let mutable hasNext = true
                // The token is checked explicitly, because a sequence is not obliged to observe the token it was given
                while hasNext && not cancellationToken.IsCancellationRequested do
                    let! moved = acquired.MoveNextAsync ()
                    if moved then observer.OnNext acquired.Current
                    else hasNext <- false
            with ex ->
                failure <- ValueSome ex
            let! failure = disposeEnumerator enumerator failure
            match failure with
            // A failure caused by disposing the subscription has no observer left to be delivered to
            | ValueSome ex when not cancellationToken.IsCancellationRequested -> observer.OnError ex
            | _ -> ()
        }
        Observable.Create<'T> (Func<IObserver<'T>, CancellationToken, Task> (fun observer cancellationToken -> enumerate observer cancellationToken))

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
        // backgroundTask, not task: besides the reasons that apply to ofAsyncEnumerable, the loop awaits a
        // concurrency slot and, at the end, the resolutions still draining on the thread pool - a subscriber's
        // synchronization context that has to be pumped for those continuations would be a deadlock waiting to happen
        let enumerate (observer : IObserver<'Result>) (cancellationToken : CancellationToken) : Task = backgroundTask {
            use linkedCancellation = CancellationTokenSource.CreateLinkedTokenSource cancellationToken
            use slots = new SemaphoreSlim (maxConcurrency, maxConcurrency)
            // Observer calls are not required to be thread-safe, but resolutions complete on arbitrary threads
            let sync = obj ()
            let emit (result : 'Result) =
                lock sync (fun () -> if not cancellationToken.IsCancellationRequested then observer.OnNext result)
            // Only the number of resolutions still in flight is tracked, not the tasks themselves, so a long-running
            // source does not retain one task per item; the last resolution to settle after the enumeration has ended
            // completes drained. Ref cells, because the resolutions run on other threads.
            let inFlight = ref 0
            let enumerationEnded = ref false
            let drained = TaskCompletionSource ()
            let resolutionFailure = ref ValueNone
            let failed () = lock sync (fun () -> resolutionFailure.Value.IsSome)
            let stopped () = cancellationToken.IsCancellationRequested || failed ()
            let reportResolutionFailure (ex : exn) =
                let shouldCancel =
                    lock sync (fun () ->
                        if resolutionFailure.Value.IsNone then
                            resolutionFailure.Value <- ValueSome ex
                            true
                        else false)
                if shouldCancel then
                    linkedCancellation.Cancel ()
            let settle () =
                lock sync (fun () ->
                    inFlight.Value <- inFlight.Value - 1
                    if enumerationEnded.Value && inFlight.Value = 0 then drained.TrySetResult () |> ignore)
            let resolveInBackground (pendingResult : AsyncVal<'Result>) =
                lock sync (fun () -> inFlight.Value <- inFlight.Value + 1)
                // backgroundTask, not task: a resolution must never resume on a caller's synchronization context,
                // and its synchronous prefix must not run on the enumeration loop's thread, which is pulling the
                // next item in parallel
                backgroundTask {
                    try
                        try
                            let! result = pendingResult |> AsyncVal.toTask
                            emit result
                        with ex ->
                            // The first failure stops the enumeration; it is delivered once every started resolution has settled
                            reportResolutionFailure ex
                    finally
                        // Released whatever happened, otherwise the enumeration would wait for this slot forever.
                        // Released before settling, because settling lets the enumeration finish and dispose the semaphore.
                        slots.Release () |> ignore
                        settle ()
                }
                |> ignore
            let mutable enumerator = ValueNone
            let mutable failure = ValueNone
            try
                // Acquired inside the try, because a source may throw when asked for its enumerator
                let acquired = source.GetAsyncEnumerator linkedCancellation.Token
                enumerator <- ValueSome acquired
                let mutable index = 0
                let mutable hasNext = true
                // The token is checked explicitly, because a sequence is not obliged to observe the token it was given
                while hasNext && not (stopped ()) do
                    do! slots.WaitAsync cancellationToken
                    // A resolution may have failed, or the subscription been disposed, while this waited for a slot
                    // or while the source was producing the next item; rechecked after each await so nothing pulled
                    // after that is resolved, let alone emitted (an item the source already produced is dropped:
                    // the failure ends the stream anyway)
                    if stopped () then
                        slots.Release () |> ignore
                        hasNext <- false
                    else
                        let! moved = acquired.MoveNextAsync ()
                        if not moved || stopped () then
                            slots.Release () |> ignore
                            hasNext <- false
                        else
                            let itemIndex = index
                            let item = acquired.Current
                            index <- index + 1
                            match resolve itemIndex item with
                            // Items resolved synchronously are emitted immediately, which keeps them in the source order
                            | Immediate result ->
                                try
                                    emit result
                                finally
                                    slots.Release () |> ignore
                            | pendingResult -> resolveInBackground pendingResult
            with
            | CanceledIndependently cancellationToken when failed () -> ()
            | ex -> failure <- ValueSome ex
            // Captured items no longer need the enumerator, so it is disposed before waiting for their resolutions
            let! failure = disposeEnumerator enumerator failure
            // Resolutions still in flight neither need the enumerator nor the loop, only their slots
            lock sync (fun () ->
                enumerationEnded.Value <- true
                if inFlight.Value = 0 then drained.TrySetResult () |> ignore)
            do! drained.Task
            let failureToReport =
                match resolutionFailure.Value, failure with
                | ValueSome resolutionFailure, ValueSome (CanceledIndependently cancellationToken) ->
                    ValueSome resolutionFailure
                | _ -> failure |> ValueOption.orElse resolutionFailure.Value
            match failureToReport with
            // A failure caused by disposing the subscription has no observer left to be delivered to
            | ValueSome ex when not cancellationToken.IsCancellationRequested -> emit (onFailure ex)
            | _ -> ()
        }
        Observable.Create<'Result> (Func<IObserver<'Result>, CancellationToken, Task> (fun observer cancellationToken -> enumerate observer cancellationToken))

    /// <summary>
    /// Wraps every element into <see cref="ValueSome"/> and emits <see cref="ValueNone"/> when the source completes.
    /// </summary>
    /// <remarks>
    /// A consumer can handle the completion like a regular element, for example to send a final message
    /// before the completion itself is processed.
    /// </remarks>
    let withCompletionMarker (source : IObservable<'T>) : IObservable<'T voption> =
        Observable.Concat (Observable.Select (source, fun item -> ValueSome item), Observable.Return ValueNone)

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
            let items = ResizeArray<'T> ()
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
                    if moved then items.Add acquired.Current
                    else hasNext <- false
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
