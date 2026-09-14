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
    /// Creates a cold observable, which enumerates the asynchronous sequence for every subscription.
    /// </summary>
    /// <remarks>
    /// Disposing the subscription cancels the enumeration and disposes the enumerator.
    /// An exception raised by the sequence is delivered through <see cref="IObserver{T}.OnError"/>.
    /// </remarks>
    let ofAsyncEnumerable (source : IAsyncEnumerable<'T>) : IObservable<'T> =
        let enumerate (observer : IObserver<'T>) (cancellationToken : CancellationToken) : Task = task {
            let enumerator = source.GetAsyncEnumerator cancellationToken
            let mutable failure = ValueNone
            try
                let mutable hasNext = true
                // The token is checked explicitly, because a sequence is not obliged to observe the token it was given
                while hasNext && not cancellationToken.IsCancellationRequested do
                    let! moved = enumerator.MoveNextAsync ()
                    if moved then observer.OnNext enumerator.Current
                    else hasNext <- false
            with ex ->
                failure <- ValueSome ex
            do! enumerator.DisposeAsync ()
            match failure with
            // A failure caused by disposing the subscription has no observer left to be delivered to
            | ValueSome ex when not cancellationToken.IsCancellationRequested -> observer.OnError ex
            | _ -> ()
        }
        Observable.Create<'T> (Func<IObserver<'T>, CancellationToken, Task> (fun observer cancellationToken -> enumerate observer cancellationToken))

    /// <summary>
    /// Wraps every element into <see langword="ValueSome"/> and emits <see langword="ValueNone"/> when the source completes.
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
module internal AsyncEnumerableExtensions =

    /// <summary>
    /// Enumerates the whole asynchronous sequence into an array using the cancellation token of the current computation.
    /// </summary>
    let toArrayAsync (source : IAsyncEnumerable<'T>) : Async<'T[]> = async {
        let! cancellationToken = Async.CancellationToken
        let enumerate () : Task<Result<'T[], exn>> = task {
            let items = ResizeArray<'T> ()
            let enumerator = source.GetAsyncEnumerator cancellationToken
            let mutable failure = ValueNone
            try
                let mutable hasNext = true
                while hasNext do
                    cancellationToken.ThrowIfCancellationRequested ()
                    let! moved = enumerator.MoveNextAsync ()
                    if moved then items.Add enumerator.Current
                    else hasNext <- false
            with ex ->
                failure <- ValueSome ex
            do! enumerator.DisposeAsync ()
            match failure with
            | ValueSome ex -> return Error ex
            | ValueNone -> return Ok (items.ToArray ())
        }
        // The failure is returned as a value and rethrown here, because awaiting a faulted task
        // would wrap the original exception into an AggregateException
        match! enumerate () |> Async.AwaitTask with
        | Ok items -> return items
        | Error ex ->
            ExceptionDispatchInfo.Capture(ex).Throw ()
            return Array.empty
    }
