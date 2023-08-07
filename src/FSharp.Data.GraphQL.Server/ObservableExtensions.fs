namespace FSharp.Data.GraphQL

open System
open System.Reactive.Linq
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
