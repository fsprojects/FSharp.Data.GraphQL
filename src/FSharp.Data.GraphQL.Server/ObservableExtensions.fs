namespace FSharp.Data.GraphQL

open System
open System.Reactive.Linq

/// Extension methods to observable, used in place of FSharp.Control.Observable
module internal Observable =
    let bind (f : 'T -> IObservable<'U>) (o : IObservable<'T>) = o.SelectMany(f)

    let ofAsync asyncOp = Observable.FromAsync(fun token -> Async.StartAsTask(asyncOp,cancellationToken = token))

    let ofSeq<'Item> (items : 'Item seq) : IObservable<'Item> = {
        new IObservable<_> with
            member __.Subscribe(observer) =
                for item in items do observer.OnNext item
                observer.OnCompleted()
                { new IDisposable with member __.Dispose() = () }
    }

    let toSeq (o : IObservable<'T>) : 'T seq = Observable.ToEnumerable(o)

    /// Projects each element of an observable sequence into consecutive non-overlapping buffers
    /// which are produced based on timing information.
    let bufferByTiming (ms : int) x =
        let span = TimeSpan.FromMilliseconds(float ms)
        Observable.Buffer(x, span) |> Observable.map List.ofSeq

    /// Projects each element of an observable sequence into consecutive non-overlapping buffers
    /// which are produced based on element count information.
    let bufferByElementCount (count : int) x =
        Observable.Buffer(x, count) |> Observable.map List.ofSeq

    /// Projects each element of an observable sequence into consecutive non-overlapping buffers
    /// which are produced based on timing and element count information.
    let bufferByTimingAndElementCount (ms : int) (count : int) x =
        let span = TimeSpan.FromMilliseconds(float ms)
        Observable.Buffer(x, span, count) |> Observable.map List.ofSeq

    let catch<'Item, 'Exception> (fx : Exception -> IObservable<'Item>) (obs : IObservable<'Item>) =
        obs.Catch(fx)

    let choose (f: 'T -> 'U option) (o: IObservable<'T>): IObservable<'U> =
        o.Select(f).Where(Option.isSome).Select(Option.get)

    let concat (sources : IObservable<IObservable<'T>>) = Observable.Concat(sources)

    let merge (sources : IObservable<IObservable<'T>>) = Observable.Merge(sources)

    let mapAsync f = Observable.map (fun x -> (ofAsync (f x))) >> concat

    let ofAsyncSeq (items : Async<'Item> seq) : IObservable<'Item> = 
        items |> Seq.map ofAsync |> Observable.Merge