namespace FSharp.Data.GraphQL

open System
open System.Reactive.Linq

/// Extension methods to observable, used in place of FSharp.Control.Observable
module internal Observable =
    let ofSeq<'Item> (items : 'Item seq) = {
        new IObservable<'Item> with
            member __.Subscribe(observer) =
                for item in items do observer.OnNext item
                observer.OnCompleted()
                { new IDisposable with member __.Dispose() = () }
    }

    let bind (f : 'T -> IObservable<'U>) (o : IObservable<'T>) = o.SelectMany(f)

    let ofAsync x = Observable.FromAsync(fun ct -> Async.StartAsTask(x, cancellationToken = ct))

    let ofAsyncVal x = x |> AsyncVal.toAsync |> ofAsync

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

    let catch<'Item, 'Exception when 'Exception :> exn> (fx : 'Exception -> IObservable<'Item>) (obs : IObservable<'Item>) =
        obs.Catch<'Item, 'Exception>(Func<'Exception, IObservable<'Item>>(fx))

    let choose (f: 'T -> 'U option) (o: IObservable<'T>): IObservable<'U> =
        o.Select(f).Where(Option.isSome).Select(Option.get)

    let concat (sources : IObservable<IObservable<'T>>) = Observable.Concat(sources)

    let concat2 (source1 : IObservable<'T>) (source2 : IObservable<'T>) = Observable.Concat(source1, source2)

    let merge (sources : IObservable<IObservable<'T>>) = Observable.Merge(sources)

    let merge2 (source1 : IObservable<'T>) (source2 : IObservable<'T>) = Observable.Merge(source1, source2)

    let concatSeq (source : IObservable<#seq<'T>>) = source |> bind ofSeq

    let mapAsync f = Observable.map (fun x -> (ofAsync (f x))) >> concat

    let ofAsyncSeq (items : Async<'Item> seq) = 
        items |> Seq.map ofAsync |> Observable.Merge

    let ofAsyncValSeq (items : AsyncVal<'Item> seq) =
        items |> Seq.map ofAsyncVal |> Observable.Merge

    let empty<'T> = Seq.empty<'T> |> ofSeq

    let singleton (value : 'T) = Seq.singleton value |> ofSeq