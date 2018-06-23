namespace FSharp.Data.GraphQL

open System
open System.Reactive.Linq
open System.Linq

/// Extension methods to observable, used in place of Fsharp.Control.Reactive
module internal Observable =
    let bind (f: 'T -> IObservable<'U>) (o: IObservable<'T>) = o.SelectMany(f)

    let ofAsync asyncOp = Observable.FromAsync(fun token -> Async.StartAsTask(asyncOp,cancellationToken = token))

    let ofSeq<'Item> (items:'Item seq) : IObservable<'Item> = {
        new IObservable<_> with
            member __.Subscribe( observer:IObserver<_> ) =
                for item in items do observer.OnNext item
                observer.OnCompleted()
                {   new IDisposable with member __.Dispose() = ()   }
    }

    let toSeq (o : IObservable<'T>) : 'T seq = Observable.ToEnumerable(o)
    
    let catch<'Item, 'Exception> (fx : Exception -> IObservable<'Item>) (obs : IObservable<'Item>) =
        obs.Catch(fx)

    let choose (f: 'T -> 'U option) (o: IObservable<'T>): IObservable<'U> =
        o.Select(f).Where(Option.isSome).Select(Option.get)

    let concat (os : IObservable<'T> seq) = Observable.Concat(os)