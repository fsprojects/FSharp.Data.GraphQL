namespace FSharp.Data.GraphQL

open System
open System.Reactive
open System.Reactive.Linq

/// Extension methods to observable, used in place of Fsharp.Control.Reactive
module Observable =
    let bind (f: 'T -> IObservable<'U>) (o: IObservable<'T>) = o.SelectMany(f)
    let ofAsync asyncOp = Observable.FromAsync(fun token -> Async.StartAsTask(asyncOp,cancellationToken = token))

    let ofSeq<'Item>(items:'Item seq) : IObservable<'Item> =
    {   
        new IObservable<_> with
            member __.Subscribe( observer:IObserver<_> ) =
                for item in items do observer.OnNext item      
                observer.OnCompleted()     
                {   new IDisposable with member __.Dispose() = ()   }
    }