namespace FSharp.Data.GraphQL

open System
open System.Reactive
open System.Reactive.Linq

/// Extension methods to observable, used in place of Fsharp.Control.Reactive
module internal Observable =
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

open System.Collections.Concurrent

// /// Wrapper type for IObservable that exposes event triggering
// type ObservableSource<'T>(onDispose: unit -> unit) = 
//     let mutable (key:uint64) = 0UL
//     let subscriptions = new ConcurrentDictionary<uint64, IObserver<'T>>()
//     let next o = subscriptions |> Seq.iter(fun (KeyValue(_, v)) -> v.OnNext(o))
//     let obs = 
//         { new IObservable<'T> with
//             member this.Subscribe(o) =
//                 let key' = key
//                 key <- key + 1UL
//                 subscriptions.AddOrUpdate(key, o, (fun _ _ -> o)) |> ignore
//                 { new IDisposable with
//                     member this.Dispose() =
//                         subscriptions.TryRemove(key') |> ignore } }
    
//     /// Dispatches an event to all subscribers
//     member this.Next(o) = next o

//     /// Access the underlying IObservable
//     member this.Observable = obs

    
