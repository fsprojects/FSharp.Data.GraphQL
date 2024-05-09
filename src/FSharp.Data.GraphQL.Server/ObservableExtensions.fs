namespace FSharp.Data.GraphQL

open System
open System.Threading.Tasks
open R3

/// Extension methods to observable, used in place of FSharp.Control.Observable
module internal Observable =

    let empty () = Observable.Empty ()

    let singleton item = Observable.Return<'T> item

    let bind f o = ObservableExtensions.SelectMany (o, Func<_,_> f)

    let map f o = ObservableExtensions.Select (o, Func<_,_> f)

    let where f o = ObservableExtensions.Where (o, Func<_,_> f)

    let choose f o =
        o |> map f |> where Option.isSome |> map Option.get

    let catchWith f o = ObservableExtensions.Catch (o, Func<_,_> f)

    let ofSeq (items : 'Item seq) = Observable.ToObservable(items)

    let ofAsync (x : Async<'T>) = Observable.FromAsync(fun _ -> x |> Async.StartAsTask |> ValueTask<'T>)

    let ofAsyncVal x = Observable.FromAsync(fun _ -> x |> AsyncVal.toValueTask)

    let ofAsyncSeq (items : Async<'Item> seq) =
        items |> Seq.map ofAsync |> Observable.Merge

    let ofAsyncValSeq (items : AsyncVal<'Item> seq) =
        items |> Seq.map ofAsyncVal |> Observable.Merge

    let toArrayAsync o = ObservableExtensions.ToArrayAsync o

    let add f o = ObservableSubscribeExtensions.Subscribe(o, onNext = Action<_>(f))

    let concat o1 o2 = ObservableExtensions.Concat(o2, o1)

    let concatInner (o : Observable<Observable<'T>>) = o.Concat()

    let merge o1 o2 = ObservableExtensions.Merge(o1, o2)

    let mergeInner (o : Observable<Observable<'T>>) = o.Merge()

    let liftAsync asyncOperation =
       asyncOperation >> ofAsync

    let flatmapAsync asyncOperation (source : Observable<'Source>) =
        source.SelectMany(fun item -> liftAsync asyncOperation item)

    let bufferCount (count : int) x = ObservableExtensions.Chunk(x, count)

    /// Projects each element of an observable sequence into consecutive non-overlapping buffers
    /// which are produced based on timing information.
    let bufferMilliseconds (ms : int) x =
        let span = TimeSpan.FromMilliseconds(float ms)
        ObservableExtensions.Chunk(x, span)

    /// Projects each element of an observable sequence into consecutive non-overlapping buffers
    /// which are produced based on timing and element count information.
    let bufferMillisecondsCount (ms : int) (count : int) x =
        let span = TimeSpan.FromMilliseconds(float ms)
        ObservableExtensions.Chunk(x, span, count)
