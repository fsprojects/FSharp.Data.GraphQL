// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.ObservableExtensionsTests

open Xunit
open FSharp.Data.GraphQL
open Helpers

open System
open System.Threading
open System.Threading.Tasks
open FSharp.Control.Reactive

[<Fact>]
let ``ofSeq should call OnComplete and return items in expected order`` () =
    let source = seq {
        for x in 1..5 do
            yield x
    }
    let obs = Observable.ofSeq source
    use sub = Observer.create obs
    sub.WaitCompleted (timeout = ms 10)
    sub.Received |> seqEquals source

[<Fact>]
let ``bind should call OnComplete and return items in expected order`` () =
    let source = seq {
        for x in 1..5 do
            yield x
    }
    let obs =
        Observable.ofSeq source
        |> Observable.bind (fun x -> Observable.ofSeq [ x; x ])
    use sub = Observer.create obs
    sub.WaitCompleted (timeout = ms 10)
    sub.Received |> seqEquals [ 1; 1; 2; 2; 3; 3; 4; 4; 5; 5 ]

[<Fact>]
let ``ofAsync should call OnComplete and return items in expected order`` () =
    let source = async { return "test" }
    let obs = Observable.ofAsync source
    use sub = Observer.create obs
    sub.WaitCompleted (timeout = ms 10)
    sub.Received |> seqEquals [ "test" ]


[<Fact>]
let ``ofAsyncVal should call OnComplete and return items in expected order`` () =
    let source = async { return "test" } |> AsyncVal.ofAsync
    let obs = Observable.ofAsyncVal source
    use sub = Observer.create obs
    sub.WaitCompleted (timeout = ms 10)
    sub.Received |> seqEquals [ "test" ]

[<Fact>]
let ``toSeq on a finite sequence should generate a finite sequence`` () =
    let source = seq {
        for x in 1..5 do
            yield x
    }
    let obs = Observable.ofSeq source
    let result = Observable.toSeq obs
    result |> seqEquals source

[<Fact>]
let ``ofSeq on an empty sequence should call OnComplete and return items in expected order`` () =
    let source = Seq.empty<int>
    let obs = Observable.ofSeq source
    use sub = Observer.create obs
    sub.WaitCompleted (timeout = ms 10)
    sub.Received |> seqEquals source

[<Fact>]
let ``ofAsyncSeq should call OnComplete and return items in expected order`` () =
    let source = seq {
        yield delay 300 2
        yield delay 100 1
        yield delay 200 3
    }
    let obs = Observable.ofAsyncSeq source
    use sub = Observer.create obs
    sub.WaitCompleted (timeout = ms 10)
    sub.Received |> seqEquals [ 1; 3; 2 ]

[<Fact>]
let ``ofAsyncValSeq should call OnComplete and return items in expected order`` () =
    let source = seq {
        yield delay 300 2 |> AsyncVal.ofAsync
        yield delay 100 1 |> AsyncVal.ofAsync
        yield delay 200 3 |> AsyncVal.ofAsync
    }
    let obs = Observable.ofAsyncValSeq source
    use sub = Observer.create obs
    sub.WaitCompleted (timeout = ms 10)
    sub.Received |> seqEquals [ 1; 3; 2 ]

[<Fact>]
let ``bufferByTiming should call OnComplete and return items in expected order`` () =
    let source = seq {
        yield delay 400 2
        yield delay 100 1
        yield delay 200 3
    }
    let obs =
        Observable.ofAsyncSeq source
        |> Observable.bufferMilliseconds (ms 300)
        |> Observable.map List.ofSeq
    use sub = Observer.create obs
    sub.WaitCompleted (timeout = ms 10)
    sub.Received |> seqEquals [ [ 1; 3 ]; [ 2 ] ]

[<Fact>]
let ``bufferByElementCount should call OnComplete and return items in expected order`` () =
    let source = seq {
        yield delay 400 2
        yield delay 100 1
        yield delay 200 3
    }
    let obs =
        Observable.ofAsyncSeq source
        |> Observable.bufferCount 2
        |> Observable.map List.ofSeq
    use sub = Observer.create obs
    sub.WaitCompleted (timeout = ms 10)
    sub.Received |> seqEquals [ [ 1; 3 ]; [ 2 ] ]

[<Fact>]
let ``bufferByTimingAndElementCount should call OnComplete and return items in expected order`` () =
    let source = seq {
        yield delay 500 2
        yield delay 50 1
        yield delay 100 3
        yield delay 150 4
    }
    let obs =
        Observable.ofAsyncSeq source
        |> Observable.bufferMillisecondsCount (ms 300) 2
        |> Observable.map List.ofSeq
    use sub = Observer.create obs
    sub.WaitCompleted (timeout = ms 10)
    sub.Received |> seqEquals [ [ 1; 3 ]; [ 4 ]; [ 2 ] ]

type IndexException (index : int) =
    inherit exn (sprintf "Error at index %i." index)
    member _.Index = index

[<Fact>]
let ``catch should call OnComplete and return items in expected order`` () =
    let source : int seq = seq {
        for x in 1..5 do
            yield raise <| IndexException (x)
    }
    let obs =
        Observable.ofSeq source
        |> Observable.catchWith (fun (ex : IndexException) -> ex.Index |> Observable.singleton)
    use sub = Observer.create obs
    sub.WaitCompleted (timeout = ms 10)
    sub.Received |> seqEquals [ 1 ]

[<Fact>]
let ``choose should cal OnComplete`` () =
    let source = seq {
        for x in 1..5 do
            yield x
    }
    let obs =
        Observable.ofSeq source
        |> Observable.choose (fun x ->
            match x % 2 with
            | 0 -> Some x
            | _ -> None)
    use sub = Observer.create obs
    sub.WaitCompleted (timeout = ms 10)
    sub.Received |> seqEquals [ 2; 4 ]

[<Fact>]
let ``concatInner should call OnComplete and return items in expected order`` () =
    let source1 = seq {
        yield delay 500 2
        yield delay 100 1
        yield delay 200 3
    }
    let source2 = seq {
        yield delay 400 4
        yield delay 300 5
    }
    let source = seq {
        yield Seq.empty
        yield source1
        yield source2
    }
    let obs =
        Observable.ofSeq source
        |> Observable.map Observable.ofAsyncSeq
        |> Observable.concatInner
    use sub = Observer.create obs
    sub.WaitCompleted (timeout = ms 10)
    sub.Received |> seqEquals [ 1; 3; 2; 5; 4 ]

[<Fact>]
let ``concat should call OnComplete and return items in expected order`` () =
    let source1 = seq {
        yield delay 500 2
        yield delay 100 1
        yield delay 200 3
    }
    let source2 = seq {
        yield delay 400 4
        yield delay 300 5
    }
    let obs =
        Observable.ofAsyncSeq source1
        |> Observable.concat (Observable.ofAsyncSeq source2)
    use sub = Observer.create obs
    sub.WaitCompleted (timeout = ms 10)
    sub.Received |> seqEquals [ 1; 3; 2; 5; 4 ]

[<Fact>]
let ``mergeInner should call OnComplete and return items in expected order`` () =
    let source1 = seq {
        yield delay 500 2
        yield delay 100 1
        yield delay 200 3
    }
    let source2 = seq {
        yield delay 400 4
        yield delay 300 5
    }
    let source = seq {
        yield Seq.empty
        yield source1
        yield source2
    }
    let obs =
        Observable.ofSeq source
        |> Observable.map Observable.ofAsyncSeq
        |> Observable.mergeInner
    use sub = Observer.create obs
    sub.WaitCompleted (timeout = ms 10)
    sub.Received |> seqEquals [ 1; 3; 5; 4; 2 ]

[<Fact>]
let ``merge should call OnComplete and return items in expected order`` () =
    let source1 = seq {
        yield delay 500 2
        yield delay 100 1
        yield delay 200 3
    }
    let source2 = seq {
        yield delay 400 4
        yield delay 300 5
    }
    let obs =
        Observable.ofAsyncSeq source1
        |> Observable.merge (Observable.ofAsyncSeq source2)
    use sub = Observer.create obs
    sub.WaitCompleted (timeout = ms 10)
    sub.Received |> seqEquals [ 1; 3; 5; 4; 2 ]

[<Fact>]
let ``concatSeq should call OnComplete and return items in expected order`` () =
    let source = seq {
        for x in 1..5 do
            yield x
    }
    let obs = Observable.ofSeq source
    use sub = Observer.create obs
    sub.WaitCompleted (timeout = ms 10)
    sub.Received |> seqEquals source

[<Fact(Skip =
    "There is only one use of flatmapAsync in the codebase (as of Jan 2023) and the order in which it returns results does not seem to matter.")>]
let ``mapAsync should call OnComplete and return items in expected order`` () =
    let source = seq {
        "a"
        "b"
        "c"
        "d"
        "e"
        "f"
        "g"
    }
    let obs =
        Observable.ofSeq source
        |> Observable.flatmapAsync (fun x -> async { return x })
        |> Observable.map (fun x -> x)
    use sub = Observer.create obs
    sub.WaitCompleted (timeout = ms 10)
    sub.Received |> seqEquals source
    // This test tries to ensure that flatmapAsync always generates the output sequence in
    // the same order as the input sequence.
    // The test is disabled because there is only one use of flatmapAsync in the codebase (as of Jan 2023),
    // and the order in which it returns results does not seem to matter.
    // If it turns out the order does matter, and flatmapAsync fails this test,
    // use something like the code below to insert an ordering index before the async calls
    // and strip it out after using it to re-sort the output.
    //let orderedObs =
    //        source
    //        |> Seq.mapi (fun i x -> (i, x))
    //        |> Observable.ofSeq
    //        |> Observable.flatmapAsync (fun x -> async { return x })
    //use sub = Observer.create orderedObs
    //sub.WaitCompleted(timeout = ms 10)
    //let reordered =
    //        sub.Received
    //        |> Seq.sortBy (fun ix -> fst ix)
    //        |> Seq.map (fun ix -> snd ix)
    //reordered |> seqEquals source

[<Fact>]
let ``singleton should call OnComplete and return item`` () =
    let obs = Observable.singleton 1
    use sub = Observer.create obs
    sub.WaitCompleted (timeout = ms 10)
    sub.Received |> seqEquals (Seq.singleton 1)

open System.Threading
open System.Threading.Tasks
open FSharp.Control

[<Fact>]
let ``ofAsyncEnumerable should call OnComplete and return items in expected order`` () =
    use sub =
        Observable.ofAsyncEnumerable (asyncItems [ 1..5 ])
        |> Observer.create
    sub.WaitCompleted (timeout = ms 10)
    sub.Received |> seqEquals [ 1; 2; 3; 4; 5 ]

[<Fact>]
let ``ofAsyncEnumerable should deliver items produced before an enumeration error`` () =
    let source = taskSeq {
        yield 1
        failwith "Boom"
    }
    use sub =
        Observable.ofAsyncEnumerable source
        |> Observable.materialize
        |> Observer.create
    sub.WaitCompleted (timeout = ms 10)
    Assert.Collection (
        sub.Received,
        (fun (notification : System.Reactive.Notification<int>) ->
            Assert.Equal (System.Reactive.NotificationKind.OnNext, notification.Kind)
            Assert.Equal (1, notification.Value)),
        (fun (notification : System.Reactive.Notification<int>) ->
            Assert.Equal (System.Reactive.NotificationKind.OnError, notification.Kind)
            Assert.Equal ("Boom", notification.Exception.Message))
    )

[<Fact>]
let ``ofAsyncEnumerable should stop the enumeration when the subscription is disposed`` () : Task = task {
    let pulled = ref 0
    let disposed = TaskCompletionSource ()
    let received = TaskCompletionSource ()
    let source = endlessNumbers pulled disposed
    let subscription =
        Observable.ofAsyncEnumerable source
        |> Observable.subscribe (fun _ -> received.TrySetResult () |> ignore)
    do! waitForTask (TimeSpan.FromSeconds (float (ms 5))) "Expected an item before the subscription is disposed" received.Task
    subscription.Dispose ()
    do! waitForTask (TimeSpan.FromSeconds (float (ms 5))) "Expected the enumerator to be disposed with the subscription" disposed.Task
    let pulledAfterDisposal = pulled.Value
    // A still running enumeration would pull more items during this delay
    do! Task.Delay 200
    Assert.Equal (pulledAfterDisposal, pulled.Value)
}

[<Fact>]
let ``ofAsyncEnumerableResolved should emit synchronously resolved results in order`` () =
    use sub =
        Observable.ofAsyncEnumerableResolved 3 (fun _ (n : int) -> AsyncVal.wrap (n * 10)) (fun _ -> -1) (asyncItems [ 1..5 ])
        |> Observer.create
    sub.WaitCompleted (timeout = ms 10)
    sub.Received |> seqEquals [ 10; 20; 30; 40; 50 ]

[<Fact>]
let ``ofAsyncEnumerableResolved should never resolve more than maxConcurrency items at the same time`` () =
    let inFlight = ref 0
    let maxObserved = ref 0
    let resolve _ (n : int) =
        async {
            let current = Interlocked.Increment inFlight
            let mutable observed = maxObserved.Value
            while current > observed
                  && Interlocked.CompareExchange (maxObserved, current, observed)
                     <> observed do
                observed <- maxObserved.Value
            do! Async.Sleep (ms 50)
            Interlocked.Decrement inFlight |> ignore
            return n
        }
        |> AsyncVal.ofAsync
    use sub =
        Observable.ofAsyncEnumerableResolved 2 resolve (fun _ -> -1) (asyncItems [ 1..6 ])
        |> Observer.create
    sub.WaitCompleted (timeout = ms 10)
    sub.Received
    |> Seq.toList
    |> List.sort
    |> seqEquals [ 1; 2; 3; 4; 5; 6 ]
    Assert.True (maxObserved.Value <= 2, $"Expected at most 2 concurrent resolutions, but observed {maxObserved.Value}")

[<Fact>]
let ``ofAsyncEnumerableResolved should emit the failure after a slower earlier item`` () =
    let source = itemThenFailure 1
    let resolve index (n : int) =
        if index = 0 then
            async {
                do! Async.Sleep (ms 200)
                return n
            }
            |> AsyncVal.ofAsync
        else
            AsyncVal.wrap n
    use sub =
        Observable.ofAsyncEnumerableResolved 4 resolve (fun _ -> -1) source
        |> Observer.create
    sub.WaitCompleted (timeout = ms 10)
    sub.Received |> seqEquals [ 1; -1 ]

[<Fact>]
let ``ofAsyncEnumerableResolved should stop resolving further items when the subscription is disposed`` () : Task = task {
    let pulled = ref 0
    let disposed = TaskCompletionSource ()
    let received = TaskCompletionSource ()
    let source = endlessNumbers pulled disposed
    let subscription =
        Observable.ofAsyncEnumerableResolved 1 (fun _ (n : int) -> AsyncVal.wrap n) (fun _ -> -1) source
        |> Observable.subscribe (fun _ -> received.TrySetResult () |> ignore)
    do! waitForTask (TimeSpan.FromSeconds (float (ms 5))) "Expected an item before the subscription is disposed" received.Task
    subscription.Dispose ()
    do! waitForTask (TimeSpan.FromSeconds (float (ms 5))) "Expected the enumerator to be disposed with the subscription" disposed.Task
    let pulledAfterDisposal = pulled.Value
    // A still running enumeration would pull more items during this delay
    do! Task.Delay 200
    Assert.Equal (pulledAfterDisposal, pulled.Value)
}

[<Fact>]
let ``ofAsyncEnumerable should deliver OnError when GetAsyncEnumerator throws`` () =
    // Regression test: acquiring the enumerator happens before the try, so a throwing source must not bypass
    // the failure handling and fault the returned Task in a way that skips OnError
    let source = ThrowingAsyncEnumerable<int> "Boom acquiring the enumerator"
    use sub =
        Observable.ofAsyncEnumerable source
        |> Observable.materialize
        |> Observer.create
    sub.WaitCompleted (timeout = ms 10)
    Assert.Collection (
        sub.Received,
        fun (notification : System.Reactive.Notification<int>) ->
            Assert.Equal (System.Reactive.NotificationKind.OnError, notification.Kind)
            Assert.Equal ("Boom acquiring the enumerator", notification.Exception.Message)
    )

[<Fact>]
let ``ofAsyncEnumerable should deliver OnError when DisposeAsync throws`` () =
    let source = itemThenDisposalFailure 1
    use sub =
        Observable.ofAsyncEnumerable source
        |> Observable.materialize
        |> Observer.create
    sub.WaitCompleted (timeout = ms 10)
    Assert.Collection (
        sub.Received,
        (fun (notification : System.Reactive.Notification<int>) ->
            Assert.Equal (System.Reactive.NotificationKind.OnNext, notification.Kind)
            Assert.Equal (1, notification.Value)),
        (fun (notification : System.Reactive.Notification<int>) ->
            Assert.Equal (System.Reactive.NotificationKind.OnError, notification.Kind)
            Assert.Equal ("Boom disposing", notification.Exception.Message))
    )

[<Fact>]
let ``ofAsyncEnumerableResolved should emit the failure through onFailure when GetAsyncEnumerator throws`` () =
    // Regression test: this used to fault the returned Task instead of going through onFailure, which terminates
    // the merged deferred stream of a query instead of producing this field's DeferredErrors
    let source = ThrowingAsyncEnumerable<int> "Boom acquiring the enumerator"
    use sub =
        Observable.ofAsyncEnumerableResolved 2 (fun _ (n : int) -> AsyncVal.wrap n) (fun _ -> -1) source
        |> Observer.create
    sub.WaitCompleted (timeout = ms 10)
    sub.Received |> seqEquals [ -1 ]

[<Fact>]
let ``ofAsyncEnumerableResolved should emit the failure through onFailure after the item when DisposeAsync throws`` () =
    let source = itemThenDisposalFailure 1
    use sub =
        Observable.ofAsyncEnumerableResolved 2 (fun _ (n : int) -> AsyncVal.wrap n) (fun _ -> -1) source
        |> Observer.create
    sub.WaitCompleted (timeout = ms 10)
    sub.Received |> seqEquals [ 1; -1 ]

[<Fact>]
let ``ofAsyncEnumerableResolved should stop and deliver the failure when a resolution fails`` () =
    // Regression test: a failed resolution used to leave its concurrency slot held forever, so with
    // maxConcurrency = 1 the enumeration would deadlock instead of ever reaching onFailure or OnCompleted
    let resolve _ (_ : int) = AsyncVal.Failure (exn "Boom resolving")
    use sub =
        Observable.ofAsyncEnumerableResolved 1 resolve (fun _ -> -1) (asyncItems [ 1 ])
        |> Observer.create
    sub.WaitCompleted (timeout = ms 10)
    sub.Received |> seqEquals [ -1 ]

[<Fact>]
let ``ofAsyncEnumerableResolved should not pull another item after a resolution fails while waiting for a slot`` () =
    // Regression test: with maxConcurrency = 1 the loop is parked in WaitAsync while the one in-flight resolution
    // runs; once that resolution fails and releases the slot, the loop used to go straight to MoveNextAsync without
    // rechecking the failure, so a synchronously resolved item 2 was pulled and emitted before the failure
    let resolve _ (n : int) =
        if n = 1 then
            async {
                do! Async.Sleep (ms 50)
                return failwith "Boom resolving"
            }
            |> AsyncVal.ofAsync
        else
            AsyncVal.wrap n
    use sub =
        Observable.ofAsyncEnumerableResolved 1 resolve (fun _ -> -1) (asyncItems [ 1; 2; 3 ])
        |> Observer.create
    sub.WaitCompleted (timeout = ms 10)
    sub.Received |> seqEquals [ -1 ]

[<Fact>]
let ``ofAsyncEnumerableResolved should not resolve an item pulled after a resolution failed while the source produced it`` () =
    // Regression test: a background resolution can fail while MoveNextAsync for the next item is still suspended;
    // when that move completed the code used to go straight to resolving it without rechecking the failure, so a
    // synchronously resolved item 2 was pulled and emitted before the failure that already happened
    let source =
        SuspendingAsyncEnumerable<int>(fun _ index -> task {
            match index with
            | 0 -> return ValueSome 1
            | 1 ->
                do! Task.Delay (ms 150)
                return ValueSome 2
            | _ -> return ValueNone
        })
    let resolve _ (n : int) =
        if n = 1 then
            async {
                do! Async.Sleep (ms 50)
                return failwith "Boom resolving"
            }
            |> AsyncVal.ofAsync
        else
            AsyncVal.wrap n
    use sub =
        Observable.ofAsyncEnumerableResolved 2 resolve (fun _ -> -1) source
        |> Observer.create
    sub.WaitCompleted (timeout = ms 10)
    sub.Received |> seqEquals [ -1 ]

[<Fact>]
let ``ofAsyncEnumerableResolved should cancel a pending MoveNextAsync after a resolution fails`` () =
    // Regression test: with maxConcurrency > 1 the loop can already be suspended in MoveNextAsync for the next item
    // when an earlier background resolution fails. That failure must cancel the in-progress move so the stream can
    // finish with onFailure instead of hanging forever in the source.
    let source =
        SuspendingAsyncEnumerable<int>(fun cancellationToken index -> task {
            match index with
            | 0 -> return ValueSome 1
            | 1 ->
                do! Task.Delay (Timeout.Infinite, cancellationToken)
                return ValueSome 2
            | _ -> return ValueNone
        })
    let resolve _ (n : int) =
        if n = 1 then
            async {
                do! Async.Sleep (ms 50)
                return failwith "Boom resolving"
            }
            |> AsyncVal.ofAsync
        else
            AsyncVal.wrap n
    use sub =
        Observable.ofAsyncEnumerableResolved 2 resolve (fun _ -> -1) source
        |> Observer.create
    sub.WaitCompleted (timeout = ms 10)
    sub.Received |> seqEquals [ -1 ]

[<Fact>]
let ``ofAsyncEnumerableResolved should release the slot and not hang when the observer throws`` () : Task = task {
    // Regression test: an observer throwing while a background resolution is delivered used to skip the slot
    // release entirely, deadlocking the enumeration the same way a failed resolution did. System.Reactive tears
    // the subscription down itself (disposing it, which cancels the enumeration) as soon as OnNext throws, so
    // onFailure/OnCompleted are never expected here: this only checks that DisposeAsync is still reached instead
    // of the enumeration hanging forever on the concurrency slot the throwing resolution never released.
    let disposed = TaskCompletionSource ()
    let source =
        SuspendingAsyncEnumerable<int>(
            (fun _ index -> task { return if index = 0 then ValueSome 1 else ValueNone }),
            fun () -> disposed.TrySetResult () |> ignore
        )
    let resolve _ (n : int) = async { return n } |> AsyncVal.ofAsync
    let onReceived (_ : TestObserver<int>) (value : int) =
        if value = 1 then
            failwith "Boom in observer"
    use sub =
        Observable.ofAsyncEnumerableResolved 1 resolve (fun _ -> -1) source
        |> Observer.createWithCallback onReceived
    do! waitForTask (TimeSpan.FromSeconds (float (ms 5))) "Expected the enumerator to be disposed despite the observer throwing" disposed.Task
    sub.Received |> seqEquals [ 1 ]
}

[<Fact>]
let ``withCompletionMarker should emit the items and then the marker when the source completes`` () =
    use sub =
        Observable.ofSeq [ 1; 2 ]
        |> Observable.withCompletionMarker
        |> Observer.create
    sub.WaitCompleted (timeout = ms 10)
    sub.Received
    |> seqEquals [ ValueSome 1; ValueSome 2; ValueNone ]

[<Fact>]
let ``withCompletionMarker should emit only the marker for an empty source`` () =
    use sub =
        Observable.ofSeq Seq.empty<int>
        |> Observable.withCompletionMarker
        |> Observer.create
    sub.WaitCompleted (timeout = ms 10)
    sub.Received |> seqEquals [ ValueNone ]
