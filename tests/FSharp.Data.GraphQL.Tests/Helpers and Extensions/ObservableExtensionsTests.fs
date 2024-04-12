// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.ObservableExtensionsTests

open Xunit
open FSharp.Data.GraphQL
open Helpers

open System
open FSharp.Control.Reactive


let delay time x = async {
    do! Async.Sleep(ms time)
    return x }

[<Fact>]
let ``ofSeq should call OnComplete and return items in expected order`` () =
    let source = seq { for x in 1 .. 5 do yield x }
    let obs = Observable.ofSeq source
    use sub = Observer.create obs
    sub.WaitCompleted(timeout = ms 10)
    sub.Received |> seqEquals source

[<Fact>]
let ``bind should call OnComplete and return items in expected order`` () =
    let source = seq { for x in 1 .. 5 do yield x }
    let obs = Observable.ofSeq source |> Observable.bind (fun x -> Observable.ofSeq [x; x])
    use sub = Observer.create obs
    sub.WaitCompleted(timeout = ms 10)
    sub.Received |> seqEquals [ 1; 1; 2; 2; 3; 3; 4; 4; 5; 5 ]

[<Fact>]
let ``ofAsync should call OnComplete and return items in expected order`` () =
    let source = async { return "test" }
    let obs = Observable.ofAsync source
    use sub = Observer.create obs
    sub.WaitCompleted(timeout = ms 10)
    sub.Received |> seqEquals [ "test" ]


[<Fact>]
let ``ofAsyncVal should call OnComplete and return items in expected order`` () =
    let source = async { return "test" } |> AsyncVal.ofAsync
    let obs = Observable.ofAsyncVal source
    use sub = Observer.create obs
    sub.WaitCompleted(timeout = ms 10)
    sub.Received |> seqEquals [ "test" ]

[<Fact>]
let ``toSeq on a finite sequence should generate a finite sequence`` () =
    let source = seq { for x in 1 .. 5 do yield x }
    let obs = Observable.ofSeq source
    let result = Observable.toSeq obs
    result |> seqEquals source

[<Fact>]
let ``ofSeq on an empty sequence should call OnComplete and return items in expected order`` () =
    let source = Seq.empty<int>
    let obs = Observable.ofSeq source
    use sub = Observer.create obs
    sub.WaitCompleted(timeout = ms 10)
    sub.Received |> seqEquals source

[<Fact>]
let ``ofAsyncSeq should call OnComplete and return items in expected order`` () =
    let source = seq {
        yield delay 300 2
        yield delay 100 1
        yield delay 200 3 }
    let obs = Observable.ofAsyncSeq source
    use sub = Observer.create obs
    sub.WaitCompleted(timeout = ms 10)
    sub.Received |> seqEquals [ 1; 3; 2 ]

[<Fact>]
let ``ofAsyncValSeq should call OnComplete and return items in expected order`` () =
    let source = seq {
        yield delay 300 2 |> AsyncVal.ofAsync
        yield delay 100 1 |> AsyncVal.ofAsync
        yield delay 200 3 |> AsyncVal.ofAsync }
    let obs = Observable.ofAsyncValSeq source
    use sub = Observer.create obs
    sub.WaitCompleted(timeout = ms 10)
    sub.Received |> seqEquals [ 1; 3; 2 ]

[<Fact>]
let ``bufferByTiming should call OnComplete and return items in expected order`` () =
    let source = seq {
        yield delay 400 2
        yield delay 100 1
        yield delay 200 3 }
    let obs = Observable.ofAsyncSeq source |> Observable.bufferMilliseconds (ms 300) |> Observable.map List.ofSeq
    use sub = Observer.create obs
    sub.WaitCompleted(timeout = ms 10)
    sub.Received |> seqEquals [ [1; 3]; [2] ]

[<Fact>]
let ``bufferByElementCount should call OnComplete and return items in expected order`` () =
    let source = seq {
        yield delay 400 2
        yield delay 100 1
        yield delay 200 3 }
    let obs = Observable.ofAsyncSeq source |> Observable.bufferCount 2 |> Observable.map List.ofSeq
    use sub = Observer.create obs
    sub.WaitCompleted(timeout = ms 10)
    sub.Received |> seqEquals [ [1; 3]; [2] ]

[<Fact>]
let ``bufferByTimingAndElementCount should call OnComplete and return items in expected order`` () =
    let source = seq {
        yield delay 500 2
        yield delay 50 1
        yield delay 100 3
        yield delay 150 4 }
    let obs = Observable.ofAsyncSeq source |> Observable.bufferMillisecondsCount (ms 300) 2 |> Observable.map List.ofSeq
    use sub = Observer.create obs
    sub.WaitCompleted(timeout = ms 10)
    sub.Received |> seqEquals [ [1; 3]; [4]; [2] ]

type IndexException(index : int) =
    inherit exn(sprintf "Error at index %i." index)
    member _.Index = index

[<Fact>]
let ``catch should call OnComplete and return items in expected order`` () =
    let source : int seq = seq { for x in 1 .. 5 do yield raise <| IndexException(x) }
    let obs =
        Observable.ofSeq source
        |> Observable.catchWith (fun (ex : IndexException) -> ex.Index |> Observable.singleton)
    use sub = Observer.create obs
    sub.WaitCompleted(timeout = ms 10)
    sub.Received |> seqEquals [ 1 ]

[<Fact>]
let ``choose should cal OnComplete`` () =
    let source = seq { for x in 1 .. 5 do yield x }
    let obs =
        Observable.ofSeq source
        |> Observable.choose (fun x -> match x % 2 with | 0 -> Some x | _ -> None)
    use sub = Observer.create obs
    sub.WaitCompleted(timeout = ms 10)
    sub.Received |> seqEquals [ 2; 4 ]

[<Fact>]
let ``concatInner should call OnComplete and return items in expected order`` () =
    let source1 = seq {
        yield delay 500 2
        yield delay 100 1
        yield delay 200 3 }
    let source2 = seq {
        yield delay 400 4
        yield delay 300 5 }
    let source = seq { yield Seq.empty; yield source1; yield source2 }
    let obs =
        Observable.ofSeq source
        |> Observable.map Observable.ofAsyncSeq
        |> Observable.concatInner
    use sub = Observer.create obs
    sub.WaitCompleted(timeout = ms 10)
    sub.Received |> seqEquals [ 1; 3; 2; 5; 4 ]

[<Fact>]
let ``concat should call OnComplete and return items in expected order`` () =
    let source1 = seq {
        yield delay 500 2
        yield delay 100 1
        yield delay 200 3 }
    let source2 = seq {
        yield delay 400 4
        yield delay 300 5 }
    let obs =
        Observable.ofAsyncSeq source1
        |> Observable.concat (Observable.ofAsyncSeq source2)
    use sub = Observer.create obs
    sub.WaitCompleted(timeout = ms 10)
    sub.Received |> seqEquals [ 1; 3; 2; 5; 4 ]

[<Fact>]
let ``mergeInner should call OnComplete and return items in expected order`` () =
    let source1 = seq {
        yield delay 500 2
        yield delay 100 1
        yield delay 200 3 }
    let source2 = seq {
        yield delay 400 4
        yield delay 300 5 }
    let source = seq { yield Seq.empty; yield source1; yield source2 }
    let obs =
        Observable.ofSeq source
        |> Observable.map Observable.ofAsyncSeq
        |> Observable.mergeInner
    use sub = Observer.create obs
    sub.WaitCompleted(timeout = ms 10)
    sub.Received |> seqEquals [ 1; 3; 5; 4; 2 ]

[<Fact>]
let ``merge should call OnComplete and return items in expected order`` () =
    let source1 = seq {
        yield delay 500 2
        yield delay 100 1
        yield delay 200 3 }
    let source2 = seq {
        yield delay 400 4
        yield delay 300 5 }
    let obs =
        Observable.ofAsyncSeq source1
        |> Observable.merge (Observable.ofAsyncSeq source2)
    use sub = Observer.create obs
    sub.WaitCompleted(timeout = ms 10)
    sub.Received |> seqEquals [ 1; 3; 5; 4; 2 ]

[<Fact>]
let ``concatSeq should call OnComplete and return items in expected order`` () =
    let source = seq { for x in 1 .. 5 do yield x }
    let obs = Observable.ofSeq source
    use sub = Observer.create obs
    sub.WaitCompleted(timeout = ms 10)
    sub.Received |> seqEquals source

[<Fact(Skip = "There is only one use of flatmapAsync in the codebase (as of Jan 2023) and the order in which it returns results does not seem to matter.")>]
let ``mapAsync should call OnComplete and return items in expected order`` () =
    let source = seq { "a"; "b"; "c"; "d"; "e"; "f"; "g" }
    let obs = Observable.ofSeq source |> Observable.flatmapAsync (fun x -> async { return x }) |> Observable.map (fun x -> x)
    use sub = Observer.create obs
    sub.WaitCompleted(timeout = ms 10)
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
    sub.WaitCompleted(timeout = ms 10)
    sub.Received |> seqEquals (Seq.singleton 1)
