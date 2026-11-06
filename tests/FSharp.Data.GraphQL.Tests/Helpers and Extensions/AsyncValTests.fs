// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.AsyncValTests

open System
open System.Threading.Tasks
open FSharp.Data.GraphQL
open Xunit

[<Fact>]
let ``AsyncVal computation allows to return constant values`` () =
    let v = asyncVal { return 1 }
    AsyncVal.isAsync v |> equals false
    AsyncVal.isSync v |> equals true
    match v with
    | Value v' -> v' |> equals 1
    | _ -> fail "unexpected result found in AsyncVal"

[<Fact>]
let ``AsyncVal computation allows to return from async computation`` () =
    let v = asyncVal { return! async { return 1 } }
    AsyncVal.isAsync v |> equals true
    AsyncVal.isSync v |> equals false
    v |> AsyncVal.get |> equals 1

[<Fact>]
let ``AsyncVal computation allows to return from another AsyncVal`` () =
    let v = asyncVal { return! asyncVal { return 1 } }
    AsyncVal.isAsync v |> equals false
    AsyncVal.isSync v |> equals true
    match v with
    | Value v' -> v' |> equals 1
    | _ -> fail "unexpected result found in AsyncVal"

[<Fact>]
let ``AsyncVal computation allows to bind async computations`` () =
    let v = asyncVal {
        let! value = async { return 1 }
        return value }
    AsyncVal.isAsync v |> equals true
    AsyncVal.isSync v |> equals false
    v |> AsyncVal.get |> equals 1

[<Fact>]
let ``AsyncVal computation allows to bind async computations preserving exception stack trace`` () : Task = task {
    let! ex = throwsAsyncVal<Exception>(
            asyncVal {
                let! value = async { return failwith "test" }
                return value
            }
        )
    ex.StackTrace |> String.IsNullOrEmpty |> Assert.False
}

[<Fact>]
let ``AsyncVal computation allows to bind another AsyncVal`` () =
    let v = asyncVal {
        let! value = asyncVal { return 1 }
        return value }
    AsyncVal.isAsync v |> equals false
    AsyncVal.isSync v |> equals true
    match v with
    | Value v' -> v' |> equals 1
    | _ -> fail "unexpected result found in AsyncVal"

[<Fact>]
let ``AsyncVal computation defines zero value`` () =
    let v = AsyncVal.empty
    AsyncVal.isAsync v |> equals false
    AsyncVal.isSync v |> equals true

[<Fact>]
let ``AsyncVal can be returned from Async computation`` () =
    let a = async { return! AsyncVal.wrap 1 }
    let res = a |> sync
    res |> equals 1

[<Fact>]
let ``AsyncVal can be bound inside Async computation`` () =
    let a = async {
        let! v = AsyncVal.wrap 1
        return v }
    let res = a |> sync
    res |> equals 1

[<Fact>]
let ``AsyncVal sequential collection resolves all values in order of execution`` () =
    let mutable flag = "none"
    let a = async {
        do! Async.Sleep 1000
        flag <- "a"
        return 2
    }
    let b = async {
        flag <- "b"
        return 4 }
    let array = [| AsyncVal.wrap 1; AsyncVal.ofAsync a; AsyncVal.wrap 3; AsyncVal.ofAsync b |]
    let v = array |> AsyncVal.collectSequential
    v |> AsyncVal.get |> equals [| 1; 2; 3; 4 |]
    flag |> equals "b"

[<Fact>]
let ``AsyncVal sequential collection preserves exception stack trace for a single exception`` () = task {
    let a = async { return failwith "test" }
    let array = [| AsyncVal.wrap 1; AsyncVal.ofAsync a; AsyncVal.wrap 3 |]
    let! ex = throwsAsyncVal<Exception>(array |> AsyncVal.collectSequential |> AsyncVal.map ignore)
    ex.StackTrace |> String.IsNullOrEmpty |> Assert.False
}

[<Fact>]
let ``AsyncVal sequential collection collects all exceptions into AggregareException`` () = task {
    let ex1 = Exception "test1"
    let ex2 = Exception "test2"
    let array = [| AsyncVal.wrap 1; AsyncVal.Failure ex1; AsyncVal.wrap 3; AsyncVal.Failure ex2 |]
    //let a = async { return failwith "test" }
    //let b = async { return failwith "test" }
    //let array = [| AsyncVal.wrap 1; AsyncVal.ofAsync a; AsyncVal.wrap 3; AsyncVal.ofAsync b |]
    let! ex = throwsAsyncVal<AggregateException>(array |> AsyncVal.collectSequential |> AsyncVal.map ignore)
    ex.InnerExceptions |> Seq.length |> equals 2
    ex.InnerExceptions[0] |> equals ex1
    ex.InnerExceptions[1] |> equals ex2
}

[<Fact>]
let ``AsyncVal lazy sequential collection maps each item only after the previous one has completed`` () : Task = task {
    let mapped = ResizeArray<int> ()
    let gate = TaskCompletionSource<int> (TaskCreationOptions.RunContinuationsAsynchronously)
    let mapping item =
        mapped.Add item
        match item with
        | 2 -> AsyncVal.ofAsync (Async.AwaitTask gate.Task)
        | _ -> AsyncVal.wrap item
    let collected = [| 1; 2; 3; 4 |] |> AsyncVal.collectSequentialWith mapping
    Assert.True (AsyncVal.isAsync collected, $"Expected the collection to become asynchronous at the asynchronous item, but got %O{collected}")
    let result = collected |> AsyncVal.toTask
    Assert.Equal<int list> ([ 1; 2 ], mapped |> Seq.toList)
    gate.SetResult 2
    let! values = result
    Assert.Equal<int[]> ([| 1; 2; 3; 4 |], values)
    Assert.Equal<int list> ([ 1; 2; 3; 4 ], mapped |> Seq.toList)
}

[<Fact>]
let ``AsyncVal lazy sequential collection stays immediate when every item is`` () =
    match [| 1; 2; 3 |] |> AsyncVal.collectSequentialWith AsyncVal.wrap with
    | Value values -> Assert.Equal<int[]> ([| 1; 2; 3 |], values)
    | collected -> fail $"Expected an immediate value, but got %O{collected}"

[<Fact>]
let ``AsyncVal lazy sequential collection does not map the items after an immediate failure`` () =
    let mapped = ResizeArray<int> ()
    let error = Exception "test"
    let mapping item =
        mapped.Add item
        match item with
        | 2 -> AsyncVal.Failure error
        | _ -> AsyncVal.wrap item
    match [| 1; 2; 3 |] |> AsyncVal.collectSequentialWith mapping with
    | Failure failure -> Assert.Same (error, failure)
    | collected -> fail $"Expected the failure of the second item, but got %O{collected}"
    Assert.Equal<int list> ([ 1; 2 ], mapped |> Seq.toList)

[<Fact>]
let ``AsyncVal lazy sequential collection does not map the items after an asynchronous failure`` () : Task = task {
    let mapped = ResizeArray<int> ()
    let error = Exception "test"
    let mapping item =
        mapped.Add item
        match item with
        | 2 -> AsyncVal.Failure error
        | _ -> AsyncVal.ofAsync (async { return item })
    let! ex = throwsAsyncVal<Exception> ([| 1; 2; 3 |] |> AsyncVal.collectSequentialWith mapping |> AsyncVal.map ignore)
    Assert.Same (error, ex)
    Assert.Equal<int list> ([ 1; 2 ], mapped |> Seq.toList)
}

[<Fact>]
let ``AsyncVal parallel collection resolves all values with no order of execution`` () =
    let mutable flag = "none"
    let a = async {
        do! Async.Sleep 1000
        flag <- "a"
        return 2
    }
    let b = async {
        flag <- "b"
        return 4 }
    let array = [| AsyncVal.wrap 1; AsyncVal.ofAsync a; AsyncVal.wrap 3; AsyncVal.ofAsync b |]
    let v = array |> AsyncVal.collectParallel
    v |> AsyncVal.get |> equals [| 1; 2; 3; 4 |]
    flag |> equals "a"

[<Fact>]
let ``AsyncVal parallel collection preserves exception stack trace for a single exception`` () = task {
    let a = async { return failwith "test" }
    let array = [| AsyncVal.wrap 1; AsyncVal.ofAsync a; AsyncVal.wrap 3 |]
    let! ex = throwsAsyncVal<Exception>(array |> AsyncVal.collectParallel |> AsyncVal.map ignore)
    ex.StackTrace |> String.IsNullOrEmpty |> Assert.False
}

[<Fact>]
let ``AsyncVal parallel collection collects all exceptions into AggregareException`` () = task {
    let ex1 = Exception "test1"
    let ex2 = Exception "test2"
    let array = [| AsyncVal.wrap 1; AsyncVal.Failure ex1; AsyncVal.wrap 3; AsyncVal.Failure ex2 |]
    //let a = async { return failwith "test" }
    //let b = async { return failwith "test" }
    //let array = [| AsyncVal.wrap 1; AsyncVal.ofAsync a; AsyncVal.wrap 3; AsyncVal.ofAsync b |]
    let! ex = throwsAsyncVal<AggregateException>(array |> AsyncVal.collectParallel |> AsyncVal.map ignore)
    ex.InnerExceptions |> Seq.length |> equals 2
    ex.InnerExceptions[0] |> equals ex1
    ex.InnerExceptions[1] |> equals ex2
}
