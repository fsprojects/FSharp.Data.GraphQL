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
    let a = async { return! asyncVal.Return 1 }
    let res = a |> sync
    res |> equals 1

[<Fact>]
let ``AsyncVal can be bound inside Async computation`` () =
    let a = async {
        let! v = asyncVal.Return 1
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
