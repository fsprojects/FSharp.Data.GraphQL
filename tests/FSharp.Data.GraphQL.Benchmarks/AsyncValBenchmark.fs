/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
module FSharp.Data.GraphQL.AsyncValBenchmark

open System
open BenchmarkDotNet
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Parser

// TEST 1 - immediatelly return a value
let asyncReturnImmediate() = 
    let v = async { return 1337 } |> Async.RunSynchronously
    ()

let asyncValImmediate() = 
    let v = asyncVal { return 1337 } |> AsyncVal.get
    ()

let asyncValAwaiting() = 
    let v = asyncVal { return async { return 1337 } } |> AsyncVal.get
    ()

// TEST 2 - immediatelly return a collection of async values
let asyncValCollection vals = 
    let v = 
        vals
        |> AsyncVal.collectParallel
        |> AsyncVal.get
    ()

let asyncCollection vals = 
    let v = 
        vals
        |> Async.Parallel
        |> Async.RunSynchronously
    ()

open BenchmarkDotNet.Attributes

[<Config(typeof<GraphQLBenchConfig>)>]
type AsyncValBenchmark() = 
    let rand = Random()
    
    let prepareAsyncs n = 
        let a = Array.zeroCreate n
        for i = 0 to n - 1 do
            let x = rand.Next()
            a.[i] <- async { return x }
        a
    
    let prepareAsyncVals nsync nasync = 
        let n = nsync + nasync
        let a = Array.zeroCreate n
        for i = 0 to nsync - 1 do
            a.[i] <- AsyncVal.wrap (rand.Next())
        for i = nsync to n - 1 do
            let x = rand.Next()
            a.[i] <- AsyncVal.ofAsync (async { return x })
        a
    
    let mutable allImmediate : AsyncVal<_> [] = [||]
    let mutable sync90async10 : AsyncVal<_> [] = [||]
    let mutable allAsyncVals : AsyncVal<_> [] = [||]
    let mutable allAsync : Async<_> [] = [||]
    
    [<Setup>]
    member x.Setup() = 
        allImmediate <- prepareAsyncVals 100 0
        sync90async10 <- prepareAsyncVals 90 10
        allAsyncVals <- prepareAsyncVals 0 100
        allAsync <- prepareAsyncs 100
    
    [<Benchmark>]
    member x.AsyncValImmediate() = asyncValImmediate()
    
    [<Benchmark>]
    member x.AsyncValAwaiting() = asyncValAwaiting()
    
    [<Benchmark>]
    member x.AsyncReturnImmediatelly() = asyncReturnImmediate()
    
    [<Benchmark>]
    member x.AsyncValCollectionAllSync() = asyncValCollection allImmediate
    
    [<Benchmark>]
    member x.AsyncValCollectionAllAsync() = asyncValCollection allAsyncVals
    
    [<Benchmark>]
    member x.AsyncCollection() = asyncCollection allAsync
    
    [<Benchmark>]
    member x.AsyncValCollectionMixed90x10() = asyncValCollection sync90async10
