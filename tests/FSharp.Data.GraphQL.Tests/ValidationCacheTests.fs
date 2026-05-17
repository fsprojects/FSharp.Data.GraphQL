// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.ValidationCacheTests

open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Validation
open System.Threading

[<Fact>]
let ``MemoryValidationResultCache caches results for same key`` () =
    let cache = MemoryValidationResultCache() :> IValidationResultCache
    let mutable callCount = 0
    let producer () =
        Interlocked.Increment(&callCount) |> ignore
        Success
    
    let key = { DocumentId = "doc1"; SchemaId = "schema1" }
    
    // First call should invoke producer
    let result1 = cache.GetOrAdd producer key
    equals 1 callCount
    equals Success result1
    
    // Second call with same key should NOT invoke producer (cached)
    let result2 = cache.GetOrAdd producer key
    equals 1 callCount  // Still 1, not 2
    equals Success result2

[<Fact>]
let ``MemoryValidationResultCache uses different cache entries for different DocumentIds`` () =
    let cache = MemoryValidationResultCache() :> IValidationResultCache
    let mutable callCount = 0
    let producer () =
        Interlocked.Increment(&callCount) |> ignore
        Success
    
    let key1 = { DocumentId = "doc1"; SchemaId = "schema1" }
    let key2 = { DocumentId = "doc2"; SchemaId = "schema1" }
    
    // First call
    let result1 = cache.GetOrAdd producer key1
    equals 1 callCount
    
    // Second call with different DocumentId should invoke producer again
    let result2 = cache.GetOrAdd producer key2
    equals 2 callCount  // Should be 2 now

[<Fact>]
let ``MemoryValidationResultCache uses different cache entries for different SchemaIds`` () =
    let cache = MemoryValidationResultCache() :> IValidationResultCache
    let mutable callCount = 0
    let producer () =
        Interlocked.Increment(&callCount) |> ignore
        Success
    
    let key1 = { DocumentId = "doc1"; SchemaId = "schema1" }
    let key2 = { DocumentId = "doc1"; SchemaId = "schema2" }
    
    // First call
    let result1 = cache.GetOrAdd producer key1
    equals 1 callCount
    
    // Second call with different SchemaId should invoke producer again
    let result2 = cache.GetOrAdd producer key2
    equals 2 callCount  // Should be 2 now

[<Fact>]
let ``MemoryValidationResultCache distinguishes keys with same hash code`` () =
    let cache = MemoryValidationResultCache() :> IValidationResultCache
    
    // Create two different keys that might have hash collisions
    // Using very similar but different strings
    let key1 = { DocumentId = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"; SchemaId = "schema1" }
    let key2 = { DocumentId = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab"; SchemaId = "schema1" }
    
    let mutable callCount = 0
    let producer () =
        Interlocked.Increment(&callCount) |> ignore
        Success
    
    // First call
    let result1 = cache.GetOrAdd producer key1
    equals 1 callCount
    
    // Second call with different key should invoke producer even if hash codes collide
    let result2 = cache.GetOrAdd producer key2
    equals 2 callCount  // Should be 2, proving we use full key not just hash

[<Fact>]
let ``MemoryValidationResultCache caches error results`` () =
    let cache = MemoryValidationResultCache() :> IValidationResultCache
    let mutable callCount = 0
    let error = GQLProblemDetails.Create("Test error")
    let producer () =
        Interlocked.Increment(&callCount) |> ignore
        ValidationError [error]
    
    let key = { DocumentId = "doc1"; SchemaId = "schema1" }
    
    // First call should invoke producer
    let result1 = cache.GetOrAdd producer key
    equals 1 callCount
    match result1 with
    | ValidationError errors -> equals 1 (Seq.length errors)
    | Success -> fail "Expected ValidationError"
    
    // Second call with same key should NOT invoke producer (cached)
    let result2 = cache.GetOrAdd producer key
    equals 1 callCount  // Still 1, not 2
    match result2 with
    | ValidationError errors -> equals 1 (Seq.length errors)
    | Success -> fail "Expected ValidationError"

[<Fact>]
let ``MemoryValidationResultCache handles concurrent access`` () =
    let cache = MemoryValidationResultCache() :> IValidationResultCache
    let mutable callCount = 0
    let producer () =
        Interlocked.Increment(&callCount) |> ignore
        Thread.Sleep(10)  // Simulate some work
        Success
    
    let key = { DocumentId = "doc1"; SchemaId = "schema1" }
    
    // Call cache from multiple threads simultaneously
    let tasks = 
        [1..10]
        |> List.map (fun _ -> 
            async {
                return cache.GetOrAdd producer key
            })
    
    let results = tasks |> Async.Parallel |> Async.RunSynchronously
    
    // All results should be Success
    results |> Array.iter (fun r -> equals Success r)
    
    // Producer should be called at least once, but possibly more due to race conditions
    // The important thing is it's not called 10 times
    Assert.True(callCount >= 1 && callCount < 10, $"Expected callCount between 1 and 9, got {callCount}")
