module FSharp.Data.GraphQL.Tests.ValidationCacheTests

open System
open System.Collections
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks
open Microsoft.Extensions.Caching.Memory
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Internal
open Xunit

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Validation

/// <summary>A schema with the single field <c>f(x): Int</c>, whose argument <c>x</c> is the given one</summary>
let private createSchema (argument : InputFieldDef) =
    Schema (Define.Object<unit>("Query", [ Define.Field ("f", IntType, "Returns 1", [ argument ], fun _ () -> 1) ]))

/// <summary>A schema with the single field <c>f(x: Int): Int</c>, so that a Boolean literal for <c>x</c> fails validation</summary>
let private intArgumentSchema = createSchema (Define.Input ("x", Nullable IntType))

let private introspected (schema : ISchema) = schema.Introspected

let private keyOf (query : string) = ValidationResultKey (introspected intArgumentSchema, parse query)

/// How long a test waits for another thread before it fails
let private timeout = TimeSpan.FromSeconds 10.0

let private coercionErrorMessage =
    "Argument field or value named 'x' can not be coerced. It does not match a valid literal representation for the type."

/// <summary>
/// A valid and an invalid document of <see cref="intArgumentSchema"/> with equal structural hash codes.
/// </summary>
/// <remarks>
/// The structural hash code of <c>IntValue i</c> is a constant plus the hash code of <c>i</c>, so the Int literal below
/// has the hash code of the Boolean literal <c>true</c>. The documents differ in that literal only, so their hash codes
/// are equal whatever the seed of string hash codes.
/// </remarks>
let private collidingDocuments () =
    let collidingInt =
        int64 (
            uint32 (
                (BooleanValue true).GetHashCode()
                - (IntValue 0L).GetHashCode()
            )
        )
    let valid = parse $"{{ f(x: %d{collidingInt}) }}"
    let invalid = parse "{ f(x: true) }"
    // The tests using these documents require them to have equal structural hash codes
    Assert.Equal (invalid.GetHashCode (), valid.GetHashCode ())
    struct (valid, invalid)

let private ensureAccepted (description : string) (result : Result<ExecutionPlan, struct (int * GQLProblemDetails list)>) =
    match result with
    | Ok _ -> ()
    | Error struct (_, errors) -> fail $"Expected %s{description} to pass validation, but it was rejected with %A{errors}"

let private ensureRejected (description : string) (result : Result<ExecutionPlan, struct (int * GQLProblemDetails list)>) =
    match result with
    | Ok _ -> fail $"Expected %s{description} to be rejected by validation, but an execution plan was created"
    | Error struct (_, errors) -> hasError coercionErrorMessage errors

[<Fact>]
let ``Document whose hash collides with a cached valid document is still rejected`` () =
    let struct (valid, invalid) = collidingDocuments ()
    let executor = Executor (intArgumentSchema)
    executor.CreateExecutionPlan valid
    |> ensureAccepted "the valid document"
    executor.CreateExecutionPlan invalid
    |> ensureRejected "the invalid document validated right after a colliding valid one"
    executor.CreateExecutionPlan valid
    |> ensureAccepted "the valid document validated again"

[<Fact>]
let ``Document whose hash collides with a cached invalid document is still accepted`` () =
    let struct (valid, invalid) = collidingDocuments ()
    let executor = Executor (intArgumentSchema)
    executor.CreateExecutionPlan invalid
    |> ensureRejected "the invalid document"
    executor.CreateExecutionPlan valid
    |> ensureAccepted "the valid document validated right after a colliding invalid one"

[<Fact>]
let ``Same document is validated separately against each schema sharing the cache`` () =
    let cache = MemoryValidationResultCache () :> IValidationResultCache
    let intExecutor = Executor (intArgumentSchema, [], ValueSome cache)
    let booleanExecutor =
        Executor (createSchema (Define.Input ("x", Nullable BooleanType)), [], ValueSome cache)
    let document = parse "{ f(x: 1) }"
    intExecutor.CreateExecutionPlan document
    |> ensureAccepted "the document with an Int argument against the Int schema"
    booleanExecutor.CreateExecutionPlan document
    |> ensureRejected "the document with an Int argument against the Boolean schema"

[<Fact>]
let ``Keys of documents with colliding structural hash codes are different`` () =
    let zero = parse "{ f(x: 0) }"
    let colliding = parse "{ f(x: 4294967297) }"
    // The test requires documents with equal structural hash codes
    Assert.Equal (zero.GetHashCode (), colliding.GetHashCode ())
    let schema = introspected intArgumentSchema
    Assert.NotEqual (ValidationResultKey (schema, zero), ValidationResultKey (schema, colliding))

[<Fact>]
let ``Keys of different documents with equal hash codes are different`` () =
    // A hash code only buckets the keys, and any two keys can share one, so equality must compare the documents
    let schema = introspected intArgumentSchema
    let first = ValidationResultKey (schema, parse "{ f(x: 1) }", 42)
    let second = ValidationResultKey (schema, parse "{ f(x: 2) }", 42)
    // The test requires keys with equal hash codes
    Assert.Equal (first.GetHashCode (), second.GetHashCode ())
    Assert.NotEqual (first, second)

[<Fact>]
let ``Keys of equal documents parsed separately are equal`` () =
    let query =
        "query Q($v: Int) { a: f(x: $v) @include(if: true) ...F } fragment F on Query { f(x: [1.5, \"s\", { y: null }]) }"
    let first = keyOf query
    let second = keyOf query
    Assert.Equal (first, second)
    Assert.Equal (first.GetHashCode (), second.GetHashCode ())

[<Fact>]
let ``Keys of one document and two schema instances are different`` () =
    let first = introspected (createSchema (Define.Input ("x", Nullable IntType)))
    let second = introspected (createSchema (Define.Input ("x", Nullable IntType)))
    // The test requires structurally equal schemas
    Assert.Equal (first, second)
    let document = parse "{ f(x: 1) }"
    Assert.NotEqual (ValidationResultKey (first, document), ValidationResultKey (second, document))

[<Fact>]
let ``Document size estimates the memory of the nodes and strings of the document`` () =
    let withInt = keyOf "{ f(x: 1) }"
    let characters = String ('s', 100_000)
    let withString = keyOf $"{{ f(x: \"%s{characters}\") }}"
    // The documents have the same nodes, with an Int and a String value for x, so they differ by the characters of the
    // string, two bytes each: a document with a single large literal must count as large
    Assert.Equal (200_000L, withString.DocumentSize - withInt.DocumentSize)
    let complex =
        keyOf "query Q($v: [Int!]) { a: f(x: $v) @include(if: true) ...F } fragment F on Query { f }"
    Assert.True (
        complex.DocumentSize > withInt.DocumentSize,
        $"Expected a document with more nodes to be larger, but got %d{complex.DocumentSize} and %d{withInt.DocumentSize}"
    )

[<Fact>]
let ``Concurrent requests for one key run the validation once`` () : Task = task {
    let cache = MemoryValidationResultCache () :> IValidationResultCache
    let key = keyOf "{ f(x: 1) }"
    let callers = 8
    let runs = ref 0
    use firstRunStarted = new ManualResetEventSlim false
    use release = new ManualResetEventSlim false
    use barrier = new Barrier (callers)
    let producer () =
        Interlocked.Increment runs |> ignore
        firstRunStarted.Set ()
        release.Wait timeout |> ignore
        Success
    let requests =
        Array.init callers (fun _ ->
            Task.Factory.StartNew<ValidationResult<GQLProblemDetails>>(
                (fun () ->
                    barrier.SignalAndWait ()
                    cache.GetOrAdd producer key),
                TaskCreationOptions.LongRunning
            ))
    Assert.True (firstRunStarted.Wait timeout, "No caller started the validation")
    // Give the other callers time to request the key while the first validation is still running; the assertion
    // below holds however late they come, since a late caller finds the cached result
    do! Task.Delay 200
    release.Set ()
    let! results = Task.WhenAll requests
    Assert.All (results, fun result -> Assert.True (result.IsSuccess, $"Expected every caller to get the shared result, but got %A{result}"))
    // The validation runs once, however many callers ask for the key at the same time
    Assert.Equal (1, runs.Value)
}

[<Fact>]
let ``Validation that throws is run again by the next request`` () =
    let cache = MemoryValidationResultCache () :> IValidationResultCache
    let key = keyOf "{ f(x: 1) }"
    let runs = ref 0
    let producer () =
        runs.Value <- runs.Value + 1
        if runs.Value = 1 then
            failwith "Validation failed"
        else
            Success
    throws<Exception>(fun () -> cache.GetOrAdd producer key |> ignore)
    |> ignore
    let result = cache.GetOrAdd producer key
    Assert.True (result.IsSuccess, $"Expected the request after a failed validation to succeed, but got %A{result}")
    Assert.Equal (2, runs.Value)

[<Fact>]
let ``Validation result cache does not keep documents larger than its size limit`` () =
    let small = keyOf "{ f }"
    let large = keyOf "{ f(x: 1) }"
    // Between the sizes of the two documents, which have 3 and 5 nodes
    let cache =
        MemoryValidationResultCache (TimeSpan.FromMinutes 1.0, small.DocumentSize + 1L) :> IValidationResultCache
    let runs = ref 0
    let producer () =
        runs.Value <- runs.Value + 1
        Success
    cache.GetOrAdd producer small |> ignore
    cache.GetOrAdd producer small |> ignore
    // A document within the size limit is validated once
    Assert.Equal (1, runs.Value)
    cache.GetOrAdd producer large |> ignore
    cache.GetOrAdd producer large |> ignore
    // A document over the size limit is validated on every request
    Assert.Equal (3, runs.Value)
    // A document over the limit is not cached, so it does not evict the documents within it either
    cache.GetOrAdd producer small |> ignore
    Assert.Equal (3, runs.Value)

/// A clock the test sets, which a memory cache counts its expirations by
type private TestClock () =
    member val UtcNow = DateTimeOffset (2026, 1, 1, 0, 0, 0, TimeSpan.Zero) with get, set
    member clock.Advance (time : TimeSpan) = clock.UtcNow <- clock.UtcNow + time
    interface ISystemClock with
        member clock.UtcNow = clock.UtcNow

/// A memory cache that counts the lookups made in it, so that a test knows when a request has passed its lookup
type private CountingMemoryCache (inner : IMemoryCache) =
    let mutable lookups = 0

    member _.Lookups = Volatile.Read &lookups

    interface IMemoryCache with
        member _.TryGetValue (key : obj, value : byref<obj>) =
            Interlocked.Increment &lookups |> ignore
            inner.TryGetValue (key, &value)
        member _.CreateEntry key = inner.CreateEntry key
        member _.Remove key = inner.Remove key
        member _.Dispose () = inner.Dispose ()

/// Requests the key on a thread of its own, as the validation it runs or waits for blocks until the test releases it
let private startRequest (cache : IValidationResultCache) (producer : ValidationResultProducer) (key : ValidationResultKey) =
    Task.Factory.StartNew<ValidationResult<GQLProblemDetails>> ((fun () -> cache.GetOrAdd producer key), TaskCreationOptions.LongRunning)

[<Fact>]
let ``Cache hit refreshes the sliding expiration of a validation result`` () =
    let clock = TestClock ()
    use memoryCache = new MemoryCache (MemoryCacheOptions (Clock = clock))
    let cache = MemoryValidationResultCache (memoryCache, TimeSpan.FromSeconds 30.0) :> IValidationResultCache
    let key = keyOf "{ f(x: 1) }"
    let runs = ref 0
    let producer () =
        runs.Value <- runs.Value + 1
        Success
    cache.GetOrAdd producer key |> ignore
    clock.Advance (TimeSpan.FromSeconds 20.0)
    cache.GetOrAdd producer key |> ignore
    // 40 s after the result was cached, but 20 s after it was last used
    clock.Advance (TimeSpan.FromSeconds 20.0)
    cache.GetOrAdd producer key |> ignore
    Assert.Equal (1, runs.Value)
    // 31 s after the last use
    clock.Advance (TimeSpan.FromSeconds 31.0)
    cache.GetOrAdd producer key |> ignore
    Assert.Equal (2, runs.Value)

[<Fact>]
let ``Validation results declare the size of their documents to a memory cache with a size limit`` () =
    let first = keyOf "{ f(x: 1) }"
    let second = keyOf "{ f(x: 2) }"
    // Room for one of the two documents. With nothing to compact the first one stays, whatever the timing
    let options =
        MemoryCacheOptions (SizeLimit = first.DocumentSize + second.DocumentSize - 1L, CompactionPercentage = 0.0)
    use memoryCache = new MemoryCache (options)
    let cache = MemoryValidationResultCache (memoryCache) :> IValidationResultCache
    let runs = ref 0
    let producer () =
        runs.Value <- runs.Value + 1
        Success
    cache.GetOrAdd producer first |> ignore
    cache.GetOrAdd producer first |> ignore
    Assert.Equal (1, runs.Value)
    cache.GetOrAdd producer second |> ignore
    cache.GetOrAdd producer second |> ignore
    // The second document does not fit beside the first, so it is validated on every request
    Assert.Equal (3, runs.Value)
    Assert.Equal (1, memoryCache.Count)

[<Fact>]
let ``Validation that outlasts the sliding expiration is shared and its result is cached from when it finished`` () : Task = task {
    let clock = TestClock ()
    use memoryCache = new MemoryCache (MemoryCacheOptions (Clock = clock))
    let counting = new CountingMemoryCache (memoryCache)
    let cache = MemoryValidationResultCache (counting, TimeSpan.FromSeconds 30.0) :> IValidationResultCache
    let key = keyOf "{ f(x: 1) }"
    let runs = ref 0
    use started = new ManualResetEventSlim false
    use release = new ManualResetEventSlim false
    let producer () =
        Interlocked.Increment runs |> ignore
        started.Set ()
        release.Wait timeout |> ignore
        Success
    let first = startRequest cache producer key
    Assert.True (started.Wait timeout, "The validation did not start")
    // The validation takes twice the sliding expiration
    clock.Advance (TimeSpan.FromSeconds 60.0)
    let second = startRequest cache producer key
    // The first request looks the key up, and its validation once more as it starts; the third lookup is the second request's
    Assert.True (SpinWait.SpinUntil ((fun () -> counting.Lookups >= 3), timeout), "The second request did not look the key up")
    // Lets the second request go on from its lookup to the validation in flight; coming later it finds the cached
    // result, so the assertions below hold either way
    do! Task.Delay 200
    release.Set ()
    let! _ = Task.WhenAll [| first; second |]
    Assert.Equal (1, runs.Value)
    // 20 s after the validation finished, although 80 s after it started
    clock.Advance (TimeSpan.FromSeconds 20.0)
    cache.GetOrAdd producer key |> ignore
    Assert.Equal (1, runs.Value)
}

[<Fact>]
let ``Concurrent requests share one validation whose result cannot be cached`` () : Task = task {
    let key = keyOf "{ f(x: 1) }"
    // No room for the document, so its result is never cached
    use memoryCache = new MemoryCache (MemoryCacheOptions (SizeLimit = key.DocumentSize - 1L, CompactionPercentage = 0.0))
    let counting = new CountingMemoryCache (memoryCache)
    let cache = MemoryValidationResultCache (counting) :> IValidationResultCache
    let callers = 8
    let runs = ref 0
    use started = new ManualResetEventSlim false
    use release = new ManualResetEventSlim false
    let producer () =
        Interlocked.Increment runs |> ignore
        started.Set ()
        release.Wait timeout |> ignore
        Success
    let requests = Array.init callers (fun _ -> startRequest cache producer key)
    Assert.True (started.Wait timeout, "No request started the validation")
    // Every request looks the key up once, and the validation once more as it starts
    Assert.True (SpinWait.SpinUntil ((fun () -> counting.Lookups > callers), timeout), "Not every request looked the key up")
    // Lets the requests go on from their lookups to the validation in flight before it finishes
    do! Task.Delay 200
    release.Set ()
    let! results = Task.WhenAll requests
    Assert.All (results, fun result -> Assert.True (result.IsSuccess, $"Expected every request to get the shared result, but got %A{result}"))
    Assert.Equal (1, runs.Value)
    Assert.Equal (0, memoryCache.Count)
}

[<Fact>]
let ``Executor caches validation results in a keyed memory cache of the service provider`` () =
    let serviceKey = "GraphQL validation results"
    let services = ServiceCollection ()
    services.AddKeyedSingleton<IMemoryCache> (
        serviceKey,
        fun _ _ -> new MemoryCache (MemoryCacheOptions (SizeLimit = MemoryValidationResultCache.DefaultSizeLimit)) :> IMemoryCache
    )
    |> ignore
    use provider = services.BuildServiceProvider ()
    let memoryCache = provider.GetRequiredKeyedService<IMemoryCache> serviceKey
    let validationCache = MemoryValidationResultCache (memoryCache) :> IValidationResultCache
    let executor = Executor (intArgumentSchema, [], ValueSome validationCache)
    executor.CreateExecutionPlan "{ f(x: 1) }"
    |> ensureAccepted "the document"
    Assert.Equal (1, (memoryCache :?> MemoryCache).Count)

/// A schema that counts how often its introspected representation is read
let private countIntrospectedReads (schema : ISchema<unit>) (reads : int ref) = {
    new ISchema<unit> with
        member _.Query = schema.Query
        member _.Mutation = schema.Mutation
        member _.Subscription = schema.Subscription
    interface ISchema with
        member _.TypeMap = schema.TypeMap
        member _.Query = (schema :> ISchema).Query
        member _.Mutation = (schema :> ISchema).Mutation
        member _.Subscription = (schema :> ISchema).Subscription
        member _.Directives = schema.Directives
        member _.TryFindType typeName = schema.TryFindType typeName
        member _.GetPossibleTypes typeDef = schema.GetPossibleTypes typeDef
        member _.IsPossibleType abstractDef objectDef = schema.IsPossibleType abstractDef objectDef
        member _.Introspected =
            Interlocked.Increment reads |> ignore
            schema.Introspected
        member _.ParseError path error = schema.ParseError path error
        member _.SubscriptionProvider = schema.SubscriptionProvider
        member _.LiveFieldSubscriptionProvider = schema.LiveFieldSubscriptionProvider
    interface IEnumerable<NamedDef> with
        member _.GetEnumerator () = (schema :> IEnumerable<NamedDef>).GetEnumerator()
    interface IEnumerable with
        member _.GetEnumerator () = (schema :> IEnumerable).GetEnumerator()
}

[<Fact>]
let ``Executor does not read the introspected schema per request`` () =
    let reads = ref 0
    let executor =
        Executor (countIntrospectedReads (createSchema (Define.Input ("x", Nullable IntType)) :> ISchema<unit>) reads)
    let readsAfterConstruction = reads.Value
    for value in 1..3 do
        executor.CreateExecutionPlan $"{{ f(x: %d{value}) }}"
        |> ensureAccepted $"the document with x = %d{value}"
    // The executor reads the introspected schema only while it is created
    Assert.Equal (readsAfterConstruction, reads.Value)
