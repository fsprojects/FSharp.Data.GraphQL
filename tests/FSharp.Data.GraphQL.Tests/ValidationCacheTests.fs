module FSharp.Data.GraphQL.Tests.ValidationCacheTests

open System
open System.Collections
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks
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
    Assert.True (
        (valid.GetHashCode () = invalid.GetHashCode ()),
        "The test requires documents with equal structural hash codes, but the hash codes differ"
    )
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
    Assert.True (
        (zero.GetHashCode () = colliding.GetHashCode ()),
        "The test requires documents with equal structural hash codes, but the hash codes differ"
    )
    let schema = introspected intArgumentSchema
    Assert.False (
        (ValidationResultKey (schema, zero)).Equals(ValidationResultKey (schema, colliding)),
        "Expected the keys of { f(x: 0) } and { f(x: 4294967297) } to be different, but they are equal"
    )

[<Fact>]
let ``Keys of different documents with equal hash codes are different`` () =
    // A hash code only buckets the keys, and any two keys can share one, so equality must compare the documents
    let schema = introspected intArgumentSchema
    let first = ValidationResultKey (schema, parse "{ f(x: 1) }", 42)
    let second = ValidationResultKey (schema, parse "{ f(x: 2) }", 42)
    Assert.True ((first.GetHashCode () = second.GetHashCode ()), "The test requires keys with equal hash codes, but they differ")
    Assert.False (
        first.Equals second,
        "Expected the keys of { f(x: 1) } and { f(x: 2) } to be different despite their equal hash codes, but they are equal"
    )

[<Fact>]
let ``Keys of equal documents parsed separately are equal`` () =
    let query =
        "query Q($v: Int) { a: f(x: $v) @include(if: true) ...F } fragment F on Query { f(x: [1.5, \"s\", { y: null }]) }"
    let first = keyOf query
    let second = keyOf query
    Assert.True (first.Equals second, "Expected the keys of two parses of one query to be equal, but they are different")
    Assert.True (
        (first.GetHashCode () = second.GetHashCode ()),
        $"Expected the keys of two parses of one query to have equal hash codes, but got %d{first.GetHashCode ()} and %d{second.GetHashCode ()}"
    )

[<Fact>]
let ``Keys of one document and two schema instances are different`` () =
    let first = introspected (createSchema (Define.Input ("x", Nullable IntType)))
    let second = introspected (createSchema (Define.Input ("x", Nullable IntType)))
    Assert.True ((first = second), "The test requires structurally equal schemas, but they differ")
    let document = parse "{ f(x: 1) }"
    Assert.False (
        (ValidationResultKey (first, document)).Equals(ValidationResultKey (second, document)),
        "Expected keys of different schema instances to be different, but they are equal"
    )

[<Fact>]
let ``Document size estimates the memory of the nodes and strings of the document`` () =
    let withInt = keyOf "{ f(x: 1) }"
    let characters = String ('s', 100_000)
    let withString = keyOf $"{{ f(x: \"%s{characters}\") }}"
    // The documents have the same nodes, with an Int and a String value for x, so they differ by the characters of the
    // string, two bytes each: a document with a single large literal must count as large
    Assert.True (
        (withString.DocumentSize - withInt.DocumentSize = 200_000L),
        $"Expected a 100 000-character string to add 200 000 bytes, but it added %d{withString.DocumentSize - withInt.DocumentSize}"
    )
    let complex =
        keyOf "query Q($v: [Int!]) { a: f(x: $v) @include(if: true) ...F } fragment F on Query { f }"
    Assert.True (
        (complex.DocumentSize > withInt.DocumentSize),
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
        release.Wait (TimeSpan.FromSeconds 10.0) |> ignore
        Success
    let requests =
        Array.init callers (fun _ ->
            Task.Factory.StartNew<ValidationResult<GQLProblemDetails>>(
                (fun () ->
                    barrier.SignalAndWait ()
                    cache.GetOrAdd producer key),
                TaskCreationOptions.LongRunning
            ))
    Assert.True (firstRunStarted.Wait (TimeSpan.FromSeconds 10.0), "No caller started the validation")
    // Give the other callers time to request the key while the first validation is still running; the assertion
    // below holds however late they come, since a late caller finds the cached result
    do! Task.Delay 200
    release.Set ()
    let! results = Task.WhenAll requests
    Assert.All (results, fun result -> Assert.True (result.IsSuccess, $"Expected every caller to get the shared result, but got %A{result}"))
    Assert.True ((runs.Value = 1), $"Expected the validation to run once for %d{callers} concurrent callers, but it ran %d{runs.Value} times")
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
    Assert.True (
        (result.IsSuccess && runs.Value = 2),
        $"Expected the request after a failed validation to validate again and succeed, but got %A{result} after %d{runs.Value} runs"
    )

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
    Assert.True ((runs.Value = 1), $"Expected a document within the size limit to be validated once, but it was validated %d{runs.Value} times")
    cache.GetOrAdd producer large |> ignore
    cache.GetOrAdd producer large |> ignore
    Assert.True (
        (runs.Value = 3),
        $"Expected a document over the size limit to be validated on every request, but it was validated %d{runs.Value - 1} times in 2 requests"
    )
    // A document over the limit is not cached, so it does not evict the documents within it either
    cache.GetOrAdd producer small |> ignore
    Assert.True ((runs.Value = 3), "Expected the document within the size limit to stay cached after the ones over it, but it was validated again")

/// A cache of strings whose size is their length, on a clock the test sets
let private createCache (policy : CacheExpirationPolicy) (sizeLimit : int64) (now : TimeSpan ref) =
    MemoryCache<string, int>(policy, sizeLimit, (fun (key : string) -> int64 key.Length), StringComparer.Ordinal, fun () -> now.Value)

/// Gets the value of the key from the cache; a value produced for the key is the number of times it was produced
let private getCounting (cache : MemoryCache<string, int>) (productions : Dictionary<string, int>) (key : string) =
    cache.GetOrAddResult key (fun () ->
        let count =
            match productions.TryGetValue key with
            | true, count -> count + 1
            | false, _ -> 1
        productions[key] <- count
        count)

let private ensureProduction (expected : int) (description : string) (actual : int) =
    Assert.True (
        (actual = expected),
        $"Expected %s{description} to return the value produced the %d{expected}. time, but got the value produced the %d{actual}. time"
    )

[<Fact>]
let ``Cache hit refreshes the sliding expiration of the entry`` () =
    let now = ref TimeSpan.Zero
    let cache = createCache (SlidingExpiration (TimeSpan.FromSeconds 30.0)) Int64.MaxValue now
    let get = getCounting cache (Dictionary StringComparer.Ordinal)
    get "key" |> ensureProduction 1 "the first request"
    now.Value <- TimeSpan.FromSeconds 20.0
    get "key"
    |> ensureProduction 1 "a request 20 s after the first"
    // 45 s after the entry was created, but 25 s after it was last used
    now.Value <- TimeSpan.FromSeconds 45.0
    cache.RemoveExpired ()
    Assert.True ((cache.Count = 1), "Expected the entry used 25 s ago to survive removing the expired entries, but it was removed")
    get "key"
    |> ensureProduction 1 "a request 25 s after the last use"
    now.Value <- TimeSpan.FromSeconds 76.0
    get "key"
    |> ensureProduction 2 "a request 31 s after the last use"

[<Fact>]
let ``Cache hit does not extend an absolute expiration`` () =
    let now = ref TimeSpan.Zero
    let cache = createCache (AbsoluteExpiration (TimeSpan.FromSeconds 30.0)) Int64.MaxValue now
    let get = getCounting cache (Dictionary StringComparer.Ordinal)
    get "key" |> ensureProduction 1 "the first request"
    now.Value <- TimeSpan.FromSeconds 20.0
    get "key"
    |> ensureProduction 1 "a request 20 s after the first"
    now.Value <- TimeSpan.FromSeconds 31.0
    get "key"
    |> ensureProduction 2 "a request 31 s after the entry was created"

[<Fact>]
let ``Cache over its size limit evicts the least recently used entries`` () =
    let now = ref TimeSpan.Zero
    let cache = createCache NoExpiration 10L now
    let get = getCounting cache (Dictionary StringComparer.Ordinal)
    get "aaaa" |> ensureProduction 1 "the first request of aaaa"
    now.Value <- TimeSpan.FromSeconds 1.0
    get "bbbb" |> ensureProduction 1 "the first request of bbbb"
    // Using aaaa makes bbbb the least recently used entry
    now.Value <- TimeSpan.FromSeconds 2.0
    get "aaaa"
    |> ensureProduction 1 "the second request of aaaa"
    now.Value <- TimeSpan.FromSeconds 3.0
    get "cccc" |> ensureProduction 1 "the first request of cccc"
    Assert.True ((cache.Size = 8L), $"Expected the cache to evict one entry of size 4 to get under its limit of 10, but its size is %d{cache.Size}")
    get "aaaa"
    |> ensureProduction 1 "a request of aaaa after the eviction"
    get "cccc"
    |> ensureProduction 1 "a request of cccc after the eviction"
    get "bbbb"
    |> ensureProduction 2 "a request of the evicted bbbb"

[<Fact>]
let ``Entry larger than the size limit is produced on every request without being cached`` () =
    let now = ref TimeSpan.Zero
    let cache = createCache NoExpiration 10L now
    let get = getCounting cache (Dictionary StringComparer.Ordinal)
    get "small"
    |> ensureProduction 1 "the first request of an entry within the limit"
    get "elevenchars"
    |> ensureProduction 1 "the first request of an entry over the limit"
    get "elevenchars"
    |> ensureProduction 2 "the second request of an entry over the limit"
    Assert.True ((cache.Count = 1), $"Expected only the entry within the limit to be cached, but the cache has %d{cache.Count} entries")
    get "small"
    |> ensureProduction 1 "a request of the entry within the limit after the ones over it"

[<Fact>]
let ``Cache evicts while many threads add entries`` () =
    let now = ref TimeSpan.Zero
    let cache = createCache NoExpiration 1_000L now
    let requests =
        Array.init 8 (fun thread ->
            Task.Factory.StartNew (
                (fun () ->
                    for i in 0..49_999 do
                        cache.GetOrAddResult $"%d{thread}-%d{i}" (fun () -> i)
                        |> ignore),
                TaskCreationOptions.LongRunning
            ))
    // Copying the entries to evict while other threads add entries threw before, failing the requests
    Task.WaitAll requests
    // A request after the concurrent ones brings the cache back within its limit, whatever they left behind
    cache.GetOrAddResult "last" (fun () -> 0) |> ignore
    Assert.True ((cache.Size <= 1_000L), $"Expected the cache to be within its size limit of 1000, but its size is %d{cache.Size}")

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
    Assert.True (
        (reads.Value = readsAfterConstruction),
        $"Expected the executor to read the introspected schema only while it is created (%d{readsAfterConstruction} times), but 3 requests made it %d{reads.Value}"
    )
