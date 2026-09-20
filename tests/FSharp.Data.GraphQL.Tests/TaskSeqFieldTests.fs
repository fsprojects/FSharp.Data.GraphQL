// The MIT License (MIT)

module FSharp.Data.GraphQL.Tests.TaskSeqFieldTests

open Xunit
open System
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks
open FSharp.Control
open Azure

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Types

type StreamItem = { Id : int; Value : Async<string> }

// Resolvers are captured as quotations, which cannot contain every taskSeq builder member,
// so the sequences are produced by functions called from the resolvers.
// Sequences that complete synchronously use taskSeq blocks (Helpers.asyncItems), while sequences that really
// suspend use SuspendingAsyncEnumerable, because taskSeq blocks do not resume correctly in Debug builds.
let gatedNumbers (gate : Task) =
    SuspendingAsyncEnumerable<int>(fun _ index -> task {
        match index with
        | 0 -> return ValueSome 1
        | 1 ->
            do! gate
            return ValueSome 2
        | _ -> return ValueNone
    })
    :> IAsyncEnumerable<int>

let signaledGatedNumbers (reachedGate : TaskCompletionSource) (gate : Task) =
    SuspendingAsyncEnumerable<int>(fun _ index -> task {
        match index with
        | 0 -> return ValueSome 1
        | 1 ->
            reachedGate.TrySetResult () |> ignore
            do! gate
            return ValueSome 2
        | _ -> return ValueNone
    })
    :> IAsyncEnumerable<int>

let failingNumbers () = taskSeq {
    yield 1
    yield 2
    failwith "Boom during enumeration"
}

/// Simulates a paged sequence that exposes the size of its pages
type PagedAsyncEnumerable<'T> (pageSize : int, items : 'T list) =
    member _.PageSize = pageSize
    interface IAsyncEnumerable<'T> with
        member _.GetAsyncEnumerator cancellationToken = (asyncItems items).GetAsyncEnumerator cancellationToken

let pageSizeOf (source : IAsyncEnumerable<int>) =
    match source with
    | :? PagedAsyncEnumerable<int> as paged -> ValueSome paged.PageSize
    | _ -> ValueNone

/// Splits the items into Azure SDK pages of the given size
let azurePages (pageSize : int) (items : int list) =
    let chunks = items |> List.chunkBySize pageSize
    chunks
    |> List.mapi (fun index chunk ->
        let continuationToken =
            if index < chunks.Length - 1 then
                string (index + 1)
            else
                null
        // The pages are not produced by a service call, so there is no raw response to attach
        Page<int>.FromValues(List.toArray chunk, continuationToken, Unchecked.defaultof<Response>))

/// <summary>
/// An Azure SDK paged sequence that remembers the page size hint it was requested with.
/// </summary>
/// <remarks>
/// <see cref="AsyncPageable{T}"/> does not expose a page size, because the size is only a hint passed to
/// <see cref="AsyncPageable{T}.AsPages"/>, so an application has to keep the hint to batch streamed items by pages.
/// </remarks>
type HintedAsyncPageable<'T> (pageSizeHint : int, pages : Page<'T> list) =
    inherit AsyncPageable<'T> ()
    member _.PageSizeHint = pageSizeHint
    override _.AsPages (continuationToken, pageSizeHint) = AsyncPageable<'T>.FromPages(pages).AsPages(continuationToken, pageSizeHint)

let azurePageSizeOf (source : IAsyncEnumerable<int>) =
    match source with
    | :? HintedAsyncPageable<int> as pageable -> ValueSome pageable.PageSizeHint
    | _ -> ValueNone

let StreamItemType =
    Define.Object<StreamItem>(
        "StreamItem",
        [
            Define.Field ("id", IntType, fun _ (item : StreamItem) -> item.Id)
            Define.AsyncField ("value", StringType, fun _ (item : StreamItem) -> item.Value)
        ]
    )

let immediateItems = [ { Id = 1; Value = async { return "one" } }; { Id = 2; Value = async { return "two" } } ]

let slowAndFastItems = [ { Id = 1; Value = delay 3000 "slow" }; { Id = 2; Value = async { return "fast" } } ]

let schemaConfig =
    SchemaConfig.DefaultWithBufferedStream (streamOptions = { Interval = ValueNone; PreferredBatchSize = ValueNone })

let executorFor (fields : FieldDef<unit> list) = Executor (Schema (Define.Object<unit>("Query", fields), config = schemaConfig))

let executeQuery (executor : Executor<unit>) (query : string) =
    executor.AsyncExecute (parse query, getMockInputContext, ())
    |> sync

let fieldError (message : string) (fieldName : string) = GQLProblemDetails.CreateWithKind (message, Execution, [ box fieldName ])

/// Builds the deferred payload of streamed items given as (index, value) pairs
let streamedBatch (fieldName : string) (items : (int * int) list) =
    match items with
    | [ index, value ] -> DeferredResult ([| box value |], [ box fieldName; box index ])
    | _ -> DeferredResult (items |> List.map (snd >> box) |> List.toArray, [ box fieldName; box (items |> List.map (fst >> box)) ])

let waitForCompletion (deferred : IObservable<GQLDeferredResponseContent>) =
    use subscription = Observer.create deferred
    subscription.WaitCompleted (timeout = ms 10)
    subscription.Received |> withoutCompleted |> Seq.toList

[<Fact>]
let ``TaskSeq field without directives returns the whole sequence as a list`` () =
    let executor =
        executorFor [
            Define.TaskSeqField ("numbers", ListOf IntType, fun _ _ -> asyncItems [ 1; 2; 3 ])
            Define.TaskSeqField ("items", ListOf StreamItemType, fun _ _ -> asyncItems immediateItems)
        ]
    let expectedData =
        NameValueLookup.ofList [
            "numbers", upcast [| box 1; box 2; box 3 |]
            "items",
            upcast
                [|
                    box (NameValueLookup.ofList [ "id", upcast 1; "value", upcast "one" ])
                    box (NameValueLookup.ofList [ "id", upcast 2; "value", upcast "two" ])
                |]
        ]
    let result = executeQuery executor "{ numbers items { id value } }"
    ensureDirect result
    <| fun data errors ->
        empty errors
        data |> equals (upcast expectedData)

[<Fact>]
let ``TaskSeq field without directives waits for a sequence that suspends`` () : Task = task {
    let gate = TaskCompletionSource ()
    let reachedGate = TaskCompletionSource ()
    let executor =
        executorFor [
            Define.TaskSeqField ("numbers", ListOf IntType, fun _ _ -> signaledGatedNumbers reachedGate gate.Task)
        ]
    let expectedData = NameValueLookup.ofList [ "numbers", upcast [| box 1; box 2 |] ]
    let execution =
        executor.AsyncExecute (parse "{ numbers }", getMockInputContext, ())
        |> Async.StartImmediateAsTask
    do!
        waitForTask
            (TimeSpan.FromSeconds (float (ms 5)))
            "Timeout while waiting for the non-stream execution to reach the suspended second item"
            reachedGate.Task
    Assert.False (execution.IsCompleted, "The non-stream execution must wait for the sequence to produce its last item")
    gate.SetResult ()
    let! result = execution
    ensureDirect result
    <| fun data errors ->
        empty errors
        data |> equals (upcast expectedData)
}

[<Fact>]
let ``TaskSeq field with defer directive delivers the whole list in one deferred payload`` () =
    let executor =
        executorFor [
            Define.TaskSeqField ("numbers", StructNullable (ListOf IntType), fun _ _ -> ValueSome (asyncItems [ 1; 2; 3 ]))
        ]
    let expectedData = NameValueLookup.ofList [ "numbers", null ]
    let result = executeQuery executor "{ numbers @defer }"
    ensureDeferred result
    <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedData)
        waitForCompletion deferred
        |> single
        |> equals (DeferredResult ([| box 1; box 2; box 3 |], [ box "numbers" ]))

[<Fact>]
let ``TaskSeq field with defer directive delivers its DeferredCompleted marker right after the payload`` () =
    let executor =
        executorFor [
            Define.TaskSeqField ("numbers", Nullable (ListOf IntType), fun _ _ -> Some (asyncItems [ 1; 2; 3 ]))
        ]
    let result = executeQuery executor "{ numbers @defer }"
    ensureDeferred result
    <| fun _ errors deferred ->
        empty errors
        use subscription = Observer.create deferred
        subscription.WaitCompleted (timeout = ms 10)
        subscription.Received
        |> Seq.toList
        |> equals [
            DeferredResult ([| box 1; box 2; box 3 |], [ box "numbers" ])
            DeferredCompleted [ box "numbers" ]
        ]

[<Fact>]
let ``TaskSeq field with stream directive delivers items before the sequence completes`` () =
    let gate = TaskCompletionSource ()
    let executor =
        executorFor [ Define.TaskSeqField ("numbers", ListOf IntType, fun _ _ -> gatedNumbers gate.Task) ]
    let expectedData = NameValueLookup.ofList [ "numbers", upcast [] ]
    use firstReceived = new ManualResetEventSlim false
    let result = executeQuery executor "{ numbers @stream }"
    ensureDeferred result
    <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedData)
        use subscription =
            deferred
            |> Observer.createWithCallback (fun _ event ->
                match event with
                | DeferredPending _ -> ()
                | _ -> firstReceived.Set ())
        if not (firstReceived.Wait (TimeSpan.FromSeconds (float (ms 5)))) then
            fail "Timeout while waiting for the first streamed item"
        // The sequence is blocked on the gate, so only its first item can have been delivered
        Assert.False (subscription.IsCompleted, "The stream must not complete before the sequence produces its last item")
        subscription.Received
        |> withoutPending
        |> single
        |> equals (streamedBatch "numbers" [ 0, 1 ])
        gate.SetResult ()
        subscription.WaitCompleted (timeout = ms 10)
        subscription.Received
        |> withoutCompleted
        |> seqEquals [ streamedBatch "numbers" [ 0, 1 ]; streamedBatch "numbers" [ 1, 2 ] ]

[<Fact>]
let ``TaskSeq field with stream directive delivers its DeferredCompleted marker once, after every item`` () =
    let executor =
        executorFor [ Define.TaskSeqField ("numbers", ListOf IntType, fun _ _ -> asyncItems [ 1; 2; 3 ]) ]
    let result = executeQuery executor "{ numbers @stream }"
    ensureDeferred result
    <| fun _ errors deferred ->
        empty errors
        use subscription = Observer.create deferred
        subscription.WaitCompleted (timeout = ms 10)
        subscription.Received
        |> withoutPending
        |> Seq.toList
        |> equals [
            streamedBatch "numbers" [ 0, 1 ]
            streamedBatch "numbers" [ 1, 2 ]
            streamedBatch "numbers" [ 2, 3 ]
            DeferredCompleted [ box "numbers" ]
        ]

[<Fact>]
let ``TaskSeq field with stream directive emits each item as soon as its fields are resolved`` () =
    // maxConcurrency is explicit (rather than the Environment.ProcessorCount default) so the fast item is
    // guaranteed a concurrent slot alongside the slow one and the assertion below does not depend on the runner's
    // CPU count
    let executor =
        executorFor [
            Define.TaskSeqField ("items", ListOf StreamItemType, (fun _ _ -> asyncItems slowAndFastItems), maxConcurrency = 2)
        ]
    let result = executeQuery executor "{ items @stream { id value } }"
    ensureDeferred result
    <| fun _ errors deferred ->
        empty errors
        waitForCompletion deferred
        |> withoutCompleted
        |> seqEquals [
            DeferredResult ([| box (NameValueLookup.ofList [ "id", upcast 2; "value", upcast "fast" ]) |], [ box "items"; box 1 ])
            DeferredResult ([| box (NameValueLookup.ofList [ "id", upcast 1; "value", upcast "slow" ]) |], [ box "items"; box 0 ])
        ]

[<Fact>]
let ``TaskSeq field with stream directive groups items by the preferred batch size of the query`` () =
    let executor =
        executorFor [ Define.TaskSeqField ("numbers", ListOf IntType, fun _ _ -> asyncItems [ 1; 2; 3; 4; 5 ]) ]
    let result = executeQuery executor "{ numbers @stream(preferredBatchSize: 2) }"
    ensureDeferred result
    <| fun _ errors deferred ->
        empty errors
        waitForCompletion deferred
        |> withoutCompleted
        |> seqEquals [
            streamedBatch "numbers" [ 0, 1; 1, 2 ]
            streamedBatch "numbers" [ 2, 3; 3, 4 ]
            streamedBatch "numbers" [ 4, 5 ]
        ]

[<Fact>]
let ``TaskSeq field with fixed batching groups streamed items without query arguments`` () =
    let executor =
        executorFor [
            Define.TaskSeqField ("numbers", ListOf IntType, (fun _ _ -> asyncItems [ 1; 2; 3; 4; 5 ]), batching = StreamBatching.Fixed 2)
        ]
    let result = executeQuery executor "{ numbers @stream }"
    ensureDeferred result
    <| fun _ errors deferred ->
        empty errors
        waitForCompletion deferred
        |> withoutCompleted
        |> seqEquals [
            streamedBatch "numbers" [ 0, 1; 1, 2 ]
            streamedBatch "numbers" [ 2, 3; 3, 4 ]
            streamedBatch "numbers" [ 4, 5 ]
        ]

[<Fact>]
let ``TaskSeq field with batching from source groups streamed items by the page size of the source`` () =
    let executor =
        executorFor [
            Define.TaskSeqField (
                "numbers",
                ListOf IntType,
                (fun _ _ -> PagedAsyncEnumerable (3, [ 1; 2; 3; 4; 5; 6 ]) :> IAsyncEnumerable<int>),
                batching = StreamBatching.FromSource pageSizeOf
            )
        ]
    let result = executeQuery executor "{ numbers @stream }"
    ensureDeferred result
    <| fun _ errors deferred ->
        empty errors
        waitForCompletion deferred
        |> withoutCompleted
        |> seqEquals [ streamedBatch "numbers" [ 0, 1; 1, 2; 2, 3 ]; streamedBatch "numbers" [ 3, 4; 4, 5; 5, 6 ] ]

[<Fact>]
let ``TaskSeq field with batching from source delivers items one by one when the source has no page size`` () =
    let executor =
        executorFor [
            Define.TaskSeqField ("numbers", ListOf IntType, (fun _ _ -> asyncItems [ 1; 2 ]), batching = StreamBatching.FromSource pageSizeOf)
        ]
    let result = executeQuery executor "{ numbers @stream }"
    ensureDeferred result
    <| fun _ errors deferred ->
        empty errors
        waitForCompletion deferred
        |> withoutCompleted
        |> seqEquals [ streamedBatch "numbers" [ 0, 1 ]; streamedBatch "numbers" [ 1, 2 ] ]

[<Fact>]
let ``TaskSeq field backed by Azure AsyncPageable returns items of all pages without directives`` () =
    let executor =
        executorFor [
            Define.TaskSeqField ("numbers", ListOf IntType, fun _ _ -> AsyncPageable<int>.FromPages(azurePages 2 [ 1..5 ]) :> IAsyncEnumerable<int>)
        ]
    let expectedData = NameValueLookup.ofList [ "numbers", upcast [| box 1; box 2; box 3; box 4; box 5 |] ]
    let result = executeQuery executor "{ numbers }"
    ensureDirect result
    <| fun data errors ->
        empty errors
        data |> equals (upcast expectedData)

[<Fact>]
let ``TaskSeq field backed by Azure AsyncPageable streams items in batches of the kept page size hint`` () =
    let executor =
        executorFor [
            Define.TaskSeqField (
                "numbers",
                ListOf IntType,
                (fun _ _ -> HintedAsyncPageable (2, azurePages 2 [ 1..5 ]) :> IAsyncEnumerable<int>),
                batching = StreamBatching.FromSource azurePageSizeOf
            )
        ]
    let result = executeQuery executor "{ numbers @stream }"
    ensureDeferred result
    <| fun _ errors deferred ->
        empty errors
        waitForCompletion deferred
        |> withoutCompleted
        |> seqEquals [
            streamedBatch "numbers" [ 0, 1; 1, 2 ]
            streamedBatch "numbers" [ 2, 3; 3, 4 ]
            streamedBatch "numbers" [ 4, 5 ]
        ]

[<Fact>]
let ``TaskSeq field backed by plain Azure AsyncPageable streams items one by one because it has no page size`` () =
    let executor =
        executorFor [
            Define.TaskSeqField (
                "numbers",
                ListOf IntType,
                (fun _ _ -> AsyncPageable<int>.FromPages(azurePages 2 [ 1..3 ]) :> IAsyncEnumerable<int>),
                batching = StreamBatching.FromSource azurePageSizeOf
            )
        ]
    let result = executeQuery executor "{ numbers @stream }"
    ensureDeferred result
    <| fun _ errors deferred ->
        empty errors
        waitForCompletion deferred
        |> withoutCompleted
        |> seqEquals [
            streamedBatch "numbers" [ 0, 1 ]
            streamedBatch "numbers" [ 1, 2 ]
            streamedBatch "numbers" [ 2, 3 ]
        ]

[<Fact>]
let ``Preferred batch size of the stream directive overrides the batching of the TaskSeq field`` () =
    let executor =
        executorFor [
            Define.TaskSeqField ("numbers", ListOf IntType, (fun _ _ -> asyncItems [ 1; 2; 3 ]), batching = StreamBatching.Fixed 3)
        ]
    let result = executeQuery executor "{ numbers @stream(preferredBatchSize: 1) }"
    ensureDeferred result
    <| fun _ errors deferred ->
        empty errors
        waitForCompletion deferred
        |> withoutCompleted
        |> seqEquals [
            streamedBatch "numbers" [ 0, 1 ]
            streamedBatch "numbers" [ 1, 2 ]
            streamedBatch "numbers" [ 2, 3 ]
        ]

[<Fact>]
let ``Batching from source runs only for a stream query that does not override the batch size, and only once`` () =
    let mutable callCount = 0
    let batching =
        StreamBatching.FromSource (fun _ ->
            callCount <- callCount + 1
            ValueNone)
    let executor =
        executorFor [
            Define.TaskSeqField ("numbers", ListOf IntType, (fun _ _ -> asyncItems [ 1; 2; 3 ]), batching = batching)
            Define.TaskSeqField ("deferrable", Nullable (ListOf IntType), (fun _ _ -> Some (asyncItems [ 1; 2 ])), batching = batching)
        ]
    executeQuery executor "{ numbers }" |> ignore
    callCount |> equals 0
    executeQuery executor "{ deferrable @defer }" |> ignore
    callCount |> equals 0
    executeQuery executor "{ numbers @stream(preferredBatchSize: 1) }"
    |> ignore
    callCount |> equals 0
    executeQuery executor "{ numbers @stream }" |> ignore
    callCount |> equals 1

[<Fact>]
let ``Throwing batching callback does not affect a query that does not stream the field`` () =
    let throwingBatching =
        StreamBatching.FromSource (fun _ -> failwith "Batching must not run for this query")
    let executor =
        executorFor [
            Define.TaskSeqField ("numbers", ListOf IntType, (fun _ _ -> asyncItems [ 1; 2; 3 ]), batching = throwingBatching)
        ]
    let expectedData = NameValueLookup.ofList [ "numbers", upcast [| box 1; box 2; box 3 |] ]
    let result = executeQuery executor "{ numbers }"
    ensureDirect result
    <| fun data errors ->
        empty errors
        data |> equals (upcast expectedData)

[<Fact>]
let ``Nullable TaskSeq field that fails during enumeration returns null with a field error`` () =
    let executor =
        executorFor [ Define.TaskSeqField ("numbers", Nullable (ListOf IntType), fun _ _ -> Some (failingNumbers ())) ]
    let expectedData = NameValueLookup.ofList [ "numbers", null ]
    let result = executeQuery executor "{ numbers }"
    ensureDirect result
    <| fun data errors ->
        data |> equals (upcast expectedData)
        errors
        |> equals [ fieldError "Boom during enumeration" "numbers" ]

[<Fact>]
let ``Non-nullable TaskSeq field that fails during enumeration propagates the error`` () =
    let executor =
        executorFor [ Define.TaskSeqField ("numbers", ListOf IntType, fun _ _ -> failingNumbers ()) ]
    let result = executeQuery executor "{ numbers }"
    ensureDirectNullData result
    <| fun errors ->
        errors
        |> single
        |> equals (fieldError "Boom during enumeration" "numbers")

[<Fact>]
let ``Streamed TaskSeq field that fails during enumeration delivers produced items and then the error`` () =
    let executor =
        executorFor [
            Define.TaskSeqField ("failing", ListOf IntType, fun _ _ -> failingNumbers ())
            Define.TaskSeqField ("numbers", ListOf IntType, fun _ _ -> asyncItems [ 10; 20 ])
        ]
    let expectedData =
        NameValueLookup.ofList [ "failing", upcast []; "numbers", upcast [| box 10; box 20 |] ]
    let result = executeQuery executor "{ failing @stream numbers }"
    ensureDeferred result
    <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedData)
        waitForCompletion deferred
        |> withoutCompleted
        |> seqEquals [
            streamedBatch "failing" [ 0, 1 ]
            streamedBatch "failing" [ 1, 2 ]
            DeferredErrors (null, [ fieldError "Boom during enumeration" "failing" ], [ box "failing" ])
        ]

[<Fact>]
let ``Streamed TaskSeq field that fails acquiring the enumerator still delivers its DeferredErrors`` () =
    // Regression test: this used to fault the merged deferred observable of the whole query instead of producing
    // this field's DeferredErrors, which would drop sibling deferred results and the final completion payload
    let executor =
        executorFor [
            Define.TaskSeqField (
                "failing",
                ListOf IntType,
                fun _ _ -> ThrowingAsyncEnumerable<int> "Boom acquiring the enumerator" :> IAsyncEnumerable<int>
            )
            Define.TaskSeqField ("numbers", ListOf IntType, fun _ _ -> asyncItems [ 10; 20 ])
        ]
    let expectedData =
        NameValueLookup.ofList [ "failing", upcast []; "numbers", upcast [| box 10; box 20 |] ]
    let result = executeQuery executor "{ failing @stream numbers }"
    ensureDeferred result
    <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedData)
        waitForCompletion deferred
        |> withoutCompleted
        |> seqEquals [
            DeferredErrors (null, [ fieldError "Boom acquiring the enumerator" "failing" ], [ box "failing" ])
        ]

[<Fact>]
let ``Streamed TaskSeq field emits a slower earlier item before the enumeration failure that follows it`` () =
    // Regression test: an item resolved asynchronously must not be overtaken by a failure of the source that
    // is pulled right after it, even though the failure itself completes immediately
    let executor =
        executorFor [
            Define.TaskSeqField ("items", ListOf StreamItemType, fun _ _ -> itemThenFailure { Id = 1; Value = delay 500 "slow" })
        ]
    let result = executeQuery executor "{ items @stream { id value } }"
    ensureDeferred result
    <| fun _ errors deferred ->
        empty errors
        waitForCompletion deferred
        |> withoutCompleted
        |> seqEquals [
            DeferredResult ([| box (NameValueLookup.ofList [ "id", upcast 1; "value", upcast "slow" ]) |], [ box "items"; box 0 ])
            DeferredErrors (null, [ fieldError "Boom during enumeration" "items" ], [ box "items" ])
        ]

[<Fact>]
let ``Streamed TaskSeq field delivers an item's own resolver error and keeps streaming the items after it`` () =
    // Regression test: an item whose own field resolution fails is a normal (non-throwing) result as far as the
    // streaming operator is concerned, so it must not be mistaken for a failure of the source or of the enumeration
    // itself: the item's error is delivered on its own path and later items keep streaming, exactly like @stream on
    // an ordinary list (see DeferredTests."Resolver list error")
    let items = [
        { Id = 1; Value = async { return failwith "Boom resolving the item" } }
        { Id = 2; Value = async { return "two" } }
    ]
    let executor =
        executorFor [
            Define.TaskSeqField ("items", ListOf StreamItemType, (fun _ _ -> asyncItems items), maxConcurrency = 1)
        ]
    let result = executeQuery executor "{ items @stream { id value } }"
    ensureDeferred result
    <| fun _ errors deferred ->
        empty errors
        waitForCompletion deferred
        |> withoutCompleted
        |> Seq.map (function
            | DeferredErrors (data, errors, path) ->
                DeferredErrors (
                    data,
                    errors
                    |> List.map (fun error -> { error with Exception = ValueNone }),
                    path
                )
            | event -> event)
        |> Seq.toList
        |> seqEquals (
            [
                DeferredErrors (
                    null,
                    [
                        GQLProblemDetails.CreateWithKind ("Boom resolving the item", Execution, [ box "items"; box 0; box "value" ])
                    ],
                    [ box "items"; box 0 ]
                )
                DeferredResult ([| box (NameValueLookup.ofList [ "id", upcast 2; "value", upcast "two" ]) |], [ box "items"; box 1 ])
            ]
        )

[<Fact>]
let ``A batch containing a failed item alongside a succeeding one is delivered as one DeferredErrors event`` () =
    // Regression test for the tenth Copilot review thread PRRT_kwDOA0s7t86i5Vu-, which claimed that
    // Execution.collectItems' chunk branch omits a failed item's index from `indices` while still reserving its
    // slot in `data`, so GraphQLWebsocketMiddleware.splitBatch's List.map2 would throw on a mixed success/error
    // batch. It does not: both arms of `merge` prepend the item's index, so `indices` and `data` always end up the
    // same length as the chunk, with the failed item's slot left null. This pins that shape end to end.
    let items = [
        { Id = 1; Value = async { return failwith "Boom resolving item 0" } }
        { Id = 2; Value = async { return "two" } }
    ]
    let executor =
        executorFor [
            Define.TaskSeqField ("items", ListOf StreamItemType, (fun _ _ -> asyncItems items), batching = StreamBatching.Fixed 2, maxConcurrency = 1)
        ]
    let result = executeQuery executor "{ items @stream { id value } }"
    ensureDeferred result
    <| fun _ errors deferred ->
        empty errors
        let actual =
            waitForCompletion deferred
            |> withoutCompleted
            |> Seq.exactlyOne
        match actual with
        | DeferredErrors (data, [ error ], path) ->
            Assert.True ((path = [ box "items"; box [ box 0; box 1 ] ]), "Unexpected batch path")
            Assert.Equal ("Boom resolving item 0", error.Message)
            Assert.True (error.Path.ToString().Contains("items"), "Expected item error path")
            Assert.NotNull data
        | _ -> Assert.Fail $"Expected one batched DeferredErrors event, but got {actual}"

[<Fact>]
let ``Disposing the stream subscription stops the enumeration of the TaskSeq field`` () : Task = task {
    let pulled = ref 0
    let disposed = TaskCompletionSource ()
    let received = TaskCompletionSource ()
    let executor =
        executorFor [ Define.TaskSeqField ("numbers", ListOf IntType, fun _ _ -> endlessNumbers pulled disposed) ]
    let! result = executor.AsyncExecute (parse "{ numbers @stream }", getMockInputContext, ())
    match result with
    | Deferred (_, errors, deferred) ->
        empty errors
        let subscription =
            deferred
            |> Observable.subscribe (fun _ -> received.TrySetResult () |> ignore)
        do! waitForTask (TimeSpan.FromSeconds (float (ms 5))) "Timeout while waiting for the first streamed item" received.Task
        subscription.Dispose ()
        do!
            waitForTask
                (TimeSpan.FromSeconds (float (ms 5)))
                "The sequence enumerator was not disposed after the subscription had been disposed"
                disposed.Task
        let pulledAfterDisposal = pulled.Value
        // A still running enumeration would pull more items during this delay
        do! Task.Delay 200
        pulled.Value |> equals pulledAfterDisposal
    | response -> fail $"Expected a 'Deferred' GQLResponse but got\n{response}"
}

[<Fact>]
let ``TaskSeq field with stream directive never resolves more than maxConcurrency items at the same time`` () =
    let inFlight = ref 0
    let maxObserved = ref 0
    let trackConcurrency (work : Async<'T>) : Async<'T> = async {
        let current = Interlocked.Increment inFlight
        let mutable observed = maxObserved.Value
        while current > observed
              && Interlocked.CompareExchange (maxObserved, current, observed)
                 <> observed do
            observed <- maxObserved.Value
        try
            return! work
        finally
            Interlocked.Decrement inFlight |> ignore
    }
    let items = [ for id in 1..6 -> { Id = id; Value = trackConcurrency (delay 100 (string id)) } ]
    let executor =
        executorFor [
            Define.TaskSeqField ("items", ListOf StreamItemType, (fun _ _ -> asyncItems items), maxConcurrency = 2)
        ]
    let result = executeQuery executor "{ items @stream { id value } }"
    ensureDeferred result
    <| fun _ errors deferred ->
        empty errors
        let received = waitForCompletion deferred
        received |> withoutCompleted |> Seq.length |> equals 6
        Assert.True (maxObserved.Value <= 2, $"Expected at most 2 concurrent item resolutions, but observed {maxObserved.Value}")

[<Fact>]
let ``TaskSeqField with a non-positive maxConcurrency fails at definition time`` () =
    throws<ArgumentException>(fun () ->
        Define.TaskSeqField ("numbers", ListOf IntType, (fun _ _ -> asyncItems [ 1 ]), maxConcurrency = 0)
        |> ignore)

[<Fact>]
let ``TaskSeq field resolved as null reports a non-null field error`` () =
    let executor =
        executorFor [
            Define.TaskSeqField ("numbers", ListOf IntType, fun _ _ -> Unchecked.defaultof<IAsyncEnumerable<int>>)
        ]
    let result = executeQuery executor "{ numbers }"
    ensureDirectNullData result
    <| fun errors -> hasError "Non-Null field numbers resolved as a null!" errors
