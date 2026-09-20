module FSharp.Data.GraphQL.Tests.AspNetCore.IncrementalDeliveryTests

open System
open System.Text.Json.Serialization
open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Server.AspNetCore
open FSharp.Data.GraphQL.Shared.WebSockets

let private itemsPath = [ box "items" ]
let private itemPath index = itemsPath @ [ box index ]
let private batchPath (indices : int list) = itemsPath @ [ box (indices |> List.map box) ]
let private fieldError message path = GQLProblemDetails.CreateWithKind (message, Execution, path)

let private pendingIds (result : SubscriptionExecutionResult voption) =
    match result with
    | ValueSome r ->
        r.Pending
        |> Skippable.toValueOption
        |> ValueOption.defaultValue []
        |> List.map _.Id
    | ValueNone -> []

let private pendingPaths (result : SubscriptionExecutionResult voption) =
    match result with
    | ValueSome r ->
        r.Pending
        |> Skippable.toValueOption
        |> ValueOption.defaultValue []
        |> List.map _.Path
    | ValueNone -> []

let private pendingLabels (result : SubscriptionExecutionResult voption) =
    match result with
    | ValueSome r ->
        r.Pending
        |> Skippable.toValueOption
        |> ValueOption.defaultValue []
        |> List.map _.Label
    | ValueNone -> []

let private incrementalOf (result : SubscriptionExecutionResult voption) =
    match result with
    | ValueSome r ->
        r.Incremental
        |> Skippable.toValueOption
        |> ValueOption.defaultValue []
    | ValueNone -> []

let private completedOf (result : SubscriptionExecutionResult voption) =
    match result with
    | ValueSome r ->
        r.Completed
        |> Skippable.toValueOption
        |> ValueOption.defaultValue []
    | ValueNone -> []

[<Fact>]
let ``In-order items are announced once and delivered one entry per event`` () =
    let delivery = IncrementalDelivery ()
    let p0 = delivery.Apply (DeferredResult (box 10, itemPath 0))
    let p1 = delivery.Apply (DeferredResult (box 11, itemPath 1))
    let pc = delivery.Apply (DeferredCompleted itemsPath)
    pendingIds p0 |> single |> ignore
    (incrementalOf p0 |> single).Items
    |> equals (Include [| box 10 |])
    pendingIds p1 |> empty
    (incrementalOf p1 |> single).Items
    |> equals (Include [| box 11 |])
    completedOf pc |> single |> fun c -> c.Errors |> equals Skip

[<Fact>]
let ``Out-of-order items are buffered until the gap before them fills, then flush together`` () =
    let delivery = IncrementalDelivery ()
    let p1 = delivery.Apply (DeferredResult (box "one", itemPath 1))
    let p0 = delivery.Apply (DeferredResult (box "zero", itemPath 0))
    let p2 = delivery.Apply (DeferredResult (box "two", itemPath 2))
    pendingIds p1 |> single |> ignore
    incrementalOf p1 |> empty
    (incrementalOf p0 |> single).Items
    |> equals (Include [| box "zero"; box "one" |])
    (incrementalOf p2 |> single).Items
    |> equals (Include [| box "two" |])

[<Fact>]
let ``A batch is delivered as the items of its own contiguous run`` () =
    let delivery = IncrementalDelivery ()
    let pBatch = delivery.Apply (DeferredResult (box [| box "B2"; box "B1" |], batchPath [ 2; 1 ]))
    let p0 = delivery.Apply (DeferredResult (box "B0", itemPath 0))
    incrementalOf pBatch |> empty
    (incrementalOf p0 |> single).Items
    |> equals (Include [| box "B0"; box "B1"; box "B2" |])

[<Fact>]
let ``An item's own error flushes with the run and does not stop the stream`` () =
    let delivery = IncrementalDelivery ()
    let error = fieldError "Boom" [ box "items"; box 0; box "value" ]
    let pErr = delivery.Apply (DeferredErrors (null, [ error ], itemPath 0))
    let p1 = delivery.Apply (DeferredResult (box "one", itemPath 1))
    let entry0 = incrementalOf pErr |> single
    entry0.Items |> equals (Include [| null |])
    entry0.Errors |> equals (Include [ error ])
    (incrementalOf p1 |> single).Items
    |> equals (Include [| box "one" |])

[<Fact>]
let ``A streamed item that is itself an empty list is preserved as the item value`` () =
    let delivery = IncrementalDelivery ()
    let payload = delivery.Apply (DeferredResult (box [||], itemPath 0))
    (incrementalOf payload |> single).Items
    |> equals (Include [| box [||] |])

[<Fact>]
let ``A stream failing after an item folds the failure into its completion, dropping unflushed items`` () =
    let delivery = IncrementalDelivery ()
    let error = fieldError "Boom during enumeration" [ box "items" ]
    let p0 = delivery.Apply (DeferredResult (box "zero", itemPath 0))
    // Item 2 arrives out of order and is buffered, waiting for item 1, which never comes
    let p2 = delivery.Apply (DeferredResult (box "two", itemPath 2))
    let pFail = delivery.Apply (DeferredErrors (null, [ error ], itemsPath))
    let pc = delivery.Apply (DeferredCompleted itemsPath)
    (incrementalOf p0 |> single).Items
    |> equals (Include [| box "zero" |])
    incrementalOf p2 |> empty // buffered, not flushable (item 1 missing)
    incrementalOf pFail |> empty
    (completedOf pFail |> single).Errors
    |> equals (Include [ error ])
    pc |> equals ValueNone // already closed by the failure; the later DeferredCompleted is a no-op

[<Fact>]
let ``A stream pending is emitted with the payload that exposes its containing data`` () =
    let delivery = IncrementalDelivery ()
    let parentPath = [ box "container" ]
    let streamPath = parentPath @ itemsPath
    delivery.Apply (DeferredPending (streamPath, ValueNone, true))
    |> equals ValueNone
    let payload =
        delivery.Apply (DeferredResult (box (NameValueLookup.ofList [ "items", upcast [||] ]), parentPath))
    let pending = pendingPaths payload
    Assert.Contains (streamPath, pending)
    Assert.Contains (parentPath, pending)
    let entry = incrementalOf payload |> single
    entry.Data
    |> equals (Include (box (NameValueLookup.ofList [ "items", upcast [||] ])))
    entry.Errors |> equals Skip

[<Fact>]
let ``A nested stream pending waits for the deferred payload that exposes it`` () =
    let delivery = IncrementalDelivery ()
    let parentPath = [ box "parent" ]
    let childPath = parentPath @ [ box "child" ]
    let streamPath = childPath @ [ box "items" ]
    delivery.Apply (DeferredPending (streamPath, ValueNone, true))
    |> equals ValueNone
    let parentPayload =
        delivery.Apply (DeferredResult (box (NameValueLookup.ofList [ "child", null ]), parentPath))
    pendingPaths parentPayload |> equals [ parentPath ]
    let childPayload =
        delivery.Apply (DeferredResult (box (NameValueLookup.ofList [ "items", upcast [||] ]), childPath))
    pendingPaths childPayload
    |> equals [ childPath; streamPath ]

[<Fact>]
let ``A nested stream pending is visible through F# list payloads`` () =
    let delivery = IncrementalDelivery ()
    let parentPath = [ box "parent" ]
    let streamPath = parentPath @ [ box "items"; box 0; box "children" ]
    delivery.Apply (DeferredPending (streamPath, ValueNone, true))
    |> equals ValueNone
    let payload =
        delivery.Apply (
            DeferredResult (box (NameValueLookup.ofList [ "items", upcast [ box (NameValueLookup.ofList [ "children", upcast [] ]) ] ]), parentPath)
        )
    pendingPaths payload |> equals [ parentPath; streamPath ]

[<Fact>]
let ``A labeled defer pending is emitted with the deferred field payload`` () =
    let delivery = IncrementalDelivery ()
    let path = [ box "testData"; box "a" ]
    delivery.Apply (DeferredPending (path, ValueSome "hero", false))
    |> equals ValueNone
    let payload = delivery.Apply (DeferredResult (box "value", path))
    pendingPaths payload |> equals [ path ]
    pendingLabels payload |> equals [ Include "hero" ]
    let entry = incrementalOf payload |> single
    entry.Data |> equals (Include (box "value"))

[<Fact>]
let ``A completed deferred path reused by a later update gets a fresh id and completion`` () =
    let delivery = IncrementalDelivery ()
    let path = [ box "parent"; box "child" ]
    let firstPayload = delivery.Apply (DeferredResult (box "first", path))
    let firstId = pendingIds firstPayload |> single
    (completedOf (delivery.Apply (DeferredCompleted path))
     |> single)
        .Id
    |> equals firstId
    let secondPayload = delivery.Apply (DeferredResult (box "second", path))
    let secondId = pendingIds secondPayload |> single
    Assert.NotEqual<string>(firstId, secondId)
    (completedOf (delivery.Apply (DeferredCompleted path))
     |> single)
        .Id
    |> equals secondId

[<Fact>]
let ``A completed stream path reused by a later update gets a fresh id and completion`` () =
    let delivery = IncrementalDelivery ()
    let streamPath = [ box "parent"; box "items" ]
    let itemPath index = streamPath @ [ box index ]
    let firstPayload = delivery.Apply (DeferredResult (box "first", itemPath 0))
    let firstId = pendingIds firstPayload |> single
    (completedOf (delivery.Apply (DeferredCompleted streamPath))
     |> single)
        .Id
    |> equals firstId
    let secondPayload = delivery.Apply (DeferredResult (box "second", itemPath 0))
    let secondId = pendingIds secondPayload |> single
    Assert.NotEqual<string>(firstId, secondId)
    (completedOf (delivery.Apply (DeferredCompleted streamPath))
     |> single)
        .Id
    |> equals secondId

[<Fact>]
let ``A stream failing before any item completes with errors instead of replacing the list with null`` () =
    let delivery = IncrementalDelivery ()
    delivery.Apply (DeferredPending ([ box "failing" ], ValueNone, true))
    |> equals ValueNone
    let error = fieldError "Boom acquiring the enumerator" [ box "failing" ]
    let pFail = delivery.Apply (DeferredErrors (null, [ error ], [ box "failing" ]))
    let pc = delivery.Apply (DeferredCompleted [ box "failing" ])
    incrementalOf pFail |> empty
    (completedOf pFail |> single).Errors
    |> equals (Include [ error ])
    pc |> equals ValueNone

[<Fact>]
let ``An empty stream still produces a completed entry after being pre-announced`` () =
    let delivery = IncrementalDelivery ()
    delivery.Apply (DeferredPending (itemsPath, ValueNone, true))
    |> equals ValueNone
    let payload = delivery.Apply (DeferredCompleted itemsPath)
    pendingIds payload |> empty
    incrementalOf payload |> empty
    (completedOf payload |> single).Errors |> equals Skip

[<Fact>]
let ``A pending stream buffered before worker initialization is emitted in the initial payload`` () =
    let delivery = IncrementalDelivery ()
    let bufferedMessages = ResizeArray<DeferredSubscriptionWorkerMessage>()
    let data = NameValueLookup.ofList [ "items", upcast [||] ]

    DeferredSubscriptionWorker.bufferMessageBeforeInitial
        delivery
        bufferedMessages
        (DeferredEvent (ValueSome (DeferredPending (itemsPath, ValueNone, true))))

    DeferredSubscriptionWorker.bufferMessageBeforeInitial delivery bufferedMessages DeferredSourceCompleted

    let initial = SubscriptionExecutionResult.CreateInitial (data, [], delivery.TakePendingVisibleIn data)
    pendingPaths (ValueSome initial) |> equals [ itemsPath ]
    bufferedMessages
    |> Seq.toList
    |> equals [ DeferredSourceCompleted ]

[<Fact>]
let ``A completion buffered before worker initialization still leaves the initial payload first`` () =
    let delivery = IncrementalDelivery ()
    let bufferedMessages = ResizeArray<DeferredSubscriptionWorkerMessage>()
    let data = NameValueLookup.ofList [ "items", upcast [||] ]

    DeferredSubscriptionWorker.bufferMessageBeforeInitial delivery bufferedMessages DeferredSourceCompleted

    let initial = SubscriptionExecutionResult.CreateInitial (data, [], delivery.TakePendingVisibleIn data)
    initial.HasNext |> equals (Include true)
    pendingPaths (ValueSome initial) |> empty
    bufferedMessages
    |> Seq.toList
    |> equals [ DeferredSourceCompleted ]

[<Fact>]
let ``An error buffered before worker initialization still leaves the initial payload first`` () =
    let delivery = IncrementalDelivery ()
    let bufferedMessages = ResizeArray<DeferredSubscriptionWorkerMessage>()
    let data = NameValueLookup.ofList [ "items", upcast [||] ]
    let ex = InvalidOperationException "boom"

    DeferredSubscriptionWorker.bufferMessageBeforeInitial delivery bufferedMessages (DeferredFaulted ex)

    let initial = SubscriptionExecutionResult.CreateInitial (data, [], delivery.TakePendingVisibleIn data)
    initial.HasNext |> equals (Include true)
    pendingPaths (ValueSome initial) |> empty

    match bufferedMessages |> Seq.toList with
    | [ DeferredFaulted bufferedEx ] -> Assert.Same (ex, bufferedEx)
    | other -> failwith $"Unexpected buffered messages: %A{other}"

[<Fact>]
let ``A defer field's own value is announced and delivered, then completes`` () =
    let delivery = IncrementalDelivery ()
    let path = [ box "testData"; box "a" ]
    let pOk = delivery.Apply (DeferredResult (box "value", path))
    let pc = delivery.Apply (DeferredCompleted path)
    pendingIds pOk |> single |> ignore
    let entry = incrementalOf pOk |> single
    entry.Data |> equals (Include (box "value"))
    entry.Errors |> equals Skip
    (completedOf pc |> single).Errors |> equals Skip

[<Fact>]
let ``Finish completes every field that has not completed on its own, with hasNext false`` () =
    let delivery = IncrementalDelivery ()
    delivery.Apply (DeferredResult (box "value", [ box "testData"; box "live" ]))
    |> ignore
    let final = delivery.Finish ()
    (final.Completed
     |> Skippable.toValueOption
     |> wantValueSome
     |> single)
        .Errors
    |> equals Skip
    final.HasNext |> equals (Include false)

[<Fact>]
let ``A live field reuses the same id across repeated updates and is only ever closed by Finish`` () =
    let delivery = IncrementalDelivery ()
    let path = [ box "testData"; box "live" ]
    let p1 = delivery.Apply (DeferredResult (box "v1", path))
    let p2 = delivery.Apply (DeferredResult (box "v2", path))
    pendingIds p1 |> single |> ignore
    pendingIds p2 |> empty
    (incrementalOf p2 |> single).Data
    |> equals (Include (box "v2"))
    (delivery.Finish ()).Completed
    |> Skippable.toValueOption
    |> wantValueSome
    |> single
    |> ignore

// ---------------------------------------------------------------------------------------------------------------------
// Incremental delivery spec v0.2 coverage of the translator. Tests marked Skip capture behaviour the spec requires but
// the translator does not implement yet; each names the gap in its Skip reason.
// ---------------------------------------------------------------------------------------------------------------------

[<Fact>]
let ``A stream pending carries its label into the pending entry`` () =
    let delivery = IncrementalDelivery ()
    delivery.Apply (DeferredPending (itemsPath, ValueSome "friends", true))
    |> equals ValueNone
    let payload = delivery.Apply (DeferredResult (box 1, itemPath 0))
    pendingPaths payload |> equals [ itemsPath ]
    pendingLabels payload |> equals [ Include "friends" ]

[<Fact>]
let ``The same deferred field announced twice with the same label is announced to the client once`` () =
    let delivery = IncrementalDelivery ()
    let path = [ box "testData"; box "a" ]
    delivery.Apply (DeferredPending (path, ValueSome "hero", false))
    |> equals ValueNone
    delivery.Apply (DeferredPending (path, ValueSome "hero", false))
    |> equals ValueNone
    let payload = delivery.Apply (DeferredResult (box "value", path))
    let id = pendingIds payload |> single
    (incrementalOf payload |> single).Id |> equals id

[<Fact(Skip = "Not implemented: deferred fragments are keyed by path only; distinct labels at the same path need distinct ids")>]
let ``Distinct labels at the same path are distinct pendings`` () =
    let delivery = IncrementalDelivery ()
    let path = [ box "testData" ]
    delivery.Apply (DeferredPending (path, ValueSome "a", false)) |> ignore
    delivery.Apply (DeferredPending (path, ValueSome "b", false)) |> ignore
    let payload = delivery.Apply (DeferredResult (box (NameValueLookup.ofList [ "a", upcast "Apple" ]), path))
    pendingLabels payload |> equals [ Include "a"; Include "b" ]
    pendingIds payload |> List.distinct |> List.length |> equals 2

[<Fact(Skip = "Not implemented: Finish completes a pre-announced field the client never saw as pending")>]
let ``A pre-announced stream whose parent is null is neither announced nor completed`` () =
    let delivery = IncrementalDelivery ()
    let streamPath = [ box "parent"; box "items" ]
    delivery.Apply (DeferredPending (streamPath, ValueNone, true))
    |> equals ValueNone
    // The parent resolved to null, so the stream is never exposed to the client
    delivery.TakePendingVisibleIn (NameValueLookup.ofList [ "parent", null ]) |> empty
    let final = delivery.Finish ()
    final.Completed |> equals Skip
    final.HasNext |> equals (Include false)

[<Fact>]
let ``A stream nested in a streamed item is announced with a path containing the item index`` () =
    let delivery = IncrementalDelivery ()
    let nestedStreamPath = [ box "items"; box 0; box "children" ]
    delivery.Apply (DeferredPending (nestedStreamPath, ValueNone, true))
    |> equals ValueNone
    let item = NameValueLookup.ofList [ "children", upcast [||] ]
    let payload = delivery.Apply (DeferredResult (box [| box item |], itemPath 0))
    let pending = pendingPaths payload
    pending |> List.length |> equals 2
    pending |> contains itemsPath |> contains nestedStreamPath |> ignore
    (incrementalOf payload |> single).Items |> equals (Include [| box item |])

[<Fact>]
let ``Errors inside a deferred payload are delivered with its partial data and the field still completes without errors`` () =
    let delivery = IncrementalDelivery ()
    let path = [ box "testData"; box "container" ]
    let partialData = NameValueLookup.ofList [ "name", upcast "Container"; "inner", null ]
    let error = fieldError "Non-Null field value resolved as a null!" (path @ [ box "inner"; box "value" ])
    let payload = delivery.Apply (DeferredErrors (box partialData, [ error ], path))
    let completion = delivery.Apply (DeferredCompleted path)
    pendingPaths payload |> equals [ path ]
    let entry = incrementalOf payload |> single
    entry.Data |> equals (Include (box partialData))
    entry.Errors |> equals (Include [ error ])
    (completedOf completion |> single).Errors |> equals Skip

[<Fact(Skip = "Not implemented: a null deferred payload is delivered as incremental data null instead of completing the field with errors")>]
let ``A deferred field whose payload is null with errors completes with those errors and no incremental entry`` () =
    let delivery = IncrementalDelivery ()
    let path = [ box "testData"; box "nullableError" ]
    let error = fieldError "Non-Null field value resolved as a null!" (path @ [ box "value" ])
    let payload = delivery.Apply (DeferredErrors (null, [ error ], path))
    pendingPaths payload |> equals [ path ]
    incrementalOf payload |> empty
    (completedOf payload |> single).Errors |> equals (Include [ error ])
    delivery.Apply (DeferredCompleted path) |> equals ValueNone
