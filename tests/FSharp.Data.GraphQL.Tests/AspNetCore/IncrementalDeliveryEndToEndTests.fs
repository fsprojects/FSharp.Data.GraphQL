module FSharp.Data.GraphQL.Tests.AspNetCore.IncrementalDeliveryEndToEndTests

open System.Collections.Generic
open System.Text.Json.Serialization
open Xunit

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Server.AspNetCore
open FSharp.Data.GraphQL.Shared.WebSockets
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Tests.DeferredTests

// Runs a query through the executor and its deferred events through IncrementalDelivery, asserting the whole sequence
// of graphql-transport-ws payloads a client would receive: the initial payload, every subsequent payload, and the final
// one with hasNext false. This is the only place the engine and the translator are exercised together.

/// <summary>
/// Replays engine events through <see cref="IncrementalDelivery"/> the way the websocket subscription worker does:
/// announcements that precede the first value-carrying event are applied before the initial payload, so they can be
/// part of its <c>pending</c>; the initial payload is built from the announcements visible in its data; every later
/// event is applied in order; and <see cref="IncrementalDelivery.Finish"/> closes the delivery.
/// </summary>
let private translate (data : Output) (errors : GQLProblemDetails list) (events : GQLDeferredResponseContent list) =
    let delivery = IncrementalDelivery ()
    let payloads = ResizeArray<SubscriptionExecutionResult> ()
    let mutable initialSent = false
    let sendInitial () =
        if not initialSent then
            initialSent <- true
            payloads.Add (SubscriptionExecutionResult.CreateInitial (data, errors, delivery.TakePendingVisibleIn data))
    for event in events do
        match event with
        | DeferredPending _
        | DeferredFragmentPending _ when not initialSent -> delivery.Apply event |> ignore
        | event ->
            sendInitial ()
            delivery.Apply event |> ValueOption.iter payloads.Add
    sendInitial ()
    payloads.Add (delivery.Finish ())
    List.ofSeq payloads

/// Collects every deferred event of the result, then translates them into the payload sequence. The wait is generous:
/// a slow item of the test data sleeps for seconds, and a loaded CI runner stretches that several times over.
let private deliver (result : GQLExecutionResult) =
    let payloads = ResizeArray<SubscriptionExecutionResult> ()
    ensureDeferred result <| fun data errors deferred ->
        use sub = Observer.create deferred
        sub.WaitCompleted (timeout = 120)
        payloads.AddRange (translate data errors (sub.Received |> Seq.toList))
    List.ofSeq payloads

let private skippableList (value : 'T list Skippable) =
    value
    |> Skippable.toValueOption
    |> ValueOption.defaultValue []

let private pendingOf (payload : SubscriptionExecutionResult) = skippableList payload.Pending
let private incrementalOf (payload : SubscriptionExecutionResult) = skippableList payload.Incremental
let private completedOf (payload : SubscriptionExecutionResult) = skippableList payload.Completed
let private pendingPathsOf (payload : SubscriptionExecutionResult) = pendingOf payload |> List.map _.Path

/// <summary>
/// Asserts the invariants every incremental delivery must satisfy on the wire and returns the announced pending
/// entries keyed by id: the first payload carries data and <c>hasNext: true</c>, only the last one has
/// <c>hasNext: false</c> and it carries no data, every id is announced once and completed once, and no entry refers
/// to an id before its announcement or after its completion.
/// </summary>
let private assertWellFormed (payloads : SubscriptionExecutionResult list) =
    match payloads with
    | [] ->
        fail "Expected at least the initial payload"
        Map.empty
    | initial :: _ ->
        let final = List.last payloads
        Assert.True (initial.Data <> Skip, "The initial payload must carry data")
        initial.HasNext |> equals (Include true)
        final.HasNext |> equals (Include false)
        final.Data |> equals Skip
        for payload in payloads |> List.take (payloads.Length - 1) do
            payload.HasNext |> equals (Include true)
        let announced = Dictionary<string, PendingResult> ()
        let completed = HashSet<string> ()
        for payload in payloads do
            for pending in pendingOf payload do
                Assert.False (announced.ContainsKey pending.Id, $"Pending id '{pending.Id}' was announced twice")
                announced[pending.Id] <- pending
            for entry in incrementalOf payload do
                Assert.True (announced.ContainsKey entry.Id, $"An incremental entry refers to id '{entry.Id}' before its announcement")
                Assert.False (completed.Contains entry.Id, $"An incremental entry refers to id '{entry.Id}' after its completion")
            for entry in completedOf payload do
                Assert.True (announced.ContainsKey entry.Id, $"A completed entry refers to id '{entry.Id}' before its announcement")
                Assert.True (completed.Add entry.Id, $"Id '{entry.Id}' was completed twice")
        for id in announced.Keys do
            Assert.True (completed.Contains id, $"Announced id '{id}' was never completed")
        announced
        |> Seq.map (fun kvp -> kvp.Key, kvp.Value)
        |> Map.ofSeq

/// <summary>
/// How a field-level <c>@defer</c> is addressed on the wire, as spec v0.2 requires: the pending entry names the
/// containing object and the incremental entry carries an object map of the one delivered field.
/// </summary>
let private expectDeferredField (parentPath : obj list) (fieldName : string) (value : obj) (pending : PendingResult) (entry : IncrementalResult) =
    pending.Path |> equals parentPath
    entry.Id |> equals pending.Id
    entry.Data |> equals (Include (ValueSome (box (NameValueLookup.ofList [ fieldName, value ]))))
    entry.Items |> equals Skip

[<Fact>]
let ``Labeled deferred field is announced in the initial payload, delivered, completed, then hasNext turns false`` () =
    let query = parse """{
        testData {
            a @defer(label: "hero")
        }
    }"""
    let payloads = executor.AsyncExecute(query, getMockInputContext) |> sync |> deliver
    assertWellFormed payloads |> ignore
    match payloads with
    | [ initial; delivered; completed; final ] ->
        initial.Data
        |> equals (Include (ValueSome (box (NameValueLookup.ofList [ "testData", upcast NameValueLookup.ofList [ "a", null ] ]))))
        let pending = pendingOf initial |> single
        pending.Label |> equals (Include "hero")
        expectDeferredField [ box "testData" ] "a" (box "Apple") pending (incrementalOf delivered |> single)
        let completion = completedOf completed |> single
        completion.Id |> equals pending.Id
        completion.Errors |> equals Skip
        pendingOf final |> empty
        incrementalOf final |> empty
        completedOf final |> empty
    | payloads -> fail $"Expected four payloads but got %A{payloads}"

[<Fact>]
let ``Streamed items resolved out of order are delivered to the client in list order in one entry`` () =
    let query = parse """{
        testData {
            delayedList @stream {
                value
            }
        }
    }"""
    let payloads = executor.AsyncExecute(query, getMockInputContext) |> sync |> deliver
    assertWellFormed payloads |> ignore
    match payloads with
    | [ initial; items; completed; final ] ->
        let pending = pendingOf initial |> single
        pending.Path |> equals [ box "testData"; box "delayedList" ]
        // The fast item (index 1) resolves first, is held back until the slow item 0 arrives, and both go out together
        let entry = incrementalOf items |> single
        entry.Id |> equals pending.Id
        entry.Items
        |> equals (
            Include [|
                box (NameValueLookup.ofList [ "value", upcast "Slow" ])
                box (NameValueLookup.ofList [ "value", upcast "Fast" ])
            |]
        )
        (completedOf completed |> single).Id |> equals pending.Id
        final.HasNext |> equals (Include false)
    | payloads -> fail $"Expected four payloads but got %A{payloads}"

[<Fact>]
let ``A stream with initial items announces itself with the initial payload and streams the remaining items`` () =
    let query = parse """{
        testData {
            ifaceList @stream(initialCount: 1) {
                id
            }
        }
    }"""
    let payloads = executor.AsyncExecute(query, getMockInputContext) |> sync |> deliver
    assertWellFormed payloads |> ignore
    match payloads with
    | [ initial; items; completed; final ] ->
        initial.Data
        |> equals (
            Include (
                ValueSome (
                    box (NameValueLookup.ofList [ "testData", upcast NameValueLookup.ofList [ "ifaceList", upcast [ NameValueLookup.ofList [ "id", upcast "2000" ] ] ] ])
                )
            )
        )
        let pending = pendingOf initial |> single
        pending.Path |> equals [ box "testData"; box "ifaceList" ]
        let entry = incrementalOf items |> single
        entry.Id |> equals pending.Id
        entry.Items |> equals (Include [| box (NameValueLookup.ofList [ "id", upcast "3000" ]) |])
        (completedOf completed |> single).Id |> equals pending.Id
        final.HasNext |> equals (Include false)
    | payloads -> fail $"Expected four payloads but got %A{payloads}"

[<Fact>]
let ``A stream nested in a deferred field is announced with the deferred payload that exposes it, never in the initial payload`` () =
    let query = parse """{
        testData {
            innerList @defer {
                a
                innerList @stream {
                    a
                }
            }
        }
    }"""
    let payloads = executor.AsyncExecute(query, getMockInputContext) |> sync |> deliver
    assertWellFormed payloads |> ignore
    // The deferred field is announced at its containing object, the stream at its own list field
    let outerPath = [ box "testData" ]
    let streamPath = [ box "testData"; box "innerList"; box 0; box "innerList" ]
    // The engine delivers the two streamed items either one by one or, when both resolve into the same buffered
    // event, as one batch, so the item payloads between the outer completion and the stream completion are one or two
    match payloads with
    | initial :: outer :: outerCompleted :: (_ :: _ :: _ :: _ as rest) ->
        let items = rest |> List.take (rest.Length - 2)
        let streamCompleted = rest[rest.Length - 2]
        let final = List.last rest
        initial.Pending |> equals Skip
        pendingPathsOf outer |> equals [ outerPath; streamPath ]
        let outerPending = pendingOf outer |> List.find (fun pending -> pending.Path = outerPath)
        let streamPending = pendingOf outer |> List.find (fun pending -> pending.Path = streamPath)
        expectDeferredField
            [ box "testData" ]
            "innerList"
            (box [| box (NameValueLookup.ofList [ "a", upcast "Inner A"; "innerList", upcast [] ]) |])
            outerPending
            (incrementalOf outer |> single)
        (completedOf outerCompleted |> single).Id |> equals outerPending.Id
        let entries = items |> List.map (incrementalOf >> single)
        for entry in entries do
            entry.Id |> equals streamPending.Id
        entries
        |> List.collect (fun entry -> entry.Items |> Skippable.toValueOption |> ValueOption.defaultValue [||] |> List.ofArray)
        |> equals [
            box (NameValueLookup.ofList [ "a", upcast "Inner B" ])
            box (NameValueLookup.ofList [ "a", upcast "Inner C" ])
        ]
        (completedOf streamCompleted |> single).Id |> equals streamPending.Id
        final.HasNext |> equals (Include false)
    | payloads -> fail $"Expected at least six payloads but got %A{payloads}"

[<Fact>]
let ``A stream that fails after an item completes with the error and the delivery still ends with hasNext false`` () =
    let executor =
        Executor (
            Schema (
                Define.Object<unit>("Query", [ Define.TaskSeqField ("failing", ListOf IntType, fun _ _ -> itemThenFailure 1) ]),
                config = SchemaConfig.DefaultWithBufferedStream (streamOptions = { Interval = ValueNone; PreferredBatchSize = ValueNone })
            )
        )
    let query = parse "{ failing @stream }"
    let payloads = executor.AsyncExecute(query, getMockInputContext, ()) |> sync |> deliver
    assertWellFormed payloads |> ignore
    match payloads with
    | [ initial; item; failed; final ] ->
        initial.Data |> equals (Include (ValueSome (box (NameValueLookup.ofList [ "failing", upcast [] ]))))
        let pending = pendingOf initial |> single
        pending.Path |> equals [ box "failing" ]
        (incrementalOf item |> single).Items |> equals (Include [| box 1 |])
        // The source failure closes the stream with errors; no incremental entry replaces the list with null
        incrementalOf failed |> empty
        (completedOf failed |> single).Errors
        |> equals (Include [ GQLProblemDetails.CreateWithKind ("Boom during enumeration", Execution, [ box "failing" ]) ])
        completedOf final |> empty
        final.HasNext |> equals (Include false)
    | payloads -> fail $"Expected four payloads but got %A{payloads}"

// A live field fixture of its own: the test assembly runs modules in parallel and DeferredTests' live data is
// module-level mutable state shared by its own live tests, so it must not be published to from here.
type private LiveSubject = { id : int; mutable value : string }

let private liveConfig = SchemaConfig.DefaultWithBufferedStream (streamOptions = { Interval = ValueNone; PreferredBatchSize = ValueNone })

let private LiveDataType =
    Define.Object<LiveSubject>("LiveData", [ Define.Field ("live", StringType, fun _ (subject : LiveSubject) -> subject.value) ])

let private liveData = { id = 1; value = "some value" }

let private liveExecutor =
    liveConfig.LiveFieldSubscriptionProvider.Register
        {
            FieldName = "live"
            TypeName = "LiveData"
            Filter = (fun (x : LiveSubject) (y : LiveSubject) -> x.id = y.id)
            Project = _.value
        }
    Executor (Schema (Define.Object<unit>("Query", [ Define.Field ("liveData", LiveDataType, fun _ _ -> liveData) ]), config = liveConfig))

[<Fact>]
let ``A live field is announced with its first update and only closed by the final payload`` () =
    let query = parse """{
        liveData {
            live @live
        }
    }"""
    let result = liveExecutor.AsyncExecute(query, getMockInputContext, ()) |> sync
    let payloads =
        let payloads = ResizeArray<SubscriptionExecutionResult> ()
        ensureDeferred result <| fun data errors deferred ->
            use sub = Observer.create deferred
            waitFor (fun () -> liveConfig.LiveFieldSubscriptionProvider.HasSubscribers "LiveData" "live") 10 "Timeout waiting for the live subscription"
            liveData.value <- "another value"
            liveConfig.LiveFieldSubscriptionProvider.Publish "LiveData" "live" liveData
            sub.WaitForItem ()
            payloads.AddRange (translate data errors (sub.Received |> Seq.toList))
        List.ofSeq payloads
    assertWellFormed payloads |> ignore
    match payloads with
    | [ initial; update; final ] ->
        initial.Data
        |> equals (Include (ValueSome (box (NameValueLookup.ofList [ "liveData", upcast NameValueLookup.ofList [ "live", upcast "some value" ] ]))))
        initial.Pending |> equals Skip
        let pending = pendingOf update |> single
        expectDeferredField [ box "liveData" ] "live" (box "another value") pending (incrementalOf update |> single)
        // A live field never completes on its own; only the final payload closes it
        (completedOf final |> single).Id |> equals pending.Id
        final.HasNext |> equals (Include false)
    | payloads -> fail $"Expected three payloads but got %A{payloads}"

[<Fact>]
let ``A deferred fragment is announced and delivered as one payload of its object`` () =
    let query = parse """{
        testData {
            id
            ... @defer(label: "rest") {
                a
                b
            }
        }
    }"""
    let payloads = executor.AsyncExecute(query, getMockInputContext) |> sync |> deliver
    assertWellFormed payloads |> ignore
    match payloads with
    | [ initial; delivered; completed; final ] ->
        initial.Data
        |> equals (Include (ValueSome (box (NameValueLookup.ofList [ "testData", upcast NameValueLookup.ofList [ "id", upcast "1" ] ]))))
        let pending = pendingOf initial |> single
        pending.Path |> equals [ box "testData" ]
        pending.Label |> equals (Include "rest")
        let entry = incrementalOf delivered |> single
        entry.Id |> equals pending.Id
        entry.Data |> equals (Include (ValueSome (box (NameValueLookup.ofList [ "a", upcast "Apple"; "b", upcast "Banana" ]))))
        (completedOf completed |> single).Id |> equals pending.Id
        final.HasNext |> equals (Include false)
    | payloads -> fail $"Expected four payloads but got %A{payloads}"

[<Fact>]
let ``A deferred fragment that fails as a whole is completed with its errors`` () =
    let query = parse """{
        testData {
            id
            ... @defer {
                nonNullError
            }
        }
    }"""
    let payloads = executor.AsyncExecute(query, getMockInputContext) |> sync |> deliver
    assertWellFormed payloads |> ignore
    match payloads with
    | [ initial; failed; final ] ->
        initial.Pending |> equals Skip
        let pending = pendingOf failed |> single
        pending.Path |> equals [ box "testData" ]
        incrementalOf failed |> empty
        let completion = completedOf failed |> single
        completion.Id |> equals pending.Id
        completion.Errors
        |> equals (Include [ GQLProblemDetails.CreateWithKind ("Non-null field error!", Execution, [ box "testData"; box "nonNullError" ]) ])
        final.HasNext |> equals (Include false)
    | payloads -> fail $"Expected three payloads but got %A{payloads}"

[<Fact>]
let ``Field-level defer is delivered as an object map at the parent's path`` () =
    let query = parse """{
        testData {
            a @defer
        }
    }"""
    let payloads = executor.AsyncExecute(query, getMockInputContext) |> sync |> deliver
    assertWellFormed payloads |> ignore
    let pending = payloads |> List.collect pendingOf |> single
    pending.Path |> equals [ box "testData" ]
    let entry = payloads |> List.collect incrementalOf |> single
    entry.Data |> equals (Include (ValueSome (box (NameValueLookup.ofList [ "a", upcast "Apple" ]))))
