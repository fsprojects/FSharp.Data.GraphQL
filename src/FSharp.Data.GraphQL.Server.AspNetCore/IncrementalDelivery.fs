namespace FSharp.Data.GraphQL.Server.AspNetCore

open System.Collections.Generic
open System.Text.Json.Serialization
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Shared.WebSockets

/// Path helpers shared by IncrementalDelivery. Paths are written as `obj list`, not the (internal, and here
/// inaccessible) `FieldPath` abbreviation they stand for: a type abbreviation is erased, so this is the exact
/// same type and unifies fine with FieldPath-typed values from the engine.
[<AutoOpen>]
module private IncrementalDeliveryPaths =

    let pathStartsWith (prefix : obj list) (path : obj list) =
        let prefixLength = List.length prefix
        List.length path >= prefixLength
        && List.truncate prefixLength path = prefix

    /// Matches a path ending in the list of indices of a batch of streamed items, as Execution.collectItems
    /// produces for more than one item resolved into the same buffered event, such as ["items"; [2; 1]].
    [<return : Struct>]
    let (|BatchPath|_|) (path : obj list) =
        match List.rev path with
        | (:? (obj list) as indices) :: fieldPathRev -> ValueSome (List.rev fieldPathRev, indices)
        | _ -> ValueNone

    /// Matches a path ending in a single streamed item's own index, such as ["items"; 0].
    [<return : Struct>]
    let (|ItemPath|_|) (path : obj list) =
        match List.rev path with
        | (:? int as index) :: fieldPathRev -> ValueSome (List.rev fieldPathRev, index)
        | _ -> ValueNone

    /// Matches the path of a deferred field: the path of its containing object, and the field's own name.
    [<return : Struct>]
    let (|DeferredFieldPath|_|) (path : obj list) =
        match List.rev path with
        | (:? string as fieldName) :: parentPathRev -> ValueSome (List.rev parentPathRev, fieldName)
        | _ -> ValueNone

/// Distinguishes a deferred fragment from the fields of the object it belongs to, in the key of its bookkeeping.
[<Struct>]
type private FragmentKey = FragmentKey of fragmentId : int

/// <summary>
/// Mutable per-field bookkeeping of IncrementalDelivery, keyed by a field's own path (with any item index or batch removed).
/// </summary>
type private FieldState
    /// <param name="id">The short id the field is identified by on the wire.</param>
    /// <param name="wirePath">
    /// The path the field is announced at: a streamed field's own path, or the path of the object containing a deferred field.
    /// </param>
    /// <param name="isStream">Whether the field is streamed rather than deferred.</param>
    (id : string, wirePath : obj list, isStream : bool) =
    member _.Id = id
    member _.WirePath = wirePath
    member _.IsStream = isStream
    member val Label : string voption = ValueNone with get, set
    member val Closed = false with get, set
    /// Whether the field's pending entry was sent to the client; only such a field may be completed.
    member val Released = false with get, set
    /// The index of the next streamed item this field expects, in order; irrelevant once IsStream is false.
    member val NextIndex = 0 with get, set
    /// Items received out of order, waiting for the item at NextIndex to fill the gap before them. `member val`,
    /// not a plain computed `member`, so the same dictionary is reused: a computed member's body runs again on
    /// every access, handing back a fresh, empty dictionary each time instead of the one already filled.
    member val Buffer : SortedDictionary<int, obj * GQLProblemDetails list> = SortedDictionary ()

/// <summary>
/// Translates the engine's <see cref="GQLDeferredResponseContent"/> events into the <c>graphql-transport-ws</c> incremental delivery wire format
/// (<c>pending</c>/<c>incremental</c>/<c>completed</c>/<c>hasNext</c>, the format of the
/// <see href="https://github.com/graphql/graphql-spec/pull/1110">incremental delivery specification</see> used by graphql-js 17 and Apollo Client's
/// <c>GraphQL17Alpha9Handler</c>).
/// </summary>
/// <remarks>
///
/// <para>Every deferred or streamed field is announced once and identified afterwards by a short id instead of its path. A deferred field is
/// announced at the path of the object containing it and delivered as an object map of that one field, which is what a client merges into the object
/// at the announced path; the announcement goes out in the same payload as the field's own value, or earlier when the field is labeled. A streamed
/// field is announced at its own path, as soon as its containing data becomes visible to the client, so later item payloads and completions can refer
/// to the id immediately. </para>
///
/// <para>A streamed field's items are delivered to the client in list order: an item produced out of order (the engine resolves up to a field's <c>
/// maxConcurrency</c> items at the same time) is buffered until the item before it arrives, then every contiguous run starting at the next expected
/// index is flushed as one entry - a batch the engine grouped into a single event is simply several items of the same run. </para>
///
/// <para>A stream failure - <see cref="DeferredErrors"/> at the field's own path, with no item index - is folded directly into that field's
/// completion once the field is known to be a stream (pre-announced, or already carrying items): no <c>incremental</c> entry is sent for it, and
/// whatever was still buffered, waiting for a gap to fill, is dropped, since the engine pulls no further items after a failure. A field whose
/// announcement was never released to the client, because the payload that should have exposed it resolved to <see langword="null"/> there, is
/// neither completed nor closed by <see cref="Finish"/>: the client never learned of it. </para>
///
/// <para>Not thread-safe by design: it is driven by exactly one subscription worker, one event at a time. </para>
/// </remarks>
type IncrementalDelivery () =

    let fields = Dictionary<obj list, FieldState>(HashIdentity.Structural)
    // Announcements not yet sent, each with the path of the field it belongs to
    let pending = ResizeArray<struct (obj list * PendingResult)>()
    let mutable nextId = 0

    let wirePathOf (fieldPath : obj list) (isStream : bool) =
        if isStream then
            fieldPath
        else
            match fieldPath with
            | DeferredFieldPath (parentPath, _) -> parentPath
            | _ -> fieldPath

    /// The bookkeeping under the key, announced at the wire path; a closed key delivered again (a live field's
    /// nested deferred fields on a later update) is a new delivery, with a fresh id and a fresh announcement
    let stateUnder (key : obj list) (wirePath : obj list) (isStream : bool) =
        match fields.TryGetValue key with
        | true, state when not state.Closed -> state, false
        | _ ->
            let state = FieldState (string nextId, wirePath, isStream)
            nextId <- nextId + 1
            fields[key] <- state
            state, true

    let stateFor (fieldPath : obj list) (isStream : bool) = stateUnder fieldPath (wirePathOf fieldPath isStream) isStream

    /// The key of a deferred fragment: the path of the object it belongs to, distinguished from that object's fields
    let fragmentKey (path : obj list) (fragmentId : int) = [ yield! path; yield box (FragmentKey fragmentId) ]

    let stateForFragment (path : obj list) (fragmentId : int) = stateUnder (fragmentKey path fragmentId) path false

    let pendingResultFor (state : FieldState) = {
        Id = state.Id
        Path = state.WirePath
        Label = state.Label |> Skippable.ofValueOption
    }

    let announce (key : obj list) (label : string voption) (initialCount : int) (state : FieldState, isNew : bool) =
        match label with
        | ValueSome _ -> state.Label <- label
        | ValueNone -> ()

        if isNew then
            // The items delivered with the initial payload are never streamed: the stream starts after them
            state.NextIndex <- initialCount
            pending.Add (struct (key, pendingResultFor state))

        state, isNew

    let announcePending (fieldPath : obj list) (label : string voption) (isStream : bool) (initialCount : int) =
        // DeferredCompleted must be able to recover the field id even when a pre-announced stream completes without
        // ever producing an item, so every pending announcement creates the per-field state eagerly.
        stateFor fieldPath isStream |> announce fieldPath label initialCount

    let announceStream (fieldPath : obj list) = announcePending fieldPath ValueNone true 0

    let announceFragment (path : obj list) (label : string voption) (fragmentId : int) =
        stateForFragment path fragmentId |> announce (fragmentKey path fragmentId) label 0

    let rec pathExistsInData (relativePath : obj list) (data : obj) =
        match relativePath, data with
        | [], _ -> true
        | _ :: _, null -> false
        | (:? string as fieldName) :: tail, (:? IDictionary<string, obj> as fields) ->
            match fields.TryGetValue fieldName with
            | true, value -> pathExistsInData tail value
            | false, _ -> false
        | (:? int as index) :: tail, (:? (obj[]) as items) when index >= 0 && index < items.Length -> pathExistsInData tail items[index]
        | (:? int as index) :: tail, (:? System.Collections.IEnumerable as items) when index >= 0 ->
            items
            |> Seq.cast<obj>
            |> Seq.tryItem index
            |> Option.exists (pathExistsInData tail)
        | _ -> false

    /// Takes the announcements the predicate selects out of the queue, marking their fields released to the client.
    let takePendingWhen (predicate : obj list -> PendingResult -> bool) =
        let ready = ResizeArray ()
        let remaining = ResizeArray ()

        for struct (fieldPath, entry) in pending do
            if predicate fieldPath entry then
                fields[fieldPath].Released <- true
                ready.Add entry
            else
                remaining.Add (struct (fieldPath, entry))

        pending.Clear ()
        pending.AddRange remaining
        List.ofSeq ready

    let takePendingVisibleIn (payloadPath : obj list) (payloadData : obj) =
        takePendingWhen (fun _ entry ->
            pathStartsWith payloadPath entry.Path
            && entry.Path
               |> List.skip (List.length payloadPath)
               |> fun relativePath -> pathExistsInData relativePath payloadData)

    let takePendingForItems (fieldPath : obj list) (flushedItems : (int * obj) list) =
        takePendingWhen (fun announcedFieldPath entry ->
            announcedFieldPath = fieldPath
            || flushedItems
               |> List.exists (fun (index, item) ->
                   let itemPath = [ yield! fieldPath; yield box index ]
                   pathStartsWith itemPath entry.Path
                   && entry.Path
                      |> List.skip (List.length itemPath)
                      |> fun relativePath -> pathExistsInData relativePath item))

    let takeFieldPending (fieldPath : obj list) = takePendingWhen (fun announcedFieldPath _ -> announcedFieldPath = fieldPath)

    /// Drops the announcement of a field the client will never learn of.
    let dropFieldPending (fieldPath : obj list) =
        pending.RemoveAll (fun struct (announcedFieldPath, _) -> announcedFieldPath = fieldPath)
        |> ignore

    /// Flushes the contiguous run of buffered items starting at the field's next expected index, if any.
    let flush (state : FieldState) =
        if state.Buffer.ContainsKey state.NextIndex then
            let items = ResizeArray ()
            let flushedItems = ResizeArray ()
            let errors = ResizeArray ()
            while state.Buffer.ContainsKey state.NextIndex do
                let index = state.NextIndex
                let item, itemErrors = state.Buffer[state.NextIndex]
                items.Add item
                flushedItems.Add (index, item)
                errors.AddRange itemErrors
                state.Buffer.Remove state.NextIndex |> ignore
                state.NextIndex <- state.NextIndex + 1
            ValueSome (
                {
                    Id = state.Id
                    SubPath = Skip
                    Data = Skip
                    Items = Include (items.ToArray ())
                    Errors =
                        (if errors.Count = 0 then
                             Skip
                         else
                             Include (List.ofSeq errors))
                },
                List.ofSeq flushedItems
            )
        else
            ValueNone

    /// The announcement of a field delivered for the first time, sent along with its own payload.
    let pendingFor (state : FieldState) (isNew : bool) =
        if isNew then
            state.Released <- true
            [ pendingResultFor state ]
        else
            []

    /// Execution.collectItems wraps a single successfully-produced item's own value in a one-element array
    /// (deferResults itself only ever handles a value at a path, not specifically an item); an item whose
    /// resolution failed outright instead carries data = null, already unwrapped.
    let unwrapItem (data : obj) =
        match data with
        | :? (obj[]) as items when items.Length = 1 -> items[0]
        | data -> data

    let itemsPayload (fieldPath : obj list) (state : FieldState) =
        match flush state with
        | ValueSome (incremental, flushedItems) ->
            let pending = takePendingForItems fieldPath flushedItems
            ValueSome (SubscriptionExecutionResult.CreateSubsequent (pending, [ incremental ], [], true))
        | ValueNone ->
            match takeFieldPending fieldPath with
            | [] -> ValueNone
            | pending -> ValueSome (SubscriptionExecutionResult.CreateSubsequent (pending, [], [], true))

    let itemEvent (fieldPath : obj list) (index : int) (data : obj) (errors : GQLProblemDetails list) =
        let state = announceStream fieldPath |> fst
        state.Buffer[index] <- (unwrapItem data, errors)
        itemsPayload fieldPath state

    /// The wire address of a deferred field's payload: the path of its containing object, and the field's value as
    /// an object map of that one field, which is what a client merges into the object at the announced path.
    let deferredPayload (fieldPath : obj list) (data : obj) =
        match fieldPath with
        | DeferredFieldPath (parentPath, fieldName) -> parentPath, box (NameValueLookup.ofList [ fieldName, data ])
        | _ -> fieldPath, data

    /// A deferred field's own value, or its value with the errors raised inside it.
    let deferredEvent (fieldPath : obj list) (data : obj) (errors : GQLProblemDetails list) =
        let state, isNew = stateFor fieldPath false
        let wirePath, wireData = deferredPayload fieldPath data
        let incremental = {
            Id = state.Id
            SubPath = Skip
            Data = Include (ValueSome wireData)
            Items = Skip
            Errors = (if errors.IsEmpty then Skip else Include errors)
        }
        let fieldPending =
            match takeFieldPending fieldPath with
            | [] -> pendingFor state isNew
            | pending -> pending
        let pending = [ yield! fieldPending; yield! takePendingVisibleIn wirePath wireData ]
        ValueSome (SubscriptionExecutionResult.CreateSubsequent (pending, [ incremental ], [], true))

    /// A deferred fragment's fields, delivered together as an object map of the object it belongs to, or the errors
    /// that propagated up to the fragment itself, which complete it without data.
    let fragmentEvent (path : obj list) (fragmentId : int) (data : Output voption) (errors : GQLProblemDetails list) =
        let key = fragmentKey path fragmentId
        let state, isNew = stateForFragment path fragmentId
        let fieldPending =
            match takeFieldPending key with
            | [] -> pendingFor state isNew
            | pending -> pending
        match data with
        | ValueSome data ->
            let incremental = {
                Id = state.Id
                SubPath = Skip
                Data = Include (ValueSome (box data))
                Items = Skip
                Errors = (if errors.IsEmpty then Skip else Include errors)
            }
            let pending = [ yield! fieldPending; yield! takePendingVisibleIn path (box data) ]
            ValueSome (SubscriptionExecutionResult.CreateSubsequent (pending, [ incremental ], [], true))
        | ValueNone ->
            // The object the fragment belongs to was already delivered, so nothing can be nulled on the client: the
            // fragment is announced, if not yet, and completed with its errors in the same payload
            state.Closed <- true
            ValueSome (SubscriptionExecutionResult.CreateSubsequent (fieldPending, [], [ { Id = state.Id; Errors = Include errors } ], true))

    /// Closes the field, completing it for the client when the client learned of it.
    let complete (fieldPath : obj list) (state : FieldState) (errors : GQLProblemDetails list Skippable) =
        state.Closed <- true
        state.Buffer.Clear ()

        if state.Released then
            ValueSome (SubscriptionExecutionResult.CreateSubsequent ([], [], [ { Id = state.Id; Errors = errors } ], true))
        else
            // Never exposed to the client (the payload that should have exposed it resolved to null there)
            dropFieldPending fieldPath
            ValueNone

    /// The announcements visible in the data, to be sent with the payload carrying it.
    member _.TakePendingVisibleIn (data : obj) = takePendingVisibleIn [] data

    /// Applies one engine event, returning the payload it produces, if any (an out-of-order item that does not
    /// complete a contiguous run, or a completion for a field already closed by a preceding stream failure,
    /// produce none).
    member _.Apply (event : GQLDeferredResponseContent) : SubscriptionExecutionResult voption =
        match event with
        | DeferredPending (fieldPath, label, isStream, initialCount) ->
            announcePending fieldPath label isStream initialCount |> ignore
            ValueNone
        | DeferredResult (data, BatchPath (fieldPath, indices)) ->
            let items = data :?> obj[]
            let state = announceStream fieldPath |> fst
            (indices, List.ofArray items)
            ||> List.iter2 (fun index item -> state.Buffer[index :?> int] <- (item, []))
            itemsPayload fieldPath state
        | DeferredResult (data, ItemPath (fieldPath, index)) -> itemEvent fieldPath index data []
        | DeferredErrors (data, errors, ItemPath (fieldPath, index)) -> itemEvent fieldPath index data errors
        | DeferredErrors (data, errors, BatchPath (fieldPath, indices)) ->
            // Execution.collectItems emits this batch shape when some streamed items succeed while others report field
            // errors in the same buffered chunk. Every error already carries the full path of the specific item it
            // came from, so the batch is handled the same way a series of single-item events would be.
            let items = data :?> obj[]
            let state = announceStream fieldPath |> fst
            (indices, List.ofArray items)
            ||> List.iter2 (fun index item ->
                let itemPath = [ yield! fieldPath; yield index ]
                let itemErrors =
                    errors
                    |> List.filter (fun e ->
                        e.Path
                        |> Skippable.toValueOption
                        |> ValueOption.map (pathStartsWith itemPath)
                        |> ValueOption.defaultValue false)
                state.Buffer[index :?> int] <- (item, itemErrors))
            itemsPayload fieldPath state
        | DeferredResult (data, fieldPath) ->
            // A plain (non-indexed) path: a @defer field's own value
            deferredEvent fieldPath data []
        | DeferredErrors (data, errors, fieldPath) ->
            match fields.TryGetValue fieldPath with
            | true, state when state.IsStream && not state.Closed ->
                // Known to be a stream (either pre-announced or already carrying items): the failure of the source
                // itself, folded directly into its completion. Anything still buffered, waiting for a gap that will
                // now never be filled (the engine pulls no further items after a failure), is dropped.
                complete fieldPath state (Include errors)
            | _ ->
                // A @defer field's own value, with the errors raised inside it
                deferredEvent fieldPath data errors
        | DeferredCompleted fieldPath ->
            match fields.TryGetValue fieldPath with
            | true, state when not state.Closed -> complete fieldPath state Skip
            | _ ->
                // Already closed by a preceding stream failure
                ValueNone
        | DeferredFragmentPending (path, label, fragmentId) ->
            announceFragment path label fragmentId |> ignore
            ValueNone
        | DeferredFragmentResult (data, errors, path, fragmentId) -> fragmentEvent path fragmentId data errors
        | DeferredFragmentCompleted (path, fragmentId) ->
            let key = fragmentKey path fragmentId
            match fields.TryGetValue key with
            | true, state when not state.Closed -> complete key state Skip
            | _ ->
                // Already closed by the errors that propagated up to the fragment
                ValueNone

    /// <summary>
    /// The final payload of the delivery: completes every field the client learned of that has not completed on
    /// its own (normally none - a <c>@live</c> field is the only field this codebase produces that never completes by
    /// itself) and reports that no further payloads follow.
    /// </summary>
    member _.Finish () : SubscriptionExecutionResult =
        let stillOpen =
            fields.Values
            |> Seq.filter (fun state -> state.Released && not state.Closed)
            |> Seq.map (fun state -> { Id = state.Id; Errors = Skip })
            |> Seq.toList
        SubscriptionExecutionResult.CreateSubsequent ([], [], stillOpen, false)
