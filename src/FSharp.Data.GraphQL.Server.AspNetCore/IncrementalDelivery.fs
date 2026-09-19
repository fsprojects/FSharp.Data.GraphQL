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
    [<return: Struct>]
    let (|BatchPath|_|) (path : obj list) =
        match List.rev path with
        | (:? (obj list) as indices) :: fieldPathRev -> ValueSome (List.rev fieldPathRev, indices)
        | _ -> ValueNone

    /// Matches a path ending in a single streamed item's own index, such as ["items"; 0].
    [<return: Struct>]
    let (|ItemPath|_|) (path : obj list) =
        match List.rev path with
        | (:? int as index) :: fieldPathRev -> ValueSome (List.rev fieldPathRev, index)
        | _ -> ValueNone

/// Mutable per-field bookkeeping of IncrementalDelivery, keyed by a field's own path (with any item index or
/// batch removed).
type private FieldState (id : string) =
    member _.Id = id
    member val IsStream = false with get, set
    member val Closed = false with get, set
    /// The index of the next streamed item this field expects, in order; irrelevant once IsStream is false.
    member val NextIndex = 0 with get, set
    /// Items received out of order, waiting for the item at NextIndex to fill the gap before them. `member val`,
    /// not a plain computed `member`, so the same dictionary is reused: a computed member's body runs again on
    /// every access, handing back a fresh, empty dictionary each time instead of the one already filled.
    member val Buffer : SortedDictionary<int, obj * GQLProblemDetails list> = SortedDictionary ()

/// <summary>
/// Translates the engine's <see cref="GQLDeferredResponseContent"/> events into the <c>graphql-transport-ws</c>
/// incremental delivery wire format (<c>pending</c>/<c>incremental</c>/<c>completed</c>/<c>hasNext</c>, the format
/// used by graphql-js 17 and Apollo Client's <c>GraphQL17Alpha9Handler</c>).
/// </summary>
/// <remarks>
/// <para>
/// Every deferred or streamed field is announced once and identified afterwards by a short id instead of its path.
/// A deferred field is announced in the same payload as its own value; a streamed field is pre-announced as soon as
/// its containing data becomes visible to the client, so later item payloads and completions can refer to the id
/// immediately.
/// </para>
/// <para>
/// A streamed field's items are delivered to the client in list order: an item produced out of order (the engine
/// resolves up to a field's <c>maxConcurrency</c> items at the same time) is buffered until the item before it
/// arrives, then every contiguous run starting at the next expected index is flushed as one entry - a batch the
/// engine grouped into a single event is simply several items of the same run.
/// </para>
/// <para>
/// A stream failure - <see cref="DeferredErrors"/> at the field's own path, with no item index - is folded directly
/// into that field's completion once at least one of its items has already been seen (so it is known to be a
/// stream, not a <c>@defer</c> field whose own resolution failed the same way): no <c>incremental</c> entry is sent
/// for it, and whatever was still buffered, waiting for a gap to fill, is dropped, since the engine pulls no
/// further items after a failure. Because streamed fields are pre-announced before their first item, the same
/// completion shape is preserved even when the source fails before producing any item at all, or completes empty.
/// </para>
/// </remarks>
type IncrementalDelivery () =

    let fields = Dictionary<obj list, FieldState>(HashIdentity.Structural)
    let pending = ResizeArray<PendingResult>()
    let mutable nextId = 0

    let stateFor (fieldPath : obj list) =
        match fields.TryGetValue fieldPath with
        | true, state -> state, false
        | false, _ ->
            let state = FieldState (string nextId)
            nextId <- nextId + 1
            fields[fieldPath] <- state
            state, true

    let announceStream (fieldPath : obj list) =
        let state, isNew = stateFor fieldPath
        state.IsStream <- true

        if isNew then
            pending.Add { Id = state.Id; Path = fieldPath }

        state

    let takePending () =
        let ready = List.ofSeq pending
        pending.Clear ()
        ready

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

    let takePendingWhen predicate =
        let ready = ResizeArray ()
        let remaining = ResizeArray<PendingResult>()

        for entry in pending do
            if predicate entry then
                ready.Add entry
            else
                remaining.Add entry

        pending.Clear ()
        pending.AddRange remaining
        List.ofSeq ready

    let takePendingVisibleIn (payloadPath : obj list) (payloadData : obj) =
        takePendingWhen (fun entry ->
            pathStartsWith payloadPath entry.Path
            && entry.Path
               |> List.skip (List.length payloadPath)
               |> fun relativePath -> pathExistsInData relativePath payloadData)

    let takePendingForItems (fieldPath : obj list) (flushedItems : (int * obj) list) =
        takePendingWhen (fun entry ->
            entry.Path = fieldPath
            || flushedItems
               |> List.exists (fun (index, item) ->
                   let itemPath = [ yield! fieldPath; yield box index ]
                   pathStartsWith itemPath entry.Path
                   && entry.Path
                      |> List.skip (List.length itemPath)
                      |> fun relativePath -> pathExistsInData relativePath item))

    let takeFieldPending (fieldPath : obj list) = takePendingWhen (fun entry -> entry.Path = fieldPath)

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

    let pendingFor (fieldPath : obj list) (state : FieldState) (isNew : bool) =
        if isNew then
            [ { Id = state.Id; Path = fieldPath } ]
        else
            []

    /// Execution.collectItems wraps a single successfully-produced item's own value in a one-element array
    /// (deferResults itself only ever handles a value at a path, not specifically an item); an item whose
    /// resolution failed outright instead carries data = null, already unwrapped.
    let unwrapItem (data : obj) =
        match data with
        | :? (obj[]) as items when items.Length = 1 -> items[0]
        | data -> data

    let itemEvent (fieldPath : obj list) (index : int) (data : obj) (errors : GQLProblemDetails list) =
        let state = announceStream fieldPath
        state.Buffer[index] <- (unwrapItem data, errors)

        match flush state with
        | ValueSome (incremental, flushedItems) ->
            let pending = takePendingForItems fieldPath flushedItems
            ValueSome (SubscriptionExecutionResult.CreateSubsequent (pending, [ incremental ], [], true))
        | ValueNone ->
            match takeFieldPending fieldPath with
            | [] -> ValueNone
            | pending -> ValueSome (SubscriptionExecutionResult.CreateSubsequent (pending, [], [], true))

    member _.TakePendingVisibleIn (data : obj) = takePendingVisibleIn [] data

    /// Applies one engine event, returning the payload it produces, if any (an out-of-order item that does not
    /// complete a contiguous run, or a completion for a field already closed by a preceding stream failure,
    /// produce none).
    member _.Apply (event : GQLDeferredResponseContent) : SubscriptionExecutionResult voption =
        match event with
        | DeferredPending fieldPath ->
            announceStream fieldPath |> ignore
            ValueNone
        | DeferredResult (data, BatchPath (fieldPath, indices)) ->
            let items = data :?> obj[]
            let state = announceStream fieldPath
            (indices, List.ofArray items)
            ||> List.iter2 (fun index item -> state.Buffer[index :?> int] <- (item, []))
            match flush state with
            | ValueSome (incremental, flushedItems) ->
                let pending = takePendingForItems fieldPath flushedItems
                ValueSome (SubscriptionExecutionResult.CreateSubsequent (pending, [ incremental ], [], true))
            | ValueNone ->
                match takeFieldPending fieldPath with
                | [] -> ValueNone
                | pending -> ValueSome (SubscriptionExecutionResult.CreateSubsequent (pending, [], [], true))
        | DeferredResult (data, ItemPath (fieldPath, index)) -> itemEvent fieldPath index data []
        | DeferredErrors (data, errors, ItemPath (fieldPath, index)) -> itemEvent fieldPath index data errors
        | DeferredErrors (data, errors, BatchPath (fieldPath, indices)) ->
            // Execution.collectItems emits this batch shape when some streamed items succeed while others report field
            // errors in the same buffered chunk. Every error already carries the full path of the specific item it
            // came from, so the batch is handled the same way a series of single-item events would be.
            let items = data :?> obj[]
            let state = announceStream fieldPath
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
            match flush state with
            | ValueSome (incremental, flushedItems) ->
                let pending = takePendingForItems fieldPath flushedItems
                ValueSome (SubscriptionExecutionResult.CreateSubsequent (pending, [ incremental ], [], true))
            | ValueNone ->
                match takeFieldPending fieldPath with
                | [] -> ValueNone
                | pending -> ValueSome (SubscriptionExecutionResult.CreateSubsequent (pending, [], [], true))
        | DeferredResult (data, fieldPath) ->
            // A plain (non-indexed) path: a @defer field's own value.
            let state, isNew = stateFor fieldPath
            let incremental = { Id = state.Id; Data = Include data; Items = Skip; Errors = Skip }
            let pending = [ yield! pendingFor fieldPath state isNew; yield! takePendingVisibleIn fieldPath data ]
            ValueSome (SubscriptionExecutionResult.CreateSubsequent (pending, [ incremental ], [], true))
        | DeferredErrors (data, errors, fieldPath) ->
            match fields.TryGetValue fieldPath with
            | true, state when state.IsStream && not state.Closed ->
                // Known to already be a stream (either pre-announced or already carrying items): the failure of the
                // source itself, folded directly into its completion. Anything still buffered, waiting for a gap that
                // will now never be filled (the engine pulls no further items after a failure), is dropped.
                state.Closed <- true
                state.Buffer.Clear ()
                ValueSome (SubscriptionExecutionResult.CreateSubsequent ([], [], [ { Id = state.Id; Errors = Include errors } ], true))
            | _ ->
                // A @defer field's own failure.
                let state, isNew = stateFor fieldPath
                let incremental = { Id = state.Id; Data = Include data; Items = Skip; Errors = Include errors }
                let pending = [ yield! pendingFor fieldPath state isNew; yield! takePendingVisibleIn fieldPath data ]
                ValueSome (SubscriptionExecutionResult.CreateSubsequent (pending, [ incremental ], [], true))
        | DeferredCompleted fieldPath ->
            match fields.TryGetValue fieldPath with
            | true, state when not state.Closed ->
                state.Closed <- true
                ValueSome (SubscriptionExecutionResult.CreateSubsequent ([], [], [ { Id = state.Id; Errors = Skip } ], true))
            | _ ->
                // Already closed by a preceding stream failure.
                ValueNone

    /// The final payload of the delivery: completes every field that has not completed on its own (normally none -
    /// a @live field is the only field this codebase produces that never completes by itself) and reports that no
    /// further payloads follow.
    member _.Finish () : SubscriptionExecutionResult =
        let stillOpen =
            fields.Values
            |> Seq.filter (fun state -> not state.Closed)
            |> Seq.map (fun state -> { Id = state.Id; Errors = Skip })
            |> Seq.toList
        SubscriptionExecutionResult.CreateSubsequent ([], [], stillOpen, false)
