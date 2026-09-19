// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc
module FSharp.Data.GraphQL.Execution

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Diagnostics
open System.Reactive.Disposables
open System.Reactive.Subjects
open System.Text.Json
open FSharp.Control.Reactive
open FsToolkit.ErrorHandling

open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Errors
open FSharp.Data.GraphQL.Extensions
open FSharp.Data.GraphQL.Helpers
open FSharp.Data.GraphQL.Shared
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns
open FSharp.Data.GraphQL

let (|RequestError|Direct|Deferred|Stream|) (response : GQLExecutionResult) =
    match response.Content with
    | RequestError errs -> RequestError errs
    | Direct (data, errors) -> Direct (data, errors)
    | Deferred (data, errors, deferred) -> Deferred (data, errors, deferred)
    | Stream data -> Stream data

let private collectDefaultArgValue acc (argDef : InputFieldDef) =
    match argDef.DefaultValue with
    | ValueSome defVal -> Map.add argDef.Name defVal acc
    | ValueNone -> acc

let internal argumentValue inputContext variables (argDef : InputFieldDef) (argument : Argument) =
    match argDef.ExecuteInput inputContext argument.Value variables with
    | Ok null ->
        match argDef.DefaultValue with
        | ValueSome value -> Ok value
        | ValueNone -> Ok null
    | result -> result

let private getArgumentValues
    (argDefs : InputFieldDef[])
    (args : Argument list)
    (inputContext : InputExecutionContextProvider)
    (variables : ImmutableDictionary<string, obj>)
    : Result<Map<string, obj>, IGQLError list> =
    argDefs
    |> Array.fold
        (fun acc argdef ->
            match List.vtryFind (fun (a : Argument) -> a.Name = argdef.Name) args with
            | ValueSome argument -> validation {
                let! acc = acc
                and! arg = argumentValue inputContext variables argdef argument
                match arg with
                | null -> return acc
                | v -> return Map.add argdef.Name v acc
              }
            | ValueNone -> validation {
                let! acc = acc
                return collectDefaultArgValue acc argdef
              })
        (Ok Map.empty)

let private getOperation =
    function
    | OperationDefinition odef -> ValueSome odef
    | _ -> ValueNone


/// Search through the Definitions in the given Document for an OperationDefinition with the given name.
/// Or, if there was no name given, and there is only one OperationDefinition in the Document, return that.
let internal findOperation doc opName =
    match doc.Definitions |> List.vchoose getOperation, opName with
    | [ def ], _ -> ValueSome def
    | defs, name -> defs |> List.vtryFind (fun def -> def.Name = name)

let private defaultResolveType possibleTypesFn abstractDef : obj -> ObjectDef =
    let possibleTypes = possibleTypesFn abstractDef
    let mapper =
        match abstractDef with
        | Union u -> u.ResolveValue
        | _ -> id
    fun value ->
        let mapped = mapper value
        possibleTypes
        |> Array.find (fun objdef ->
            match objdef.IsTypeOf with
            | ValueSome isTypeOf -> isTypeOf mapped
            | ValueNone -> false)

let private resolveInterfaceType possibleTypesFn (interfacedef : InterfaceDef) =
    match interfacedef.ResolveType with
    | ValueSome resolveType -> resolveType
    | ValueNone -> defaultResolveType possibleTypesFn interfacedef

let private resolveUnionType possibleTypesFn (uniondef : UnionDef) =
    match uniondef.ResolveType with
    | ValueSome resolveType -> resolveType
    | ValueNone -> defaultResolveType possibleTypesFn uniondef

let private createFieldContext objdef inputContext argDefs ctx (info : ExecutionInfo) (path : FieldPath) = result {
    let fdef = info.Definition
    let! args = getArgumentValues argDefs info.Ast.Arguments inputContext ctx.Variables
    return {
        ExecutionInfo = info
        Context = ctx.Context
        ReturnType = fdef.TypeDef
        ParentType = objdef
        Schema = ctx.Schema
        Args = args
        Variables = ctx.Variables
        Path = normalizeErrorPath path
    }
}

let private resolveField (execute : ExecuteField) (ctx : ResolveFieldContext) (parentValue : obj) =
    if ctx.ExecutionInfo.IsNullable then
        execute ctx parentValue |> AsyncVal.map (objectOptionCast)
    else
        execute ctx parentValue
        |> AsyncVal.map (fun v -> if isNull v then ValueNone else ValueSome v)


type ResolverResult<'T> = Result<'T * IObservable<GQLDeferredResponseContent> voption * GQLProblemDetails list, GQLProblemDetails list>

[<RequireQualifiedAccess>]
module ResolverResult =

    let data data = Ok (data, ValueNone, [])
    let defered data deferred = Ok (data, ValueSome deferred, [])

    let mapValue (f : 'T -> 'U) (r : ResolverResult<'T>) : ResolverResult<'U> = Result.map (fun (data, deferred, errs) -> (f data, deferred, errs)) r


type StreamOutput =
    | NonList of (KeyValuePair<string, obj> * GQLProblemDetails list)
    | NonBufferedList of int * (KeyValuePair<string, obj> * GQLProblemDetails list)
    | BufferedList of int list * (KeyValuePair<string, obj> * GQLProblemDetails list) list

/// An event of a streamed list: a resolved item with its index in the source,
/// or a failure raised while enumerating an asynchronous source.
[<Struct>]
type private StreamEvent =
    | StreamedItem of index : int * result : ResolverResult<KeyValuePair<string, obj>>
    | StreamFailure of error : exn

let private raiseErrors errs = AsyncVal.wrap <| Error errs

/// Given an error e, call ParseError in the given context's Schema to convert it into
/// a list of one or more <see href="IGQLErrors">IGQLErrors</see>, then convert those
/// to a list of <see href="GQLProblemDetails">GQLProblemDetails</see>.
let private resolverError path ctx e =
    ctx.Schema.ParseError path e
    |> List.map (GQLProblemDetails.OfFieldExecutionError (normalizeErrorPath path))
// Helper functions for generating more specific <see href="GQLProblemDetails">GQLProblemDetails</see>.
let private nullResolverError name path ctx =
    resolverError path ctx (GQLMessageException $"Non-Null field %s{name} resolved as a null!")
let private coercionError value tyName path ctx =
    resolverError path ctx (GQLMessageException $"Value '{value}' could not be coerced to scalar %s{tyName}")
let private interfaceImplError ifaceName tyName path ctx =
    resolverError path ctx (GQLMessageException $"GraphQL Interface '%s{ifaceName}' is not implemented by the type '%s{tyName}'")
let private unionImplError unionName tyName path ctx =
    resolverError path ctx (GQLMessageException $"GraphQL Union '%s{unionName}' is not implemented by the type '%s{tyName}'")
let private deferredNullableError name tyName path ctx =
    resolverError path ctx (GQLMessageException $"Deferred field %s{name} of type '%s{tyName}' must be nullable")
let private streamListError name tyName path ctx =
    resolverError path ctx (GQLMessageException $"Streamed field %s{name} of type '%s{tyName}' must be list")

let private resolved name v : AsyncVal<ResolverResult<KeyValuePair<string, obj>>> =
    KeyValuePair (name, box v)
    |> ResolverResult.data
    |> AsyncVal.wrap

let private deferLabel (field : Field) =
    field.Directives
    |> List.vtryFind (fun directive -> directive.Name = "defer")
    |> ValueOption.bind (fun directive ->
        directive.Arguments
        |> List.vtryFind (fun argument -> argument.Name = "label")
        |> ValueOption.bind (fun argument ->
            match argument.Value with
            | StringValue label -> ValueSome label
            | NullValue -> ValueNone
            | _ ->
                Debug.Fail "Must be prevented by validation"
                ValueNone))

/// The result at path itself, not including any of its own nested deferred/streamed fields.
let private ownDeferredResult
    path
    (res : ResolverResult<obj>)
    : IObservable<GQLDeferredResponseContent> * IObservable<GQLDeferredResponseContent> voption =
    let formattedPath = normalizeErrorPath path
    match res with
    | Ok (data, nested, errs) ->
        let ownResult =
            match errs with
            | [] -> DeferredResult (data, formattedPath)
            | _ -> DeferredErrors (data, errs, formattedPath)
            |> Observable.singleton
        ownResult, nested
    | Error errs -> Observable.singleton (DeferredErrors (null, errs, formattedPath)), ValueNone

/// Replays the initial nested stream announcements before the containing payload that makes them visible, while
/// keeping every later nested event in its original relative position afterwards.
let private prependNestedPending
    (ownResult : IObservable<GQLDeferredResponseContent>)
    (nested : IObservable<GQLDeferredResponseContent> voption)
    (completed : IObservable<GQLDeferredResponseContent> voption)
    : IObservable<GQLDeferredResponseContent> =
    let appendCompletion events =
        match completed with
        | ValueSome completed -> events |> Observable.concat completed
        | ValueNone -> events

    match nested with
    | ValueNone -> ownResult |> appendCompletion
    | ValueSome nested -> {
        new IObservable<GQLDeferredResponseContent> with
            member _.Subscribe (observer) =
                let gate = obj ()
                let pendingPrefix = ResizeArray<GQLDeferredResponseContent>()
                let tail = new ReplaySubject<GQLDeferredResponseContent> ()
                let mutable capturePendingPrefix = true

                let nestedSubscription =
                    nested.Subscribe (
                        (fun event ->
                            lock gate (fun () ->
                                if capturePendingPrefix then
                                    match event with
                                    | DeferredPending _ -> pendingPrefix.Add event
                                    | _ ->
                                        capturePendingPrefix <- false
                                        tail.OnNext event
                                else
                                    tail.OnNext event)),
                        (fun ex ->
                            lock gate (fun () ->
                                capturePendingPrefix <- false
                                tail.OnError ex)),
                        (fun () ->
                            lock gate (fun () ->
                                capturePendingPrefix <- false
                                tail.OnCompleted ()))
                    )

                lock gate (fun () -> capturePendingPrefix <- false)

                let combined =
                    Observable.ofSeq pendingPrefix
                    |> Observable.concat ownResult
                    |> appendCompletion
                    |> Observable.concat (tail :> IObservable<GQLDeferredResponseContent>)

                new CompositeDisposable (nestedSubscription, combined.Subscribe observer, tail) :> IDisposable
      }

let deferResults path (res : ResolverResult<obj>) : IObservable<GQLDeferredResponseContent> =
    let ownResult, nested = ownDeferredResult path res
    prependNestedPending ownResult nested ValueNone

/// As <see cref="deferResults"/>, followed by a <see cref="DeferredCompleted"/> for path once that field's own
/// payload has been delivered; any nested deferred or streamed fields keep using their own pending ids afterwards.
let private deferResultsCompleted path (res : ResolverResult<obj>) : IObservable<GQLDeferredResponseContent> =
    let ownResult, nested = ownDeferredResult path res
    let completed = Observable.singleton (DeferredCompleted (normalizeErrorPath path))
    prependNestedPending ownResult nested (ValueSome completed)

/// Collect together an array of results using the appropriate execution strategy.
let collectFields
    (strategy : ExecutionStrategy)
    (rs : AsyncVal<ResolverResult<KeyValuePair<string, obj>>>[])
    : AsyncVal<ResolverResult<KeyValuePair<string, obj>[]>> = asyncVal {
    let! collected =
        match strategy with
        | Parallel -> AsyncVal.collectParallel rs
        | Sequential -> AsyncVal.collectSequential rs

    let data = Array.zeroCreate (collected.Length)

    let merge r acc =
        match (r, acc) with
        | Ok (field, d, e), Ok (i, deferred, errs) ->
            Array.set data i field
            Ok (i - 1, ValueOption.mergeWith Observable.merge deferred d, e @ errs)
        | Error e, Ok (_, _, errs) -> Error (e @ errs)
        | Ok (_, _, e), Error errs -> Error (e @ errs)
        | Error e, Error errs -> Error (e @ errs)
    return
        Array.foldBack merge collected (Ok (data.Length - 1, ValueNone, []))
        |> ResolverResult.mapValue (fun _ -> data)
}

let rec private direct
    (returnDef : OutputDef)
    (inputContext : InputExecutionContextProvider)
    (ctx : ResolveFieldContext)
    (path : FieldPath)
    (parent : obj)
    (value : obj)
    : AsyncVal<ResolverResult<KeyValuePair<string, obj>>> =
    let name = ctx.ExecutionInfo.Identifier
    match returnDef with

    | Object objDef ->
        let fields =
            match ctx.ExecutionInfo.Kind with
            | SelectFields fields -> fields
            | kind -> failwithf $"Unexpected value of ctx.ExecutionPlan.Kind: %A{kind}"
        executeObjectFields fields name objDef inputContext ctx path value

    | Scalar scalarDef ->
        match scalarDef.CoerceOutput (downcast value) with
        | Some v' -> resolved name v'
        | None -> raiseErrors <| coercionError value scalarDef.Name path ctx

    | Enum enumDef ->
        let enumCase =
            enumDef.Options
            |> Array.vtryPick (fun case ->
                if case.Value.Equals (value) then
                    ValueSome case.Name
                else
                    ValueNone)
        match enumCase with
        | ValueSome v' -> resolved name (v' :> obj)
        | ValueNone -> raiseErrors <| coercionError value enumDef.Name path ctx

    | List (Output innerDef) ->
        let innerCtx =
            match ctx.ExecutionInfo.Kind with
            | ResolveCollection innerPlan -> { ctx with ExecutionInfo = { innerPlan with ReturnDef = innerDef } }
            | kind -> failwithf "Unexpected value of ctx.ExecutionPlan.Kind: %A" kind
        let resolveItem index item =
            executeResolvers inputContext innerCtx (box index :: path) value (toValueOption item |> AsyncVal.wrap)
        let resolveItems (items : obj[]) =
            items
            |> Array.mapi resolveItem
            |> collectFields Parallel
            |> AsyncVal.map (ResolverResult.mapValue (fun items -> KeyValuePair (name, items |> Array.map _.Value |> box)))
        match value with
        | :? IAsyncEnumerableFieldValue as fieldValue ->
            async {
                // The sequence is drained first, the same way a lazy seq is materialized below.
                // Enumeration errors are caught inside the computation, because resolveWith only catches synchronous exceptions.
                let! drained = async {
                    try
                        let! items = AsyncEnumerable.toArrayAsync fieldValue.Items
                        return Ok items
                    with e ->
                        return Error (resolverError path ctx e)
                }
                match drained with
                | Error errs -> return Error errs
                | Ok items -> return! resolveItems items
            }
            |> AsyncVal.ofAsync
        | :? System.Collections.IEnumerable as enumerable -> enumerable |> Seq.cast<obj> |> Seq.toArray |> resolveItems
        | _ ->
            raise
            <| GQLMessageException (ErrorMessages.expectedEnumerableValue ctx.ExecutionInfo.Identifier (value.GetType ()))

    | Nullable (Output innerDef) ->
        let innerCtx = {
            ctx with
                ExecutionInfo = { ctx.ExecutionInfo with IsNullable = true; ReturnDef = innerDef }
        }
        executeResolvers inputContext innerCtx path parent (toValueOption value |> AsyncVal.wrap)
        |> AsyncVal.map (
            Result.valueOr (fun errs -> (KeyValuePair (name, null), ValueNone, errs))
            >> Ok
        )

    | Interface iDef ->
        let possibleTypesFn = ctx.Schema.GetPossibleTypes
        let resolver = resolveInterfaceType possibleTypesFn iDef
        let resolvedDef = resolver value
        let typeMap =
            match ctx.ExecutionInfo.Kind with
            | ResolveAbstraction typeMap -> typeMap
            | kind -> failwithf $"Unexpected value of ctx.ExecutionPlan.Kind: %A{kind}"
        match Map.vtryFind resolvedDef.Name typeMap with
        | ValueSome fields -> executeObjectFields fields name resolvedDef inputContext ctx path value
        | ValueNone ->
            KeyValuePair (name, obj ())
            |> ResolverResult.data
            |> AsyncVal.wrap

    | Union uDef ->
        let possibleTypesFn = ctx.Schema.GetPossibleTypes
        let resolver = resolveUnionType possibleTypesFn uDef
        let resolvedDef = resolver value
        let typeMap =
            match ctx.ExecutionInfo.Kind with
            | ResolveAbstraction typeMap -> typeMap
            | kind -> failwithf $"Unexpected value of ctx.ExecutionPlan.Kind: %A{kind}"
        match Map.vtryFind resolvedDef.Name typeMap with
        | ValueSome fields -> executeObjectFields fields name resolvedDef inputContext ctx path (uDef.ResolveValue value)
        | ValueNone ->
            KeyValuePair (name, obj ())
            |> ResolverResult.data
            |> AsyncVal.wrap

    | _ -> failwithf "Unexpected value of returnDef: %O" returnDef

and deferred (inputContext : InputExecutionContextProvider) (ctx : ResolveFieldContext) (path : FieldPath) (parent : obj) (value : obj) =
    let info = ctx.ExecutionInfo
    let deferred =
        executeResolvers inputContext ctx path parent (toValueOption value |> AsyncVal.wrap)
        |> Observable.ofAsyncVal
        |> Observable.bind (
            ResolverResult.mapValue (_.Value)
            >> deferResultsCompleted path
        )
        |> fun events ->
            match deferLabel info.Ast with
            | ValueSome label ->
                Observable.singleton (DeferredPending (normalizeErrorPath path, ValueSome label, false))
                |> Observable.concat events
            | ValueNone -> events
    ResolverResult.defered (KeyValuePair (info.Identifier, null)) deferred
    |> AsyncVal.wrap

and private streamed
    (options : BufferedStreamOptions)
    (innerDef : OutputDef)
    (inputContext : InputExecutionContextProvider)
    (ctx : ResolveFieldContext)
    (path : FieldPath)
    (parent : obj)
    (value : obj)
    =
    let info = ctx.ExecutionInfo
    let name = info.Identifier
    let innerCtx =
        match info.Kind with
        | ResolveCollection innerPlan -> { ctx with ExecutionInfo = innerPlan }
        | kind -> failwithf "Unexpected value of ctx.ExecutionPlan.Kind: %A" kind

    // A batch size requested by the @stream directive takes precedence over the batching policy declared on the field.
    // The policy is evaluated here, lazily, so it never runs for an ordinary or deferred query, and only once per
    // streamed query even when the query itself supplies a batch size.
    let options =
        match options.PreferredBatchSize, value with
        | ValueNone, (:? IAsyncEnumerableFieldValue as fieldValue) -> { options with PreferredBatchSize = fieldValue.GetPreferredBatchSize () }
        | _ -> options

    let collectItems : struct (int * ResolverResult<KeyValuePair<string, obj>>) list -> IObservable<GQLDeferredResponseContent> =
        function
        | [] -> Observable.empty
        | [ struct (index, result) ] ->
            result
            |> ResolverResult.mapValue (fun d -> box [| d.Value |])
            |> deferResults (box index :: path)
        | chunk ->
            let data = Array.zeroCreate (chunk.Length)
            let merge struct (index, r : ResolverResult<KeyValuePair<string, obj>>) (i, indices, deferred, errs) =
                match r with
                | Ok (item, d, e) ->
                    Array.set data i item.Value
                    (i - 1, box index :: indices, ValueOption.mergeWith Observable.merge deferred d, e @ errs)
                | Error e -> (i - 1, box index :: indices, deferred, e @ errs)
            let (_, indices, deferred, errs) = List.foldBack merge chunk (chunk.Length - 1, [], ValueNone, [])
            deferResults (box indices :: path) (Ok (box data, deferred, errs))

    let collectBuffered (events : StreamEvent list) : IObservable<GQLDeferredResponseContent> =
        // An enumeration failure is delivered as a value after the items of the same buffer,
        // so it neither loses buffered items nor terminates sibling deferred streams
        let struct (items, failures) =
            (events, struct ([], []))
            ||> List.foldBack (fun event struct (items, failures) ->
                match event with
                | StreamedItem (index, result) -> struct (index, result) :: items, failures
                | StreamFailure error ->
                    items,
                    DeferredErrors (null, resolverError path ctx error, normalizeErrorPath path)
                    :: failures)
        match failures with
        | [] -> collectItems items
        | failures ->
            collectItems items
            |> Observable.concat (Observable.ofSeq failures)

    let buffer (events : IObservable<StreamEvent>) : IObservable<GQLDeferredResponseContent> =
        let buffered =
            match options.Interval, options.PreferredBatchSize with
            | ValueSome i, ValueNone ->
                Observable.bufferMilliseconds i events
                |> Observable.map List.ofSeq
            | ValueNone, ValueSome c -> Observable.bufferCount c events |> Observable.map List.ofSeq
            | ValueSome i, ValueSome c ->
                Observable.bufferMillisecondsCount i c events
                |> Observable.map List.ofSeq
            | ValueNone, ValueNone -> Observable.map (List.singleton) events
        buffered |> Observable.bind collectBuffered

    /// A DeferredCompleted for path once every item, or the source's own failure, has been delivered.
    let withStreamCompleted (events : IObservable<GQLDeferredResponseContent>) =
        events
        |> Observable.concat (Observable.singleton (DeferredCompleted (normalizeErrorPath path)))

    let announceStream (events : IObservable<GQLDeferredResponseContent>) =
        Observable.singleton (DeferredPending (normalizeErrorPath path, ValueNone, true))
        |> Observable.concat events

    let resolveItem index item = asyncVal {
        let! result =
            executeResolvers inputContext innerCtx (box index :: path) parent (toValueOption item |> AsyncVal.wrap)
        return (index, result)
    }

    match value with
    | :? IAsyncEnumerableFieldValue as fieldValue ->
        let resolveStreamedItem index item = resolveItem index item |> AsyncVal.map StreamedItem
        let stream : IObservable<GQLDeferredResponseContent> =
            fieldValue.Items
            // At most fieldValue.MaxConcurrency items are pulled from the source and resolved at the same time,
            // each emitted as soon as it is resolved; a failure of the source itself is emitted last
            |> Observable.ofAsyncEnumerableResolved fieldValue.MaxConcurrency resolveStreamedItem StreamFailure
            |> buffer
            |> announceStream
            |> withStreamCompleted
        ResolverResult.defered (KeyValuePair (name, box [])) stream
        |> AsyncVal.wrap
    | :? System.Collections.IEnumerable as enumerable ->
        let stream : IObservable<GQLDeferredResponseContent> =
            enumerable
            |> Seq.cast<obj>
            |> Seq.toArray
            |> Array.mapi resolveItem
            |> Observable.ofAsyncValSeq
            |> Observable.map StreamedItem
            |> buffer
            |> announceStream
            |> withStreamCompleted
        ResolverResult.defered (KeyValuePair (name, box [])) stream
        |> AsyncVal.wrap
    | _ ->
        raise
        <| GQLMessageException (ErrorMessages.expectedEnumerableValue ctx.ExecutionInfo.Identifier (value.GetType ()))

and private live (inputContext : InputExecutionContextProvider) (ctx : ResolveFieldContext) (path : FieldPath) (parent : obj) (value : obj) =
    let info = ctx.ExecutionInfo
    let name = info.Identifier

    let rec getObjectName =
        function
        | Object objDef -> objDef.Name
        | Scalar scalarDef -> scalarDef.Name
        | Enum enumDef -> enumDef.Name
        | Nullable (Output innerDef) -> getObjectName innerDef
        | Interface iDef -> iDef.Name
        | Union uDef ->
            let resolver = resolveUnionType ctx.Schema.GetPossibleTypes uDef
            getObjectName (resolver value)
        | returnDef -> failwithf $"Unexpected value of returnDef: {returnDef}"

    let typeName = getObjectName info.ParentDef

    /// So the updatedValue here is actually the fresh parent.
    let resolveUpdate updatedValue =
        executeResolvers inputContext ctx path parent (updatedValue |> ValueSome |> AsyncVal.wrap)
        |> AsyncVal.map (
            ResolverResult.mapValue (fun d -> d.Value)
            >> deferResults path
        )
        |> Observable.ofAsyncVal
        |> Observable.mergeInner

    let provider = ctx.Schema.LiveFieldSubscriptionProvider
    let filter = provider.TryFind typeName name |> Option.map _.Filter
    let updates =
        match filter with
        | Some filterFn ->
            provider.Add (filterFn parent) typeName name
            |> Observable.bind resolveUpdate
        | None -> failwithf "No live provider for %s:%s" typeName name

    executeResolvers inputContext ctx path parent (value |> ValueSome |> AsyncVal.wrap)
    // TODO: Add tests for `Observable.merge deferred updates` correct order
    |> AsyncVal.map (
        Result.map (fun (data, deferred, errs) ->
            (data,
             ValueSome
             <| ValueOption.foldBack Observable.merge deferred updates,
             errs))
    )

/// Actually execute the resolvers.
and private executeResolvers
    (inputContext : InputExecutionContextProvider)
    (ctx : ResolveFieldContext)
    (path : FieldPath)
    (parent : obj)
    (value : AsyncVal<obj voption>)
    : AsyncVal<ResolverResult<KeyValuePair<string, obj>>> =
    let info = ctx.ExecutionInfo
    let name = info.Identifier
    let returnDef = info.ReturnDef

    let rec innerListDef =
        function
        | Nullable (Output innerDef) -> innerListDef innerDef
        | List (Output innerDef) -> ValueSome innerDef
        | _ -> ValueNone

    let (|HasList|_|) = innerListDef

    /// Run a resolution strategy with the provided context.
    /// This handles all null resolver errors/error propagation.
    let resolveWith
        (ctx : ResolveFieldContext)
        (onSuccess : ResolveFieldContext -> FieldPath -> obj -> obj -> AsyncVal<ResolverResult<KeyValuePair<string, obj>>>)
        : AsyncVal<ResolverResult<KeyValuePair<string, obj>>> = asyncVal {
        let! resolved = value |> AsyncVal.rescue path ctx.Schema.ParseError
        let additionalErrs =
            match ctx.Context.Errors.TryGetValue ctx with
            | true, errors ->
                errors
                |> Seq.map (GQLProblemDetails.OfFieldExecutionError (normalizeErrorPath path))
                |> Seq.toList
            | false, _ -> []
        match resolved with
        | Error errs when ctx.ExecutionInfo.IsNullable -> return Ok (KeyValuePair (name, null), ValueNone, errs @ additionalErrs)
        | Ok ValueNone when ctx.ExecutionInfo.IsNullable -> return Ok (KeyValuePair (name, null), ValueNone, additionalErrs)
        | Error errs -> return Error (errs @ additionalErrs)
        | Ok ValueNone -> return Error ((nullResolverError name path ctx) @ additionalErrs)
        | Ok (ValueSome v) ->
            let! onSuccessResult =
                try
                    onSuccess ctx path parent v
                with e ->
                    resolverError path ctx e |> Error |> AsyncVal.wrap
            match onSuccessResult with
            | Ok (res, deferred, errs) -> return Ok (res, deferred, errs @ additionalErrs)
            | Error errs when ctx.ExecutionInfo.IsNullable -> return Ok (KeyValuePair (name, null), ValueNone, errs @ additionalErrs)
            | Error errs -> return Error (errs @ additionalErrs)
    }

    match info.Kind, returnDef with
    | ResolveDeferred innerInfo, _ when innerInfo.IsNullable -> // We can only defer nullable fields
        deferred inputContext
        |> resolveWith { ctx with ExecutionInfo = innerInfo }
    | ResolveDeferred innerInfo, _ ->
        raiseErrors
        <| deferredNullableError (innerInfo.Identifier) (innerInfo.ReturnDef.ToString ()) path ctx
    | ResolveStreamed (innerInfo, mode), HasList innerDef -> // We can only stream lists
        streamed mode innerDef inputContext
        |> resolveWith { ctx with ExecutionInfo = innerInfo }
    | ResolveStreamed (innerInfo, _), _ ->
        raiseErrors
        <| streamListError innerInfo.Identifier (returnDef.ToString ()) path ctx
    | ResolveLive innerInfo, _ ->
        live inputContext
        |> resolveWith { ctx with ExecutionInfo = innerInfo }
    | _ -> direct returnDef inputContext |> resolveWith ctx


and executeObjectFields
    (fields : ExecutionInfo list)
    (objName : string)
    (objDef : ObjectDef)
    (inputContext : InputExecutionContextProvider)
    (ctx : ResolveFieldContext)
    (path : FieldPath)
    (value : obj)
    : AsyncVal<ResolverResult<KeyValuePair<string, obj>>> = asyncVal {
    let executeField field =
        let argDefs = ctx.Context.FieldExecuteMap.GetArgs (objDef.Name, field.Definition.Name)
        let resolver = ctx.Context.FieldExecuteMap.GetExecute (objDef.Name, field.Definition.Name)
        let fieldPath = (box field.Identifier :: path)
        match createFieldContext objDef inputContext argDefs ctx field fieldPath with
        | Ok fieldCtx -> executeResolvers inputContext fieldCtx fieldPath value (resolveField resolver fieldCtx value)
        | Error errs -> asyncVal { return Error (errs |> List.map GQLProblemDetails.OfError) }

    let! res =
        fields
        |> Seq.map executeField
        |> Seq.toArray
        |> collectFields Parallel
    match res with
    | Error errs -> return Error errs
    | Ok (kvps, def, errs) -> return Ok (KeyValuePair (objName, box <| NameValueLookup (kvps)), def, errs)
}

let internal compileSubscriptionField (subfield : SubscriptionFieldDef) =
    match subfield.Resolve with
    | Resolve.BoxedFilterExpr (_, _, _, filter) -> fun ctx a b -> filter ctx a b |> AsyncVal.wrap |> AsyncVal.toAsync
    | Resolve.BoxedAsyncFilterExpr (_, _, _, filter) -> filter
    | _ ->
        raise
        <| GQLMessageException ("Invalid filter expression for subscription field!")

let internal compileField (fieldDef : FieldDef) : ExecuteField =
    match fieldDef.Resolve with
    | Resolve.BoxedSync (_, _, resolve) ->
        fun resolveFieldCtx value ->
            try
                resolve resolveFieldCtx value |> AsyncVal.wrap
            with e ->
                AsyncVal.Failure (e)
    | Resolve.BoxedAsync (_, _, resolve) -> fun resolveFieldCtx value -> asyncVal { return! resolve resolveFieldCtx value }
    | Resolve.BoxedTaskSeq (_, _, resolve) ->
        fun resolveFieldCtx value ->
            try
                resolve resolveFieldCtx value |> AsyncVal.wrap
            with e ->
                AsyncVal.Failure (e)
    | Resolve.BoxedExpr (resolve) -> fun resolveFieldCtx value -> downcast resolve resolveFieldCtx value
    | _ ->
        fun _ _ ->
            raise (
                InvalidOperationException (
                    sprintf
                        "Field '%s' has been accessed, but no resolve function for that field definition was provided. Make sure, you've specified resolve function or declared field with Define.AutoField method"
                        fieldDef.Name
                )
            )

let private (|String|Other|) (o : obj) =
    match o with
    | :? string as s -> String s
    | _ -> Other

let private executeQueryOrMutation
    (resultSet : (string * ExecutionInfo)[])
    (ctx : ExecutionContext)
    (objDef : ObjectDef)
    (rootValue : obj)
    : AsyncVal<GQLExecutionResult> =
    let executeRootOperation (name, info) (args : Map<string, obj>) =
        let fDef = info.Definition
        let path = [ box info.Identifier ]
        let fieldCtx = {
            ExecutionInfo = info
            Context = ctx
            ReturnType = fDef.TypeDef
            ParentType = objDef
            Schema = ctx.Schema
            Args = args
            Variables = ctx.Variables
            Path = normalizeErrorPath path
        }
        let execute = ctx.FieldExecuteMap.GetExecute (ctx.ExecutionPlan.RootDef.Name, info.Definition.Name)
        asyncVal {
            let! result =
                executeResolvers ctx.GetInputContext fieldCtx path rootValue (resolveField execute fieldCtx rootValue)
                |> AsyncVal.rescue path ctx.Schema.ParseError
            let result =
                match result with
                | Ok (Ok value) -> Ok value
                | Ok (Error errs)
                | Error errs -> Error errs
            match result with
            | Error errs when info.IsNullable -> return Ok (KeyValuePair (name, null), ValueNone, errs)
            | Error errs -> return Error errs
            | Ok r -> return Ok r
        }

    asyncVal {
        let documentId = ctx.ExecutionPlan.DocumentId
        // Inline argument coercion is request validation, the same as variable coercion in Executor.eval's
        // coerceVariables: it rejects the request before any root resolver runs, so its errors must never be
        // reported as an execution result with null data
        let coerced = SortedDictionary<int, struct (Map<string, obj> * IGQLError list)>()
        resultSet
        |> Array.iteri (fun i (_, info) ->
            let argDefs = ctx.FieldExecuteMap.GetArgs (ctx.ExecutionPlan.RootDef.Name, info.Definition.Name)
            match getArgumentValues argDefs info.Ast.Arguments ctx.GetInputContext ctx.Variables with
            | Ok args -> coerced.Add (i, struct (args, []))
            | Error errs -> coerced.Add (i, struct (Map.empty, errs)))
        let coercionErrors =
            coerced.Values
            |> Seq.collect (fun struct (_, errs) -> errs)
            |> Seq.toList
        if not coercionErrors.IsEmpty then
            return GQLExecutionResult.Error (documentId, coercionErrors, ctx.Metadata)
        else
            let operations =
                coerced
                |> Seq.map (fun (KeyValue (i, struct (args, _))) -> executeRootOperation resultSet[i] args)
                |> Seq.toArray
            match! operations |> collectFields ctx.ExecutionPlan.Strategy with
            | Ok (data, ValueSome deferred, errs) ->
                return GQLExecutionResult.Deferred (documentId, NameValueLookup (data), errs, deferred, ctx.Metadata)
            | Ok (data, ValueNone, errs) -> return GQLExecutionResult.Direct (documentId, NameValueLookup (data), errs, ctx.Metadata)
            // Only a non-null root field failing during execution reaches this branch: an execution result whose
            // data is null, as the spec requires, unlike the request error returned above for a coercion failure
            | Error errs -> return GQLExecutionResult.Direct (documentId, null, errs, ctx.Metadata)
    }

let private executeSubscription
    (resultSet : (string * ExecutionInfo)[])
    (inputContext : InputExecutionContextProvider)
    (ctx : ExecutionContext)
    (objDef : SubscriptionObjectDef)
    value
    = result {
    // Subscription queries can only have one root field
    let nameOrAlias, info = Array.head resultSet
    let subDef = info.Definition :?> SubscriptionFieldDef
    let! args = getArgumentValues subDef.Args info.Ast.Arguments inputContext ctx.Variables
    let returnType = subDef.OutputTypeDef
    let fieldPath = [ box info.Identifier ]
    let fieldCtx = {
        ExecutionInfo = info
        Context = ctx
        ReturnType = returnType
        ParentType = objDef
        Schema = ctx.Schema
        Args = args
        Variables = ctx.Variables
        Path = fieldPath |> List.rev
    }
    let onValue v = asyncVal {
        match! executeResolvers inputContext fieldCtx fieldPath value (toValueOption v |> AsyncVal.wrap) with
        | Ok (data, ValueNone, []) -> return SubscriptionResult (NameValueLookup.ofList [ nameOrAlias, data.Value ])
        | Ok (data, ValueNone, errs) -> return SubscriptionErrors (ValueSome (NameValueLookup.ofList [ nameOrAlias, data.Value ]), errs)
        | Ok (_, ValueSome _, _) -> return failwith "Deferred/Streamed/Live are not supported for subscriptions!"
        | Error errs -> return SubscriptionErrors (ValueNone, errs)
    }
    return
        ctx.Schema.SubscriptionProvider.Add fieldCtx value subDef
        |> Observable.bind (onValue >> Observable.ofAsyncVal)
}

let private compileInputObject (inputDef : InputObjectDef) (inputContext : InputExecutionContextProvider) =
    inputDef.Fields
    |> Array.iter (fun inputField ->
        // TODO: Implement compilation cache to reuse for the same type
        let inputFieldTypeDef = inputField.TypeDef
        inputField.ExecuteInput <- compileByType [ box inputField.Name ] Unknown (inputFieldTypeDef, inputFieldTypeDef) inputContext
        match inputField.TypeDef with
        | InputObject inputObjDef -> inputObjDef.ExecuteInput <- inputField.ExecuteInput
        | _ -> ())
#if DEBUG
    if isNull (box inputDef.ExecuteInput) then
        System.Diagnostics.Debug.Fail ($"Input object '{inputDef.Name}' has no ExecuteInput function!")
#endif

let private compileObject (objDef : ObjectDef) (executeFields : FieldDef -> unit) (inputContext : InputExecutionContextProvider) =
    objDef.Fields
    |> Map.iter (fun _ fieldDef ->
        executeFields fieldDef
        fieldDef.Args
        |> Array.iter (fun arg ->
            //let errMsg = $"Object '%s{objdef.Name}': field '%s{fieldDef.Name}': argument '%s{arg.Name}': "
            // TODO: Pass arg name
            let argTypeDef = arg.TypeDef
            arg.ExecuteInput <- compileByType [] (Argument arg) (argTypeDef, argTypeDef) inputContext
            match arg.TypeDef with
            | InputObject inputObjDef -> inputObjDef.ExecuteInput <- arg.ExecuteInput
            | _ -> ()))

let internal compileSchema (ctx : SchemaCompileContext) =
    ctx.Schema.TypeMap.ToSeq ()
    |> Seq.iter (fun (tName, x) ->
        match x with
        | SubscriptionObject subDef ->
            compileObject
                subDef
                (fun sub ->
                    let filter =
                        match sub with
                        | :? SubscriptionFieldDef as subField -> compileSubscriptionField subField
                        | _ ->
                            failwith
                                $"Schema error: subscription object '%s{subDef.Name}' does have a field '%s{sub.Name}' that is not a subscription field definition."
                    ctx.Schema.SubscriptionProvider.Register { Name = sub.Name; Filter = filter })
                ctx.GetInputContext
        | Object objDef -> compileObject objDef (fun fieldDef -> ctx.FieldExecuteMap.SetExecute (tName, fieldDef)) ctx.GetInputContext
        | InputObject inputDef -> compileInputObject inputDef ctx.GetInputContext
        | _ -> ())

let internal coerceVariables
    (variables : VarDef list)
    (inputContext : InputExecutionContextProvider)
    (vars : ImmutableDictionary<string, JsonElement>)
    = result {
    let variables, inlineValues, nulls =
        variables
        |> List.fold
            (fun (valiables, inlineValues, missing) varDef ->
                match vars.TryGetValue varDef.Name with
                | false, _ ->
                    match varDef.DefaultValue with
                    | Some defaultValue ->
                        let item = struct (varDef, defaultValue)
                        (valiables, item :: inlineValues, missing)
                    | None ->
                        let item =
                            match varDef.TypeDef with
                            | Nullable _ -> Ok <| KeyValuePair (varDef.Name, null)
                            | Named typeDef ->
                                Error [
                                    {
                                        Message =
                                            $"A variable '$%s{varDef.Name}' of type '%s{typeDef.Name}!' is not nullable but neither value was provided, nor a default value was specified."
                                        ErrorKind = InputCoercion
                                        InputSource = Variable varDef
                                        Path = []
                                        FieldErrorDetails = ValueNone
                                    }
                                    :> IGQLError
                                ]
                            | _ ->
                                System.Diagnostics.Debug.Fail $"{varDef.TypeDef.GetType().Name} is not Named"
                                failwith "Impossible case"
                        (valiables, inlineValues, item :: missing)
                | true, jsonElement ->
                    let item = struct (varDef, jsonElement)
                    (item :: valiables, inlineValues, missing))
            ([], [], [])

    // First we need to coerce variables
    let! variablesBuilder =
        variables
        |> List.fold
            (fun (acc : Result<ImmutableDictionary<string, obj>.Builder, IGQLError list>) struct (varDef, jsonElement) -> validation {
                let! value =
                    let varTypeDef = varDef.TypeDef
                    let ctx = {
                        IsNullable = false
                        InputObjectPath = []
                        ObjectFieldErrorDetails = ValueNone
                        OriginalTypeDef = varTypeDef
                        TypeDef = varTypeDef
                        VarDef = varDef
                        Input = jsonElement
                    }
                    coerceVariableValue (ctx, inputContext)
                    |> Result.mapError (
                        List.map (fun err ->
                            match err with
                            | :? IInputSourceError as err ->
                                match err.InputSource with
                                | Variable _ -> ()
                                | _ -> err.InputSource <- Variable varDef
                            | _ -> ()
                            err)
                    )
                and! acc = acc
                acc.Add (varDef.Name, value)
                return acc
            })
            (ImmutableDictionary.CreateBuilder<string, obj>() |> Ok)

    let suppliedVariables = variablesBuilder.ToImmutable ()

    // TODO: consider how to execute inline objects validation having some variables coercion or validation failed
    // Having variables we can coerce inline values that contain on variables
    let! variablesBuilder =
        inlineValues
        |> List.fold
            (fun (acc : Result<ImmutableDictionary<string, obj>.Builder, IGQLError list>) struct (varDef, defaultValue) -> validation {
                let varTypeDef = varDef.TypeDef
                let executeInput = compileByType [] (Variable varDef) (varTypeDef, varTypeDef) inputContext
                let! value = executeInput inputContext defaultValue suppliedVariables
                and! acc = acc
                acc.Add (varDef.Name, value)
                return acc
            })
            (variablesBuilder |> Ok)

    and! nulls = nulls |> splitSeqErrorsList

    nulls |> Array.iter variablesBuilder.Add

    return variablesBuilder.ToImmutable ()
}

#nowarn "0046"

let internal executeOperation (ctx : ExecutionContext) : AsyncVal<GQLExecutionResult> =
    let includeResults =
        ctx.ExecutionPlan.Fields
        |> List.map (fun info ->
            info.Include ctx.Variables
            |> Result.map (fun include -> struct (info, include)))
    match includeResults |> splitSeqErrorsList with
    | Error errs -> asyncVal { return GQLExecutionResult.Error (ctx.ExecutionPlan.DocumentId, errs, ctx.Metadata) }
    | Ok includes ->

    let resultSet =
        includes
        |> Seq.filter sndv
        |> Seq.map fstv
        |> Seq.map (fun info -> (info.Identifier, info))
        |> Seq.toArray
    match ctx.ExecutionPlan.Operation.OperationType with
    | Query -> executeQueryOrMutation resultSet ctx ctx.Schema.Query ctx.RootValue
    | Mutation ->
        match ctx.Schema.Mutation with
        | ValueSome m -> executeQueryOrMutation resultSet ctx m ctx.RootValue
        | ValueNone -> raise (InvalidOperationException ("Attempted to make a mutation but no mutation schema was present!"))
    | Subscription ->
        match ctx.Schema.Subscription with
        | ValueSome s ->
            match executeSubscription resultSet ctx.GetInputContext ctx s ctx.RootValue with
            | Ok data -> AsyncVal.wrap (GQLExecutionResult.Stream (ctx.ExecutionPlan.DocumentId, data, ctx.Metadata))
            | Error errs -> asyncVal { return GQLExecutionResult.Error (ctx.ExecutionPlan.DocumentId, errs, ctx.Metadata) }

        | ValueNone -> raise (InvalidOperationException ("Attempted to make a subscription but no subscription schema was present!"))
