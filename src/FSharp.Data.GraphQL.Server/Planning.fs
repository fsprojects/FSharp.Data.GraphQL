// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Planning

open System
open System.Collections.Generic
open System.Diagnostics
open FsToolkit.ErrorHandling

open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Extensions
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns
open FSharp.Data.GraphQL.Types.Introspection
open FSharp.Data.GraphQL.Introspection

/// Field definition allowing to access the current type schema of this server.
let SchemaMetaFieldDef =
    Define.Field(
        name = "__schema",
        description = "Access the current type schema of this server.",
        typedef = __Schema,
        resolve = fun ctx (_: obj) -> ctx.Schema.Introspected)

/// Field definition allowing to request the type information of a single type.
let TypeMetaFieldDef =
    Define.Field(
        name = "__type",
        description = "Request the type information of a single type.",
        typedef = StructNullable __Type,
        args = [
            { Name = "name"
              Description = ValueNone
              IsSkippable = false
              TypeDef = StringType
              DefaultValue = ValueNone
              ExecuteInput = variableOrElse(InlineConstant >> coerceStringInput >> Result.map box) }
        ],
        resolve = fun ctx (_:obj) ->
            ctx.Schema.Introspected.Types
            |> Seq.vtryFind (fun t -> t.Name = ctx.Arg("name"))
            |> ValueOption.map IntrospectionTypeRef.Named)

/// Field definition allowing to resolve a name of the current Object type at runtime.
let TypeNameMetaFieldDef : FieldDef<obj> =
    Define.Field(
        name = "__typename",
        description = "The name of the current Object type at runtime.",
        typedef = StringType,
        resolve = fun ctx (_:obj) -> ctx.ParentType.Name)

let private tryFindDef (schema: ISchema) (objdef: ObjectDef) (field: Field) : FieldDef option =
        match field.Name with
        | "__schema" when Object.ReferenceEquals(schema.Query, objdef) -> Some (upcast SchemaMetaFieldDef)
        | "__type" when Object.ReferenceEquals(schema.Query, objdef) -> Some (upcast TypeMetaFieldDef)
        | "__typename" -> Some (upcast TypeNameMetaFieldDef)
        | fieldName -> objdef.Fields |> Map.tryFind fieldName

let private objectInfo (ctx: PlanningContext) (parentDef: ObjectDef) field includer =
    match tryFindDef ctx.Schema parentDef field with
    | Some fdef ->
        { Identifier = field.AliasOrName
          Kind = ResolveValue
          ParentDef = parentDef
          ReturnDef =
            match parentDef with
            | SubscriptionObject _ -> (fdef :?> SubscriptionFieldDef).OutputTypeDef
            | Object _ -> fdef.TypeDef
            | _ ->
                Debug.Fail "Must be prevented by validation"
                raise (
                    NotSupportedException
                        $"Object definition '%s{parentDef.Name}' is implemented by '%s{parentDef.GetType().FullName}', which is not supported by query planning"
                )
          Definition = fdef
          Ast = field
          Include = includer
          IsNullable = false }
    | None ->
        Debug.Fail "Must be prevented by validation"
        raise (MalformedGQLQueryException $"No field '%s{field.Name}' was defined in object definition '%s{parentDef.Name}'")

let rec private abstractionInfo (ctx : PlanningContext) (parentDef : AbstractDef) field typeCondition includer =
    let objDefs = ctx.Schema.GetPossibleTypes parentDef
    match typeCondition with
    | ValueNone ->
        objDefs
        |> Array.choose (fun objDef ->
            match tryFindDef ctx.Schema objDef field with
            | Some fdef ->
                let data =
                    { Identifier = field.AliasOrName
                      ParentDef = parentDef :?> OutputDef
                      ReturnDef = fdef.TypeDef
                      Definition = fdef
                      Ast = field
                      Kind = ResolveAbstraction Map.empty
                      Include = includer
                      IsNullable = false }
                Some (objDef.Name, data)
            | None -> None)
        |> Map.ofArray
    | ValueSome typeName ->
        match objDefs |> Array.tryFind (fun o -> o.Name = typeName) with
        | Some objDef ->
            match tryFindDef ctx.Schema objDef field with
            | Some fdef ->
                let data =
                    { Identifier = field.AliasOrName
                      ParentDef = parentDef  :?> OutputDef
                      ReturnDef = fdef.TypeDef
                      Definition = fdef
                      Ast = field
                      Kind = ResolveAbstraction Map.empty
                      Include = includer
                      IsNullable = false }
                Map.ofList [ objDef.Name, data ]
            | None -> Map.empty
        | None ->
            match ctx.Schema.TryFindType typeName with
            | ValueSome (Abstract abstractDef) ->
                abstractionInfo ctx abstractDef field ValueNone includer
            | _ ->
                // Type condition doesn't match any possible types of the abstract type.
                // This is valid and should return an empty map (no fields for this type condition).
                Map.empty

let private directiveIncluder (directive: Directive) : Includer =
    fun variables ->
        match directive.If.Value with
        | VariableName vname -> Ok <| downcast variables.[vname]
        | other -> coerceBoolInput (InlineConstant other)

let private incl: Includer = fun _ -> Ok true
let private excl: Includer = fun _ -> Ok false
let private getIncluder (directives: Directive list) parentIncluder : Includer =
    directives
    |> List.fold (fun acc directive ->
        match directive.Name with
        | "skip" ->
            fun vars -> result {
                let! accValue = acc vars
                let! skipValue = directiveIncluder directive vars
                return accValue && not(skipValue)
            }
        | "include" ->
            fun vars -> result {
                let! accValue = acc vars
                let! includeValue = directiveIncluder directive vars
                return accValue && includeValue
            }
        | _ -> acc) parentIncluder

let private doesFragmentTypeApply (schema: ISchema) fragment (objectType: ObjectDef) =
    match fragment.TypeCondition with
    | ValueNone -> true
    | ValueSome typeCondition ->
        match schema.TryFindType typeCondition with
        | ValueNone -> false
        | ValueSome conditionalType when conditionalType.Name = objectType.Name -> true
        | ValueSome (Abstract conditionalType) -> schema.IsPossibleType conditionalType objectType
        | _ -> false

/// <summary>
/// Whether an <c>@defer</c> or <c>@stream</c> directive applies as far as planning can tell: it is disabled only by a
/// literal <c>if: false</c>; an <c>if</c> given through a variable is decided at execution, so the field is planned
/// as deferred.
/// </summary>
let private isEnabledAtPlanning (directive : Directive) =
    match directive.Arguments |> List.vtryFind (fun argument -> argument.Name = "if") with
    | ValueSome { Value = BooleanValue false } -> false
    | _ -> true

let private isDeferredField (field: Field) =
    field.Directives |> List.exists (fun d -> d.Name = "defer" && isEnabledAtPlanning d)

let private isStreamedField (field : Field) =
    field.Directives |> List.exists (fun d -> d.Name = "stream" && isEnabledAtPlanning d)

let private getStreamBufferMode (field : Field) =
    let cast argName value =
        match value with
        | IntValue v -> int v
        | _ ->
            Debug.Fail "Must be prevented by validation"
            raise (
                MalformedGQLQueryException
                    $"Argument '%s{argName}' of the @stream directive on field '%s{field.AliasOrName}' must be an integer, but '%O{value}' was provided"
            )
    let directive =
        field.Directives
        |> List.vtryFind (fun d -> d.Name = "stream")
    let getArg argName (d : Directive) =
        d.Arguments
        |> List.vtryFind (fun x -> x.Name = argName)
        |> ValueOption.map (fun x -> x.Value |> cast argName)
    let interval = getArg "interval"
    let preferredBatchSize = getArg "preferredBatchSize"
    match directive with
    | ValueSome d -> { Interval = interval d; PreferredBatchSize = preferredBatchSize d }
    | ValueNone ->
        // Buffer options are read only for fields that have the @stream directive, so this indicates a planner bug
        Debug.Fail "Must be prevented by validation"
        raise (InvalidOperationException $"Field '%s{field.AliasOrName}' is planned as streamed, but it has no @stream directive")

let private isLiveField (field : Field) =
    field.Directives |> List.exists (fun d -> d.Name = "live")

let private (|Planned|Deferred|Streamed|Live|) field =
    if isStreamedField field then Streamed (getStreamBufferMode field)
    elif isDeferredField field then Deferred
    elif isLiveField field then Live
    else Planned

/// Returns the name of the execution kind for exception messages, without printing the whole planned subtree
let private kindName (kind : ExecutionInfoKind) =
    match kind with
    | ResolveValue -> nameof ResolveValue
    | SelectFields _ -> nameof SelectFields
    | ResolveCollection _ -> nameof ResolveCollection
    | ResolveAbstraction _ -> nameof ResolveAbstraction
    | ResolveDeferred _ -> nameof ResolveDeferred
    | ResolveStreamed _ -> nameof ResolveStreamed
    | ResolveLive _ -> nameof ResolveLive
    | ResolveDeferredFragment _ -> nameof ResolveDeferredFragment

let private getSelectionFrag = function
    | SelectFields(fragmentFields) -> fragmentFields
    | kind ->
        Debug.Fail "Must be prevented by validation"
        raise (InvalidOperationException $"Expected a fragment to be planned as {nameof SelectFields}, but it was planned as {kindName kind}")

let private getAbstractionFrag = function
    | ResolveAbstraction(fragmentFields) -> fragmentFields
    | kind ->
        Debug.Fail "Must be prevented by validation"
        raise (InvalidOperationException $"Expected a fragment to be planned as {nameof ResolveAbstraction}, but it was planned as {kindName kind}")

/// <summary>
/// The fields of a selection set while it is planned, in selection order, indexed by identifier, so that adding a field
/// or merging a field selected again takes constant time instead of a search through the fields planned so far.
/// </summary>
[<Sealed>]
type private PlannedFields
    /// <param name="mergeField">Merges a field selected again into the field planned with the same identifier.</param>
    (mergeField : ExecutionInfo -> ExecutionInfo -> ExecutionInfo) =
    let fields = ResizeArray<ExecutionInfo> ()
    let positions = Dictionary<string, int> (StringComparer.Ordinal)

    /// Whether a field with the identifier is planned already
    member _.Contains (identifier : string) = positions.ContainsKey identifier

    /// Adds the field after the fields planned so far
    member _.Add (field : ExecutionInfo) =
        positions.TryAdd (field.Identifier, fields.Count) |> ignore
        fields.Add field

    /// Merges the field into the field planned with the same identifier, or adds it when there is none
    member this.Merge (field : ExecutionInfo) =
        match positions.TryGetValue field.Identifier with
        | true, position -> fields[position] <- mergeField (fields[position]) field
        | false, _ -> this.Add field

    /// The fields in selection order
    member _.ToList () = List.ofSeq fields

/// The fields of both selections as one: a field selected by both has the selections under it merged, in the order
/// of the first selection, and the fields only the second selects follow.
let rec internal deepMerge (xs: ExecutionInfo list) (ys: ExecutionInfo list) =
    match xs, ys with
    | [], _ -> ys
    | _, [] -> xs
    | _ ->
        // Indexed by identifier: searching the other list for every field is quadratic in the number of fields
        let merged = PlannedFields mergeField
        xs |> List.iter merged.Add
        ys |> List.iter merged.Merge
        merged.ToList ()

/// The field selected twice as one, with the selections under it merged in the order of the first selection
and private mergeField (x: ExecutionInfo) (y: ExecutionInfo) =
    match x.Kind, y.Kind with
    | ResolveValue, ResolveValue -> x
    | ResolveCollection(x'), ResolveCollection(y') -> { x with Kind = ResolveCollection(mergeField x' y') }
    | ResolveAbstraction(xs'), ResolveAbstraction(ys') -> { x with Kind = ResolveAbstraction(Map.merge (fun _ x' y' -> deepMerge x' y') xs' ys')}
    | SelectFields(xs'), SelectFields(ys') -> { x with Kind = SelectFields(deepMerge xs' ys') }
    | _ ->
        Debug.Fail "Must be prevented by validation"
        raise (
            InvalidOperationException
                $"Cannot merge field '%s{x.Identifier}' planned as {kindName x.Kind} with the same field planned as {kindName y.Kind}"
        )

/// <summary>The fields of a selection set to plan, merged as <see cref="deepMerge"/> merges them.</summary>
let private newPlannedFields () = PlannedFields mergeField

/// <summary>
/// Compares the field of the document, the type it returns and its includer by reference: planning the selection set
/// of the field again for the same three gives the same plan.
/// </summary>
let private plannedSelectionKeyComparer =
    { new IEqualityComparer<struct (Field * OutputDef * Includer)> with
        member _.Equals (x, y) =
            let struct (xField, xReturnDef, xInclude) = x
            let struct (yField, yReturnDef, yInclude) = y
            obj.ReferenceEquals (xField, yField)
            && obj.ReferenceEquals (xReturnDef, yReturnDef)
            && obj.ReferenceEquals (xInclude, yInclude)
        member _.GetHashCode key =
            let struct (field, returnDef, includer) = key
            HashCode.Combine (
                LanguagePrimitives.PhysicalHash field,
                LanguagePrimitives.PhysicalHash returnDef,
                LanguagePrimitives.PhysicalHash includer
            ) }

/// Compares the directives of a selection and the includer of its parent by reference
let private includerKeyComparer =
    { new IEqualityComparer<struct (Directive list * Includer)> with
        member _.Equals (x, y) =
            let struct (xDirectives, xParent) = x
            let struct (yDirectives, yParent) = y
            obj.ReferenceEquals (xDirectives, yDirectives) && obj.ReferenceEquals (xParent, yParent)
        member _.GetHashCode key =
            let struct (directives, parent) = key
            HashCode.Combine (LanguagePrimitives.PhysicalHash directives, LanguagePrimitives.PhysicalHash parent) }

/// <summary>
/// The state of planning one operation: the fragments of the document by name, the includers built so far, and the
/// plans of the selection sets planned so far.
/// </summary>
/// <remarks>
/// <para>
/// A field of an abstract type is planned once for every possible type of its parent, so without reusing the plans of
/// its selection set, nesting such fields multiplies the planning work by the number of possible types at every level:
/// a document of a few hundred characters could take hours to plan.
/// </para>
/// <para>
/// A plan is reused for the same field, return type and includer, which are compared by reference, so the includer of
/// a selection is built once for its directives and the includer of its parent. Otherwise every possible type would
/// build its own includers for the selections under it, and none of their plans would be reused.
/// </para>
/// <para>
/// A reused plan keeps the deferred fragment entries planned the first time, which copy the parent field's definition;
/// execution reads only their includer and kind, which are the same for every reuse.
/// </para>
/// </remarks>
[<Sealed>]
type private PlanState (ctx : PlanningContext) =
    let fragments = Dictionary<string, FragmentDefinition> (StringComparer.Ordinal)
    do
        for definition in ctx.Document.Definitions do
            match definition with
            // The first definition of a name wins, as validation does
            | FragmentDefinition fragment when fragment.Name.IsSome && not (fragments.ContainsKey fragment.Name.Value) ->
                fragments.Add (fragment.Name.Value, fragment)
            | _ -> ()
    let includers = Dictionary<struct (Directive list * Includer), Includer> (includerKeyComparer)

    member _.Context = ctx

    /// The fragment definition with the name
    member _.TryFindFragment (name : string) =
        match fragments.TryGetValue name with
        | true, fragment -> ValueSome fragment
        | false, _ -> ValueNone

    /// The includer of a selection with the directives under the parent includer, the same instance for the same two
    member _.GetIncluder (directives : Directive list, parentIncluder : Includer) =
        match directives with
        | [] -> parentIncluder
        | _ ->
            let key = struct (directives, parentIncluder)
            match includers.TryGetValue key with
            | true, includer -> includer
            | false, _ ->
                let includer = getIncluder directives parentIncluder
                includers.Add (key, includer)
                includer

    /// The plans of the selection sets of fields, as their execution kinds
    member val PlannedSelections =
        Dictionary<struct (Field * OutputDef * Includer), ExecutionInfoKind> (plannedSelectionKeyComparer)

/// <summary>
/// The state of planning one selection set together with the fragments spread into it: the fragments already spread
/// into the object's own selection, the fragments already spread deferred, and the next id of a deferred fragment,
/// unique among the deferred fragments of the selection set.
/// </summary>
/// <remarks>
/// A fragment is spread once. A deferred spread never stands in for a direct spread of the same fragment, whichever
/// comes first in the document, so the two are tracked apart: a direct spread is skipped only after a direct spread,
/// a deferred spread after a spread of either kind.
/// </remarks>
type private SelectionScope = {
    VisitedFragments : HashSet<string>
    DeferredFragments : HashSet<string>
    mutable NextFragmentId : int
}

/// The scope of a selection set no fragment was spread into yet.
let private newScope () = {
    VisitedFragments = HashSet<string> (StringComparer.Ordinal)
    DeferredFragments = HashSet<string> (StringComparer.Ordinal)
    NextFragmentId = 0
}

/// <summary>
/// Whether the fragment spread is planned, recording it in the scope when it is, as documented on
/// <see cref="SelectionScope"/>.
/// </summary>
let private tryVisitSpread (scope : SelectionScope) (spreadName : string) (isDeferred : bool) =
    let spreadDirectly = scope.VisitedFragments.Contains spreadName
    if isDeferred then
        if spreadDirectly || scope.DeferredFragments.Contains spreadName then
            false
        else
            scope.DeferredFragments.Add spreadName |> ignore
            true
    elif spreadDirectly then
        false
    else
        scope.VisitedFragments.Add spreadName |> ignore
        true

/// <summary>
/// The <c>@defer</c> directive of a fragment spread or inline fragment, when it applies as far as planning can tell.
/// </summary>
let private deferredFragmentDirective (directives : Directive list) =
    directives |> List.vtryFind (fun d -> d.Name = "defer" && isEnabledAtPlanning d)

/// <summary>
/// The <c>label</c> argument of the directive: a string literal, as validation requires, so there is none for any
/// other value.
/// </summary>
let private directiveLabel (directive : Directive) =
    directive.Arguments
    |> List.vtryFind (fun argument -> argument.Name = "label")
    |> ValueOption.bind (fun argument ->
        match argument.Value with
        | StringValue label -> ValueSome label
        | _ -> ValueNone)

/// <summary>
/// Whether the directive is enabled with the variables of a request: a literal <c>if</c> was decided by
/// <see cref="isEnabledAtPlanning"/>, so only an <c>if</c> given through a variable is evaluated here, the same way
/// the execution engine evaluates it for a deferred field.
/// </summary>
let private directiveEnabledAtExecution (directive : Directive) : Includer =
    match directive.Arguments |> List.vtryFind (fun argument -> argument.Name = "if") with
    | ValueSome { Value = VariableName name } ->
        fun variables ->
            match variables.TryGetValue name with
            | true, (:? bool as enabled) -> Ok enabled
            | _ -> Ok true
    | _ -> incl

/// The next id of a deferred fragment of the selection set.
let private allocateFragmentId (scope : SelectionScope) =
    let fragmentId = scope.NextFragmentId
    scope.NextFragmentId <- fragmentId + 1
    fragmentId

/// The plan entry delivering the fields of a deferred fragment as one payload of the object containing them; it
/// stands among that object's fields under an identifier no field can have
let private deferredFragmentEntry (directive : Directive) (fragmentId : int) (info : ExecutionInfo) (fragmentFields : ExecutionInfo list) =
    { info with
        Identifier = $"@defer#{fragmentId}"
        Kind = ResolveDeferredFragment (directiveLabel directive, fragmentId, directiveEnabledAtExecution directive, fragmentFields) }

/// A field selected directly on the object is executed with it, so a deferred fragment that also selects it
/// delivers only its other fields, and whatever the fragment selects under that field is selected on the object's
/// own field instead, wherever in the selection the fragment stands; a fragment left without fields delivers nothing
/// and is dropped
let private withoutDirectlySelectedFields (plannedFields : ExecutionInfo list) =
    let isDeferredFragment (field : ExecutionInfo) =
        match field.Kind with
        | ResolveDeferredFragment _ -> true
        | _ -> false
    if not (plannedFields |> List.exists isDeferredFragment) then
        plannedFields
    else
        let ownFields = newPlannedFields ()
        let fragments = ResizeArray<ExecutionInfo> ()
        for field in plannedFields do
            if isDeferredFragment field then fragments.Add field else ownFields.Add field
        let remainingFragments = ResizeArray<ExecutionInfo> (fragments.Count)
        for fragment in fragments do
            match fragment.Kind with
            | ResolveDeferredFragment (label, fragmentId, enabled, fragmentFields) ->
                // Merging an overlapping field adds no identifier, so only the fields selected directly are contained
                let overlapping, remaining =
                    fragmentFields |> List.partition (fun fragmentField -> ownFields.Contains fragmentField.Identifier)
                overlapping |> List.iter ownFields.Merge
                match remaining with
                | [] -> ()
                | remaining -> remainingFragments.Add { fragment with Kind = ResolveDeferredFragment (label, fragmentId, enabled, remaining) }
            | _ -> ()
        [ yield! ownFields.ToList (); yield! remainingFragments ]

let rec private plan (state : PlanState) (info : ExecutionInfo) : ExecutionInfo =
    match info.ReturnDef with
    | Leaf _ -> info
    | SubscriptionObject _
    | Object _
    | Abstract _ -> planFieldSelection state info
    | Nullable returnDef ->
        let inner = plan state { info with ParentDef = info.ReturnDef; ReturnDef = downcast returnDef }
        { inner with IsNullable = true }
    | List returnDef ->
        // We dont yet know the indices of our elements so we append a dummy value on
        let inner = plan state { info with ParentDef = info.ReturnDef; ReturnDef = downcast returnDef; }
        { info with Kind = ResolveCollection inner }
    | returnDef ->
        Debug.Fail "Must be prevented by validation"
        raise (
            NotSupportedException
                $"Field '%s{info.Identifier}' returns the type definition '{returnDef}' implemented by '%s{returnDef.GetType().FullName}', which is not supported by query planning"
        )

/// <summary>
/// The plan of the selection set of a field returning an object or an abstract type, reused for the same field, return
/// type and includer, as documented on <see cref="PlanState"/>.
/// </summary>
and private planFieldSelection (state : PlanState) (info : ExecutionInfo) : ExecutionInfo =
    let key = struct (info.Ast, info.ReturnDef, info.Include)
    match state.PlannedSelections.TryGetValue key with
    | true, kind -> { info with Kind = kind }
    | false, _ ->
        let planned =
            match info.ReturnDef with
            | Abstract _ -> planAbstraction state info.Ast.SelectionSet info (newScope ()) ValueNone
            | _ -> planSelection state info.Ast.SelectionSet info (newScope ())
        state.PlannedSelections.Add (key, planned.Kind)
        planned

and private planSelection (state : PlanState) (selectionSet: Selection list) (info: ExecutionInfo) (scope : SelectionScope) : ExecutionInfo =
    let ctx = state.Context
    let parentDef = downcast info.ReturnDef
    let fields = newPlannedFields ()
    /// The fields of a fragment merged into the object's selection, or, when the fragment is deferred, delivered
    /// later as one payload of the object; the entry carries the fragment's own includer, so `@skip`/`@include` on
    /// the spread or inline fragment decide whether it is delivered at all
    let addFragmentFields (fragmentInfo : ExecutionInfo) (directives : Directive list) (fragmentFields : ExecutionInfo list) =
        match deferredFragmentDirective directives with
        | ValueSome directive -> fields.Add (deferredFragmentEntry directive (allocateFragmentId scope) fragmentInfo fragmentFields)
        | ValueNone -> fragmentFields |> List.iter fields.Merge // filter out already existing fields
    for selection in selectionSet do
        // FIXME: includer is not passed along from top level fragments (both inline and spreads)
        let includer = state.GetIncluder (selection.Directives, info.Include)
        let updatedInfo = { info with Include = includer }
        match selection with
        | Field field ->
            if not (fields.Contains field.AliasOrName) then
                let innerInfo = objectInfo ctx parentDef field includer
                let executionPlan = plan state innerInfo
                match field with
                | Deferred -> fields.Add { executionPlan with Kind = ResolveDeferred executionPlan }
                | Live -> fields.Add { executionPlan with Kind = ResolveLive executionPlan }
                | Streamed mode -> fields.Add { executionPlan with Kind = ResolveStreamed (executionPlan, mode) }
                | Planned -> fields.Add executionPlan
        | FragmentSpread spread ->
            // A fragment already found is not spread again
            if tryVisitSpread scope spread.Name (deferredFragmentDirective spread.Directives).IsSome then
                match state.TryFindFragment spread.Name with
                | ValueSome fragment when doesFragmentTypeApply ctx.Schema fragment parentDef ->
                    // Retrieve fragment data just as it was normal selection set
                    // TODO: Check if the path is correctly defined
                    let fragmentInfo = planSelection state fragment.SelectionSet updatedInfo scope
                    addFragmentFields updatedInfo spread.Directives (getSelectionFrag fragmentInfo.Kind)
                | _ -> ()
        | InlineFragment fragment when doesFragmentTypeApply ctx.Schema fragment parentDef ->
            // retrieve fragment data just as it was normal selection set
            let fragmentInfo = planSelection state fragment.SelectionSet updatedInfo scope
            addFragmentFields updatedInfo fragment.Directives (getSelectionFrag fragmentInfo.Kind)
        | InlineFragment _ -> ()
    { info with Kind = SelectFields (withoutDirectlySelectedFields (fields.ToList ())) }

and private planAbstraction (state : PlanState) (selectionSet: Selection list) (info : ExecutionInfo) (scope : SelectionScope) typeCondition : ExecutionInfo =
    let ctx = state.Context
    let typeFields = Dictionary<string, PlannedFields> (StringComparer.Ordinal)
    let fieldsOfType (typeName : string) =
        match typeFields.TryGetValue typeName with
        | true, fields -> fields
        | false, _ ->
            let fields = newPlannedFields ()
            typeFields.Add (typeName, fields)
            fields
    /// The fields of a fragment merged into every type's selection, or, when the fragment is deferred, delivered
    /// later as one payload of the object, whatever its concrete type turns out to be
    let addFragmentFields (fragmentInfo : ExecutionInfo) (directives : Directive list) (fragmentFields : Map<string, ExecutionInfo list>) =
        match deferredFragmentDirective directives with
        | ValueSome directive ->
            let fragmentId = allocateFragmentId scope
            // One entry per concrete type, appended to that type's fields: an entry's identifier is no field's, so
            // there is nothing to merge
            for KeyValue (typeName, fields) in fragmentFields do
                (fieldsOfType typeName).Add (deferredFragmentEntry directive fragmentId fragmentInfo fields)
        | ValueNone ->
            // Filter out already existing fields
            for KeyValue (typeName, fields) in fragmentFields do
                let typeFields = fieldsOfType typeName
                fields |> List.iter typeFields.Merge
    for selection in selectionSet do
        let includer = state.GetIncluder (selection.Directives, info.Include)
        let innerData = { info with Include = includer }
        match selection with
        | Field field ->
            for KeyValue (typeName, data) in abstractionInfo ctx (info.ReturnDef :?> AbstractDef) field typeCondition includer do
                let executionPlan = plan state data
                let executionPlan =
                    match field with
                    | Deferred -> { executionPlan with Kind = ResolveDeferred executionPlan }
                    | Live -> { executionPlan with Kind = ResolveLive executionPlan }
                    | Streamed mode -> { executionPlan with Kind = ResolveStreamed (executionPlan, mode) }
                    | Planned -> executionPlan
                (fieldsOfType typeName).Merge executionPlan
        | FragmentSpread spread ->
            // A fragment already found is not spread again
            if tryVisitSpread scope spread.Name (deferredFragmentDirective spread.Directives).IsSome then
                match state.TryFindFragment spread.Name with
                | ValueSome fragment ->
                    // Retrieve fragment data just as it was normal selection set
                    let fragmentInfo = planAbstraction state fragment.SelectionSet innerData scope fragment.TypeCondition
                    addFragmentFields innerData spread.Directives (getAbstractionFrag fragmentInfo.Kind)
                | ValueNone -> ()
        | InlineFragment fragment ->
            // Retrieve fragment data just as it was normal selection set
            let fragmentInfo = planAbstraction state fragment.SelectionSet innerData scope fragment.TypeCondition
            addFragmentFields innerData fragment.Directives (getAbstractionFrag fragmentInfo.Kind)
    // Always return ResolveAbstraction kind, even for empty maps.
    // An empty map is a valid state representing "no fields selected for this type condition."
    let plannedTypeFields =
        typeFields
        |> Seq.map (fun (KeyValue (typeName, fields)) -> typeName, withoutDirectlySelectedFields (fields.ToList ()))
        |> Map.ofSeq
    { info with Kind = ResolveAbstraction plannedTypeFields }

let private planVariables (schema: ISchema) (operation: OperationDefinition) =
    operation.VariableDefinitions
    |> List.map (fun vdef ->
        let vname = vdef.VariableName
        match Values.tryConvertAst schema vdef.Type with
        | ValueNone ->
            Debug.Fail "Must be prevented by validation"
            raise (MalformedGQLQueryException $"GraphQL query defined variable '$%s{vname}' of type '%s{vdef.Type.ToString()}' which is not known in the current schema")
        | ValueSome (:? InputDef as idef) ->
            { VarDef.Name = vname; TypeDef = idef; DefaultValue = vdef.DefaultValue }
        | ValueSome tdef ->
            Debug.Fail "Must be prevented by validation"
            raise (MalformedGQLQueryException $"GraphQL query defined variable '$%s{vname}' of type '%s{tdef.ToString()}' which is not an input type definition"))

let internal planOperation (ctx: PlanningContext) : ExecutionPlan =
    // Create artificial plan info to start with
    let rootInfo = {
        Identifier = null
        Kind = Unchecked.defaultof<ExecutionInfoKind>
        Ast = Unchecked.defaultof<Field>
        ParentDef = ctx.RootDef
        ReturnDef = ctx.RootDef
        Definition = Unchecked.defaultof<FieldDef>
        Include = incl
        IsNullable = false }
    let resolvedInfo = planSelection (PlanState ctx) ctx.Operation.SelectionSet rootInfo (newScope ())
    let fields =
        match resolvedInfo.Kind with
        | SelectFields tf -> tf
        | kind -> raise (InvalidOperationException $"Expected the operation root to be planned as {nameof SelectFields}, but it was planned as {kindName kind}")
    let variables = planVariables ctx.Schema ctx.Operation
    match ctx.Operation.OperationType with
    | Query ->
        { DocumentId = ctx.DocumentId
          Operation = ctx.Operation
          RootDef = ctx.Schema.Query
          Fields = fields
          Variables = variables
          Strategy = Parallel
          Metadata = ctx.Metadata }
    | Mutation ->
        match ctx.Schema.Mutation with
        | ValueSome mutationDef ->
            { DocumentId = ctx.DocumentId
              Operation = ctx.Operation
              RootDef = mutationDef
              Fields = fields
              Variables = variables
              Strategy = Sequential
              Metadata = ctx.Metadata }
        | ValueNone ->
            Debug.Fail "Must be prevented by validation"
            raise (
                MalformedGQLQueryException
                    "Operation to be executed is of type mutation, but no mutation root object was defined in current schema"
            )
    | Subscription ->
        match ctx.Schema.Subscription with
        | ValueSome subscriptionDef ->
            { DocumentId = ctx.DocumentId
              Operation = ctx.Operation
              RootDef = subscriptionDef
              Fields = fields
              Variables = variables
              Strategy = Sequential
              Metadata = ctx.Metadata }
        | ValueNone ->
            Debug.Fail "Must be prevented by validation"
            raise (
                MalformedGQLQueryException
                    "Operation to be executed is of type subscription, but no subscription root object was defined in the current schema"
            )
