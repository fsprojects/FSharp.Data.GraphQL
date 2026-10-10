namespace FSharp.Data.GraphQL.Server.Middleware

open System
open System.Collections.Generic
open System.Collections.Immutable
open FSharp.Data.GraphQL.Shared
open FsToolkit.ErrorHandling

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types.Patterns
open FSharp.Data.GraphQL.Types

type internal QueryWeightMiddleware (threshold : float, reportToMetadata : bool) =

    let middleware
        (threshold : float)
        (inputContext : InputExecutionContextProvider)
        (ctx : ExecutionContext)
        (next : ExecutionContext -> AsyncVal<GQLExecutionResult>)
        =
        let measureThreshold (threshold : float) (fields : ExecutionInfo list) =
            let getWeight f =
                if f.ParentDef = upcast ctx.ExecutionPlan.RootDef then
                    0.0
                else
                    match f.Definition.Metadata.TryFind<float>("queryWeight") with
                    | ValueSome w -> w
                    | ValueNone -> 0.0
            // The weights are added field by field in selection order, the weights of every possible type of an
            // abstract field included, and the plan is rejected as soon as the sum exceeds the threshold.
            //
            // A plan shares the plans of the selection sets that are planned the same way, so walking it field by field
            // would visit a shared plan once for every path to it: exponentially often with the nesting of abstract
            // fields. Each plan is therefore measured once, as the weight it adds and the highest the sum gets while
            // adding it, both from the sum before it.
            let measures = Dictionary<ExecutionInfoKind, struct (float * float)> (HashIdentity.Reference)
            let rec measureFields (fields : ExecutionInfo list) =
                let mutable total = 0.0
                let mutable peak = -infinity
                for field in fields do
                    let struct (fieldTotal, fieldPeak) = measureField field
                    peak <- max peak (total + fieldPeak)
                    total <- total + fieldTotal
                struct (total, peak)
            and measureField (field : ExecutionInfo) =
                let weight = getWeight field
                match field.Kind with
                // The weight of a list field is checked, but its item is measured from the sum before the field
                | ResolveCollection item ->
                    let struct (total, peak) = measureField item
                    struct (total, max weight peak)
                | kind ->
                    let struct (total, peak) = measureKind kind
                    struct (weight + total, max weight (weight + peak))
            and measureKind (kind : ExecutionInfoKind) =
                match measures.TryGetValue kind with
                | true, measure -> measure
                | false, _ ->
                    let measure =
                        match kind with
                        | ResolveValue -> struct (0.0, -infinity)
                        | SelectFields fields
                        | ResolveDeferredFragment (_, _, _, fields) -> measureFields fields
                        | ResolveAbstraction typeFields -> typeFields |> Map.toList |> List.collect snd |> measureFields
                        | ResolveCollection item
                        | ResolveDeferred item
                        | ResolveStreamed (item, _)
                        | ResolveLive item -> measureField item
                    measures.Add (kind, measure)
                    measure
            // The sum at the field where it first exceeds the threshold, found by walking into the fields whose peak exceeds it
            let rec exceedingWeight (sum : float) (fields : ExecutionInfo list) =
                match fields with
                | [] -> ValueNone
                | field :: rest ->
                    let struct (total, peak) = measureField field
                    if sum + peak <= threshold then
                        exceedingWeight (sum + total) rest
                    else
                        let current = sum + getWeight field
                        if current > threshold then
                            ValueSome current
                        else
                            match field.Kind with
                            | ResolveValue -> ValueSome current
                            | ResolveCollection item -> exceedingWeight sum [ item ]
                            | SelectFields fields
                            | ResolveDeferredFragment (_, _, _, fields) -> exceedingWeight current fields
                            | ResolveAbstraction typeFields -> exceedingWeight current (typeFields |> Map.toList |> List.collect snd)
                            | ResolveDeferred item
                            | ResolveStreamed (item, _)
                            | ResolveLive item -> exceedingWeight current [ item ]
            let struct (total, peak) = measureFields fields
            if peak <= threshold then
                (true, total)
            else
                (false, exceedingWeight 0.0 fields |> ValueOption.defaultValue peak)
        let error (ctx : ExecutionContext) =
            GQLExecutionResult.ErrorAsync (
                ctx.ExecutionPlan.DocumentId,
                "Query complexity exceeds maximum threshold. Please reduce query complexity and try again.",
                ctx.Metadata
            )
        let (pass, totalWeight) = measureThreshold threshold ctx.ExecutionPlan.Fields
        let ctx =
            match reportToMetadata with
            | true -> {
                ctx with
                    Metadata = ctx.Metadata.Add("queryWeightThreshold", threshold).Add("queryWeight", totalWeight)
              }
            | false -> ctx
        if pass then next ctx else error ctx

    interface IExecutorMiddleware with
        member _.CompileSchema = ValueNone
        member _.PostCompileSchema = ValueNone
        member _.PlanOperation = ValueNone
        member _.ExecuteOperationAsync = ValueSome (middleware threshold)

/// <summary>
/// The path of a field from the root of an operation, by response names, with the plans walked under it.
/// </summary>
/// <remarks>
/// A path has a single instance, which its parent hands out, so that equal paths are compared in constant time however
/// deep they are.
/// </remarks>
[<Sealed; AllowNullLiteral>]
type internal SelectionPath private (parent : SelectionPath, name : string) =
    let mutable children : Dictionary<string, SelectionPath> = null
    let mutable walkedPlans : HashSet<ExecutionInfoKind> = null

    /// The path of the root of an operation, which no field has
    static member Root () = SelectionPath (null, null)

    member _.Parent = parent

    member _.Name = name

    /// The path of the field with the response name under the field of this path, the same instance every time
    member this.Child (name : string) =
        if isNull children then
            children <- Dictionary<string, SelectionPath> (StringComparer.Ordinal)
        match children.TryGetValue name with
        | true, child -> child
        | false, _ ->
            let child = SelectionPath (this, name)
            children.Add (name, child)
            child

    /// Whether the plan is walked under this path for the first time
    member _.FirstWalkOf (kind : ExecutionInfoKind) =
        if isNull walkedPlans then
            walkedPlans <- HashSet<ExecutionInfoKind> (HashIdentity.Reference)
        walkedPlans.Add kind

    /// The response names of the fields of the path from the root of the operation
    member this.ToList () : obj list =
        let mutable names = []
        let mutable path = this
        while not (isNull path.Parent) do
            names <- box path.Name :: names
            path <- path.Parent
        names

type internal ObjectListFilterMiddleware<'ObjectType, 'ListType> (reportToMetadata : bool) =

    let compileMiddleware (ctx : SchemaCompileContext) (next : SchemaCompileContext -> unit) =
        let modifyFields (object : ObjectDef<'ObjectType>) (fields : FieldDef<'ObjectType> seq) =
            let args = [ Define.Input ("filter", Nullable ObjectListFilterType) ]
            let fields = fields |> Seq.map _.WithArgs(args) |> Seq.toList
            object.WithFields (fields)
        let typesWithListFields = ctx.TypeMap.GetTypesWithListFields<'ObjectType, 'ListType>()
        if Seq.isEmpty typesWithListFields then
            failwith $"No lists with specified type '{typeof<'ObjectType>}' where found on object of type '{typeof<'ListType>}'."
        let modifiedTypes =
            typesWithListFields
            |> Seq.map (fun (object, fields) -> modifyFields object fields)
            |> Seq.cast<NamedDef>
        ctx.TypeMap.AddTypes (modifiedTypes, overwrite = true)
        next ctx

    let reportMiddleware
        (inputContext : InputExecutionContextProvider)
        (ctx : ExecutionContext)
        (next : ExecutionContext -> AsyncVal<GQLExecutionResult>)
        =
        // The filters of the filter argument of a list field
        let fieldFilters (field : ExecutionInfo) =
            let filterResults =
                field.Ast.Arguments
                |> Seq.map (fun x ->
                    match x.Name, x.Value with
                    | "filter", (VariableName variableName) -> Ok (ValueSome (ctx.Variables[variableName] :?> ObjectListFilter))
                    | "filter", inlineConstant ->
                        ObjectListFilterType.CoerceInput inputContext (InlineConstant inlineConstant) ctx.Variables
                        |> Result.map ValueOption.ofObj
                    | _ -> Ok ValueNone)
                |> Seq.toList
            filterResults
            |> splitSeqErrorsList
            |> Result.map (Seq.vchoose id >> Seq.toList)
        // The filters of the list fields of the operation by path; the filters of the fields of a list field's items are
        // not collected, and a path filtered differently under different types of an abstract field keeps the last
        // filter.
        //
        // A plan shares the plans of the selection sets that are planned the same way, so walking it field by field
        // would walk a shared plan once for every path through the plans to it: exponentially often with the nesting of
        // abstract fields. A shared plan is therefore walked once per path of response names, which the plans sharing it
        // have in common.
        let filters = Dictionary<SelectionPath, ObjectListFilter> (HashIdentity.Reference)
        // The fields selected under a field, those of every possible type of an abstract field included
        let selectedFields (kind : ExecutionInfoKind) =
            match kind with
            | SelectFields fields -> fields
            | ResolveAbstraction typeFields -> typeFields |> Map.toList |> List.collect snd
            | _ -> []
        let rec collectFilters (parent : SelectionPath) (fields : ExecutionInfo list) =
            let mutable errors = ValueNone
            let mutable remaining = fields
            while errors.IsNone && not remaining.IsEmpty do
                let field = remaining.Head
                remaining <- remaining.Tail
                // Only the fields with these plans have an AST: the deferred fragments at the root have none
                let collected =
                    match field.Kind with
                    | SelectFields _
                    | ResolveAbstraction _ ->
                        let path = parent.Child field.Ast.AliasOrName
                        if path.FirstWalkOf field.Kind then collectFilters path (selectedFields field.Kind) else Ok ()
                    | ResolveCollection item ->
                        let path = parent.Child field.Ast.AliasOrName
                        fieldFilters item
                        |> Result.map (List.iter (fun filter -> filters[path] <- filter))
                    | _ -> Ok ()
                match collected with
                | Error errs -> errors <- ValueSome errs
                | Ok () -> ()
            match errors with
            | ValueSome errs -> Error errs
            | ValueNone -> Ok ()
        let filtersByPath () =
            let builder = ImmutableDictionary.CreateBuilder<obj list, ObjectListFilter> ()
            for KeyValue (path, filter) in filters do
                builder.Add (path.ToList (), filter)
            builder.ToImmutable ()
        let ctxResult = result {
            do! collectFilters (SelectionPath.Root ()) ctx.ExecutionPlan.Fields

            match reportToMetadata with
            | true -> return { ctx with Metadata = ctx.Metadata.Add ("filters", filtersByPath ()) }
            | false -> return ctx
        }
        match ctxResult with
        | Ok ctx -> next ctx
        | Error errs -> asyncVal {
            return GQLExecutionResult.RequestError (ctx.ExecutionPlan.DocumentId, (errs |> List.map GQLProblemDetails.OfError), ctx.Metadata)
          }
    interface IExecutorMiddleware with
        member _.CompileSchema = ValueSome compileMiddleware
        member _.PostCompileSchema = ValueNone
        member _.PlanOperation = ValueNone
        member _.ExecuteOperationAsync = ValueSome reportMiddleware

/// A function that resolves an identity name for a schema object, based on a object definition of it.
type IdentityNameResolver = ObjectDef -> string

type internal LiveQueryMiddleware (identityNameResolver : IdentityNameResolver) =

    let middleware (ctx : SchemaCompileContext) (next : SchemaCompileContext -> unit) =
        let identity (identityName : string) (x : obj) = x.GetType().GetProperty(identityName).GetValue(x)
        let project (fieldName : string) (x : obj) = x.GetType().GetProperty(fieldName).GetValue(x)
        let makeSubscription id typeName fieldName : LiveFieldSubscription = {
            Filter = (fun x y -> identity id x = identity id y)
            Project = project fieldName
            TypeName = typeName
            FieldName = fieldName
        }
        let getObjDefs (def : FieldDef) =
            let rec helper (acc : ObjectDef list) (def : TypeDef) =
                match def with
                | Object objdef ->
                    if not (acc |> List.exists (fun x -> x.Name = objdef.Name)) then
                        helper (objdef :: acc) objdef
                    else
                        acc
                | Nullable innerdef -> helper acc innerdef
                | List innerdef -> helper acc innerdef
                | Union udef -> (udef.Options |> List.ofArray) @ acc
                | _ -> []
            helper [] def.TypeDef
        ctx.Schema.Query.Fields
        |> Map.toSeq
        |> Seq.collect (snd >> getObjDefs)
        |> Seq.map (fun objdef -> identityNameResolver objdef, objdef)
        |> Seq.filter (fun (id, objdef) -> not (isNull (objdef.Type.GetProperty (id))))
        |> Seq.collect (fun (id, objdef) ->
            objdef.Fields
            |> Map.toSeq
            |> Seq.map (
                snd
                >> (fun fdef -> makeSubscription id objdef.Name fdef.Name)
            ))
        |> Seq.iter (fun x ->
            if not (ctx.Schema.LiveFieldSubscriptionProvider.IsRegistered x.TypeName x.FieldName) then
                ctx.Schema.LiveFieldSubscriptionProvider.Register x)
        next ctx

    interface IExecutorMiddleware with
        member _.CompileSchema = ValueSome middleware
        member _.PostCompileSchema = ValueNone
        member _.PlanOperation = ValueNone
        member _.ExecuteOperationAsync = ValueNone
