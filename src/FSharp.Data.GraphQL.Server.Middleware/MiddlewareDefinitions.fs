namespace FSharp.Data.GraphQL.Server.Middleware

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
        // The filters of the list fields under the fields, each with the path of its field from them, and each path and
        // filter once; the filters of the fields of a list field's items are not collected.
        //
        // A plan shares the plans of the selection sets that are planned the same way, so walking it field by field
        // would visit a shared plan once for every path to it: exponentially often with the nesting of abstract fields.
        // The filters under each plan are therefore collected once, with paths from the field holding the plan.
        let filtersByKind =
            Dictionary<ExecutionInfoKind, Result<struct (obj list * ObjectListFilter) list, IGQLError list>> (HashIdentity.Reference)
        let rec collectFilters (fields : ExecutionInfo list) =
            let filters = ResizeArray<struct (obj list * ObjectListFilter)> ()
            let found = HashSet<struct (obj list * ObjectListFilter)> (HashIdentity.Structural)
            let add (path : obj list) (filter : ObjectListFilter) =
                let entry = struct (path, filter)
                if found.Add entry then filters.Add entry
            let rec collect (fields : ExecutionInfo list) =
                match fields with
                | [] -> Ok ()
                | field :: rest ->
                    let name = box field.Ast.AliasOrName
                    let collected =
                        match field.Kind with
                        | SelectFields _
                        | ResolveAbstraction _ ->
                            collectKindFilters field.Kind
                            |> Result.map (List.iter (fun struct (path, filter) -> add (name :: path) filter))
                        | ResolveCollection item -> fieldFilters item |> Result.map (List.iter (add [ name ]))
                        | _ -> Ok ()
                    match collected with
                    | Error errs -> Error errs
                    | Ok () -> collect rest
            collect fields |> Result.map (fun () -> List.ofSeq filters)
        and collectKindFilters (kind : ExecutionInfoKind) =
            match filtersByKind.TryGetValue kind with
            | true, collected -> collected
            | false, _ ->
                let collected =
                    match kind with
                    | SelectFields fields -> collectFilters fields
                    | ResolveAbstraction typeFields -> typeFields |> Map.toList |> List.collect snd |> collectFilters
                    | _ -> Ok []
                filtersByKind.Add (kind, collected)
                collected
        let ctxResult = result {
            let! filters = collectFilters ctx.ExecutionPlan.Fields
            let args = filters |> List.map (fun struct (path, filter) -> KeyValuePair (path, filter))

            match reportToMetadata with
            | true ->
                let filters = ImmutableDictionary.CreateRange args
                return { ctx with Metadata = ctx.Metadata.Add ("filters", filters) }
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
