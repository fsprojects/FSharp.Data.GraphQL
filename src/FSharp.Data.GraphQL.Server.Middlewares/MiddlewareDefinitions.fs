namespace FSharp.Data.GraphQL.Server.Middlewares

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types.Patterns
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Execution

type internal QueryWeightMiddleware(threshold : float, reportToMetadata : bool) =
    let middleware (threshold : float) (ctx : ExecutionContext) (next : ExecutionContext -> AsyncVal<GQLResponse>) =
        let measureThreshold (threshold : float) (fields : ExecutionInfo list) =
            let getWeight f =
                if f.ParentDef = upcast ctx.ExecutionPlan.RootDef 
                then 0.0
                else
                    match f.Definition.Metadata.TryFind<float>("queryWeight") with
                    | Some w -> w
                    | None -> 0.0
            let rec checkThreshold acc fields =
                match fields with
                | [] -> (true, acc)
                | x :: xs ->
                    let current = acc + (getWeight x)
                    if current > threshold then (false, current)
                    else match x.Kind with
                         | SelectFields fields ->
                            let (pass, current) = checkThreshold current fields
                            if pass then checkThreshold current xs else (false, current)
                         | ResolveCollection field ->
                            let (pass, current) = checkThreshold acc [ field ]
                            if pass then checkThreshold current xs else (false, current)
                         | ResolveAbstraction typeFields ->
                            let fields = typeFields |> Map.toList |> List.collect (fun (_, v) -> v)
                            let (pass, current) = checkThreshold current fields
                            if pass then checkThreshold current xs else (false, current)
                         | _ ->
                            checkThreshold current xs
            checkThreshold 0.0 fields
        // let directFields = ctx.ExecutionPlan.Fields
        // let fields = directFields
        let error (ctx : ExecutionContext) =
            GQLResponse.ErrorAsync("Query complexity exceeds maximum threshold. Please reduce query complexity and try again.", ctx.Metadata)
        let (pass, totalWeight) = measureThreshold threshold ctx.ExecutionPlan.Fields
        let ctx =
            match reportToMetadata with
            | true -> { ctx with Metadata = ctx.Metadata.Add("queryWeightThreshold", threshold).Add("queryWeight", totalWeight) }
            | false -> ctx
        if pass
        then next ctx
        else error ctx
    interface IExecutorMiddleware with
        member __.CompileSchema = None
        member __.PostCompileSchema = None
        member __.PlanOperation = None
        member __.ExecuteOperationAsync = Some (middleware threshold)

type internal ObjectListFilterMiddleware<'ObjectType, 'ListType>(reportToMetadata : bool) =
    let compileMiddleware (ctx : SchemaCompileContext) (next : SchemaCompileContext -> unit) =
        let modifyFields (object : ObjectDef<'ObjectType>) (fields : FieldDef<'ObjectType> seq) =
            let args = [ Define.Input("filter", Nullable ObjectListFilter) ]
            let fields = fields |> Seq.map (fun x -> x.WithArgs(args)) |> List.ofSeq
            object.WithFields(fields)
        let typesWithListFields =
            ctx.TypeMap.GetTypesWithListFields<'ObjectType, 'ListType>()
        if Seq.isEmpty typesWithListFields
        then failwith <| sprintf "No lists with specified type '%A' where found on object of type '%A'." typeof<'ObjectType> typeof<'ListType>
        let modifiedTypes =
            typesWithListFields
            |> Seq.map (fun (object, fields) -> modifyFields object fields)
            |> Seq.cast<NamedDef>
        ctx.TypeMap.AddTypes(modifiedTypes, overwrite = true)
        next ctx
    let reportMiddleware (ctx : ExecutionContext) (next : ExecutionContext -> AsyncVal<GQLResponse>) =
        let rec collectArgs (acc : (string * ObjectListFilter) list) (fields : ExecutionInfo list) =
            let fieldArgs field =
                field.Ast.Arguments
                |> Seq.map (fun x ->
                    match x.Name with
                    | "filter" -> ObjectListFilter.CoerceInput x.Value
                    | _ -> None)
                |> Seq.choose id
                |> Seq.map (fun x -> field.Ast.AliasOrName, x)
                |> List.ofSeq
            match fields with
            | [] -> acc
            | x :: xs ->
                match x.Kind with
                | SelectFields fields ->
                    let acc = collectArgs acc fields
                    collectArgs acc xs
                | ResolveCollection field ->
                    let acc = fieldArgs field
                    collectArgs acc xs
                | ResolveAbstraction typeFields ->
                    let fields = typeFields |> Map.toList |> List.collect (fun (_, v) -> v)
                    let acc = collectArgs acc fields
                    collectArgs acc xs
                | _ -> collectArgs acc xs
        let ctx =
            match reportToMetadata with
            | true -> 
                { ctx with Metadata = ctx.Metadata.Add("filters", collectArgs [] ctx.ExecutionPlan.Fields) }
            | false -> ctx
        next ctx
    interface IExecutorMiddleware with
        member __.CompileSchema = Some compileMiddleware
        member __.PostCompileSchema = None
        member __.PlanOperation = None
        member __.ExecuteOperationAsync = Some reportMiddleware

/// A function that resolves an identity name for a schema object, based on a object definition of it.
type IdentityNameResolver = ObjectDef -> string

type internal LiveQueryMiddleware(identityNameResolver : IdentityNameResolver) =
    let middleware (ctx : SchemaCompileContext) (next : SchemaCompileContext -> unit) =
        let identity (identityName : string) (x : obj) =
            x.GetType().GetProperty(identityName).GetValue(x)
        let makeSubscription id typeName fieldName : LiveFieldSubscription =
            { Identity = identity id; TypeName = typeName; FieldName = fieldName }
        let getObjDefs (def : FieldDef) =
            let rec helper (acc : ObjectDef list) (def : TypeDef) =
                match def with
                | Object objdef ->
                    if not (acc |> List.exists (fun x -> x.Name = objdef.Name))
                    then helper (objdef :: acc) objdef
                    else acc
                | Nullable innerdef -> helper acc innerdef
                | List innerdef -> helper acc innerdef
                | Union udef -> (udef.Options |> List.ofArray) @ acc
                | _ -> []
            helper [] def.TypeDef
        ctx.Schema.Query.Fields
        |> Map.toSeq
        |> Seq.collect (snd >> getObjDefs)
        |> Seq.map (fun objdef -> identityNameResolver objdef, objdef)
        |> Seq.filter (fun (id, objdef) -> not (isNull (objdef.Type.GetProperty(id))))
        |> Seq.collect (fun (id, objdef) ->
            objdef.Fields
            |> Map.toSeq
            |> Seq.map (snd >> (fun fdef -> makeSubscription id objdef.Name fdef.Name)))
        |> Seq.iter (fun x ->
            if not (ctx.Schema.LiveFieldSubscriptionProvider.IsRegistered x.TypeName x.FieldName)
            then ctx.Schema.LiveFieldSubscriptionProvider.Register x)
        next ctx
    interface IExecutorMiddleware with
        member __.CompileSchema = Some middleware
        member __.PostCompileSchema = None
        member __.PlanOperation = None
        member __.ExecuteOperationAsync = None
