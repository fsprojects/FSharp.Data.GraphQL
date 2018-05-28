namespace FSharp.Data.GraphQL.Server.Middlewares

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Execution

type internal QueryWeightMiddleware(threshold : float, reportToMetadata : bool) =
    let middleware (threshold : float) (ctx : ExecutionContext) (next : ExecutionContext -> AsyncVal<GQLResponse>) =
        let measureThreshold (threshold : float) (fields : ExecutionInfo list) =
            let getWeight f =
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
        let deferredFields = ctx.ExecutionPlan.DeferredFields |> List.map (fun f -> f.Info)
        let directFields = ctx.ExecutionPlan.Fields
        let fields = directFields @ deferredFields
        let error (ctx : ExecutionContext) =
            GQLResponse.ErrorAsync("Query complexity exceeds maximum threshold. Please reduce query complexity and try again.", ctx.Metadata)
        let (pass, totalWeight) = measureThreshold threshold fields
        let ctx =
            match reportToMetadata with
            | true -> { ctx with Metadata = ctx.Metadata.WithQueryWeightThreshold(threshold).Add("queryWeight", totalWeight) }
            | false -> ctx
        if pass
        then next ctx
        else error ctx
    interface IExecutorMiddleware with
        member __.CompileSchema = None
        member __.PlanOperation = None
        member __.ExecuteOperationAsync = Some (middleware threshold)

type internal ObjectListFilterMiddleware<'ObjectType, 'ListType>(reportToMetadata : bool) =
    let compileMiddleware (ctx : SchemaCompileContext) (next : SchemaCompileContext -> unit) =
        let modifyFields (object : ObjectDef<'ObjectType>) (fields : FieldDef<'ObjectType> seq) =
            let args = [ Define.Input("filter", ObjectListFilter) ]
            let fields = fields |> Seq.map (fun x -> x.WithArgs(args)) |> List.ofSeq
            object.WithFields(fields)
        let typesWithListFields =
            ctx.Schema.TypeMap.GetTypesWithListFields<'ObjectType, 'ListType>()
        if Seq.isEmpty typesWithListFields 
        then failwith <| sprintf "No lists with specified type '%A' where found on object of type '%A'." typeof<'ObjectType> typeof<'ListType>
        let modifiedTypes =
            typesWithListFields 
            |> Seq.map (fun (object, fields) -> modifyFields object fields)
            |> Seq.cast<NamedDef>
        ctx.Schema.TypeMap.AddTypes(modifiedTypes, overwrite = true)
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
            | true -> { ctx with Metadata = ctx.Metadata.Add("filters", collectArgs [] ctx.ExecutionPlan.Fields) }
            | false -> ctx
        next ctx
    interface IExecutorMiddleware with
        member __.CompileSchema = Some compileMiddleware
        member __.PlanOperation = None
        member __.ExecuteOperationAsync = Some reportMiddleware