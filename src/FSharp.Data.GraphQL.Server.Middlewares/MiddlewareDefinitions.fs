namespace FSharp.Data.GraphQL.Server.Middlewares

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open Literals
open FSharp.Data.GraphQL.Execution

module internal MiddlewareDefinitions =
    let objectListFilter<'ObjectType, 'ListType> (ctx : SchemaCompileContext) (next : SchemaCompileContext -> unit) =
        let modifyFields (object : ObjectDef<'ObjectType>) (fields : FieldDef<'ObjectType> seq) =
            let args = [ Define.Input(ArgumentKeys.ObjectListFilterMiddleware.Filter, ObjectListFilter) ]
            let fields = fields |> Seq.map (fun x -> x.WithArgs(args)) |> List.ofSeq
            object.WithFields(fields)
        let typesWithListFields =
            ctx.Schema.TypeMap.GetTypesWithListFields<'ObjectType, 'ListType>()
        let modifiedTypes =
            typesWithListFields 
            |> Seq.map (fun (object, fields) -> modifyFields object fields)
            |> Seq.cast<NamedDef>
        ctx.Schema.TypeMap.AddOrOverwriteTypes(modifiedTypes, overwrite = true)
        next ctx

    let queryWeight (threshold : float option) (ctx : ExecutionContext) (next : ExecutionContext -> AsyncVal<GQLResponse>) =
        let measureThreshold (threshold : float) (fields : ExecutionInfo list) =
            let getWeight f =
                match f.Definition.Metadata.TryFind<float>(MetadataKeys.QueryWeightMiddleware.QueryWeight) with
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
        let metadataThreshold =
            ctx.Metadata.TryFind<float>(MetadataKeys.QueryWeightMiddleware.QueryWeightThreshold)
        let threshold = 
            match threshold with
            | Some t -> Some t
            | None -> metadataThreshold
        let ctx =
            match threshold with
            | Some t -> { ctx with Metadata = ctx.Metadata.WithQueryWeightThreshold(t) }
            | None -> ctx
        let deferredFields = ctx.ExecutionPlan.DeferredFields |> List.map (fun f -> f.Info)
        let directFields = ctx.ExecutionPlan.Fields
        let fields = directFields @ deferredFields
        let error = fun _ -> 
            GQLResponse.ErrorAsync("Query complexity exceeds maximum threshold. Please reduce query complexity and try again.", ctx.Metadata)
        match threshold with
        | Some threshold ->
            let pass = measureThreshold threshold fields |> fst
            if pass
            then next ctx
            else error ctx
        | None -> next ctx