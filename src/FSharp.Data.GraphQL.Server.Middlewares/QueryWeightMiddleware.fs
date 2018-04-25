namespace FSharp.Data.GraphQL.Server.Middlewares

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Execution

type QueryWeightMiddleware() =
    let measureThreshold (threshold : float) (fields : ExecutionInfo list) =
        let getWeight f =
            match f.Definition.Metadata.TryFind<float>(Constants.MetadataKeys.queryWeight) with
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
    let middleware (ctx : ExecutionContext) (next : ExecutionContext -> AsyncVal<GQLResponse>) =
            let threshold = ctx.Metadata.TryFind<float>(Constants.MetadataKeys.queryWeightThreshold)
            let deferredFields = ctx.ExecutionPlan.DeferredFields |> List.map (fun f -> f.Info)
            let directFields = ctx.ExecutionPlan.Fields
            let fields = directFields @ deferredFields
            let error msg = GQLResponse.directErrorAsync msg [ "" ]
            match threshold with
            | Some threshold ->
                let pass = measureThreshold threshold fields |> fst
                if pass
                then next ctx
                else (fun _ -> error "Query complexity exceeds maximum threshold. Please reduce query complexity and try again.") ctx
            | None -> next ctx
    interface IOperationExecutionMiddleware with
        member __.ExecuteOperationAsync = middleware
    interface IExecutorMiddleware with
        member __.ExecuteOperationAsync = Some <|
            { new IOperationExecutionMiddleware with 
                member __.ExecuteOperationAsync = middleware }
        member __.PlanOperation = None
        member __.CompileSchema = None