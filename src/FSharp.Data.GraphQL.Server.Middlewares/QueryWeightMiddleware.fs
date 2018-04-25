namespace FSharp.Data.GraphQL.Server.Middlewares

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types

type QueryWeightMiddleware<'Root>() =
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
    interface IExecutionMiddleware<'Root> with
        member __.ExecuteAsync = fun (plan, data, variables, args) next ->
            let threshold = args.TryFind<float>(Constants.MetadataKeys.queryWeightThreshold)
            let deferredFields = plan.DeferredFields |> List.map (fun x -> x.Info)
            let fields = plan.Fields @ deferredFields
            let error msg = ExecutionFunc.error msg [ "" ] (plan, data, variables, args)
            match threshold with
            | Some threshold -> 
                let pass = measureThreshold threshold fields |> fst
                if pass
                then next (plan, data, variables, args)
                else error "Query complexity exceeds maximum threshold. Please reduce the amount of queried data and try again."
            | None -> next (plan, data, variables, args)