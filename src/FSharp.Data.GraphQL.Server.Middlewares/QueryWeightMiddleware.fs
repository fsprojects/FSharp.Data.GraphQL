namespace FSharp.Data.GraphQL.Server.Middlewares

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types

type QueryWeightMiddleware<'Root>(threshold : float) =
    let measureThreshold (threshold : float) (fields : ExecutionInfo list) =
        let getWeight f =
            match f.Definition.Metadata.TryFind<float>(Constants.MetadataKeys.weight) with
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
        member __.ExecuteAsync = fun args next ->
            let (plan, _, _) = args
            let deferredFields = plan.DeferredFields |> List.map (fun x -> x.Info)
            let fields = plan.Fields @ deferredFields
            let error msg = ExecutionFunc.error msg [ "" ] args
            let (pass, _) = measureThreshold threshold fields
            if pass
            then next args
            else error "Query complexity exceeds maximum threshold. Please reduce the amount of queried data and try again."