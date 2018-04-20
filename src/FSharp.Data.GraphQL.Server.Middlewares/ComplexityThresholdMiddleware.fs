namespace FSharp.Data.GraphQL.Server.Middlewares

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types

type ComplexityThresholdMiddleware<'Root>() =
    let rec isSafeQuery (threshold : float) (acc : float) (fields : ExecutionInfo list) =
        match fields with
        | [] -> (true, acc)
        | x :: xs ->
            let current =
                match x.Definition.Metadata.TryFind<float>(Constants.MetadataKeys.weight) with
                | Some weight -> acc + weight
                | None -> acc
            printfn "%f" current
            if current > threshold then (false, current)
            else match x.Kind with
                 | SelectFields fields ->
                    let (isSafe, current) = isSafeQuery threshold current fields
                    if isSafe then isSafeQuery threshold current xs else (false, current)
                 | ResolveCollection field ->
                    let (isSafe, current) = isSafeQuery threshold acc [ field ] 
                    if isSafe then isSafeQuery threshold current xs else (false, current)
                 | ResolveAbstraction typeFields ->
                    let fields = typeFields |> Map.toList |> List.collect (fun (_, v) -> v)
                    let (isSafe, current) = isSafeQuery threshold current fields
                    if isSafe then isSafeQuery threshold current xs else (false, current)
                 | _ -> isSafeQuery threshold current xs
    interface IExecutionMiddleware<'Root> with
        member __.ExecuteAsync = fun args next ->
            let (plan, _, _) = args
            let threshold = plan.Metadata.TryFind<float>(Constants.MetadataKeys.weightThreshold)
            match threshold with
            | Some threshold -> 
                let (isSafe, _) = isSafeQuery threshold 0.0 plan.Fields
                if isSafe then next args else failwith "Threshold exceeded limit."
            | None -> next args