namespace FSharp.Data.GraphQL.Server.Middlewares

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types

type ComplexityThresholdMiddleware<'Root>() =
    let rec isSafeQuery (threshold : float) (acc : float) (fields : ExecutionInfo list) =
        match fields with
        | [] -> true
        | x :: xs ->
            let current =
                match x.Definition.Metadata.TryFind<float>(Constants.MetadataKeys.weight) with
                | Some weight -> acc + weight
                | None -> acc
            if current > threshold then false
            else match x.Kind with
                 | SelectFields fields ->
                    if isSafeQuery threshold current fields then isSafeQuery threshold current xs else false
                 | ResolveCollection field ->
                    if isSafeQuery threshold current [ field ] then isSafeQuery threshold current xs else false
                 | ResolveAbstraction typeFields ->
                    let fields = typeFields |> Map.toList |> List.collect (fun (_, v) -> v)
                    if isSafeQuery threshold current fields then isSafeQuery threshold current xs else false
                 | _ -> true
    interface IExecutionMiddleware<'Root> with
        member __.ExecuteAsync = fun args next ->
            let (plan, _, _) = args
            let threshold = plan.Metadata.TryFind<float>(Constants.MetadataKeys.weightThreshold)
            match threshold with
            | Some threshold -> if isSafeQuery threshold 0.0 plan.Fields then next args else failwith "Threshold exceeded limit."
            | None -> next args