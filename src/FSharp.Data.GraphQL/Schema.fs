/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Validation
open FSharp.Data.GraphQL.Introspection

type Schema(query: TypeDef, ?mutation: TypeDef, ?types:TypeDef list, ?directives: DirectiveDef list) =
    let rec insert ns typedef =
        let inline addOrReturn name typedef' ns' =
            if Map.containsKey name ns' 
            then ns' 
            else Map.add name typedef' ns'

        match typedef with
        | Scalar scalardef -> addOrReturn scalardef.Name typedef ns
        | Enum enumdef -> addOrReturn enumdef.Name typedef ns
        | Object objdef -> 
            let ns' = addOrReturn typedef.Name typedef ns
            let withFields' =
                objdef.Fields
                |> List.map (fun x -> x.Type)
                |> List.filter (fun x -> not (Map.containsKey x.Name ns'))
                |> List.fold (fun n t -> insert n t) ns'
            objdef.Implements
            |> List.fold (fun n t -> insert n t) withFields'
        | Interface interfacedef ->
            let ns' = 
                interfacedef.Fields
                |> List.map (fun x -> x.Type)
                |> List.filter (fun x -> not (Map.containsKey x.Name ns))
                |> List.fold (fun n t -> insert n t) ns
            addOrReturn typedef.Name typedef ns' 
        | Union uniondef ->
            let ns' =
                uniondef.Options
                |> List.fold (fun n t -> insert n t) ns
            addOrReturn typedef.Name typedef ns' 
        | ListOf innerdef -> insert ns innerdef 
        | NonNull innerdef -> insert ns innerdef
        | InputObject innerdef -> insert ns (Object innerdef)
        
    let initialTypes = [ 
        Int
        String
        Boolean
        Float
        ID
        __Schema
        query] 

    let mutable types: Map<string, TypeDef> = 
        initialTypes @ (Option.toList mutation)
        |> List.fold insert Map.empty

    let implementations =
        types
        |> Map.toSeq
        |> Seq.choose (fun (_, v) ->
            match v with
            | Object odef -> Some odef
            | _ -> None)
        |> Seq.fold (fun acc objdef -> 
            objdef.Implements
            |> List.fold (fun acc' iface ->
                match Map.tryFind iface.Name acc' with
                | Some list -> Map.add iface.Name (iface::list) acc'
                | None -> Map.add iface.Name [iface] acc') acc
            ) Map.empty

    interface ISchema with
        member val TypeMap = types
        member val Query = query
        member val Mutation = mutation
        member val Directives = match directives with None -> [IncludeDirective; SkipDirective] | Some d -> d
        member x.TryFindType typeName = Map.tryFind typeName types
        member x.GetPossibleTypes typedef = 
            match typedef with
            | Union u -> u.Options
            | Interface i -> Map.find i.Name implementations
            | _ -> []

    interface System.Collections.Generic.IEnumerable<TypeDef> with
        member x.GetEnumerator() = (types |> Map.toSeq |> Seq.map snd).GetEnumerator()

    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = (types |> Map.toSeq |> Seq.map snd :> System.Collections.IEnumerable).GetEnumerator()

    