/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Validation
open FSharp.Data.GraphQL.Introspection

type Schema(query: ObjectDef, ?mutation: ObjectDef, ?types: NamedDef list, ?directives: DirectiveDef list) =
    let rec insert ns typedef =
        let inline addOrReturn tname (tdef: NamedDef) acc =
            if Map.containsKey tname acc 
            then acc 
            else Map.add tname tdef acc

        match typedef with
        | Scalar scalardef -> addOrReturn scalardef.Name typedef ns
        | Enum enumdef -> addOrReturn enumdef.Name typedef ns
        | Object objdef -> 
            let ns' = addOrReturn objdef.Name typedef ns
            let withFields' =
                objdef.Fields
                |> List.collect (fun x -> (x.Type :> TypeDef) :: (x.Args |> List.map (fun a -> a.Type)))
                |> List.filter (fun (Named x) -> not (Map.containsKey x.Name ns'))
                |> List.fold (fun n (Named t) -> insert n t) ns'
            objdef.Implements
            |> List.fold (fun n t -> insert n t) withFields'
        | Interface interfacedef ->
            let ns' = addOrReturn typedef.Name typedef ns
            interfacedef.Fields
            |> List.map (fun x -> x.Type)
            |> List.filter (fun (Named x) -> not (Map.containsKey x.Name ns'))
            |> List.fold (fun n (Named t) -> insert n t) ns'            
        | Union uniondef ->
            let ns' = addOrReturn typedef.Name typedef ns
            uniondef.Options
            |> List.fold (fun n t -> insert n t) ns'            
        | List (Named innerdef) -> insert ns innerdef 
        | NonNull (Named innerdef) -> insert ns innerdef
        | InputObject objdef -> 
            let ns' = addOrReturn objdef.Name typedef ns
            objdef.FieldsFn()
            |> List.collect (fun x -> (x.Type :> TypeDef) :: (x.Args |> List.map (fun a -> a.Type)))
            |> List.filter (fun (Named x) -> not (Map.containsKey x.Name ns'))
            |> List.fold (fun n (Named t) -> insert n t) ns'
        
    let initialTypes: NamedDef list = [ 
        Int
        String
        Boolean
        Float
        ID
        __Schema
        query]

    let mutable typeMap: Map<string, NamedDef> = 
        let m = 
            mutation 
            |> Option.map (fun (Named n) -> n) 
            |> Option.toList
        initialTypes @ m @ (match types with None -> [] | Some t -> t)
        |> List.fold insert Map.empty

    let implementations =
        typeMap
        |> Map.toSeq
        |> Seq.choose (fun (_, v) ->
            match v with
            | Object odef -> Some odef
            | _ -> None)
        |> Seq.fold (fun acc objdef -> 
            objdef.Implements
            |> List.fold (fun acc' iface ->
                match Map.tryFind iface.Name acc' with
                | Some list -> Map.add iface.Name (objdef::list) acc'
                | None -> Map.add iface.Name [objdef] acc') acc
            ) Map.empty

    interface ISchema with
        member val TypeMap = typeMap
        member val Query = query
        member val Mutation = mutation
        member val Directives = match directives with None -> [IncludeDirective; SkipDirective] | Some d -> d
        member x.TryFindType typeName = Map.tryFind typeName typeMap
        member x.GetPossibleTypes typedef = 
            match typedef with
            | Union u -> u.Options
            | Interface i -> Map.find i.Name implementations
            | _ -> []
        member x.IsPossibleType abstractdef possibledef =
            match (x :> ISchema).GetPossibleTypes abstractdef with
            | [] -> false
            | possibleTypes -> possibleTypes |> List.exists (fun t -> t.Name = possibledef.Name)

    interface System.Collections.Generic.IEnumerable<NamedDef> with
        member x.GetEnumerator() = (typeMap |> Map.toSeq |> Seq.map snd).GetEnumerator()

    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = (typeMap |> Map.toSeq |> Seq.map snd :> System.Collections.IEnumerable).GetEnumerator()

    