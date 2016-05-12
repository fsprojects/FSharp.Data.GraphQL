/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Introspection
open FSharp.Data.GraphQL.Introspection
open FSharp.Data.GraphQL.Validation

type SchemaConfig =
    { Types : NamedDef list
      Directives : DirectiveDef list }
    static member Default = 
        { Types = []
          Directives = [IncludeDirective; SkipDirective] }

type Schema(query: ObjectDef, ?mutation: ObjectDef, ?config: SchemaConfig) as this =
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
                |> List.collect (fun x -> (x.Type :> TypeDef) :: (x.Args |> List.map (fun a -> upcast a.Type)))
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
        | Nullable (Named innerdef) -> insert ns innerdef
        | InputObject objdef -> 
            let ns' = addOrReturn objdef.Name typedef ns
            objdef.Fields
            |> List.collect (fun x -> [x.Type :> TypeDef])
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

    let schemaConfig = match config with None -> SchemaConfig.Default | Some c -> c
    let mutable typeMap: Map<string, NamedDef> = 
        let m = 
            mutation 
            |> Option.map (fun (Named n) -> n) 
            |> Option.toList
        initialTypes @ m @ schemaConfig.Types
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
    
    let getPossibleTypes abstractDef =
        match abstractDef with
        | Union u -> u.Options
        | Interface i -> Map.find i.Name implementations
        | _ -> []

    let rec introspectTypeRef isNullable namedTypes typedef =
        match typedef with
        | Named named -> 
            if isNullable
            then NamedTypeRef(Map.find named.Name namedTypes)
            else NonNullTypeRef(introspectTypeRef true namedTypes typedef)
        | List inner -> 
            if isNullable 
            then ListTypeRef(introspectTypeRef false namedTypes inner)
            else NonNullTypeRef(introspectTypeRef true namedTypes typedef)
        | Nullable inner -> introspectTypeRef true namedTypes inner

    let introspectInput namedTypes (inputDef: InputFieldDef) : IntrospectionInputVal =
        { Name = inputDef.Name
          Description = inputDef.Description
          Type = introspectTypeRef false namedTypes inputDef.Type
          DefaultValue = inputDef.DefaultValue |> Option.map (fun x -> x.ToString()) }

    let introspectField namedTypes (fdef: FieldDef) =
        { Name = fdef.Name
          Description = fdef.Description
          Args = fdef.Args |> List.map (introspectInput namedTypes)
          Type = introspectTypeRef false namedTypes fdef.Type
          IsDeprecated = Option.isSome fdef.DeprecationReason
          DeprecationReason = fdef.DeprecationReason }

    let introspectEnumVal (enumVal: EnumVal) : IntrospectionEnumVal =
        { Name = enumVal.Name
          Description = enumVal.Description
          IsDeprecated = Option.isSome enumVal.DeprecationReason
          DeprecationReason = enumVal.DeprecationReason }
          
    let locationToList location =
        System.Enum.GetValues(typeof<DirectiveLocation>)
        |> Seq.cast<DirectiveLocation>
        |> Seq.filter (fun v -> int(location) &&& int(v) <> 0)

    let introspectDirective namedTypes (directive: DirectiveDef) : IntrospectionDirective =
        { Name = directive.Name
          Description = directive.Description
          Locations = locationToList directive.Locations
          Args = directive.Args |> List.map (introspectInput namedTypes) }

    let introspectType namedTypes typedef =
        match typedef with
        | Scalar scalardef -> 
            IntrospectionScalar(scalardef.Name, scalardef.Description)
        | Object objdef -> 
            let fields = 
                objdef.Fields 
                |> List.map (introspectField namedTypes)
            let interfaces = 
                objdef.Implements 
                |> List.map (fun idef -> Map.find idef.Name namedTypes)
            IntrospectionObject(objdef.Name, objdef.Description, fields, interfaces)
        | InputObject inObjDef -> 
            let inputs = inObjDef.Fields |> List.map (introspectInput namedTypes)
            IntrospectionInputObject(inObjDef.Name, inObjDef.Description, inputs)
        | Union uniondef -> 
            let possibleTypes = 
                getPossibleTypes uniondef
                |> List.map (fun tdef -> Map.find tdef.Name namedTypes)
            IntrospectionUnion(uniondef.Name, uniondef.Description, possibleTypes)
        | Enum enumdef -> 
            let enumVals = 
                enumdef.Options
                |> List.map introspectEnumVal
            IntrospectionEnum(enumdef.Name, enumdef.Description, enumVals)
        | Interface idef ->
            let fields = 
                idef.Fields 
                |> List.map (introspectField namedTypes)
            let possibleTypes = 
                getPossibleTypes idef
                |> List.map (fun tdef -> Map.find tdef.Name namedTypes)
            IntrospectionInterface(idef.Name, idef.Description, fields, possibleTypes)

    let introspectSchema types : IntrospectionSchema =
        let inamed = 
            types 
            |> Map.map (fun typeName typedef -> 
                match typedef with
                | Scalar x -> { Kind = TypeKind.SCALAR; Name = typeName; Description = x.Description }
                | Object x -> { Kind = TypeKind.OBJECT; Name = typeName; Description = x.Description }
                | InputObject x -> { Kind = TypeKind.INPUT_OBJECT; Name = typeName; Description = x.Description }
                | Union x -> { Kind = TypeKind.UNION; Name = typeName; Description = x.Description }
                | Enum x -> { Kind = TypeKind.ENUM; Name = typeName; Description = x.Description }
                | Interface x -> { Kind = TypeKind.INTERFACE; Name = typeName; Description = x.Description })

        let itypes =
            types
            |> Map.toList
            |> List.map (snd >> (introspectType inamed))

        let idirectives = schemaConfig.Directives |> List.map (introspectDirective inamed)
            
        let ischema =
            { QueryType = Map.find query.Name inamed
              MutationType = mutation |> Option.map (fun m -> Map.find m.Name inamed)
              SubscriptionType = None // not supported yet
              Types = itypes
              Directives = idirectives }
        ischema

    let introspected = introspectSchema typeMap

    interface ISchema with
        member val TypeMap = typeMap
        member val Query = query
        member val Mutation = mutation
        member val Directives = schemaConfig.Directives
        member val Introspected = introspected
        member x.TryFindType typeName = Map.tryFind typeName typeMap
        member x.GetPossibleTypes typedef = getPossibleTypes typedef
        member x.IsPossibleType abstractdef possibledef =
            match (x :> ISchema).GetPossibleTypes abstractdef with
            | [] -> false
            | possibleTypes -> possibleTypes |> List.exists (fun t -> t.Name = possibledef.Name)

    interface System.Collections.Generic.IEnumerable<NamedDef> with
        member x.GetEnumerator() = (typeMap |> Map.toSeq |> Seq.map snd).GetEnumerator()

    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = (typeMap |> Map.toSeq |> Seq.map snd :> System.Collections.IEnumerable).GetEnumerator()