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

    let rec introspectTypeRef isNullable (namedTypes: Map<string, IntrospectionTypeRef>) typedef =
        match typedef with
        | Nullable inner -> introspectTypeRef true namedTypes inner
        | List inner -> 
            if isNullable 
            then IntrospectionTypeRef.List(introspectTypeRef false namedTypes inner)
            else IntrospectionTypeRef.NonNull(introspectTypeRef true namedTypes typedef)
        | Named named -> 
            if isNullable
            then Map.find named.Name namedTypes
            else IntrospectionTypeRef.NonNull(introspectTypeRef true namedTypes typedef)

    let introspectInput (namedTypes: Map<string, IntrospectionTypeRef>) (inputDef: InputFieldDef) : IntrospectionInputVal =
        { Name = inputDef.Name
          Description = inputDef.Description
          Type = introspectTypeRef false namedTypes inputDef.Type
          DefaultValue = inputDef.DefaultValue |> Option.map (fun x -> x.ToString()) }

    let introspectField (namedTypes: Map<string, IntrospectionTypeRef>) (fdef: FieldDef) =
        { Name = fdef.Name
          Description = fdef.Description
          Args = fdef.Args |> List.map (introspectInput namedTypes) |> List.toArray
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
        |> Seq.toArray

    let introspectDirective (namedTypes: Map<string, IntrospectionTypeRef>) (directive: DirectiveDef) : IntrospectionDirective =
        { Name = directive.Name
          Description = directive.Description
          Locations = locationToList directive.Locations
          Args = directive.Args |> List.map (introspectInput namedTypes) |> List.toArray }

    let introspectType (namedTypes: Map<string, IntrospectionTypeRef>) typedef =
        match typedef with
        | Scalar scalardef -> 
            IntrospectionType.Scalar(scalardef.Name, scalardef.Description)
        | Object objdef -> 
            let fields = 
                objdef.Fields 
                |> List.map (introspectField namedTypes)
                |> List.toArray
            let interfaces = 
                objdef.Implements 
                |> List.map (fun idef -> Map.find idef.Name namedTypes)
                |> List.toArray
            IntrospectionType.Object(objdef.Name, objdef.Description, fields, interfaces)
        | InputObject inObjDef -> 
            let inputs = 
                inObjDef.Fields 
                |> List.map (introspectInput namedTypes)
                |> List.toArray
            IntrospectionType.InputObject(inObjDef.Name, inObjDef.Description, inputs)
        | Union uniondef -> 
            let possibleTypes = 
                getPossibleTypes uniondef
                |> List.map (fun tdef -> Map.find tdef.Name namedTypes)
                |> List.toArray
            IntrospectionType.Union(uniondef.Name, uniondef.Description, possibleTypes)
        | Enum enumdef -> 
            let enumVals = 
                enumdef.Options
                |> List.map introspectEnumVal
                |> List.toArray
            IntrospectionType.Enum(enumdef.Name, enumdef.Description, enumVals)
        | Interface idef ->
            let fields = 
                idef.Fields 
                |> List.map (introspectField namedTypes)
                |> List.toArray
            let possibleTypes = 
                getPossibleTypes idef
                |> List.map (fun tdef -> Map.find tdef.Name namedTypes)
                |> List.toArray
            IntrospectionType.Interface(idef.Name, idef.Description, fields, possibleTypes )

    let introspectSchema types : IntrospectionSchema =
        let inamed = 
            types 
            |> Map.map (fun typeName typedef -> 
                match typedef with
                | Scalar x -> { Kind = TypeKind.SCALAR; Name = Some typeName; Description = x.Description; OfType = None }
                | Object x -> { Kind = TypeKind.OBJECT; Name = Some typeName; Description = x.Description; OfType = None }
                | InputObject x -> { Kind = TypeKind.INPUT_OBJECT; Name = Some typeName; Description = x.Description; OfType = None }
                | Union x -> { Kind = TypeKind.UNION; Name = Some typeName; Description = x.Description; OfType = None }
                | Enum x -> { Kind = TypeKind.ENUM; Name = Some typeName; Description = x.Description; OfType = None }
                | Interface x -> { Kind = TypeKind.INTERFACE; Name = Some typeName; Description = x.Description; OfType = None })

        let itypes =
            types
            |> Map.toArray
            |> Array.map (snd >> (introspectType inamed))

        let idirectives = 
            schemaConfig.Directives 
            |> List.map (introspectDirective inamed)
            |> List.toArray
            
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

open System.Collections.Generic

type NameValueLookup(keyValues: KeyValuePair<string, obj> []) =
    let kvals = keyValues |> Array.distinctBy (fun kv -> kv.Key)
    let setValue key value =
        let mutable i = 0
        while i < kvals.Length do
            if kvals.[i].Key = key then
                kvals.[i] <- KeyValuePair<string, obj>(key, value) 
                i <- Int32.MaxValue
            else i <- i+1
    let getValue key = (kvals |> Array.find (fun kv -> kv.Key = key)).Value
    let rec structEq (x: NameValueLookup) (y: NameValueLookup) =
        if Object.ReferenceEquals(x, y) then true
        elif Object.ReferenceEquals(y, null) then false
        elif x.Count <> y.Count then false
        else
            x.Buffer
            |> Array.forall2 (fun (a: KeyValuePair<string, obj>) (b: KeyValuePair<string, obj>) ->
                if a.Key <> b.Key then false
                else 
                    match a.Value, b.Value with
                    | :? NameValueLookup, :? NameValueLookup as o -> structEq (downcast fst o) (downcast snd o)
                    | a1, b1 -> a1 = b1) y.Buffer
    let pad (sb: System.Text.StringBuilder) times = for i in 0..times do sb.Append("\t") |> ignore
    let rec stringify (sb: System.Text.StringBuilder) deep (o:obj) =
        match o with
        | :? NameValueLookup as lookup ->
            sb.Append("{ ") |> ignore
            lookup.Buffer
            |> Array.iter (fun kv -> 
                sb.Append(kv.Key).Append(": ") |> ignore
                stringify sb (deep+1) kv.Value
                sb.Append(",\r\n") |> ignore
                pad sb deep)
            sb.Remove(sb.Length - 4 - deep, 4 + deep).Append(" }") |> ignore
        | :? string as s ->
            sb.Append("\"").Append(s).Append("\"") |> ignore
        | :? System.Collections.IEnumerable as s -> 
            sb.Append("[") |> ignore
            for i in s do 
                stringify sb (deep+1) i
                sb.Append(", ") |> ignore
            sb.Append("]") |> ignore
        | other -> 
            if other <> null 
            then sb.Append(other.ToString()) |> ignore
            else sb.Append("null") |> ignore
        ()
    static member ofList (l: (string * obj) list) = NameValueLookup(l)
    member private x.Buffer : KeyValuePair<string, obj> [] = kvals
    member x.Count = kvals.Length
    member x.Update key value = setValue key value
    override x.Equals(other) = 
        match other with
        | :? NameValueLookup as lookup -> structEq x lookup
        | _ -> false
    override x.ToString() = 
        let sb =Text.StringBuilder()
        stringify sb 1 x
        sb.ToString()
    interface IEquatable<NameValueLookup> with
        member x.Equals(other) = structEq x other
    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = (kvals :> System.Collections.IEnumerable).GetEnumerator()
    interface IEnumerable<KeyValuePair<string, obj>> with
        member x.GetEnumerator() = (kvals :> IEnumerable<KeyValuePair<string, obj>>).GetEnumerator()
    interface IDictionary<string, obj> with
        member x.Add(key, value) = raise (NotSupportedException "NameValueLookup doesn't allow to add/remove entries")
        member x.Add(item) = raise (NotSupportedException "NameValueLookup doesn't allow to add/remove entries")
        member x.Clear() = raise (NotSupportedException "NameValueLookup doesn't allow to add/remove entries")
        member x.Contains(item) = kvals |> Array.exists ((=) item)
        member x.ContainsKey(key) = kvals |> Array.exists (fun kv -> kv.Key = key)
        member x.CopyTo(array, arrayIndex) = kvals.CopyTo(array, arrayIndex)
        member x.Count = x.Count
        member x.IsReadOnly = true
        member x.Item
            with get (key) = getValue key
            and set (key) v = setValue key v
        member x.Keys = upcast (kvals |> Array.map (fun kv -> kv.Key))
        member x.Values = upcast (kvals |> Array.map (fun kv -> kv.Value))
        member x.Remove(_:string) = 
            raise (NotSupportedException "NameValueLookup doesn't allow to add/remove entries")
            false
        member x.Remove(_:KeyValuePair<string,obj>) = 
            raise (NotSupportedException "NameValueLookup doesn't allow to add/remove entries")
            false
        member x.TryGetValue(key, value) = 
            match kvals |> Array.tryFind (fun kv -> kv.Key = key) with
            | Some kv -> value <- kv.Value; true
            | None -> value <- null; false
    new(t: (string * obj) list) = 
        NameValueLookup(t |> List.map (fun (k, v) -> KeyValuePair<string,obj>(k, v)) |> List.toArray)
    new(t: string []) = 
        NameValueLookup(t |> Array.map (fun k -> KeyValuePair<string,obj>(k, null)))