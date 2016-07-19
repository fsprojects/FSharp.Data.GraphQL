/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open System.Collections.Generic
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Introspection
open FSharp.Data.GraphQL.Introspection
open FSharp.Data.GraphQL.Planning
open FSharp.Data.GraphQL.Execution

type SchemaConfig =
    { Types : NamedDef list
      Directives : DirectiveDef list }
    static member Default = 
        { Types = []
          Directives = [IncludeDirective; SkipDirective] }

type Schema<'Root> (query: ObjectDef<'Root>, ?mutation: ObjectDef<'Root>, ?config: SchemaConfig) as this =
    //FIXME: for some reason static do or do invocation in module doesn't work
    // for this reason we're compiling executors as part of identifier evaluation
    let __done =
        // we don't need to know possible types at this point
        SchemaMetaFieldDef.Execute <- compileField Unchecked.defaultof<TypeDef -> ObjectDef[]> SchemaMetaFieldDef
        TypeMetaFieldDef.Execute <- compileField Unchecked.defaultof<TypeDef -> ObjectDef[]> TypeMetaFieldDef
        TypeNameMetaFieldDef.Execute <- compileField Unchecked.defaultof<TypeDef -> ObjectDef[]> TypeNameMetaFieldDef

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
                |> Map.toArray
                |> Array.map snd
                |> Array.collect (fun x -> Array.append [| x.Type :> TypeDef |] (x.Args |> Array.map (fun a -> upcast a.Type)))
                |> Array.filter (fun (Named x) -> not (Map.containsKey x.Name ns'))
                |> Array.fold (fun n (Named t) -> insert n t) ns'
            objdef.Implements
            |> Array.fold (fun n t -> insert n t) withFields'
        | Interface interfacedef ->
            let ns' = addOrReturn typedef.Name typedef ns
            interfacedef.Fields
            |> Array.map (fun x -> x.Type)
            |> Array.filter (fun (Named x) -> not (Map.containsKey x.Name ns'))
            |> Array.fold (fun n (Named t) -> insert n t) ns'            
        | Union uniondef ->
            let ns' = addOrReturn typedef.Name typedef ns
            uniondef.Options
            |> Array.fold (fun n t -> insert n t) ns'            
        | List (Named innerdef) -> insert ns innerdef 
        | Nullable (Named innerdef) -> insert ns innerdef
        | InputObject objdef -> 
            let ns' = addOrReturn objdef.Name typedef ns
            objdef.Fields
            |> Array.collect (fun x -> [| x.Type :> TypeDef |])
            |> Array.filter (fun (Named x) -> not (Map.containsKey x.Name ns'))
            |> Array.fold (fun n (Named t) -> insert n t) ns'
        
    let initialTypes: NamedDef list = [ 
        Int
        String
        Boolean
        Float
        ID
        Date
        Uri
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
            |> Array.fold (fun acc' iface ->
                match Map.tryFind iface.Name acc' with
                | Some list -> Map.add iface.Name (objdef::list) acc'
                | None -> Map.add iface.Name [objdef] acc') acc
            ) Map.empty
    
    let getPossibleTypes abstractDef =
        match abstractDef with
        | Union u -> u.Options
        | Interface i -> Map.find i.Name implementations |> Array.ofList
        | _ -> [||]

    //-- INTROSPECTION SCHEMA GENERATION

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
          Args = fdef.Args |> Array.map (introspectInput namedTypes) 
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
          Args = directive.Args |> Array.map (introspectInput namedTypes) }

    let introspectType (namedTypes: Map<string, IntrospectionTypeRef>) typedef =
        match typedef with
        | Scalar scalardef -> 
            IntrospectionType.Scalar(scalardef.Name, scalardef.Description)
        | Object objdef -> 
            let fields = 
                objdef.Fields 
                |> Map.toArray
                |> Array.map snd
                |> Array.map (introspectField namedTypes)
            let interfaces = 
                objdef.Implements 
                |> Array.map (fun idef -> Map.find idef.Name namedTypes)
            IntrospectionType.Object(objdef.Name, objdef.Description, fields, interfaces)
        | InputObject inObjDef -> 
            let inputs = 
                inObjDef.Fields 
                |> Array.map (introspectInput namedTypes)
            IntrospectionType.InputObject(inObjDef.Name, inObjDef.Description, inputs)
        | Union uniondef -> 
            let possibleTypes = 
                getPossibleTypes uniondef
                |> Array.map (fun tdef -> Map.find tdef.Name namedTypes)
            IntrospectionType.Union(uniondef.Name, uniondef.Description, possibleTypes)
        | Enum enumdef -> 
            let enumVals = 
                enumdef.Options
                |> Array.map introspectEnumVal
            IntrospectionType.Enum(enumdef.Name, enumdef.Description, enumVals)
        | Interface idef ->
            let fields = 
                idef.Fields 
                |> Array.map (introspectField namedTypes)
            let possibleTypes = 
                getPossibleTypes idef
                |> Array.map (fun tdef -> Map.find tdef.Name namedTypes)
            IntrospectionType.Interface(idef.Name, idef.Description, fields, possibleTypes)

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
    do
        compileSchema getPossibleTypes typeMap
        match Validation.validate typeMap with
        | Validation.Success -> ()
        | Validation.Error errors -> raise (GraphQLException (System.String.Join("\n", errors)))
        
    member this.AsyncExecute(ast: Document, ?data: 'Root, ?variables: Map<string, obj>, ?operationName: string): Async<IDictionary<string,obj>> =
        let executionPlan = 
            match operationName with
            | Some opname -> this.CreateExecutionPlan(ast, opname)
            | None -> this.CreateExecutionPlan(ast)
        this.AsyncEvaluate(executionPlan, data, defaultArg variables Map.empty)

    member this.AsyncExecute(queryOrMutation: string, ?data: 'Root, ?variables: Map<string, obj>, ?operationName: string): Async<IDictionary<string,obj>> =
        let ast = parse queryOrMutation
        let executionPlan = 
            match operationName with
            | Some opname -> this.CreateExecutionPlan(ast, opname)
            | None -> this.CreateExecutionPlan(ast)
        this.AsyncEvaluate(executionPlan, data, defaultArg variables Map.empty)

    member this.AsyncEvaluate(executionPlan: ExecutionPlan, data: 'Root option, variables: Map<string, obj>): Async<IDictionary<string,obj>> =
        async {
            try
                let errors = System.Collections.Concurrent.ConcurrentBag<exn>()
                let rootObj = data |> Option.map box |> Option.toObj
                let! result = evaluate this executionPlan variables rootObj errors
                let output = [ "data", box result ] @ if errors.IsEmpty then [] else [ "errors", upcast (errors.ToArray() |> Array.map (fun e -> e.Message)) ]
                return upcast NameValueLookup.ofList output
            with 
            | ex -> 
                let msg = ex.ToString()
                return upcast NameValueLookup.ofList [ "errors", upcast [ msg ]]
        }

    member this.CreateExecutionPlan(ast: Document, ?operationName: string): ExecutionPlan =
        match findOperation ast operationName with
        | Some operation -> 
            let rootDef = 
                match operation.OperationType with
                | Query -> query
                | Mutation -> 
                    match mutation with
                    | Some m -> m
                    | None -> raise (GraphQLException "Operation to be executed is of type mutation, but no mutation root object was defined in current schema")
            let planningCtx = { Schema = this; RootDef = rootDef; Document = ast }
            planOperation planningCtx operation
        | None -> raise (GraphQLException "No operation with specified name has been found for provided document")

    member this.CreateExecutionPlan(queryOrMutation: string, ?operationName: string) =
        match operationName with
        | None -> this.CreateExecutionPlan(parse queryOrMutation)
        | Some o -> this.CreateExecutionPlan(parse queryOrMutation, o)
        

    interface ISchema with        
        member val TypeMap = typeMap
        member val Directives = schemaConfig.Directives |> List.toArray
        member val Introspected = introspected
        member x.Query = upcast query
        member x.Mutation = mutation |> Option.map (fun x -> upcast x)
        member x.TryFindType typeName = Map.tryFind typeName typeMap
        member x.GetPossibleTypes typedef = getPossibleTypes typedef
        member x.IsPossibleType abstractdef (possibledef: ObjectDef) =
            match (x :> ISchema).GetPossibleTypes abstractdef with
            | [||] -> false
            | possibleTypes -> possibleTypes |> Array.exists (fun t -> t.Name = possibledef.Name)

    interface System.Collections.Generic.IEnumerable<NamedDef> with
        member x.GetEnumerator() = (typeMap |> Map.toSeq |> Seq.map snd).GetEnumerator()

    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = (typeMap |> Map.toSeq |> Seq.map snd :> System.Collections.IEnumerable).GetEnumerator()
        