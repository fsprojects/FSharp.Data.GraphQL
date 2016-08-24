/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Execution

open System
open System.Reflection
open System.Collections.Generic
open System.Collections.Concurrent
open Hopac
open Hopac.Extensions
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Planning
open FSharp.Data.GraphQL.Types.Introspection
open FSharp.Data.GraphQL.Introspection

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
                    | :? seq<obj>, :? seq<obj> -> Seq.forall2 (=) (a.Value :?> seq<obj>) (b.Value :?> seq<obj>)
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
        member x.Add(_, _) = raise (NotSupportedException "NameValueLookup doesn't allow to add/remove entries")
        member x.Add(_) = raise (NotSupportedException "NameValueLookup doesn't allow to add/remove entries")
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
        
let private collectDefaultArgValue acc (argdef: InputFieldDef) =
    match argdef.DefaultValue with
    | Some defVal -> Map.add argdef.Name defVal acc
    | None -> acc

let private getArgumentValues (argDefs: InputFieldDef []) (args: Argument list) (variables: Map<string, obj>) : Map<string, obj> = 
    argDefs
    |> Array.fold (fun acc argdef -> 
        match List.tryFind (fun (a: Argument) -> a.Name = argdef.Name) args with
        | Some argument ->
            match argdef.ExecuteInput variables argument.Value with
            | null -> collectDefaultArgValue acc argdef
            | value -> Map.add argdef.Name value acc
        | None -> collectDefaultArgValue acc argdef
    ) Map.empty
            
let private getOperation = function
    | OperationDefinition odef -> Some odef
    | _ -> None

let internal findOperation doc opName =
    match doc.Definitions |> List.choose getOperation, opName with
    | [def], _ -> Some def
    | defs, name -> 
        defs
        |> List.tryFind (fun def -> def.Name = name)

let private coerceVariables (schema: #ISchema) (variables: VariableDefinition list) (vars: Map<string, obj>) =
    if vars = Map.empty
    then
        variables
        |> List.filter (fun vardef -> Option.isSome vardef.DefaultValue)
        |> List.fold (fun acc vardef ->
            let variableName = vardef.VariableName
            Map.add variableName (coerceVariable schema vardef Map.empty) acc) Map.empty
    else
        variables
        |> List.fold (fun acc vardef ->
            let variableName = vardef.VariableName
            Map.add variableName (coerceVariable schema vardef vars) acc) Map.empty
    
let private coerceDirectiveValue (ctx: ExecutionContext) (directive: Directive) =
    match directive.If.Value with
    | Variable vname -> downcast ctx.Variables.[vname]
    | other -> 
        match coerceBoolInput other with
        | Some s -> s
        | None ->
            (directive.Name, other)
            ||> sprintf "Expected 'if' argument of directive '@%s' to have boolean value but got %A"
            |> GraphQLException |> raise

let private shouldSkip (ctx: ExecutionContext) (directive: Directive) =
    match directive.Name with
    | "skip" when not <| coerceDirectiveValue ctx directive -> false
    | "include" when  coerceDirectiveValue ctx directive -> false
    | _ -> true
    
let private defaultResolveType possibleTypesFn abstractDef : obj -> ObjectDef =
    let possibleTypes = possibleTypesFn abstractDef
    let mapper = match abstractDef with Union u -> u.ResolveValue | _ -> id
    fun value ->
        let mapped = mapper value
        possibleTypes
        |> Array.find (fun objdef ->
            match objdef.IsTypeOf with
            | Some isTypeOf -> isTypeOf mapped
            | None -> false)
        
let private resolveInterfaceType possibleTypesFn (interfacedef: InterfaceDef) = 
    match interfacedef.ResolveType with
    | Some resolveType -> resolveType
    | None -> defaultResolveType possibleTypesFn interfacedef

let resolveUnionType possibleTypesFn (uniondef: UnionDef) = 
    match uniondef.ResolveType with
    | Some resolveType -> resolveType
    | None -> defaultResolveType possibleTypesFn uniondef
                
let rec createCompletion (possibleTypesFn: TypeDef -> ObjectDef []) (returnDef: OutputDef): ResolveFieldContext -> obj -> Job<obj> =
    match returnDef with
    | Object objdef -> 
        fun (ctx: ResolveFieldContext) value -> 
            match ctx.ExecutionPlan.Kind with
            | SelectFields fields -> executeFields objdef ctx value fields
            | kind -> failwithf "Unexpected value of ctx.ExecutionPlan.Kind: %A" kind 
    | Scalar scalardef ->
        let (coerce: obj -> obj option) = scalardef.CoerceValue
        fun _ value -> 
            coerce value
            |> Option.toObj
            |> Job.result
    | List (Output innerdef) ->
        let (innerfn: ResolveFieldContext -> obj -> Job<obj>) = createCompletion possibleTypesFn innerdef
        fun ctx (value: obj) -> job {
            let innerCtx =
                match ctx.ExecutionPlan.Kind with
                | ResolveCollection innerPlan -> { ctx with ExecutionPlan = innerPlan }
                | kind -> failwithf "Unexpected value of ctx.ExecutionPlan.Kind: %A" kind 
            match value with
            | :? string as s -> 
                let! inner = innerfn innerCtx (s)
                return [| inner |] :> obj
            | :? System.Collections.IEnumerable as enumerable ->
                let! completed =
                    enumerable
                    |> Seq.cast<obj>
                    |> Seq.map (fun x -> innerfn innerCtx x)
                    |> Job.conCollect
                return box (completed.ToArray())
            | _ ->
                return (ctx.ExecutionPlan.Identifier, value.GetType())
                ||> sprintf "Expected to have enumerable value in field '%s' but got '%O'"
                |> GraphQLException |> raise 
        }
    | Nullable (Output innerdef) ->
        let innerfn = createCompletion possibleTypesFn innerdef
        let optionDef = typedefof<option<_>>
        fun ctx value -> job {
            let t = value.GetType()
            if value = null then return null
            elif t.IsGenericType && t.GetGenericTypeDefinition() = optionDef then
                let _, fields = Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(value, t)
                return! innerfn ctx (fields.[0])
            else return! innerfn ctx value
        }
    | Interface idef ->
        let resolver = resolveInterfaceType possibleTypesFn idef
        fun ctx value -> job {
            let resolvedDef = resolver value
            let typeMap =
                match ctx.ExecutionPlan.Kind with
                | ResolveAbstraction typeMap -> typeMap
                | kind -> failwithf "Unexpected value of ctx.ExecutionPlan.Kind: %A" kind 
            match Map.tryFind resolvedDef.Name typeMap with
            | Some fields -> return! executeFields resolvedDef ctx value fields
            | None -> return raise(GraphQLException (sprintf "GraphQL interface '%s' is not implemented by the type '%s'" idef.Name resolvedDef.Name))   
        }
    | Union udef ->
        let resolver = resolveUnionType possibleTypesFn udef
        fun ctx value -> job {
            let resolvedDef = resolver value
            let typeMap =
                match ctx.ExecutionPlan.Kind with
                | ResolveAbstraction typeMap -> typeMap
                | kind -> failwithf "Unexpected value of ctx.ExecutionPlan.Kind: %A" kind 
            match Map.tryFind resolvedDef.Name typeMap with
            | Some fields -> return! executeFields resolvedDef ctx (udef.ResolveValue value) fields
            | None -> return raise(GraphQLException (sprintf "GraphQL union '%s' doesn't have a case of type '%s'" udef.Name resolvedDef.Name))   
        }
    | Enum _ ->
        fun _ value -> 
            let result = coerceStringValue value
            Job.result (result |> Option.map box |> Option.toObj)
    | _ -> failwithf "Unexpected value of returnDef: %O" returnDef

and internal compileField possibleTypesFn (fieldDef: FieldDef) : ExecuteField =
    let completed = createCompletion possibleTypesFn (fieldDef.Type)
    let resolve = fieldDef.Resolve
    fun resolveFieldCtx value -> (job {
        try
            let! res = resolve resolveFieldCtx value
            if res = null 
            then return null
            else return! completed resolveFieldCtx res
        with
        | ex -> 
            resolveFieldCtx.AddError(ex)
            return null
    } |> Job.toAsync)

    /// Takes an object type and a field, and returns that field’s type on the object type, or null if the field is not valid on the object type
and private getFieldDefinition (ctx: ExecutionContext) (objectType: ObjectDef) (field: Field) : FieldDef option =
        match field.Name with
        | "__schema" when Object.ReferenceEquals(ctx.Schema.Query, objectType) -> Some (upcast SchemaMetaFieldDef)
        | "__type" when Object.ReferenceEquals(ctx.Schema.Query, objectType) -> Some (upcast TypeMetaFieldDef)
        | "__typename" -> Some (upcast TypeNameMetaFieldDef)
        | fieldName -> objectType.Fields |> Map.tryFind fieldName
        
and private createFieldContext objdef ctx (info: ExecutionPlanInfo) =
    let fdef = info.Definition
    let args = getArgumentValues fdef.Args info.Ast.Arguments ctx.Variables
    { ExecutionPlan = info
      Context = ctx.Context
      ReturnType = fdef.Type
      ParentType = objdef
      Schema = ctx.Schema
      Args = args
      Variables = ctx.Variables }         

and private executeFields (objdef: ObjectDef) (ctx: ResolveFieldContext) (value: obj) fieldInfos : Job<obj> = job {
    let resultSet =
        fieldInfos
        |> List.filter (fun info -> info.Include ctx.Variables)
        |> List.map (fun info -> (info.Identifier, info))
        |> List.toArray
    let result = 
        resultSet
        |> Array.map fst
        |> NameValueLookup 
    do! resultSet
        |> Array.map (fun (name, info) -> job { 
            let innerCtx = createFieldContext objdef ctx info
            let! res = info.Definition.Execute innerCtx value
            do result.Update name res })
        |> Job.conIgnore
    return box result }

let internal executePlan (ctx: ExecutionContext) (plan: ExecutionPlan) (objdef: ObjectDef) value = job {
    let resultSet =
        plan.Fields
        |> List.filter (fun info -> info.Include ctx.Variables)
        |> List.map (fun info -> (info.Identifier, info))
        |> List.toArray
    let result = 
        resultSet
        |> Array.map fst
        |> NameValueLookup 
    do! resultSet
        |> Array.map (fun (name, info) -> job { 
            let fdef = info.Definition
            let args = getArgumentValues fdef.Args info.Ast.Arguments ctx.Variables
            let fieldCtx = 
                { ExecutionPlan = info
                  Context = ctx
                  ReturnType = fdef.Type
                  ParentType = objdef
                  Schema = ctx.Schema
                  Args = args
                  Variables = ctx.Variables } 
            let! res = info.Definition.Execute fieldCtx value
            do result.Update name res })
        |> match plan.Strategy with
           | Parallel -> Job.conIgnore
           | Serial   -> Job.seqIgnore
    return box result }

let private compileInputObject (indef: InputObjectDef) =
    indef.Fields
    |> Array.iter(fun input -> 
        let errMsg = sprintf "Input object '%s': in field '%s': " indef.Name input.Name
        input.ExecuteInput <- compileByType errMsg input.Type)

let internal compileSchema possibleTypesFn types =
    types
    |> Map.toSeq 
    |> Seq.map snd
    |> Seq.iter (fun x ->
        match x with
        | Object objdef -> 
            objdef.Fields
            |> Map.iter (fun _ fieldDef -> 
                fieldDef.Execute <- compileField possibleTypesFn fieldDef
                fieldDef.Args
                |> Array.iter (fun arg -> 
                    let errMsg = sprintf "Object '%s': field '%s': argument '%s': " objdef.Name fieldDef.Name arg.Name
                    arg.ExecuteInput <- compileByType errMsg arg.Type))
        | InputObject indef -> compileInputObject indef
        | _ -> ())
                
let internal evaluate (schema: #ISchema) (executionPlan: ExecutionPlan) (variables: Map<string, obj>) (root: obj) errors = 
    job {
        let variables = coerceVariables schema executionPlan.Operation.VariableDefinitions variables
        let ctx = {
            Schema = schema
            ExecutionPlan = executionPlan
            RootValue = root
            Variables = variables
            Errors = errors }
        return! executePlan ctx executionPlan schema.Query root
    } |> Job.toAsync
