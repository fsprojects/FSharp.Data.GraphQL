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

let private findOperation doc opName =
    match doc.Definitions |> List.choose getOperation, opName with
    | [def], _ -> Some def
    | defs, name -> 
        defs
        |> List.tryFind (fun def -> def.Name = name)
    | _ -> None

let private coerceVariables (schema: #ISchema) (variables: VariableDefinition list) (inputs: Map<string, obj> option) =
    match inputs with
    | None -> 
        variables
        |> List.filter (fun vardef -> Option.isSome vardef.DefaultValue)
        |> List.fold (fun acc vardef ->
            let variableName = vardef.VariableName
            Map.add variableName (coerceVariable schema vardef Map.empty) acc) Map.empty
    | Some vars -> 
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
        | None -> raise (GraphQLException (sprintf "Expected 'if' argument of directive '@%s' to have boolean value but got %A" directive.Name other))

let private shouldSkip (ctx: ExecutionContext) (directive: Directive) =
    match directive.Name with
    | "skip" when not <| coerceDirectiveValue ctx directive -> false
    | "include" when  coerceDirectiveValue ctx directive -> false
    | _ -> true

let private doesFragmentTypeApply (ctx: ExecutionContext) fragment (objectType: ObjectDef) = 
    match fragment.TypeCondition with
    | None -> true
    | Some typeCondition ->
        match ctx.Schema.TryFindType typeCondition with
        | None -> false
        | Some conditionalType when conditionalType.Name = objectType.Name -> true
        | Some (Abstract conditionalType) -> ctx.Schema.IsPossibleType conditionalType objectType
        | _ -> false
            
// 6.5 Evaluating selection sets
let rec private collectFields (ctx: ExecutionContext) typedef (selectionSet: Selection list) (visitedFragments): (string * Field []) [] =
    let rec findGroupIndexByName (groupedFields: System.Collections.Generic.List<string * Field []>) (name: string) (i: int) : int =
        if i < 0
        then -1
        else 
            let (k, _) = groupedFields.[i]
            if k = name then i
            else findGroupIndexByName groupedFields name (i-1)

    let groupedFields = System.Collections.Generic.List(selectionSet.Length)
    selectionSet 
    |> List.iteri(fun i selection ->
        if not (List.exists (shouldSkip ctx) selection.Directives)
        then
            match selection with
            | Field field ->
                let name = field.AliasOrName
                match findGroupIndexByName groupedFields name (groupedFields.Count-1) with
                | -1 -> 
                    groupedFields.Add (name, [| field |])
                | idx -> 
                    let (_, value) = groupedFields.[idx]
                    groupedFields.[idx] <- (name, Array.append [| field |] value)
            | FragmentSpread spread ->
                let fragmentSpreadName = spread.Name
                if not (List.exists (fun fragmentName -> fragmentName = fragmentSpreadName) !visitedFragments)
                then 
                    visitedFragments := (fragmentSpreadName::!visitedFragments)
                    let found =
                        ctx.Document.Definitions
                        |> List.tryFind (function FragmentDefinition f when f.Name.Value = fragmentSpreadName -> true | _ -> false)
                    match found with
                    | Some (FragmentDefinition fragment) -> 
                        if doesFragmentTypeApply ctx fragment typedef
                        then 
                            let fragmentSelectionSet = fragment.SelectionSet
                            let fragmentGroupedFieldSet = collectFields ctx typedef fragmentSelectionSet visitedFragments
                            for j = 0 to fragmentGroupedFieldSet.Length - 1 do
                                let (responseKey, fragmentGroup) = fragmentGroupedFieldSet.[j]
                                match findGroupIndexByName groupedFields responseKey (groupedFields.Count-1) with
                                | -1 ->
                                    groupedFields.Add (responseKey, fragmentGroup)
                                | idx ->
                                    let (_, value) = groupedFields.[idx]
                                    groupedFields.[idx] <- (responseKey, Array.append fragmentGroup value)
                    | _ -> ()
            | InlineFragment fragment -> 
                if doesFragmentTypeApply ctx fragment typedef
                then 
                    let fragmentSelectionSet = fragment.SelectionSet
                    let fragmentGroupedFieldSet = collectFields ctx typedef fragmentSelectionSet visitedFragments
                    for j = 0 to fragmentGroupedFieldSet.Length - 1 do
                        let (responseKey, fragmentGroup) = fragmentGroupedFieldSet.[j]
                        match findGroupIndexByName groupedFields responseKey (groupedFields.Count-1) with
                        | -1 ->
                            groupedFields.Add (responseKey, fragmentGroup)
                        | idx ->
                            let (_, value) = groupedFields.[idx]
                            groupedFields.[idx] <- (responseKey, Array.append fragmentGroup value))
    groupedFields.ToArray()
    
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
    
let SchemaMetaFieldDef = Define.Field(
    name = "__schema",
    description = "Access the current type schema of this server.",
    typedef = __Schema,
    resolve = fun ctx (_: obj) -> ctx.Schema.Introspected)
    
let TypeMetaFieldDef = Define.Field(
    name = "__type",
    description = "Request the type information of a single type.",
    typedef = __Type,
    args = [
        { Name = "name"
          Description = None
          Type = String
          DefaultValue = None
          ExecuteInput = variableOrElse(coerceStringInput >> Option.map box >> Option.toObj) }
    ],
    resolve = fun ctx (_:obj) -> 
        ctx.Schema.Introspected.Types 
        |> Seq.find (fun t -> t.Name = ctx.Arg("name")) 
        |> IntrospectionTypeRef.Named)
    
let TypeNameMetaFieldDef : FieldDef<obj> = Define.Field(
    name = "__typename",
    description = "The name of the current Object type at runtime.",
    typedef = String,
    resolve = fun ctx (_:obj) -> ctx.ParentType.Name)
        
let rec createCompletion (possibleTypesFn: TypeDef -> ObjectDef []) (returnDef: OutputDef): ResolveFieldContext -> obj -> Job<obj> =
    match returnDef with
    | Object objdef -> createObjectCompletion objdef
    | Scalar scalardef ->
        let (coerce: obj -> obj option) = scalardef.CoerceValue
        fun _ value -> 
            coerce value
            |> Option.toObj
            |> Job.result
    | List (Output innerdef) ->
        let (innerfn: ResolveFieldContext -> obj -> Job<obj>) = createCompletion possibleTypesFn innerdef
        fun ctx (value: obj) -> job {
            match value with
            | :? string as s -> 
                let! inner = innerfn ctx (s)
                return [| inner |] :> obj
            | :? System.Collections.IEnumerable as enumerable ->
                let! completed =
                    enumerable
                    |> Seq.cast<obj>
                    |> Seq.map (fun x -> innerfn ctx x)
                    |> Job.conCollect
                return completed.ToArray() :> obj
            | _ -> return raise (GraphQLException (sprintf "Expected to have enumerable value in field '%s' but got '%O'" ctx.FieldName (value.GetType())))
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
            let resolved = resolver value
            return! createObjectCompletion resolved ctx value
        }
    | Union udef ->
        let resolver = resolveUnionType possibleTypesFn udef
        fun ctx value -> job {
            let resolved = resolver value
            return! createObjectCompletion resolved ctx (udef.ResolveValue value)
        }
    | Enum _ ->
        fun _ value -> 
            let result = coerceStringValue value
            Job.result (result |> Option.map box |> Option.toObj)

and private createObjectCompletion objdef =
    fun (ctx: ResolveFieldContext) value -> job {
    let groupedFieldSet = 
        ctx.Fields
        |> Array.fold (fun _ field -> collectFields ctx.ExecutionContext objdef field.SelectionSet (ref [])) [||]
    let! res = executeFields ctx.ExecutionContext objdef value groupedFieldSet 
    return res :> obj }


and private compileField possibleTypesFn (fieldDef: FieldDef) : ExecuteField =
    let completed = createCompletion possibleTypesFn (fieldDef.Type)
    let resolve = fieldDef.Resolve
    fun resolveFieldCtx value -> job {
        try
            let! res = (resolve resolveFieldCtx value)
            if res = null 
            then return null
            else return! completed resolveFieldCtx res
        with
        | ex -> 
            resolveFieldCtx.AddError(ex)
            return null
    }

    /// Takes an object type and a field, and returns that field’s type on the object type, or null if the field is not valid on the object type
and private getFieldDefinition (ctx: ExecutionContext) (objectType: ObjectDef) (field: Field) : FieldDef option =
        match field.Name with
        | "__schema" when Object.ReferenceEquals(ctx.Schema.Query, objectType) -> Some (upcast SchemaMetaFieldDef)
        | "__type" when Object.ReferenceEquals(ctx.Schema.Query, objectType) -> Some (upcast TypeMetaFieldDef)
        | "__typename" -> Some (upcast TypeNameMetaFieldDef)
        | fieldName -> objectType.Fields |> Array.tryFind (fun f -> f.Name = fieldName)

and private getFieldEntry (ctx: ExecutionContext) typedef value (fields: Field []) : Job<obj> = 
    let firstField = fields.[0]
    match getFieldDefinition ctx typedef firstField with
    | None -> Job.result null
    | Some fieldDef -> 
        let args = getArgumentValues fieldDef.Args firstField.Arguments ctx.Variables
        let resolveFieldCtx = {
            FieldName = fieldDef.Name
            Fields = fields
            FieldType = fieldDef
            ReturnType = fieldDef.Type
            ParentType = typedef
            Schema = ctx.Schema
            Args = args
            Operation = ctx.Operation
            Fragments = ctx.Fragments
            Variables = ctx.Variables
            AddError = ctx.Errors.Add
            ExecutionContext = ctx
        } 
        fieldDef.Execute resolveFieldCtx value

and private executeFields (ctx: ExecutionContext) (typedef: ObjectDef) (value: obj) (groupedFieldSet: (string * Field []) []) : Job<NameValueLookup> = job {
    let result =
        groupedFieldSet
        |> Array.map fst
        |> NameValueLookup
    do! groupedFieldSet
        |> Array.map (fun (responseKey, fields) -> job { 
            let! res = getFieldEntry ctx typedef value fields
            do result.Update responseKey res })
        |> Job.conIgnore
    return result }

and private executeFieldsSync ctx typedef value (groupedFieldSet: (string * Field []) []) = job {
    let result =
        groupedFieldSet
        |> Array.map fst
        |> NameValueLookup
    do! groupedFieldSet
        |> Array.map (fun (responseKey, fields) -> job {
            let! entry = getFieldEntry ctx typedef value fields
            result.Update responseKey entry
           })
        |> Job.seqIgnore
    return result }

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
            |> Array.iter (fun fieldDef -> 
                fieldDef.Execute <- compileField possibleTypesFn fieldDef
                fieldDef.Args
                |> Array.iter (fun arg -> 
                    let errMsg = sprintf "Object '%s': field '%s': argument '%s': " objdef.Name fieldDef.Name arg.Name
                    arg.ExecuteInput <- compileByType errMsg arg.Type))
        | InputObject indef -> compileInputObject indef
        | _ -> ())
                
let private evaluate (schema: #ISchema) doc operation variables root errors = job {
    let variables = coerceVariables schema operation.VariableDefinitions variables
    let ctx = {
        Schema = schema
        RootValue = match root with None -> null | Some x -> x
        Document = doc
        Variables = variables
        Operation = operation
        Fragments = doc.Definitions |> List.choose (fun x -> match x with FragmentDefinition f -> Some f | _ -> None)
        Errors = errors
    }
    match operation.OperationType with
    | Mutation -> 
        let groupedFieldSet = 
            collectFields ctx schema.Mutation.Value operation.SelectionSet  (ref [])
        return! executeFieldsSync ctx schema.Mutation.Value ctx.RootValue groupedFieldSet
    | Query ->
        let groupedFieldSet = 
            collectFields ctx schema.Query operation.SelectionSet  (ref [])
        return! executeFields ctx schema.Query ctx.RootValue groupedFieldSet }

let internal execute (schema: #ISchema) doc operationName variables root errors = async {
    match findOperation doc operationName with
    | Some operation -> return! evaluate schema doc operation variables root errors |> Async.Global.ofJob
    | None -> return raise (GraphQLException "No operation with specified name has been found for provided document") }

// we don't need to know possible types at this point
SchemaMetaFieldDef.Execute <- compileField Unchecked.defaultof<TypeDef -> ObjectDef[]> SchemaMetaFieldDef
TypeMetaFieldDef.Execute <- compileField Unchecked.defaultof<TypeDef -> ObjectDef[]> TypeMetaFieldDef
TypeNameMetaFieldDef.Execute <- compileField Unchecked.defaultof<TypeDef -> ObjectDef[]> TypeNameMetaFieldDef