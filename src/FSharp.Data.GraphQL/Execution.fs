/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Execution

open System
open System.Collections.Generic
open System.Collections.Concurrent
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types

type internal NameValueLookup(kvals: KeyValuePair<string, obj> []) =
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
        NameValueLookup(Array.map (fun k -> KeyValuePair<string,obj>(k, null)) t)

let private option = function
    | null -> None
    | other -> Some other

let rec private getTypeName = function
    | NamedType name -> name
    | ListType inner -> getTypeName inner
    | NonNullType inner -> getTypeName inner
    
let rec private coerceValue allowNull typedef (input: obj) : obj = 
    match typedef with
    | Scalar scalardef -> 
        match scalardef.CoerceValue input with
        | None when allowNull -> null
        | None -> raise (GraphQLException (sprintf "Value '%A' has been coerced to null, but type definition mark corresponding field as non nullable" input))
        | Some res -> res
    | Nullable innerdef -> coerceValue true innerdef input 
    | List innerdef ->
        match input with
        | null -> null
        // special case - while single values should be wrapped with a list in this scenario,
        // string would be treat as IEnumerable and coerced into a list of chars
        | :? string as s -> upcast [coerceValue false innerdef s]
        | :? System.Collections.IEnumerable as iter -> 
            let mapped =
                iter
                |> Seq.cast<obj>
                |> Seq.map (fun elem -> coerceValue false innerdef elem)
                |> List.ofSeq
            upcast mapped
        | other -> upcast [coerceValue false innerdef other]
    | Object objdef -> coerceObjectValue objdef.Fields input
    | InputObject objdef -> coerceInputObjectValue objdef.Fields input
    | other -> raise (GraphQLException (sprintf "Cannot coerce value '%A' of type '%A'. Only Scalars, Nullables, Lists and Objects are valid type definitions." other typedef))
    
and private coerceInputObjectValue (fields: InputFieldDef list) (input: obj) =
    //TODO: this should be eventually coerced to complex object
    let map = input :?> Map<string, obj>
    let mapped = 
        fields
        |> List.map (fun field -> 
            let valueFound = Map.tryFind field.Name map |> Option.toObj
            (field.Name, coerceValue false field.Type valueFound))
        |> Map.ofList
    upcast mapped

and private coerceObjectValue (fields: FieldDef list) (input: obj) =
    let map = input :?> Map<string, obj>
    let mapped = 
        fields
        |> List.map (fun field -> 
            let valueFound = Map.tryFind field.Name map |> Option.toObj
            (field.Name, coerceValue false field.Type valueFound))
        |> Map.ofList
    upcast mapped
    
let inline private collectDefaultArgValue acc (argdef: InputFieldDef) =
    match argdef.DefaultValue with
    | Some defVal -> Map.add argdef.Name defVal acc
    | None -> acc

let private getArgumentValues (argDefs: InputFieldDef list) (args: Argument list) (variables: Map<string, obj>) : Map<string, obj> = 
    argDefs
    |> List.fold (fun acc argdef -> 
        match List.tryFind (fun (a: Argument) -> a.Name = argdef.Name) args with
        | Some argument ->
            match coerceAstValue variables argument.Value with
            | null -> collectDefaultArgValue acc argdef
            | value -> Map.add argdef.Name value acc
        | None -> collectDefaultArgValue acc argdef
    ) Map.empty

/// The result of execution. `Data` is the result of executing the
/// query, `Errors` is null if no errors occurred, and is a
/// non-empty array if an error occurred.
type ExecutionResult =
    {
        Data: IDictionary<string, obj> option
        Errors: GraphQLError [] option
    }

type ExecutionContext = 
    {
        Schema: ISchema
        RootValue: obj
        Document: Document
        Operation: OperationDefinition
        Fragments: FragmentDefinition list
        Variables: Map<string, obj>
        Errors: ConcurrentBag<GraphQLError>
    }

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
   
let private coerceVariable (schema: #ISchema) (vardef: VariableDefinition) (input: obj option) = 
    let typedef = 
        match schema.TryFindType (getTypeName vardef.Type) with
        | None -> raise (GraphQLException (sprintf "Variable '%s' expected value of type '%A', which cannot be used as an input type." vardef.VariableName vardef.Type))
        | Some t when not (t :? InputDef) -> raise (GraphQLException (sprintf "Variable '%s' expected value of type '%A', which cannot be used as an input type." vardef.VariableName vardef.Type))
        | Some t -> t :?> InputDef
    match input with
    | None -> 
        match vardef.DefaultValue with
        | Some defaultValue -> coerceValue false typedef defaultValue
        | None -> raise (GraphQLException (sprintf "Variable '%s' of required type '%A' was not provided." vardef.VariableName vardef.Type))
    | Some input -> coerceValue false typedef input

let private coerceVariables (schema: #ISchema) variables (inputs: Map<string, obj> option) =
    match inputs with
    | None -> Map.empty
    | Some vars -> 
        variables
        |> List.fold (fun acc vardef ->
            let variableName = vardef.VariableName
            let input = Map.tryFind variableName vars
            Map.add variableName (coerceVariable schema vardef input) acc) Map.empty
        
let inline private coerceArgument ctx (arg: Argument) =
    coerceBoolValue(coerceAstValue ctx.Variables arg.Value).Value

let private shouldSkip ctx (directive: Directive) =
    match directive.Name with
    | "skip" when not <| coerceArgument ctx directive.If -> false
    | "include" when  coerceArgument ctx directive.If -> false
    | _ -> true

let private doesFragmentTypeApply ctx fragment (objectType: ObjectDef) = 
    match fragment.TypeCondition with
    | None -> true
    | Some typeCondition ->
        match ctx.Schema.TryFindType typeCondition with
        | None -> false
        | Some conditionalType when conditionalType.Name = objectType.Name -> true
        | Some (Abstract conditionalType) -> ctx.Schema.IsPossibleType conditionalType objectType
        | _ -> false

// 6.5 Evaluating selection sets
let rec private collectFields ctx typedef selectionSet visitedFragments =
    selectionSet
    |> List.fold (collectField ctx typedef visitedFragments) [] 
    |> List.rev

and collectField ctx (typedef: ObjectDef) visitedFragments groupedFields (selection: Selection) =
    if List.exists (shouldSkip ctx) selection.Directives
    then groupedFields
    else 
        match selection with
        | Field field ->
            let name = field.AliasOrName
            match List.tryFind (fun (n, _) -> n = name) groupedFields with
            | Some (_, groupForResponseKey) -> (name, (field::groupForResponseKey))::groupedFields
            | None -> (name, [field])::groupedFields
        | FragmentSpread spread ->
            let fragmentSpreadName = spread.Name
            if List.exists (fun fragmentName -> fragmentName = fragmentSpreadName) !visitedFragments 
            then groupedFields
            else
                visitedFragments := (fragmentSpreadName::!visitedFragments)
                let found =
                    ctx.Document.Definitions
                    |> List.tryFind (fun def -> 
                        match def with
                        | FragmentDefinition f when f.Name.Value = fragmentSpreadName -> true
                        | _ -> false)
                match found with
                | Some (FragmentDefinition fragment) -> 
                    collectFragment ctx typedef visitedFragments groupedFields fragment
                | _ -> groupedFields
        | InlineFragment fragment -> 
            collectFragment ctx typedef visitedFragments groupedFields fragment

// this is a common part for inline framgent and fragents spread
and collectFragment ctx typedef visitedFragments groupedFields fragment =
    let fragmentType = fragment.TypeCondition
    if not <| doesFragmentTypeApply ctx fragment typedef
    then groupedFields
    else 
        let fragmentSelectionSet = fragment.SelectionSet
        let fragmentGroupedFieldSet = collectFields ctx typedef fragmentSelectionSet visitedFragments
        fragmentGroupedFieldSet
        |> List.fold (fun acc (responseKey, fragmentGroup) -> 
            match List.tryFind (fun (n, _) -> n = responseKey) acc with
            | Some (_, groupForResponseKey) -> (responseKey, (fragmentGroup @ groupForResponseKey))::acc
            | None -> (responseKey, fragmentGroup)::acc
        ) groupedFields
    
open FSharp.Data.GraphQL.Introspection

let private defaultResolveType ctx abstractDef objectValue =
    let possibleTypes = ctx.Schema.GetPossibleTypes abstractDef
    let mapper = match abstractDef with Union u -> u.ResolveValue | _ -> id
    possibleTypes
    |> List.find (fun objdef ->
        match objdef.IsTypeOf with
        | Some isTypeOf ->
            isTypeOf(mapper objectValue)
        | None -> false)
        
let private resolveInterfaceType ctx (interfacedef: InterfaceDef) objectValue = 
    match interfacedef.ResolveType with
    | Some resolveType -> resolveType(objectValue)
    | None -> defaultResolveType ctx interfacedef objectValue

let private resolveUnionType ctx (uniondef: UnionDef) objectValue = 
    match uniondef.ResolveType with
    | Some resolveType -> resolveType(objectValue)
    | None -> defaultResolveType ctx uniondef objectValue

//---------------------------
// ENTRY: 6.6.1 Field entries

/// Complete an ObjectType value by executing all sub-selections
let rec private completeObjectValue ctx objectType (fields: Field list) (result: obj) : Async<NameValueLookup> = async {
    let groupedFieldSet = 
        fields
        |> List.fold (fun subFields field -> collectFields ctx objectType field.SelectionSet (ref [])) []
    let! res = executeFields ctx objectType result groupedFieldSet 
    return res }

and private completeValue ctx fieldDef fields (result: obj): Async<obj> = async {
    match fieldDef with
    | Nullable innerdef -> 
        let unwrapped = match result with Option o -> o | o -> o
        return! completeValue ctx innerdef fields unwrapped
    | List innerdef ->
        match result with
        | :? string as s -> 
            let! res = completeValue ctx innerdef fields s
            return upcast [res]
        | :? System.Collections.IEnumerable as enumerable ->
            let! completed=
                enumerable
                |> Seq.cast<obj>
                |> Seq.map (completeValue ctx innerdef fields)
                |> Async.Parallel
            return upcast (completed |> List.ofArray)
        | _ -> return raise (GraphQLException (sprintf "Expected to have enumerable value on the list field '%s'" (fieldDef.ToString())))
    | Scalar scalardef -> 
        return scalardef.CoerceValue result |> Option.toObj
    | Enum _ -> 
        match coerceStringValue result with
        | Some s -> return (upcast s)
        | None -> return null
    | Object objectdef ->
        let! completed = completeObjectValue ctx objectdef fields result
        return upcast completed
    | Interface typedef -> 
        let objectdef = resolveInterfaceType ctx typedef result
        let! completed = completeObjectValue ctx objectdef fields result
        return upcast completed
    | Union uniondef ->
        let objectdef = resolveUnionType ctx uniondef result
        let! completed = completeObjectValue ctx objectdef fields (uniondef.ResolveValue result)
        return upcast completed }
                
/// Takes an object, current execution context and a field definition, and returns the result of resolving that field on the object
and private resolveField value ctx (fieldDef: FieldDef) = async {
    try
        let! resolved = fieldDef.Resolve ctx value 
        let unboxed = 
            match resolved with
            | Option o -> o     // if resolved value was an option unwrap it to object
            | o -> o
        return Choice1Of2 unboxed
    with
    | ex -> return Choice2Of2 (GraphQLError (ex.ToString())) }

/// Takes an object type and a field, and returns that field’s type on the object type, or null if the field is not valid on the object type
and private getFieldDefinition ctx (objectType: ObjectDef) (field: Field) : FieldDef option =
        match field.Name with
        | "__schema" when Object.ReferenceEquals(ctx.Schema.Query, objectType) -> Some (upcast SchemaMetaFieldDef)
        | "__type" when Object.ReferenceEquals(ctx.Schema.Query, objectType) -> Some (upcast TypeMetaFieldDef)
        | "__typename" -> Some (upcast TypeNameMetaFieldDef)
        | fieldName -> objectType.Fields |> List.tryFind (fun f -> f.Name = fieldName)

and private getFieldEntry ctx typedef value (fields: Field list) : Async<obj> = async {
    let firstField::_ = fields
    match getFieldDefinition ctx typedef firstField with
    | None -> return null
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
        }
        let! resolved = resolveField value resolveFieldCtx fieldDef 
        match resolved with
        | Choice1Of2 null -> return null
        | Choice1Of2 resolvedObject ->
            return! completeValue ctx fieldDef.Type fields resolvedObject
        | Choice2Of2 error ->
            ctx.Errors.Add error
            return null }

and private executeFields ctx typedef value groupedFieldSet = async {
    let result =
        groupedFieldSet
        |> List.toSeq
        |> Seq.map fst
        |> Seq.toArray
        |> NameValueLookup
    let! whenAll = 
        groupedFieldSet
        |> List.toSeq
        |> Seq.map (fun (responseKey, fields) -> async { 
            let! res = getFieldEntry ctx typedef value fields
            do result.Update responseKey res })
        |> Async.Parallel
    return result }

// EXIT
//---------------------------

and private executeFieldsSync ctx typedef value (groupedFieldSet: (string * Field list) list) = async {
    let result =
        groupedFieldSet
        |> List.toSeq
        |> Seq.map fst
        |> Seq.toArray
        |> NameValueLookup
    groupedFieldSet
    |> List.iter (fun (responseKey, fields) -> result.Update responseKey ((getFieldEntry ctx typedef value fields) |> Async.RunSynchronously))
    return result }

let private evaluate (schema: #ISchema) doc operation variables root errors = async {
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
        let groupedFieldSet = collectFields ctx schema.Mutation.Value operation.SelectionSet  (ref [])
        return! executeFieldsSync ctx schema.Mutation.Value ctx.RootValue groupedFieldSet
    | Query ->
        let groupedFieldSet = collectFields ctx schema.Query operation.SelectionSet  (ref [])
        return! executeFields ctx schema.Query ctx.RootValue groupedFieldSet }

let private execute (schema: #ISchema) doc operationName variables root errors = async {
    match findOperation doc operationName with
    | Some operation -> return! evaluate schema doc operation variables root errors
    | None -> return raise (GraphQLException "No operation with specified name has been found for provided document") }

open FSharp.Data.GraphQL.Parser
type FSharp.Data.GraphQL.Schema with

    member schema.AsyncExecute(ast: Document, ?data: obj, ?variables: Map<string, obj>, ?operationName: string): Async<ExecutionResult> =
        async {
            try
                let errors = ConcurrentBag()
                let! result = execute schema ast operationName variables data errors
                return { Data = Some (upcast result); Errors = if errors.IsEmpty then None else Some (errors.ToArray()) }
            with 
            | ex -> 
                let msg = ex.ToString()
                return { Data = None; Errors = Some [| GraphQLError msg |]} }

    member schema.AsyncExecute(queryOrMutation: string, ?data: obj, ?variables: Map<string, obj>, ?operationName: string): Async<ExecutionResult> =
        async {
            try
                let ast = parse queryOrMutation
                let errors = ConcurrentBag()
                let! result = execute schema ast operationName variables data errors
                return { Data = Some (upcast result); Errors = if errors.IsEmpty then None else Some (errors.ToArray()) }
            with 
            | ex -> 
                let msg = ex.ToString()
                return { Data = None; Errors = Some [| GraphQLError msg |]} }