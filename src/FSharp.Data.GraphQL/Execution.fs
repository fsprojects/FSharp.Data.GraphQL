/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Execution

open System
open System.Collections.Concurrent
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types

let private option = function
    | null -> None
    | other -> Some other

let rec private getTypeName = function
    | NamedType name -> name
    | ListType inner -> getTypeName inner
    | NonNullType inner -> getTypeName inner
//
//let rec private isValidValue (schema: Schema) typedef value =
//    match typedef with
//    | Scalar scalardef -> scalardef.CoerceValue(value) <> null
//    | NonNull innerdef -> if value = null then false else isValidValue schema innerdef value
//    | ListOf innerdef  -> 
//        match value with
//        | null -> true
//        | :? System.Collections.IEnumerable as iter -> 
//            iter
//            |> Seq.cast<obj>
//            |> Seq.forall (fun v -> isValidValue schema innerdef v)
//        | _ -> false
//    | Object objdef | InputObject objdef ->
//        let map = value :?> Map<string, obj>
//        objdef.Fields
//        |> List.forall (fun field -> isValidValue schema field.Type map.[field.Name])
//    | _ -> false
//            
//
let rec private coerceValue typedef (input: obj) : obj = 
    match typedef with
    | Scalar scalardef -> scalardef.CoerceValue input |> Option.toObj
    | NonNull innerdef -> 
        match coerceValue innerdef input with
        | null -> raise (GraphQLException (sprintf "Value '%A' has been coerced to null, but type definition mark corresponding field as non nullable" input))
        | other -> other
    | List innerdef ->
        match input with
        | null -> null
        // special case - while single values should be wrapped with a list in this scenario,
        // string would be treat as IEnumerable and coerced into a list of chars
        | :? string as s -> upcast [coerceValue innerdef s]
        | :? System.Collections.IEnumerable as iter -> 
            let mapped =
                iter
                |> Seq.cast<obj>
                |> Seq.map (fun elem -> coerceValue innerdef elem)
                |> List.ofSeq
            upcast mapped
        | other -> upcast [coerceValue innerdef other]
    | Object objdef -> coerceObjectValue objdef.Fields input
    | InputObject objdef -> coerceObjectValue objdef.Fields input
    | other -> raise (GraphQLException (sprintf "Cannot coerce value '%A' of type '%A'. Only Scalars, NonNulls, Lists and Objects are valid type definitions." other typedef))

and private coerceObjectValue (fields: FieldDef list) (input: obj) =
    let map = input :?> Map<string, obj>
    let mapped = 
        fields
        |> List.map (fun field -> 
            let valueFound = Map.tryFind field.Name map |> Option.toObj
            (field.Name, coerceValue field.Type valueFound))
        |> Map.ofList
    upcast mapped
    
let inline private collectDefaultArgValue acc (argdef: ArgDef) =
    match argdef.DefaultValue with
    | Some defVal -> Map.add argdef.Name defVal acc
    | None -> acc

let private getArgumentValues (argDefs: ArgDef list) (args: Argument list) (variables: Map<string, obj>) : Map<string, obj> = 
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
        Data: Map<string,obj> option
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
        | Some defaultValue -> coerceValue typedef defaultValue
        | None -> raise (GraphQLException (sprintf "Variable '%s' of required type '%A' was not provided." vardef.VariableName vardef.Type))
    | Some input -> coerceValue typedef input

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
    |> List.fold (collectField ctx typedef visitedFragments) Map.empty 

and collectField ctx (typedef: ObjectDef) visitedFragments groupedFields (selection: Selection) =
    if List.exists (shouldSkip ctx) selection.Directives
    then groupedFields
    else 
        match selection with
        | Field field ->
            let name = field.AliasOrName
            match Map.tryFind name groupedFields with
            | Some groupForResponseKey -> Map.add name (field::groupForResponseKey) groupedFields
            | None -> Map.add name [field] groupedFields
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
        |> Map.fold (fun acc responseKey fragmentGroup -> 
            match Map.tryFind responseKey acc with
            | Some groupForResponseKey -> Map.add responseKey (fragmentGroup @ groupForResponseKey) acc
            | None -> Map.add responseKey (fragmentGroup) acc
        ) groupedFields    
                
/// Takes an object type, a field, and an object, and returns the result of resolving that field on the object
let private resolveField value ctx fieldDef = async {
    try
        let! resolved = fieldDef.Resolve ctx value 
        return Choice1Of2 (Option.ofObj resolved )
    with
    | ex -> 
        let error = (value, ctx, fieldDef)
        return Choice2Of2 (GraphQLError ex.Message) }

open FSharp.Data.GraphQL.Introspection
/// Takes an object type and a field, and returns that field’s type on the object type, or null if the field is not valid on the object type
let private getFieldDefinition ctx (objectType: ObjectDef) (field: Field) =
    let result =
        match field.Name with
        | "__schema" when Object.ReferenceEquals(ctx.Schema.Query, objectType) -> Some SchemaMetaFieldDef
        | "__type" when Object.ReferenceEquals(ctx.Schema.Query, objectType) -> Some TypeMetaFieldDef
        | "__typename" -> Some TypeNameMetaFieldDef
        | fieldName -> objectType.Fields |> List.tryFind (fun f -> f.Name = fieldName)
    result

let private defaultResolveType ctx abstractDef objectValue =
    let possibleTypes = ctx.Schema.GetPossibleTypes abstractDef
    possibleTypes
    |> List.find (fun objdef ->
        match objdef.IsTypeOf with
        | Some isTypeOf ->
            isTypeOf(objectValue)
        | None -> false)
        
let private resolveInterfaceType ctx (interfacedef: InterfaceDef) objectValue = 
    match interfacedef.ResolveType with
    | Some resolveType -> resolveType(objectValue)
    | None -> defaultResolveType ctx interfacedef objectValue

let private resolveUnionType ctx (uniondef: UnionDef) objectValue = 
    match uniondef.ResolveType with
    | Some resolveType -> resolveType(objectValue)
    | None -> defaultResolveType ctx uniondef objectValue

/// Complete an ObjectType value by executing all sub-selections
let rec private completeObjectValue ctx objectType (fields: Field list) (result: obj) = async {
    let groupedFieldSet = 
        fields
        |> List.fold (fun subFields field -> collectFields ctx objectType field.SelectionSet (ref [])) Map.empty
    let! res = executeFields ctx objectType result groupedFieldSet 
    return res }

and private completeValue ctx fieldDef fields (result: obj): Async<obj> = async {
    match fieldDef with
    | NonNull innerdef ->
        let! completed = completeValue ctx innerdef fields result
        match completed with
        | null -> return raise (GraphQLException (sprintf "Null value is not allowed on the field '%s'" (fieldDef.ToString())))
        | completedResult -> return completedResult
    | List innerdef ->
        match result with
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
    | Union typedef ->
        let objectdef = resolveUnionType ctx typedef result
        let! completed = completeObjectValue ctx objectdef fields result
        return upcast completed }

// 6.6.1 Field entries
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
        | Choice1Of2 None -> return null
        | Choice1Of2 (Some resolvedObject) ->
            return! completeValue ctx fieldDef.Type fields resolvedObject
        | Choice2Of2 error ->
            ctx.Errors.Add error
            return null }

and private executeFields ctx typedef value (groupedFieldSet): Async<Map<string, obj>> = async {
    let! whenAll = 
        groupedFieldSet
        |> Map.toSeq
        |> Seq.map (fun (responseKey, fields) -> async { 
            let! res = getFieldEntry ctx typedef value fields
            return responseKey, res })
        |> Async.Parallel
    return whenAll
        |> Array.fold (fun acc (responseKey, res) -> Map.add responseKey res acc) Map.empty }

and private executeFieldsSync ctx typedef value (groupedFieldSet): Async<Map<string, obj>> = async {
    let result = 
        groupedFieldSet
        |> Map.fold (fun acc responseKey fields -> Map.add responseKey ((getFieldEntry ctx typedef value fields) |> Async.RunSynchronously) acc) Map.empty
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
                return { Data = Some result; Errors = if errors.IsEmpty then None else Some (errors.ToArray()) }
            with 
            | ex -> 
                let msg = ex.Message
                return { Data = None; Errors = Some [| GraphQLError msg |]} }

    member schema.AsyncExecute(queryOrMutation: string, ?data: obj, ?variables: Map<string, obj>, ?operationName: string): Async<ExecutionResult> =
        async {
            try
                let ast = parse queryOrMutation
                let errors = ConcurrentBag()
                let! result = execute schema ast operationName variables data errors
                return { Data = Some result; Errors = if errors.IsEmpty then None else Some (errors.ToArray()) }
            with 
            | ex -> 
                let msg = ex.Message
                return { Data = None; Errors = Some [| GraphQLError msg |]} }