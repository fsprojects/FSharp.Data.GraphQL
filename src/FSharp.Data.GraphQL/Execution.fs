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
let rec private coerceValue typedef (input: obj) = 
    match typedef with
    | Scalar scalardef -> scalardef.CoerceValue input
    | NonNull innerdef -> 
        match coerceValue innerdef input with
        | None -> raise (GraphQLException (sprintf "Value '%A' has been coerced to null, but type definition mark corresponding field as non nullable" input))
        | other -> other
    | ListOf innerdef ->
        match input with
        | null -> None
        | :? System.Collections.IEnumerable as iter -> 
            let mapped =
                iter
                |> Seq.cast<obj>
                |> Seq.map (fun elem -> coerceValue innerdef elem)
            Some(upcast mapped)
        | other -> raise (GraphQLException (sprintf "Value '%A' was expected to be an enumberable collection" input))
    | Object objdef | InputObject objdef ->
        let map = input :?> Map<string, obj>
        let mapped = 
            objdef.Fields
            |> List.map (fun field -> (field.Name, coerceValue field.Type map.[field.Name]))
            |> Map.ofList
        Some(upcast mapped)
    | other -> raise (GraphQLException (sprintf "Cannot coerce defintion '%A'. Only Scalars, NonNulls, Lists and Objects are valid type definitions." other))

let private getArgumentValues (argDefs: ArgDef list) (args: Argument list) (variables: Map<string, obj option>) : Map<string, obj> = 
    argDefs
    |> List.fold (fun acc argdef -> 
        match List.tryFind (fun (a: Argument) -> a.Name = argdef.Name) args with
        | Some argument ->
            match coerceAstValue variables argument.Value with
            | null -> acc
            | value -> Map.add argdef.Name value acc
        | None -> 
            match argdef.DefaultValue with
            | Some defVal -> Map.add argdef.Name defVal acc
            | None -> acc
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
        Variables: Map<string, obj option>
        Errors: ConcurrentBag<GraphQLError>
    }

let private getOperation = function
    | OperationDefinition odef -> Some odef
    | _ -> None

let private findOperation (schema: #ISchema) doc opName =
    match doc.Definitions |> List.choose getOperation, opName with
    | [def], _ -> Some def
    | defs, name -> 
        defs
        |> List.tryFind (fun def -> def.Name = name)
    | _ -> None
   
let private coerceVariables (schema: #ISchema) variables inputs =
    match inputs with
    | None -> Map.empty
    | Some vars -> 
        variables
        |> List.fold (fun acc (var: VariableDefinition) -> 
            let typedef = 
                match schema.TryFindType (getTypeName var.Type) with
                | None -> raise (GraphQLException (sprintf "Variable '%s' is of type '%A', which is not defined in current schema" var.VariableName var.Type ))
                | Some t -> t
            let value = 
                match Map.tryFind var.VariableName vars with
                | None -> coerceValue typedef var.DefaultValue.Value // this may throw if variable has no input and no default value defined
                | Some input -> coerceValue typedef input
            Map.add var.VariableName value acc) Map.empty
        
let inline private coerceArgument ctx (arg: Argument) =
    coerceBoolValue(coerceAstValue ctx.Variables arg.Value).Value

let private shouldSkip ctx (directive: Directive) =
    match directive.Name with
    | "skip" when coerceArgument ctx directive.If -> false
    | "include" when not <| coerceArgument ctx directive.If -> false
    | _ -> true

let private doesFragmentTypeApply ctx fragment (typedef:TypeDef) = 
    match fragment.TypeCondition with
    | None -> true
    | Some typeCondition ->
        match ctx.Schema.TryFindType typeCondition with
        | None -> false
        | Some conditionalType when conditionalType.Name = typedef.Name -> true
        | Some conditionalType -> ctx.Schema.IsPossibleType conditionalType typedef

// 6.5 Evaluating selection sets
let rec private collectFields ctx typedef selectionSet visitedFragments =
    selectionSet
    |> List.fold (collectField ctx typedef visitedFragments) Map.empty 

and collectField ctx typedef visitedFragments groupedFields (selection: Selection) =
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
    | ex -> return Choice2Of2 (GraphQLError ex.Message) }

open FSharp.Data.GraphQL.Introspection
/// Takes an object type and a field, and returns that field’s type on the object type, or null if the field is not valid on the object type
let private getFieldDefinition ctx typedef (field: Field) =
    match field.Name with
    | "__schema" when ctx.Schema.Query = typedef -> Some SchemaMetaFieldDef
    | "__type" when ctx.Schema.Query = typedef -> Some TypeMetaFieldDef
    | "__typename" -> Some TypeNameMetaFieldDef
    | fieldName ->
        match typedef with
        | Object objectType -> objectType.Fields |> List.tryFind (fun f -> f.Name = fieldName)
        | _ -> raise (GraphQLException (sprintf "Tried to retrieve '%s' field from non-object type '%s'" fieldName typedef.Name))
        
let private resolveInterfaceType interfaceType objectValue = failwith "Not implemented"

let private resolveUnionType unionType objectValue = failwith "Not implemented"

/// Complete an ObjectType value by executing all sub-selections
let rec private completeObjectValue ctx objectType (fields: Field list) (result: obj) = async {
    let typedef = Object objectType
    let groupedFieldSet = 
        fields
        |> List.fold (fun subFields field -> collectFields ctx typedef field.SelectionSet (ref [])) Map.empty
    let! res = executeFields ctx typedef result groupedFieldSet 
    return res }

and private completeValue ctx fieldType fields (result: obj): Async<obj> = async {
    match fieldType with
    | NonNull innerType ->
        let! completed = completeValue ctx innerType fields result
        match completed with
        | null -> return raise (GraphQLException (sprintf "Null value is not allowed on the field '%s'" fieldType.Name))
        | completedResult -> return completedResult
    | ListOf innerType ->
        match result with
        | :? System.Collections.IEnumerable as enumerable ->
            let! completed=
                enumerable
                |> Seq.cast<obj>
                |> Seq.map (completeValue ctx innerType fields)
                |> Async.Parallel
            return upcast (completed |> List.ofArray)
        | _ -> return raise (GraphQLException (sprintf "Expected to have enumerable value on the list field '%s'" fieldType.Name))
    | Scalar scalarType -> 
        return scalarType.CoerceValue result |> Option.toObj
    | Enum enumType -> 
        match coerceStringValue result with
        | Some s -> return (upcast s)
        | None -> return null
    | InputObject objectType | Object objectType ->
        let! completed = completeObjectValue ctx objectType fields result
        return upcast completed
    | Interface typedef ->
        let objectType = resolveInterfaceType typedef result
        let! completed = completeObjectValue ctx objectType fields result
        return upcast completed
    | Union typedef ->
        let objectType = resolveUnionType typedef result
        let! completed = completeObjectValue ctx objectType fields result
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
    match findOperation schema doc operationName with
    | Some operation -> return! evaluate schema doc operation variables root errors
    | None -> return raise (GraphQLException "No operation with specified name has been found for provided document") }

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