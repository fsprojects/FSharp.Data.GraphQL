/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Execution

open System
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

let private getArgumentValues (argDefs: ArgumentDefinition list) (args: Argument list) (variables: Map<string, obj option>) : Map<string, obj> = 
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
        Errors: GraphQLError list option
    }

type ExecutionContext = 
    {
        Schema: Schema
        RootValue: obj
        Document: Document
        Variables: Map<string, obj option>
    }

let private findOperation (schema: Schema) doc opName =
    match doc.Definitions, opName with
    | [OperationDefinition def], _ -> Some def
    | defs, name -> 
        defs
        |> List.choose (fun def -> 
            match def with
            | OperationDefinition o -> Some o
            | _ -> None)
        |> List.tryFind (fun def -> def.Name = name)
    | _ -> None
   
let private coerceVariables (schema: Schema) variables inputs =
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

let private doesFragmentTypeApply ctx typedef = function
    | Some typeName ->
        match ctx.Schema.TryFindType typeName with
        | Some fragmentType ->
            match fragmentType, typedef with
            | Object x, Object y -> Object.ReferenceEquals(x, y)
            | Interface _, Object o -> 
                o.Implements
                |> List.exists (fun i -> i = fragmentType)
            | Union u, Object o ->
                u.Options 
                |> List.exists (fun o -> o = fragmentType)
            | _, _ -> false
        | None -> false
    | None -> false

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
    if not <| doesFragmentTypeApply ctx ctx.Schema.Query fragmentType
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
let private resolveField _ fieldDef value args resolveInfo =
    fieldDef.Resolve value args resolveInfo |> Option.ofObj

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
let rec private completeObjectValue ctx objectType (fields: Field list) (result: obj) = 
    let typedef = Object objectType
    let groupedFieldSet = 
        fields
        |> List.fold (fun subFields field -> collectFields ctx typedef field.SelectionSet (ref [])) Map.empty
    let res = executeFields ctx typedef result groupedFieldSet 
    res

and completeValue ctx fieldType fields (result: obj)  =
    match fieldType with
    | NonNull innerType ->
        match completeValue ctx innerType fields result  with
        | null -> raise (GraphQLException (sprintf "Null value is not allowed on the field '%s'" fieldType.Name))
        | completedResult -> completedResult
    | ListOf innerType ->
        match result with
        | :? System.Collections.IEnumerable as enumerable ->
            enumerable
            |> Seq.cast<obj>
            |> Seq.map (completeValue ctx innerType fields)
            |> List.ofSeq
            |> box
        | _ -> raise (GraphQLException (sprintf "Expected to have enumerable value on the list field '%s'" fieldType.Name))
    | Scalar scalarType -> 
        scalarType.CoerceValue result |> Option.toObj
    | Enum enumType -> 
        match coerceStringValue result with
        | Some s -> upcast s
        | None -> null
    | InputObject objectType | Object objectType ->
        upcast completeObjectValue ctx objectType fields result
    | Interface typedef ->
        let objectType = resolveInterfaceType typedef result
        upcast completeObjectValue ctx objectType fields result
    | Union typedef ->
        let objectType = resolveUnionType typedef result
        upcast completeObjectValue ctx objectType fields result

// 6.6.1 Field entries
and getFieldEntry ctx typedef value (fields: Field list) = 
    let firstField::_ = fields
    match getFieldDefinition ctx typedef firstField with
    | None -> null
    | Some fieldDef -> 
        let resolveInfo = {
            FieldDefinition = fieldDef
            Fields = fields
            Fragments = Map.empty
            RootValue = value
        }
        let args = getArgumentValues fieldDef.Arguments firstField.Arguments ctx.Variables
        match resolveField ctx fieldDef value {Args = args} resolveInfo with
        | None -> null
        | Some resolvedObject ->
            completeValue ctx fieldDef.Type fields resolvedObject

and executeFields ctx typedef value (groupedFieldSet): Map<string, obj> = 
    let result = 
        groupedFieldSet
        |> Map.fold (fun acc responseKey fields -> Map.add responseKey (getFieldEntry ctx typedef value fields) acc) Map.empty
    result

let private evaluate (schema: Schema) doc operation variables root = 
    let variables = coerceVariables schema operation.VariableDefinitions variables
    let ctx = {
        Schema = schema
        RootValue = root
        Document = doc
        Variables = variables
    }
    match operation.OperationType with
    | Mutation -> 
        // at the moment both execution procedures works the same
        // ultimately only difference is that mutation requires to 
        // maintain order of executed resolve invocations, while query doesn't
        let groupedFieldSet = collectFields ctx schema.Query operation.SelectionSet  (ref [])
        executeFields ctx schema.Query ctx.RootValue groupedFieldSet
    | Query ->
        let groupedFieldSet = collectFields ctx schema.Query operation.SelectionSet  (ref [])
        executeFields ctx schema.Query ctx.RootValue groupedFieldSet

let execute (schema: Schema) doc operationName variables root = 
    match findOperation schema doc operationName with
    | Some operation -> evaluate schema doc operation variables root
    | None -> raise (GraphQLException "No operation with specified name has been found for provided document")

type FSharp.Data.GraphQL.Schema with
    member schema.Execute(ast: Document, data: obj, ?variables: Map<string, obj>, ?operationName: string): ExecutionResult =
        try
            let result = execute schema ast operationName variables data 
            { Data = Some result; Errors = None }
        with 
        | ex -> { Data = None; Errors = Some [ GraphQLError ex.Message ]}