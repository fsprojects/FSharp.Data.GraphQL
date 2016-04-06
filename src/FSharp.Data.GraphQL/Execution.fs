/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Execution

open System
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types

let rec private getTypeName = function
    | NamedType name -> name
    | ListType inner -> getTypeName inner
    | NonNullType inner -> getTypeName inner

let rec private isValidValue (schema: Schema) typedef value =
    match typedef with
    | Scalar scalardef -> scalardef.CoerceValue(value) <> null
    | NonNull innerdef -> if value = null then false else isValidValue schema innerdef value
    | ListOf innerdef  -> 
        match value with
        | null -> true
        | :? System.Collections.IEnumerable as iter -> 
            iter
            |> Seq.cast<obj>
            |> Seq.forall (fun v -> isValidValue schema innerdef v)
        | _ -> false
    | Object objdef | InputObject objdef ->
        let map = value :?> Map<string, obj>
        objdef.Fields
        |> List.forall (fun field -> isValidValue schema field.Schema map.[field.Name])
    | _ -> false
            

let rec private coerceValue schema typedef (input: obj) variables = 
    match typedef with
    | Scalar scalardef -> scalardef.CoerceValue input
    | NonNull innerdef -> 
        match coerceValue schema innerdef input variables with
        | null -> raise (GraphQLException (sprintf "Value '%A' has been coerced to null, but type definition mark corresponding field as non nullable" input))
        | other -> other
    | ListOf innerdef ->
        match input with
        | null -> null
        | :? System.Collections.IEnumerable as iter -> 
            let mapped =
                iter
                |> Seq.cast<obj>
                |> Seq.map (fun elem -> coerceValue schema innerdef elem variables)
            upcast mapped
        | other -> raise (GraphQLException (sprintf "Value '%A' was expected to be an enumberable collection" input))
    | Object objdef | InputObject objdef ->
        let map = input :?> Map<string, obj>
        let mapped = 
            objdef.Fields
            |> List.map (fun field -> (field.Name, coerceValue schema field.Schema map.[field.Name] variables))
            |> Map.ofList
        upcast mapped
    | other -> raise (GraphQLException (sprintf "Cannot coerce defintion '%A'. Only Scalars, NonNulls, Lists and Objects are valid type definitions." other))

let private getVariableValue (schema: Schema) (variable: VariableDefinition) raw =
    let typeName = getTypeName variable.Type
    match schema.TryFindType typeName with
    | Some typedef when isValidValue schema typedef raw -> coerceValue schema typedef raw Map.empty
    | Some typedef -> raise (GraphQLException (sprintf "Value '%A' is not valid for variable of type '%s'" raw typedef.Name))
    | None -> raise (GraphQLException (sprintf "Couldn't find definition of type '%s' in current schema" typeName))

let private getVariableValues schema variableDefs inputs =
    variableDefs
    |> List.map (fun variable ->
        let value = 
            match Map.tryFind variable.VariableName inputs with
            | Some v -> getVariableValue schema variable v
            | None ->
                match variable.DefaultValue with
                | Some v -> getVariableValue schema variable v
                | None -> raise (GraphQLException (sprintf "Variable '$%s' has no value specified in current context" variable.VariableName))
        (variable.VariableName, value))
    |> Map.ofList
   
let private getArgumentValue schema argument =
    ()

/// Data that must be available at all points during query execution. 
/// Namely, schema of the type system that is currently executing,
/// and the fragments defined in the query document
type ExecutionContext = 
    {
        Schema: Schema
        Fragments: Map<string, FragmentDefinition>
        RootValue: obj
        ContextValue: obj
        Operation: OperationDefinition
        Variables: Map<string, obj>
        Errors: GraphQLError list
    }
    static member Create(schema: Schema, document: Document, rootValue: obj, contextValue: obj, rawVariables: Map<string, obj>, operationName: string option) =
        let (o, f) = 
            document.Definitions
            |> List.partition (fun def -> 
                match def with
                | OperationDefinition _ -> true
                | FragmentDefinition _ -> false)
        let operations = o |> List.map (fun x -> match x with | OperationDefinition def -> def)
        let fragments = 
            f 
            |> List.map (fun x -> match x with | FragmentDefinition def -> (def.Name.Value, def))
            |> Map.ofList
        if operations.Length > 1 && operationName.IsNone
        then raise (GraphQLException "Must provide operation name if query contains multiple operations.")
        let operation = 
            match operationName with
            | Some name -> operations |> List.find (fun o -> o.Name.IsSome && o.Name.Value = name)
            | _ -> operations.Head
        let variables = getVariableValues schema operation.VariableDefinitions rawVariables
        {
            Schema = schema
            Fragments = fragments
            RootValue = rootValue
            ContextValue = contextValue   
            Operation = operation
            Variables = variables
            Errors = []
        }

/// The result of execution. `Data` is the result of executing the
/// query, `Errors` is null if no errors occurred, and is a
/// non-empty array if an error occurred.
type ExecutionResult =
    {
        Data: obj option
        Errors: GraphQLError list option
    }

let private getOperationRootType (schema: Schema) operation = 
    match operation.OperationType with
    | Query -> schema.Query
    | Mutation -> 
        match schema.Mutation with
        | Some mutationType -> mutationType
        | None -> raise (NotSupportedException "This GraphQL schema has not root mutation type defined")

let private shouldIncludeNode ctx (directives: Directive list) =
    let shouldInclude (directive: Directive) = 
        match directive.Name with
        | "skip" -> not (coerceBoolValue(getArgumentValue ctx directive.If).Value)
        | "include" -> coerceBoolValue(getArgumentValue ctx directive.If).Value

    directives
    |> List.forall shouldInclude

let rec private collectFields ctx typedef selectionSet fields visitedFragments = 
    selectionSet
    |> List.fold (fun acc field -> collectField ctx acc field) fields
and collectField ctx fields = function
    | Field field ->
        if shouldIncludeNode ctx field.Directives
        then 
            let name = 
                match field.Alias with
                | Some alias -> alias
                | None -> field.Name
            match Map.tryFind name fields with
            | Some list -> Map.add name (field::list) fields
            | None -> Map.add name [field] fields
        else fields
    | FragmentSpread spread -> fields
    | InlineFragment fragdef -> fields

let private executeFields ctx typedef sourceVal fields = obj()

let executeOperation context (operation: OperationDefinition) rootValue = async {
    let rootType = getOperationRootType context.Schema operation
    let fields = collectFields context rootType operation.SelectionSet Map.empty Map.empty
    match operation.OperationType with
    | Query -> return executeFields context rootType rootValue fields
    | Mutation -> return raise (NotSupportedException "Mutations are not supported yet")
}

type FSharp.Data.GraphQL.Schema with
    member x.Execute(document: Document, ?rootValue: obj, ?contextValue: obj, ?variables: Map<string, obj>, ?operationName: string): Async<ExecutionResult> =
        let v =
            match variables with
            | Some vars -> vars
            | None -> Map.empty
        let ctx = ExecutionContext.Create(x, document, rootValue, contextValue, v, operationName)
        async {
            try
                let! result = executeOperation ctx ctx.Operation rootValue
                match ctx.Errors with
                | []    -> return { Data = Some result; Errors = None }
                | other -> return { Data = None; Errors = Some other }
            with
            | :? GraphQLException as error -> 
                return { Data = None; Errors = Some ((GraphQLError error.Message) :: ctx.Errors) }
        }