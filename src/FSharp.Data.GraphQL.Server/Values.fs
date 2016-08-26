[<AutoOpen>]
module internal FSharp.Data.GraphQL.Values

open System
open System.Reflection
open System.Collections.Generic
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types

/// Tries to convert type defined in AST into one of the type defs known in schema.
let inline tryConvertAst schema ast =
    let rec convert isNullable (schema: ISchema) (ast: InputType) : TypeDef option =
        match ast with
        | NamedType name -> 
            match schema.TryFindType name with
            | Some namedDef ->
                Some (if isNullable then upcast namedDef.MakeNullable() else upcast namedDef)
            | None -> None
        | ListType inner ->
            convert true schema inner
            |> Option.map (fun i -> 
                if isNullable 
                then upcast i.MakeList().MakeNullable()
                else upcast i.MakeList())
        | NonNullType inner ->
            convert false schema inner
    convert true schema ast

let private findMatchingConstructor (indef: InputObjectDef) (constructors: ConstructorInfo []) =
    let fieldNames = 
        indef.Fields
        |> Array.map (fun f -> f.Name)
        |> Set.ofArray
    let (ctor, _) =
        constructors
        |> Array.map (fun ctor -> (ctor, ctor.GetParameters() |> Array.map (fun param -> param.Name)))
        // start from most complete constructors
        |> Array.sortBy (fun (_, paramNames) -> -paramNames.Length)                  
        // try match field with params by name
        // at last, default constructor should be used if defined
        |> Array.find (fun (_, paramNames) -> Set.isSubset (Set.ofArray paramNames) fieldNames)    
    ctor

let inline private notAssignableMsg (innerDef: InputDef) value : string =
    sprintf "value of type %s is not assignable from %s" innerDef.InputType.Name (value.GetType().Name)

let rec compileByType (errMsg: string) (inputDef: InputDef): ExecuteInput =
    match inputDef with
    | Scalar scalardef -> 
        variableOrElse (scalardef.CoerceInput >> Option.toObj)
    | InputObject objdef -> 
        let objtype = objdef.InputType
        let ctor = findMatchingConstructor objdef (objtype.GetConstructors())
        let mapper =
            ctor.GetParameters()
            |> Array.map(fun param -> objdef.Fields |> Array.find(fun field -> field.Name = param.Name))

        fun variables value ->
            match value with
            | ObjectValue props ->
                let args = 
                    mapper 
                    |> Array.map (fun field -> 
                        match Map.tryFind field.Name props with
                        | None -> null
                        | Some prop -> field.ExecuteInput variables prop)
                let instance = ctor.Invoke(args)
                instance
            | Variable variableName -> 
                match Map.tryFind variableName variables with
                | Some found -> found
                | None -> null
            | _ -> null                 
    | List (Input innerdef) -> 
        let inner = compileByType errMsg innerdef       
        let cons, nil = ReflectionHelper.listOfType innerdef.InputType

        fun variables value ->
            match value with
            | ListValue list ->
                let mappedValues = list |> List.map (inner variables)
                nil |> List.foldBack cons mappedValues
            | Variable variableName -> variables.[variableName]
            | _ -> 
                // try to construct a list from single element
                let single = inner variables value
                if single = null then null else cons single nil
    | Nullable (Input innerdef) -> 
        let inner = compileByType errMsg innerdef
        let some, none = ReflectionHelper.optionOfType innerdef.InputType
                
        fun variables value ->
            let i = inner variables value
            match i with
            | null -> none
            | coerced -> 
                let c = some coerced
                if c <> null then c
                else raise(GraphQLException (errMsg + notAssignableMsg innerdef coerced))
    | Enum enumdef -> 
        fun variables value ->
            match value with
            | Variable variableName -> variables.[variableName]
            | _ ->
                let coerced = coerceStringInput value
                match coerced with
                | None -> null
                | Some s -> Enum.Parse(enumdef.InputType, s, ignoreCase = true)
    | _ -> failwithf "Unexpected value of inputDef: %O" inputDef
                
let rec private coerceVariableValue isNullable typedef (vardef: VariableDefinition) (input: obj) (errMsg: string) : obj = 
    match typedef with
    | Scalar scalardef -> 
        match scalardef.CoerceValue input with
        | None when isNullable -> null
        | None -> 
            raise (GraphQLException <| errMsg + (sprintf "expected value of type %O but got None" scalardef.InputType))
        | Some res -> res
    | Nullable (Input innerdef) -> 
        let some, none = ReflectionHelper.optionOfType innerdef.InputType
        let coerced = coerceVariableValue true innerdef vardef input errMsg
        if coerced <> null
        then 
            let s = some coerced
            if s <> null
            then s 
            else raise (GraphQLException <| errMsg + (sprintf "value of type %O is not assignable from %O" innerdef.InputType (coerced.GetType())))
        else none
    | List (Input innerdef) ->
        let cons, nil = ReflectionHelper.listOfType innerdef.InputType
        match input with
        | null when isNullable -> null
        | null -> raise(GraphQLException <| errMsg + (sprintf "expected value of type %O, but no value was found." vardef.Type))
        // special case - while single values should be wrapped with a list in this scenario,
        // string would be treat as IEnumerable and coerced into a list of chars
        | :? string as s -> 
            let single = coerceVariableValue false innerdef vardef s (errMsg + "element ")
            cons single nil
        | :? System.Collections.IEnumerable as iter -> 
            let mapped =
                iter
                |> Seq.cast<obj>
                |> Seq.map (fun elem -> coerceVariableValue false innerdef vardef elem (errMsg + "list element "))
                //TODO: optimize
                |> Seq.toList
                |> List.rev
                |> List.fold (fun acc coerced -> cons coerced acc) nil
            mapped
        | other -> raise (GraphQLException <| errMsg + (sprintf "Cannot coerce value of type '%O' to list." (other.GetType())))
    | InputObject objdef -> coerceVariableInputObject objdef vardef input (errMsg + (sprintf "in input object '%s': " objdef.Name))
    | _ -> raise (GraphQLException <| errMsg + "Only Scalars, Nullables, Lists and InputObjects are valid type definitions.")

and private coerceVariableInputObject (objdef) (vardef: VariableDefinition) (input: obj) errMsg =
    //TODO: this should be eventually coerced to complex object
    match input with
    | :? Map<string, obj> as map ->
        let mapped = 
            objdef.Fields
            |> Array.map (fun field -> 
                let valueFound = Map.tryFind field.Name map |> Option.toObj
                (field.Name, coerceVariableValue false field.Type vardef valueFound (errMsg + (sprintf "in field '%s': " field.Name))))
            |> Map.ofArray
        upcast mapped
    | _ -> input

let coerceVariable (schema: #ISchema) (vardef: VariableDefinition) (inputs) = 
    let typedef = 
        match tryConvertAst schema vardef.Type with
        | None -> raise (GraphQLException (sprintf "Variable '$%s' expected value of type %s, which cannot be used as an input type." vardef.VariableName (vardef.Type.ToString())))
        | Some t when not (t :? InputDef) -> raise (GraphQLException (sprintf "Variable '$%s' expected value of type %s, which cannot be used as an input type." vardef.VariableName (vardef.Type.ToString())))
        | Some t -> t :?> InputDef
    match Map.tryFind vardef.VariableName inputs with
    | None -> 
        match vardef.DefaultValue with
        | Some defaultValue -> 
            let errMsg = (sprintf "Variable '%s': " vardef.VariableName)
            let executeInput = compileByType errMsg typedef
            executeInput inputs defaultValue
        | _ -> raise (GraphQLException (sprintf "Variable '$%s' of required type %s has no value provided." vardef.VariableName (vardef.Type.ToString())))
    | Some input -> coerceVariableValue false typedef vardef input (sprintf "Variable '$%s': " vardef.VariableName)