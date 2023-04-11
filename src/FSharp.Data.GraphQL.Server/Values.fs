// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

[<AutoOpen>]
module internal FSharp.Data.GraphQL.Values

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Text.Json
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns

/// Tries to convert type defined in AST into one of the type defs known in schema.
let inline tryConvertAst schema ast =
    let rec convert isNullable (schema : ISchema) (ast : InputType) : TypeDef option =
        match ast with
        | NamedType name ->
            match schema.TryFindType name with
            | Some namedDef ->
                Some (
                    if isNullable then
                        upcast namedDef.MakeNullable ()
                    else
                        upcast namedDef
                )
            | None -> None
        | ListType inner ->
            convert true schema inner
            |> Option.map (fun i ->
                if isNullable then
                    upcast i.MakeList().MakeNullable ()
                else
                    upcast i.MakeList ())
        | NonNullType inner -> convert false schema inner

    convert true schema ast

let inline private notAssignableMsg (innerDef : InputDef) value : string =
    sprintf "value of type %s is not assignable from %s" innerDef.Type.Name (value.GetType().Name)

let rec internal compileByType (errMsg : string) (inputDef : InputDef) : ExecuteInput =
    match inputDef with

    | Scalar scalardef -> variableOrElse (InlineConstant >> scalardef.CoerceInput >> Option.toObj)

    | InputObject objdef ->
        let objtype = objdef.Type
        let ctor = ReflectionHelper.matchConstructor objtype (objdef.Fields |> Array.map (fun x -> x.Name))

        let mapper =
            ctor.GetParameters ()
            |> Array.map (fun param ->
                match
                    objdef.Fields
                    |> Array.tryFind (fun field -> field.Name = param.Name)
                with
                | Some field -> (field, param)
                | None ->
                    failwithf
                        "Input object '%s' refers to type '%O', but constructor parameter '%s' doesn't match any of the defined input fields"
                        objdef.Name
                        objtype
                        param.Name)

        fun value variables ->
            match value with
            | ObjectValue props ->
                let args =
                    mapper
                    |> Array.map (fun (field, param) ->
                        match Map.tryFind field.Name props with
                        | None -> null
                        | Some prop -> field.ExecuteInput prop variables)

                let instance = ctor.Invoke (args)
                instance
            | VariableName variableName ->
                match variables.TryGetValue variableName with
                | true, found ->
                    // TODO: Figure out how does this happen
                    let optionType = typeof<option<_>>.GetGenericTypeDefinition().MakeGenericType(objdef.Type)
                    //let voptionType = typeof<voption<_>>.GetGenericTypeDefinition().MakeGenericType(objdef.Type)
                    if found.GetType() = optionType then found
                    //elif found.GetType() = voptionType then found
                    else
                    let variables = found :?> ImmutableDictionary<string, obj>
                    let args =
                        mapper
                        |> Array.map (fun (field, param) ->
                            match variables.TryGetValue field.Name with
                            | true, value ->
                                let paramType = param.ParameterType
                                if paramType.IsGenericType && paramType.GetGenericTypeDefinition() = typeof<voption<_>>.GetGenericTypeDefinition() then
                                    let valuesome, valuenone, _ = ReflectionHelper.vOptionOfType field.TypeDef.Type.GenericTypeArguments[0]
                                    match value with
                                    | null -> valuenone
                                    | _ ->
                                        let valueType = value.GetType()
                                        if valueType.IsGenericType && valueType.GetGenericTypeDefinition() = typeof<option<_>>.GetGenericTypeDefinition()
                                        then
                                            let _, _, getValue = ReflectionHelper.optionOfType valueType.GenericTypeArguments[0]
                                            value |> getValue |> valuesome
                                        else
                                            value |> valuesome
                                else
                                    value
                            | false, _ -> null)

                    let instance = ctor.Invoke (args)
                    instance
                | false, _ -> null
            | _ -> null

    | List (Input innerdef) ->
        let isArray = inputDef.Type.IsArray
        let inner = compileByType errMsg innerdef
        let cons, nil = ReflectionHelper.listOfType innerdef.Type

        fun value variables ->
            match value with
            | ListValue list ->
                let mappedValues = list |> List.map (fun value -> inner value variables)

                if isArray then
                    ReflectionHelper.arrayOfList innerdef.Type mappedValues
                else
                    nil |> List.foldBack cons mappedValues
            | VariableName variableName -> variables.[variableName]
            | _ ->
                // try to construct a list from single element
                let single = inner value variables

                if single = null then
                    null
                else if isArray then
                    ReflectionHelper.arrayOfList innerdef.Type [ single ]
                else
                    cons single nil

    | Nullable (Input innerdef) ->
        let inner = compileByType errMsg innerdef
        let some, none, _ = ReflectionHelper.optionOfType innerdef.Type

        fun value variables ->
            let i = inner value variables
            match i with
            | null -> none
            | coerced ->
                let c = some coerced
                if c <> null then
                    c
                else
                    raise
                    <| GraphQLException (errMsg + notAssignableMsg innerdef coerced)

    | Enum enumdef ->
        fun value variables ->
            match value with
            | VariableName variableName ->
                match variables.TryGetValue variableName with
                | true, var -> var
                | false, _ -> failwithf "Variable '%s' not supplied.\nVariables: %A" variableName variables
            | _ ->
                let coerced = coerceEnumInput value

                match coerced with
                | None -> null
                | Some s ->
                    enumdef.Options
                    |> Seq.tryFind (fun v -> v.Name = s)
                    |> Option.map (fun x -> x.Value :?> _)
                    |> Option.defaultWith (fun () -> ReflectionHelper.parseUnion enumdef.Type s)
    | _ -> failwithf "Unexpected value of inputDef: %O" inputDef


let rec private coerceVariableValue isNullable typedef (vardef : VarDef) (input : JsonElement) (errMsg : string) : obj =
    match typedef with
    | Scalar scalardef ->
        match scalardef.CoerceInput (Variable input) with
        | None when isNullable -> null
        // TODO: Capture position in the JSON document
        | None -> raise <| GraphQLException $"%s{errMsg}expected value of type '%s{scalardef.Name}' but got 'null'."
        | Some res -> res
    | Nullable (InputObject innerdef) ->
        coerceVariableValue true (innerdef :> InputDef) vardef input errMsg
    | Nullable (Input innerdef) ->
        let some, none, innerValue = ReflectionHelper.optionOfType innerdef.Type
        let coerced = coerceVariableValue true innerdef vardef input errMsg

        if coerced <> null then
            let s = some coerced

            if s <> null then
                s
            else
                raise
                <| GraphQLException ($"%s{errMsg}value of type '%O{innerdef.Type}' is not assignable from '%O{coerced.GetType ()}'.")
        else
            none
    | List (Input innerdef) ->
        let cons, nil = ReflectionHelper.listOfType innerdef.Type

        match input with
        | _ when input.ValueKind = JsonValueKind.Null && isNullable -> null
        | _ when input.ValueKind = JsonValueKind.Null ->
            raise
            <| GraphQLException ($"%s{errMsg}expected value of type '%s{vardef.TypeDef.ToString ()}', but no value was found.")
        | _ when input.ValueKind = JsonValueKind.Array ->
            let mapped =
                input.EnumerateArray()
                |> Seq.map (fun elem -> coerceVariableValue false innerdef vardef elem (errMsg + "list element "))
                //TODO: optimize
                |> Seq.toList
                |> List.rev
                |> List.fold (fun acc coerced -> cons coerced acc) nil

            mapped
        | other ->
            raise
            <| GraphQLException ($"{errMsg}Cannot coerce value of type '%O{other.GetType ()}' to list.")
    // TODO: Improve error message generation
    | InputObject objdef -> coerceVariableInputObject objdef vardef input ($"{errMsg[..(errMsg.Length-3)]} of type '%s{objdef.Name}' ")
    | Enum enumdef ->
        match input with
        | _ when input.ValueKind = JsonValueKind.Null && isNullable -> null
        | _ when input.ValueKind = JsonValueKind.Null ->
            raise
            <| GraphQLException ($"%s{errMsg}Expected value of Enum '%s{enumdef.Name}', but no value was found.")
        | _ when input.ValueKind = JsonValueKind.String ->
            let value = input.GetString()
            match enumdef.Options |> Array.tryFind (fun o -> o.Name.Equals(value, StringComparison.InvariantCultureIgnoreCase)) with
            | Some option -> option.Value
            | None ->
                raise
                <| GraphQLException $"%s{errMsg}Value '%s{value}' is not defined in Enum '%s{enumdef.Name}'."
        | _ ->
            raise
            <| GraphQLException $"%s{errMsg}Enum values must be strings but got '%O{input.ValueKind}'."
    | _ ->
        raise
        <| GraphQLException ($"%s{errMsg}Only Scalars, Nullables, Lists, and InputObjects are valid type definitions.")

// TODO: Collect errors from subfields
and private coerceVariableInputObject (objdef) (vardef : VarDef) (input : JsonElement) errMsg =
    //TODO: this should be eventually coerced to complex object
    if input.ValueKind = JsonValueKind.Object then
        let mapped =
            objdef.Fields
            |> Array.map (fun field ->
                let inline coerce value = KeyValuePair (field.Name, coerceVariableValue false field.TypeDef vardef value $"%s{errMsg}in field '%s{field.Name}': ")
                // TODO: Consider using of option
                match input.TryGetProperty field.Name with
                | true, value -> coerce value
                | false, _ ->
                    match field.DefaultValue with
                    | Some value -> KeyValuePair (field.Name, value)
                    | None -> coerce (JsonDocument.Parse("null").RootElement)
            )
            |> ImmutableDictionary.CreateRange

        upcast mapped
    else
        raise
        <| GraphQLException ($"%s{errMsg}expected to be '%O{JsonValueKind.Object}' but got '%O{input.ValueKind}'.")

let internal coerceVariable (vardef : VarDef) (inputs : ImmutableDictionary<string, JsonElement>) =
    let vname = vardef.Name

    // TODO: Use FSharp.Collection.Immutable
    match inputs.TryGetValue vname with
    | false, _ ->
        match vardef.DefaultValue with
        | Some defaultValue ->
            let errMsg = (sprintf "Variable '%s': " vname)
            let executeInput = compileByType errMsg vardef.TypeDef
            executeInput defaultValue (ImmutableDictionary.Empty) // TODO: Check if empty is enough
        | None ->
            match vardef.TypeDef with
            | Nullable _ -> null
            | _ ->
                raise
                <| GraphQLException ($"Variable '$%s{vname}' of required type '%s{vardef.TypeDef.ToString ()}' has no value provided.")
    | true, jsonElement ->
        coerceVariableValue false vardef.TypeDef vardef jsonElement (sprintf "Variable '$%s': " vname)
