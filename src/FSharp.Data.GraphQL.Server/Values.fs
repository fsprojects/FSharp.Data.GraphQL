// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

[<AutoOpen>]
module internal FSharp.Data.GraphQL.Values

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Reflection
open System.Text.Json
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns
open Microsoft.FSharp.Reflection

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
    | Scalar scalardef -> variableOrElse (scalardef.CoerceInput >> Option.toObj)
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
                | Some x -> x
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
                    |> Array.map (fun field ->
                        match Map.tryFind field.Name props with
                        | None -> null
                        | Some prop -> field.ExecuteInput prop variables)

                let instance = ctor.Invoke (args)
                instance
            | Variable variableName ->
                match variables.TryGetValue variableName with
                | true, found -> found
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
            | Variable variableName -> variables.[variableName]
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

        fun variables value ->
            let i = inner variables value

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
            | Variable variableName ->
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
        match scalardef.CoerceValue input with
        | None when isNullable -> null
        | None ->
            raise (
                GraphQLException
                <| $"%s{errMsg}expected value of type '%s{scalardef.Name}' but got 'None'."
            )
        | Some res -> res
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
    | InputObject objdef -> coerceVariableInputObject objdef vardef input (errMsg + (sprintf "in input object '%s': " objdef.Name))
    | Enum enumdef ->
        match input with
        | _ when input.ValueKind = JsonValueKind.Null && isNullable -> null
        | _ when input.ValueKind = JsonValueKind.Null ->
            raise
            <| GraphQLException ($"%s{errMsg}Expected Enum '%s{enumdef.Name}', but no value was found.")
        | _ -> input.Deserialize(enumdef.Type) // TODO: Add JsonSerializerOptions with FSharp.SystemtextJson
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
                // TODO: Consider using of option
                let value =
                    match input.TryGetProperty field.Name with
                    | true, valueFound -> valueFound
                    | false, _ -> JsonDocument.Parse("null").RootElement
                KeyValuePair (field.Name, coerceVariableValue false field.TypeDef vardef value $"%s{errMsg}in field '%s{field.Name}': "))
            |> ImmutableDictionary.CreateRange

        upcast mapped
    else
        input

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
