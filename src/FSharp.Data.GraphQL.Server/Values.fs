// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

[<AutoOpen>]
module internal FSharp.Data.GraphQL.Values

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Diagnostics
open System.Linq
open System.Reflection
open System.Text.Json
open FsToolkit.ErrorHandling

open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Errors
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns
open FSharp.Data.GraphQL.Validation

let private wrapOptionalNone (outputType: Type) (inputType: Type) =
    if inputType.Name <> outputType.Name then
        if outputType.FullName.StartsWith "Microsoft.FSharp.Core.FSharpValueOption`1" then
            let _, valuenone, _ = ReflectionHelper.vOptionOfType outputType.GenericTypeArguments[0]
            valuenone
        elif outputType.IsValueType then
            Activator.CreateInstance(outputType)
        else null
    else
        null

let private wrapOptional (outputType: Type) value=
    match value with
    | null -> wrapOptionalNone outputType typeof<obj>
    | value ->
        let inputType = value.GetType()
        if inputType.Name <> outputType.Name then
            let expectedType = outputType.GenericTypeArguments[0]
            if outputType.FullName.StartsWith "Microsoft.FSharp.Core.FSharpOption`1" && expectedType.IsAssignableFrom inputType then
                let some, _, _ = ReflectionHelper.optionOfType outputType.GenericTypeArguments[0]
                some value
            elif outputType.FullName.StartsWith "Microsoft.FSharp.Core.FSharpValueOption`1" && expectedType.IsAssignableFrom inputType then
                let valuesome, _, _ = ReflectionHelper.vOptionOfType outputType.GenericTypeArguments[0]
                valuesome value
            else
                value
        else
            value

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

let inline private notAssignableMsg errMsg (innerDef : InputDef) value =
    $"%s{errMsg}value of type %s{innerDef.Type.Name } is not assignable from %s{value.GetType().Name}"

let rec internal compileByType (errMsg : string) (inputDef : InputDef) : ExecuteInput =
    match inputDef with

    | Scalar scalardef -> variableOrElse (InlineConstant >> scalardef.CoerceInput)

    | InputObject objdef ->
        let objtype = objdef.Type
        let ctor = ReflectionHelper.matchConstructor objtype (objdef.Fields |> Array.map (fun x -> x.Name))

        let struct (mapper, nullableMismatchParameters, missingParameters) =
            ctor.GetParameters ()
            |> Array.fold (
                fun
                    struct(all: ResizeArray<_>, areNullable: HashSet<_>, missing: HashSet<_>)
                    param ->
                match
                    objdef.Fields
                    |> Array.tryFind (fun field -> field.Name = param.Name)
                with
                | Some field ->
                    match field.TypeDef with
                    | Nullable _ when ReflectionHelper.isPrameterMandatory param && field.DefaultValue.IsNone ->
                        areNullable.Add param.Name |> ignore
                    | _ ->
                        all.Add(struct (ValueSome field, param)) |> ignore
                | None ->
                    if ReflectionHelper.isParameterOptional param then
                        all.Add <| struct (ValueNone, param) |> ignore
                    else
                        missing.Add param.Name |> ignore
                struct(all,areNullable, missing))
                struct (ResizeArray(), HashSet(), HashSet())

        if missingParameters.Any() then
            raise
            <| InvalidInputTypeException(
                $"Input object '%s{objdef.Name}' refers to type '%O{objtype}', but mandatory constructor parameters '%A{missingParameters}' don't match any of the defined input fields",
                missingParameters.ToImmutableHashSet()
               )
        if nullableMismatchParameters.Any() then
            raise
            <| InvalidInputTypeException(
                $"Input object %s{objdef.Name} refers to type '%O{objtype}', but optional fields '%A{missingParameters}' are not optional parameters of the constructor",
                nullableMismatchParameters.ToImmutableHashSet()
               )

        fun value variables ->
            match value with
            | ObjectValue props -> result {
                    let argResults =
                        mapper
                        |> Seq.map (fun struct (field, param) ->
                            match field with
                            | ValueSome field ->
                                match Map.tryFind field.Name props with
                                | None -> Ok <| wrapOptionalNone param.ParameterType field.TypeDef.Type
                                | Some prop -> field.ExecuteInput prop variables |> Result.map (wrapOptional param.ParameterType)
                            | ValueNone -> Ok <| wrapOptionalNone param.ParameterType typeof<obj>)

                    let! args = argResults |> splitSeqErrorsList

                    let instance = ctor.Invoke args
                    do! objdef.Validator instance
                    return instance
                }
            | VariableName variableName -> result {
                    match variables.TryGetValue variableName with
                    | true, found ->
                        match found with
                        | :? IReadOnlyDictionary<string, obj> as objectFields ->

                            let argResults =
                                mapper
                                |> Seq.map (fun struct (field, param) -> result {
                                    match field with
                                    | ValueSome field ->
                                        let! value = field.ExecuteInput (VariableName field.Name) objectFields
                                        return wrapOptional param.ParameterType value
                                    | ValueNone -> return wrapOptionalNone param.ParameterType typeof<obj>
                                })

                            let! args = argResults |> splitSeqErrorsList

                            let instance = ctor.Invoke args
                            do! objdef.Validator instance
                            return instance
                        | null ->
                            return null
                        | _ ->
                            let ty = found.GetType()
                            if ty = objtype || (ty.FullName.StartsWith "Microsoft.FSharp.Core.FSharpOption`1" && ty.GetGenericArguments()[0] = objtype) then
                                return found
                            else
                                Debugger.Break()
                                return! Error [{ new IGQLError with member _.Message = $"Variable '{variableName}' is not an object" }]
                    | false, _ -> return null
                }
            | _ -> Ok null

    | List (Input innerdef) ->
        let isArray = inputDef.Type.IsArray
        let inner = compileByType errMsg innerdef
        let cons, nil = ReflectionHelper.listOfType innerdef.Type

        fun value variables ->
            match value with
            | ListValue list -> result {
                    let! mappedValues =
                        list |> Seq.map (fun value -> inner value variables) |> splitSeqErrorsList
                    let mappedValues =
                        mappedValues
                        |> Seq.map (wrapOptional innerdef.Type)
                        |> Seq.toList

                    if isArray then
                        return ReflectionHelper.arrayOfList innerdef.Type mappedValues
                    else
                        return List.foldBack cons mappedValues nil
                }
            | VariableName variableName -> Ok variables.[variableName]
            | _ -> result {
                    // try to construct a list from single element
                    let! single = inner value variables

                    if single = null then
                        return null
                    else if isArray then
                        return ReflectionHelper.arrayOfList innerdef.Type [ single ]
                    else
                        return cons single nil
                }

    | Nullable (Input innerdef) ->
        let inner = compileByType errMsg innerdef
        match innerdef with
        | InputObject inputObjDef -> inputObjDef.ExecuteInput <- inner
        | _ -> ()
        fun value variables -> inner value variables

    | Enum enumdef ->
        fun value variables ->
            match value with
            | VariableName variableName ->
                match variables.TryGetValue variableName with
                | true, var -> Ok var
                | false, _ -> Error [ { new IGQLError with member _.Message = $"Variable '{variableName}' not provided" } ]
            | _ -> result {
                    let! coerced = coerceEnumInput value

                    match coerced with
                    | null -> return null
                    | s ->
                        return
                            enumdef.Options
                            |> Seq.tryFind (fun v -> v.Name = s)
                            |> Option.map (fun x -> x.Value :?> _)
                            |> Option.defaultWith (fun () -> ReflectionHelper.parseUnion enumdef.Type s)
                }
    | _ ->
        Debug.Fail "Unexpected InputDef"
        failwithf "Unexpected value of inputDef: %O" inputDef

let rec internal coerceVariableValue isNullable typedef (vardef : VarDef) (input : JsonElement) (errMsg : string) : Result<obj, IGQLError list> =
    match typedef with
    | Scalar scalardef ->
        if input.ValueKind = JsonValueKind.Null then
            Error [ { new IGQLError with member _.Message = $"%s{errMsg}expected value of type '%s{scalardef.Name}!' but got 'null'." } ]
        else
            match scalardef.CoerceInput (Variable input) with
            | Ok null when isNullable -> Ok null
            // TODO: Capture position in the JSON document
            | Ok null -> Error [ { new IGQLError with member _.Message = $"%s{errMsg}expected value of type '%s{scalardef.Name}!' but got 'null'." } ]
            | result ->
                let mapError (err : IGQLError) : IGQLError =
                    match err with
                    | :? IGQLErrorExtensions as ext ->
                        { new ICoerceGQLError with
                            member _.Message = err.Message
                            member _.VariableMessage = errMsg
                          interface IGQLErrorExtensions with
                            member _.Extensions = ext.Extensions }
                    | _ ->
                        { new ICoerceGQLError with
                            member _.Message = err.Message
                            member _.VariableMessage = errMsg }
                result |> Result.mapError (fun errs -> errs |> List.map mapError)
    | Nullable (InputObject innerdef) ->
        if input.ValueKind = JsonValueKind.Null then Ok null
        else coerceVariableValue true (innerdef :> InputDef) vardef input errMsg
    | Nullable (Input innerdef) ->
        if input.ValueKind = JsonValueKind.Null then Ok null
        else coerceVariableValue true innerdef vardef input errMsg
    | List (Input innerdef) ->
        let cons, nil = ReflectionHelper.listOfType innerdef.Type

        match input with
        | _ when input.ValueKind = JsonValueKind.Null && isNullable -> Ok null
        | _ when input.ValueKind = JsonValueKind.Null ->
            Error [ { new IGQLError with member _.Message = $"%s{errMsg}expected value of type '%s{vardef.TypeDef.ToString ()}', but no value was found." } ]
        | _ when input.ValueKind = JsonValueKind.Array -> result {
                let areItemsNullable =
                    match innerdef with
                    | Nullable _ -> true
                    | _ -> false
                let! items =
                    input.EnumerateArray()
                    |> Seq.map (fun elem -> coerceVariableValue areItemsNullable innerdef vardef elem (errMsg + "list element "))
                    |> Seq.rev
                    |> splitSeqErrorsList

                if areItemsNullable then
                    let some, none, _ = ReflectionHelper.optionOfType innerdef.Type.GenericTypeArguments[0]
                    return items |> Seq.map (fun item -> if item = null then none else some item) |> Seq.fold (fun acc coerced -> cons coerced acc) nil
                else
                    return items |> Seq.fold (fun acc coerced -> cons coerced acc) nil
            }
        | other ->
            Error [ { new IGQLError with member _.Message = $"{errMsg}Cannot coerce value of type '%O{other.GetType ()}' to list." } ]
    | InputObject objdef -> coerceVariableInputObject objdef vardef input ($"{errMsg[..(errMsg.Length-3)]} of type '%s{objdef.Name}': ")
    | Enum enumdef ->
        match input with
        | _ when input.ValueKind = JsonValueKind.Null && isNullable -> Ok null
        | _ when input.ValueKind = JsonValueKind.Null ->
            Error [ { new IGQLError with member _.Message = $"%s{errMsg}expected value of type '%s{enumdef.Name}!', but no value was found." } ]
        | _ when input.ValueKind = JsonValueKind.String ->
            let value = input.GetString()
            match enumdef.Options |> Array.tryFind (fun o -> o.Name.Equals(value, StringComparison.InvariantCultureIgnoreCase)) with
            | Some option -> Ok option.Value
            | None -> Error [ { new IGQLError with member _.Message = $"%s{errMsg}Value '%s{value}' is not defined in Enum '%s{enumdef.Name}'." } ]
        | _ ->
            Error [ { new IGQLError with member _.Message = $"%s{errMsg}Enum values must be strings but got '%O{input.ValueKind}'." } ]
    | _ ->
        failwith $"%s{errMsg}Only Scalars, Nullables, Lists, and InputObjects are valid type definitions."

and private coerceVariableInputObject (objdef) (vardef : VarDef) (input : JsonElement) errMsg =
    if input.ValueKind = JsonValueKind.Object then result {
        let mappedResult =
            objdef.Fields
            |> Array.map (fun field ->
                let inline coerce value =
                    let value = coerceVariableValue false field.TypeDef vardef value $"%s{errMsg}in field '%s{field.Name}': "
                    KeyValuePair (field.Name, value)
                match input.TryGetProperty field.Name with
                | true, value -> coerce value
                | false, _ ->
                    match field.DefaultValue with
                    | Some value -> KeyValuePair (field.Name, Ok value)
                    | None -> coerce (JsonDocument.Parse("null").RootElement)
            )
            |> ImmutableDictionary.CreateRange

        let! mapped = mappedResult |> splitObjectErrorsList
        let variables = seq { KeyValuePair (vardef.Name, mapped :> obj) } |> ImmutableDictionary.CreateRange

        return! objdef.ExecuteInput (VariableName vardef.Name) variables
    }
    else
        Error [ { new IGQLError with member _.Message = $"%s{errMsg}expected to be '%O{JsonValueKind.Object}' but got '%O{input.ValueKind}'." } ]
