// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

[<AutoOpen>]
module internal FSharp.Data.GraphQL.Values

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Diagnostics
open System.Linq
open System.Text.Json
open FsToolkit.ErrorHandling

open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns
open FSharp.Data.GraphQL.Validation
open FSharp.Data.GraphQL

let private wrapOptionalNone (outputType : Type) (inputType : Type) =
    if inputType.Name <> outputType.Name then
        if outputType.FullName.StartsWith ReflectionHelper.ValueOptionTypeName then
            let _, valuenone, _ = ReflectionHelper.vOptionOfType outputType.GenericTypeArguments[0]
            valuenone
        elif outputType.IsValueType then
            Activator.CreateInstance (outputType)
        else
            null
    else
        null

let private normalizeOptional (outputType : Type) value =
    match value with
    | null -> wrapOptionalNone outputType typeof<obj>
    | value ->
        let inputType = value.GetType ()
        if inputType.Name <> outputType.Name then
            // Use only when option or voption so must not be null
            let expectedOutputType = outputType.GenericTypeArguments.FirstOrDefault()
            if
                outputType.FullName.StartsWith ReflectionHelper.OptionTypeName
                && expectedOutputType.IsAssignableFrom inputType
            then
                let some, _, _ = ReflectionHelper.optionOfType expectedOutputType
                some value
            elif
                outputType.FullName.StartsWith ReflectionHelper.ValueOptionTypeName
                && expectedOutputType.IsAssignableFrom inputType
            then
                let valuesome, _, _ = ReflectionHelper.vOptionOfType expectedOutputType
                valuesome value
            else
                // Use only when option or voption so must not be null
                let actualInputType = inputType.GenericTypeArguments.FirstOrDefault()
                if
                    inputType.FullName.StartsWith ReflectionHelper.OptionTypeName
                    && outputType.IsAssignableFrom actualInputType
                then
                    let _, _, getValue = ReflectionHelper.optionOfType actualInputType
                    // none is null so it is already covered above
                    getValue value
                elif
                    inputType.FullName.StartsWith ReflectionHelper.ValueOptionTypeName
                    && outputType.IsAssignableFrom actualInputType
                then
                    let _, valueNone, getValue = ReflectionHelper.vOptionOfType actualInputType
                    if value = valueNone then null else getValue value
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

let rec internal compileByType
    (inputObjectPath : FieldPath)
    (inputSource : InputSource)
    (originalInputDef : InputDef, inputDef : InputDef)
    : ExecuteInput =
    match inputDef with

    | Scalar scalardef -> variableOrElse (InlineConstant >> scalardef.CoerceInput)

    | InputObject objDef ->
        let objtype = objDef.Type
        let ctor = ReflectionHelper.matchConstructor objtype (objDef.Fields |> Array.map (fun x -> x.Name))

        let struct (mapper, typeMismatchParameters, nullableMismatchParameters, missingParameters) =
            ctor.GetParameters ()
            |> Array.fold
                (fun struct (
                            all : ResizeArray<_>,
                            mismatch : HashSet<_>,
                            areNullable : HashSet<_>,
                            missing : HashSet<_>
                        )
                        param
                        ->
                    match
                        objDef.Fields
                        |> Array.tryFind (fun field -> field.Name = param.Name)
                    with
                    | Some field ->
                        match field.TypeDef with
                        | Nullable _ when
                            ReflectionHelper.isPrameterMandatory param
                            && field.DefaultValue.IsNone
                            ->
                            areNullable.Add param.Name |> ignore
                        | inputDef ->
                            if ReflectionHelper.isAssignableWithUnwrap inputDef.Type param.ParameterType then
                                all.Add (struct (ValueSome field, param)) |> ignore
                            else
                                // TODO: Consider improving by specifying type mismatches
                                mismatch.Add param.Name |> ignore
                    | None ->
                        if ReflectionHelper.isParameterOptional param then
                            all.Add <| struct (ValueNone, param) |> ignore
                        else
                            missing.Add param.Name |> ignore
                    struct (all, mismatch, areNullable, missing))
                struct (ResizeArray (), HashSet (), HashSet (), HashSet ())

        let exceptions : exn list =  [
            if missingParameters.Any () then
                InvalidInputTypeException (
                    $"Input object '%s{objDef.Name}' refers to type '%O{objtype}', but mandatory constructor parameters '%A{missingParameters}' don't match any of the defined input fields",
                    missingParameters.ToImmutableHashSet ()
                )
            if nullableMismatchParameters.Any () then
                InvalidInputTypeException (
                    $"Input object %s{objDef.Name} refers to type '%O{objtype}', but optional fields '%A{missingParameters}' are not optional parameters of the constructor",
                    nullableMismatchParameters.ToImmutableHashSet ()
                )
            if typeMismatchParameters.Any () then
                InvalidInputTypeException (
                    $"Input object %s{objDef.Name} refers to type '%O{objtype}', but fields '%A{typeMismatchParameters}' have different types than constructor parameters",
                    typeMismatchParameters.ToImmutableHashSet ()
                )
        ]
        match exceptions with
        | [] -> ()
        | [ ex ] -> raise ex
        | _ -> raise (AggregateException ($"Invalid input object '%O{objtype}'", exceptions))

        let attachErrorExtensionsIfScalar inputSource path objDef (fieldDef : InputFieldDef) result =

            let mapFieldError err : IGQLError = {
                InputSource = inputSource
                InnerError = err
                ErrorKind = InputCoercion
                Path = (box fieldDef.Name) :: path
                FieldErrorDetails = ValueSome { ObjectDef = objDef; FieldDef = ValueSome fieldDef }
            }

            match fieldDef.TypeDef with
            | :? ScalarDef ->
                result
                |> Result.mapError (fun errs -> errs |> List.map mapFieldError)
            | _ -> result

        let mapInputObjectError inputSource inputObjectPath objectType (err : IGQLError) : IGQLError = {
            InputSource = inputSource
            InnerError = err
            ErrorKind = InputObjectValidation
            Path = inputObjectPath
            FieldErrorDetails = ValueSome { ObjectDef = objectType; FieldDef = ValueNone }
        }

        fun value variables ->
            match value with
            | ObjectValue props -> result {
                let argResults =
                    mapper
                    |> Seq.map (fun struct (field, param) ->
                        match field with
                        | ValueSome field ->
                            match Map.tryFind field.Name props with
                            | None ->
                                Ok
                                <| wrapOptionalNone param.ParameterType field.TypeDef.Type
                            | Some prop ->
                                field.ExecuteInput prop variables
                                |> Result.map (normalizeOptional param.ParameterType)
                                |> attachErrorExtensionsIfScalar inputSource inputObjectPath originalInputDef field
                        | ValueNone -> Ok <| wrapOptionalNone param.ParameterType typeof<obj>)
                    |> Seq.toList

                let! args = argResults |> splitSeqErrorsList

                let instance = ctor.Invoke args
                do!
                    objDef.Validator instance
                    |> ValidationResult.mapErrors (fun err ->
                        err
                        |> mapInputObjectError inputSource inputObjectPath originalInputDef)
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
                                    let! value =
                                        field.ExecuteInput (VariableName field.Name) objectFields
                                        // TODO: Take into account variable name
                                        |> attachErrorExtensionsIfScalar inputSource inputObjectPath originalInputDef field
                                    return normalizeOptional param.ParameterType value
                                | ValueNone -> return wrapOptionalNone param.ParameterType typeof<obj>
                            })
                            |> Seq.toList

                        let! args = argResults |> splitSeqErrorsList

                        let instance = ctor.Invoke args
                        do!
                            objDef.Validator instance
                            |> ValidationResult.mapErrors (fun err ->
                                err
                                |> mapInputObjectError inputSource inputObjectPath originalInputDef)
                        return instance
                    | null -> return null
                    | _ ->
                        let ty = found.GetType ()
                        if
                            ty = objtype
                            || (ty.FullName.StartsWith "Microsoft.FSharp.Core.FSharpOption`1"
                                && ty.GetGenericArguments().[0] = objtype)
                        then
                            return found
                        else
                            Debugger.Break ()
                            return!
                                Error [
                                    { new IGQLError with
                                        member _.Message = $"A variable '${variableName}' is not an object"
                                    }
                                ]
                | false, _ -> return null
              }
            | _ -> Ok null

    | List (Input innerDef) ->
        let isArray = inputDef.Type.IsArray
        // TODO: Improve creation of inner
        let inner index = compileByType ((box index) :: inputObjectPath) inputSource (innerDef, innerDef)
        let cons, nil = ReflectionHelper.listOfType innerDef.Type

        fun value variables ->
            match value with
            | ListValue list -> result {
                let! mappedValues =
                    list
                    |> Seq.mapi (fun i value -> inner i value variables)
                    |> Seq.toList
                    |> splitSeqErrorsList
                let mappedValues =
                    mappedValues
                    |> Seq.map (normalizeOptional innerDef.Type)
                    |> Seq.toList

                if isArray then
                    return ReflectionHelper.arrayOfList innerDef.Type mappedValues
                else
                    return List.foldBack cons mappedValues nil
              }
            | VariableName variableName -> Ok variables.[variableName]
            | _ -> result {
                // try to construct a list from single element
                let! single = inner 0 value variables

                if single = null then
                    return null
                else if isArray then
                    return ReflectionHelper.arrayOfList innerDef.Type [ single ]
                else
                    return cons single nil
              }

    | Nullable (Input innerDef) ->
        let inner = compileByType inputObjectPath inputSource (inputDef, innerDef)
        match innerDef with
        | InputObject inputObjDef -> inputObjDef.ExecuteInput <- inner
        | _ -> ()
        fun value variables ->
            match value with
            | NullValue -> Ok null
            | _ -> inner value variables

    | Enum enumDef ->
        fun value variables ->
            match value with
            | VariableName variableName ->
                match variables.TryGetValue variableName with
                | true, var -> Ok var
                | false, _ ->
                    Error [
                        { new IGQLError with
                            member _.Message = $"A variable '${variableName}' not found"
                        }
                    ]
            | _ -> result {
                let! coerced = coerceEnumInput value

                match coerced with
                | null -> return null
                | s ->
                    return
                        enumDef.Options
                        |> Seq.tryFind (fun v -> v.Name = s)
                        |> Option.map (fun x -> x.Value :?> _)
                        |> Option.defaultWith (fun () -> ReflectionHelper.parseUnion enumDef.Type s)
              }
    | _ ->
        Debug.Fail "Unexpected InputDef"
        failwithf "Unexpected value of inputDef: %O" inputDef

let rec internal coerceVariableValue
    isNullable
    inputObjectPath
    (objectFieldErrorDetails : ObjectFieldErrorDetails voption)
    (originalTypeDef, typeDef)
    (varDef : VarDef)
    (input : JsonElement)
    : Result<obj, IGQLError list> =

    let createVariableCoercionError message =
        Error [
            {
                CoercionError.InputSource = Variable varDef
                CoercionError.Message = message
                CoercionError.ErrorKind = InputCoercion
                CoercionError.Path = inputObjectPath
                CoercionError.FieldErrorDetails = objectFieldErrorDetails
            }
            :> IGQLError
        ]

    let createNullError typeDef =
        let message =
            match objectFieldErrorDetails with
            | ValueSome details ->
                $"Non-nullable field '%s{details.FieldDef.Value.Name}' expected value of type '%s{string typeDef}', but got 'null'."
            | ValueNone -> $"Non-nullable variable '$%s{varDef.Name}' expected value of type '%s{string typeDef}', but got 'null'."
        createVariableCoercionError message

    let mapInputError varDef inputObjectPath (objectFieldErrorDetails : ObjectFieldErrorDetails voption) (err : IGQLError) : IGQLError = {
        InnerError = err
        ErrorKind = InputCoercion
        InputSource = Variable varDef
        Path = inputObjectPath
        FieldErrorDetails = objectFieldErrorDetails
    }

    match typeDef with
    | Scalar scalardef ->
        if input.ValueKind = JsonValueKind.Null then
            createNullError originalTypeDef
        else
            match scalardef.CoerceInput (InputParameterValue.Variable input) with
            | Ok null when isNullable -> Ok null
            // TODO: Capture position in the JSON document
            | Ok null -> createNullError originalTypeDef
            | Ok value when not isNullable ->
                let ``type`` = value.GetType ()
                if
                    ``type``.IsValueType
                    && ``type``.FullName.StartsWith ReflectionHelper.ValueOptionTypeName
                    && value = Activator.CreateInstance ``type``
                then
                    createNullError originalTypeDef
                else
                    Ok value
            | result ->
                result
                |> Result.mapError (List.map (mapInputError varDef inputObjectPath objectFieldErrorDetails))
    | Nullable (InputObject innerdef) ->
        if input.ValueKind = JsonValueKind.Null then
            Ok null
        else
            coerceVariableValue true inputObjectPath ValueNone (typeDef, innerdef :> InputDef) varDef input
    | Nullable (Input innerdef) ->
        if input.ValueKind = JsonValueKind.Null then
            Ok null
        else
            coerceVariableValue true inputObjectPath ValueNone (typeDef, innerdef) varDef input
    | List (Input innerDef) ->
        let cons, nil = ReflectionHelper.listOfType innerDef.Type

        match input with
        | _ when input.ValueKind = JsonValueKind.Null && isNullable -> Ok null
        | _ when input.ValueKind = JsonValueKind.Null -> createNullError typeDef
        | _ -> result {
            let areItemsNullable =
                match innerDef with
                | Nullable _ -> true
                | _ -> false

            let! items =
                if input.ValueKind = JsonValueKind.Array then
                    result {
                        let! items =
                            input.EnumerateArray ()
                            |> Seq.mapi (fun i elem ->
                                coerceVariableValue areItemsNullable ((box i) :: inputObjectPath) ValueNone (originalTypeDef, innerDef) varDef elem)
                            |> Seq.toList
                            |> splitSeqErrorsList
                        if areItemsNullable then
                            let some, none, _ = ReflectionHelper.optionOfType innerDef.Type.GenericTypeArguments[0]
                            return
                                items
                                |> Seq.map (fun item -> if item = null then none else some item)
                                |> Seq.toList
                        else
                            return items |> Seq.toList
                    }
                else
                    result {
                        let! single = coerceVariableValue areItemsNullable inputObjectPath ValueNone (innerDef, innerDef) varDef input

                        if areItemsNullable then
                            let some, none, _ = ReflectionHelper.optionOfType innerDef.Type.GenericTypeArguments[0]
                            return [
                                if single = null then yield none else yield some single
                            ]
                        else
                            return [ single ]
                    }

            let isArray = typeDef.Type.IsArray
            if isArray then
                return ReflectionHelper.arrayOfList innerDef.Type items
            else
                return List.foldBack cons items nil
          }
    | InputObject objdef -> coerceVariableInputObject inputObjectPath (originalTypeDef, objdef) varDef input
    | Enum enumdef ->
        match input with
        | _ when input.ValueKind = JsonValueKind.Null && isNullable -> Ok null
        | _ when input.ValueKind = JsonValueKind.Null ->
            createVariableCoercionError $"A variable '$%s{varDef.Name}' expected value of type '%s{enumdef.Name}!', but no value was found."
        | _ when input.ValueKind = JsonValueKind.String ->
            let value = input.GetString ()
            match
                enumdef.Options
                |> Array.tryFind (fun o -> o.Name.Equals (value, StringComparison.InvariantCultureIgnoreCase))
            with
            | Some option -> Ok option.Value
            | None -> createVariableCoercionError $"A value '%s{value}' is not defined in Enum '%s{enumdef.Name}'."
        | _ -> createVariableCoercionError $"Enum values must be strings but got '%O{input.ValueKind}'."
    | _ -> failwith $"Variable '$%s{varDef.Name}': Only Scalars, Nullables, Lists, and InputObjects are valid type definitions."

and private coerceVariableInputObject inputObjectPath (originalObjDef, objDef) (varDef : VarDef) (input : JsonElement) =
    match input.ValueKind with
    | JsonValueKind.Object -> result {
        let mappedResult =
            objDef.Fields
            |> Array.map (fun field ->
                let inline coerce value =
                    let inputObjectPath' = (box field.Name) :: inputObjectPath
                    let objectFieldErrorDetails =
                        ValueSome
                        <| { ObjectDef = originalObjDef; FieldDef = ValueSome field }
                    let fieldTypeDef = field.TypeDef
                    let value =
                        coerceVariableValue false inputObjectPath' objectFieldErrorDetails (fieldTypeDef, fieldTypeDef) varDef value
                    KeyValuePair (field.Name, value)
                match input.TryGetProperty field.Name with
                | true, value -> coerce value
                | false, _ ->
                    match field.DefaultValue with
                    | Some value -> KeyValuePair (field.Name, Ok value)
                    | None -> coerce (JsonDocument.Parse("null").RootElement))
            |> ImmutableDictionary.CreateRange

        let! mapped = mappedResult |> splitObjectErrorsList
        // TODO: Improve without creating a dictionary
        // This also causes incorrect error messages and extensions to be generated
        let variables =
            seq { KeyValuePair (varDef.Name, mapped :> obj) }
            |> ImmutableDictionary.CreateRange

        return! objDef.ExecuteInput (VariableName varDef.Name) variables
      }
    | JsonValueKind.Null -> Ok null
    | valueKind ->
        Error [
            {
                InputSource = Variable varDef
                Message = $"A variable '$%s{varDef.Name}' expected to be '%O{JsonValueKind.Object}' but got '%O{valueKind}'."
                ErrorKind = InputCoercion
                Path = inputObjectPath
                FieldErrorDetails = ValueSome { ObjectDef = originalObjDef; FieldDef = ValueNone }
            }
            :> IGQLError
        ]
