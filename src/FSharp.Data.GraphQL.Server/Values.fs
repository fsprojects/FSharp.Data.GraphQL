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
open FSharp.Data.GraphQL.Shared
open FsToolkit.ErrorHandling

open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns
open FSharp.Data.GraphQL.Validation
open FSharp.Data.GraphQL

let private wrapOptionalNone (outputType : Type) (inputType : Type) =
    if inputType.Name <> outputType.Name then
        if outputType.FullName.StartsWith ReflectionHelper.ValueOptionTypeName then
            let _, valueNone, _ = ReflectionHelper.vOptionOfType outputType.GenericTypeArguments[0]
            valueNone
        elif outputType.IsValueType then
            Activator.CreateInstance (outputType)
        else
            null
    else
        null

let normalizeOptional (outputType : Type) value =
    match value with
    | null -> wrapOptionalNone outputType typeof<obj>
    | value ->
        let inputType = value.GetType ()
        if inputType.Name <> outputType.Name then
            // Use only when option or voption so must not be null
            let expectedOutputType = outputType.GenericTypeArguments.FirstOrDefault ()
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
                let valueSome, _, _ = ReflectionHelper.vOptionOfType expectedOutputType
                valueSome value
            else
                // Use only when option or voption so must not be null
                let actualInputType = inputType.GenericTypeArguments.FirstOrDefault ()
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
    let rec convert isNullable (schema : ISchema) (ast : InputType) : TypeDef voption =
        match ast with
        | NamedType name ->
            schema.TryFindType name
            |> ValueOption.map (fun namedDef ->
                if isNullable then
                    upcast namedDef.MakeNullable ()
                else
                    upcast namedDef
            )
        | ListType inner ->
            convert true schema inner
            |> ValueOption.map (fun i ->
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
    (inputContext : InputExecutionContextProvider)
    : ExecuteInput =
    match inputDef with

    | Scalar scalarDef -> variableOrElse (InlineConstant >> scalarDef.CoerceInput)
    | InputCustom customDef -> fun inputContext value variables -> customDef.CoerceInput inputContext (InlineConstant value) variables
    | InputObject objDef ->
        let objType = objDef.Type
        let ctor = ReflectionHelper.matchConstructor objType (objDef.Fields |> Array.map (fun x -> x.Name))

        let parametersMap =
            let typeMismatchParameters = HashSet ()
            let skippableMismatchParameters = HashSet ()
            let nullableMismatchParameters = HashSet ()
            let missingParameters = HashSet ()

            let allParameters =
                ctor.GetParameters ()
                |> Array.fold
                    (fun (allParameters : _ ResizeArray) param ->
                        match
                            objDef.Fields
                            // TODO: Improve parameter name matching logic
                            |> Array.tryFind (fun field -> String.Equals (field.Name, param.Name, StringComparison.InvariantCultureIgnoreCase))
                        with
                        | Some field ->
                            let isParameterSkippable = ReflectionHelper.isParameterSkippable param
                            match field.TypeDef with
                            | Nullable _ when field.IsSkippable <> isParameterSkippable -> skippableMismatchParameters.Add param.Name |> ignore
                            | Nullable _ when
                                not (isParameterSkippable)
                                && ReflectionHelper.isPrameterMandatory param
                                && field.DefaultValue.IsNone
                                ->
                                nullableMismatchParameters.Add param.Name |> ignore
                            | inputDef ->
                                let inputType, paramType =
                                    if isParameterSkippable then
                                        inputDef.Type, param.ParameterType.GenericTypeArguments[0]
                                    else
                                        inputDef.Type, param.ParameterType
                                if ReflectionHelper.isAssignableWithUnwrap inputType paramType then
                                    allParameters.Add (struct (ValueSome field, param))
                                else
                                    // TODO: Consider improving by specifying type mismatches
                                    typeMismatchParameters.Add param.Name |> ignore
                        | None ->
                            if
                                ReflectionHelper.isParameterSkippable param
                                || ReflectionHelper.isParameterOptional param
                            then
                                allParameters.Add <| struct (ValueNone, param)
                            else
                                missingParameters.Add param.Name |> ignore
                        allParameters)
                    (ResizeArray ())
                |> ImmutableArray.CreateRange

            let exceptions : exn list = [
                if missingParameters.Any () then
                    let message =
                        let ``params`` = String.Join ("', '", missingParameters)
                        $"Input object '%s{objDef.Name}' refers to type '%O{objType}', but mandatory constructor parameters '%s{``params``}' don't match any of the defined GraphQL input fields"
                    InvalidInputTypeException (message, missingParameters.ToImmutableHashSet ())
                if nullableMismatchParameters.Any () then
                    let message =
                        let ``params`` = String.Join ("', '", nullableMismatchParameters)
                        $"Input object %s{objDef.Name} refers to type '%O{objType}', but constructor parameters for optional GraphQL fields '%s{``params``}' are not optional"
                    InvalidInputTypeException (message, nullableMismatchParameters.ToImmutableHashSet ())
                if skippableMismatchParameters.Any () then
                    let message =
                        let ``params`` = String.Join ("', '", skippableMismatchParameters)
                        $"Input object %s{objDef.Name} refers to type '%O{objType}', but skippable '%s{``params``}' GraphQL fields and constructor parameters do not match"
                    InvalidInputTypeException (message, skippableMismatchParameters.ToImmutableHashSet ())
                if typeMismatchParameters.Any () then
                    let message =
                        let ``params`` = String.Join ("', '", typeMismatchParameters)
                        $"Input object %s{objDef.Name} refers to type '%O{objType}', but GraphQL fields '%s{``params``}' have different types than constructor parameters"
                    InvalidInputTypeException (message, typeMismatchParameters.ToImmutableHashSet ())
            ]
            match exceptions with
            | [] -> ()
            | [ ex ] -> raise ex
            | _ -> raise (AggregateException ($"Invalid input object '%O{objType}'", exceptions))

            allParameters

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

        fun inputContext value variables ->
#if DEBUG
            let objDef = objDef
#endif
            match value with
            | ObjectValue props -> result {
                let argResults =
                    parametersMap
                    |> Seq.map (fun struct (field, param) ->
                        match field with
                        | ValueSome field -> result {
                            match Map.tryFind field.Name props with
                            | None when field.IsSkippable -> return Activator.CreateInstance param.ParameterType
                            | None -> return wrapOptionalNone param.ParameterType field.TypeDef.Type
                            | Some prop ->
                                let! value =
                                    field.ExecuteInput inputContext prop variables
                                    |> attachErrorExtensionsIfScalar inputSource inputObjectPath originalInputDef field
                                if field.IsSkippable then
                                    let innerType = param.ParameterType.GenericTypeArguments[0]
                                    if
                                        not (ReflectionHelper.isTypeOptional innerType)
                                        && (value = null
                                            || (innerType.IsValueType
                                                && value = Activator.CreateInstance innerType))
                                    then
                                        return Activator.CreateInstance param.ParameterType
                                    else
                                        let ``include``, _ = ReflectionHelper.ofSkippable param.ParameterType
                                        return normalizeOptional innerType value |> ``include``
                                else
                                    return normalizeOptional param.ParameterType value
                          }
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
                            parametersMap
                            |> Seq.map (fun struct (field, param) -> result {
                                match field with
                                | ValueSome field when field.IsSkippable && not (objectFields.ContainsKey field.Name) ->
                                    return (Activator.CreateInstance param.ParameterType)
                                | ValueSome field ->
                                    let! value =
                                        field.ExecuteInput inputContext (VariableName field.Name) objectFields
                                        // TODO: Take into account variable name
                                        |> attachErrorExtensionsIfScalar inputSource inputObjectPath originalInputDef field
                                    if field.IsSkippable then
                                        let innerType = param.ParameterType.GenericTypeArguments[0]
                                        if not (ReflectionHelper.isTypeOptional innerType) &&
                                            (value = null || (innerType.IsValueType && value = Activator.CreateInstance innerType))
                                        then
                                            return Activator.CreateInstance param.ParameterType
                                        else
                                            let normalizedValue = normalizeOptional innerType value
                                            let ``include``, _ = ReflectionHelper.ofSkippable param.ParameterType
                                            return ``include`` normalizedValue
                                    else
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
                            ty = objType
                            || (ty.FullName.StartsWith "Microsoft.FSharp.Core.FSharpOption`1"
                                && ty.GetGenericArguments().[0] = objType)
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
        match innerDef with
        | InputObject inputObjDef
        | Nullable (InputObject inputObjDef) ->
            let inner = compileByType inputObjectPath inputSource (inputDef, innerDef) inputContext
            inputObjDef.ExecuteInput <- inner
        | _ -> ()

        let isArray = inputDef.Type.IsArray
        // TODO: Improve creation of inner
        let inner index = compileByType ((box index) :: inputObjectPath) inputSource (innerDef, innerDef) inputContext
        let cons, nil = ReflectionHelper.listOfType innerDef.Type

        fun inputContext value variables ->
            match value with
            | ListValue list -> result {
                let! mappedValues =
                    list
                    |> Seq.mapi (fun i value -> inner i inputContext value variables)
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
            | VariableName variableName -> Ok variables[variableName]
            | _ -> result {
                // try to construct a list from single element
                let! single = inner 0 inputContext value variables

                if single = null then
                    return null
                else if isArray then
                    return ReflectionHelper.arrayOfList innerDef.Type [ single ]
                else
                    return cons single nil
              }

    | Nullable (Input innerDef) ->
        let inner = compileByType inputObjectPath inputSource (inputDef, innerDef) inputContext
        match innerDef with
        | InputObject inputObjDef -> inputObjDef.ExecuteInput <- inner
        | _ -> ()
        fun inputContext value variables ->
            match value with
            | NullValue -> Ok null
            | _ -> inner inputContext value variables

    | Enum enumDef ->
        fun inputContext value variables ->
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
        failwithf $"Unexpected value of inputDef: {inputDef}"

type CoerceVariableContext = {
    IsNullable : bool
    InputObjectPath : FieldPath
    ObjectFieldErrorDetails : ObjectFieldErrorDetails voption
    OriginalTypeDef : InputDef
    TypeDef : InputDef
    VarDef : VarDef
    Input : JsonElement
}

type CoerceVariableInputContext = {
    InputObjectPath : FieldPath
    OriginalObjectDef : InputDef
    ObjectDef : InputObjectDef
    VarDef : VarDef
    Input : JsonElement
}

let rec internal coerceVariableValue (ctx : CoerceVariableContext, inputContext : InputExecutionContextProvider) : Result<obj, IGQLError list> =

    //let {
    //        IsNullable = isNullable
    //        InputObjectPath = inputObjectPath
    //        ObjectFieldErrorDetails = objectFieldErrorDetails
    //        OriginalTypeDef = originalTypeDef
    //        TypeDef = typeDef
    //        VarDef = varDef
    //        Input = input
    //    } =
    //    ctx

    let createVariableCoercionError message =
        Error [
            {
                CoercionError.InputSource = Variable ctx.VarDef
                CoercionError.Message = message
                CoercionError.ErrorKind = InputCoercion
                CoercionError.Path = ctx.InputObjectPath
                CoercionError.FieldErrorDetails = ctx.ObjectFieldErrorDetails
            }
            :> IGQLError
        ]

    let createNullError typeDef =
        let message =
            match ctx.ObjectFieldErrorDetails with
            | ValueSome details ->
                $"Non-nullable field '%s{details.FieldDef.Value.Name}' expected value of type '%s{string typeDef}', but got 'null'."
            | ValueNone -> $"Non-nullable variable '$%s{ctx.VarDef.Name}' expected value of type '%s{string typeDef}', but got 'null'."
        createVariableCoercionError message

    let mapInputError varDef inputObjectPath (objectFieldErrorDetails : ObjectFieldErrorDetails voption) (err : IGQLError) : IGQLError = {
        InnerError = err
        ErrorKind = InputCoercion
        InputSource = Variable varDef
        Path = inputObjectPath
        FieldErrorDetails = objectFieldErrorDetails
    }

    match ctx.TypeDef with
    | Scalar scalarDef ->
        if ctx.Input.ValueKind = JsonValueKind.Null then
            createNullError ctx.OriginalTypeDef
        else
            match scalarDef.CoerceInput (InputParameterValue.Variable ctx.Input) with
            | Ok null when ctx.IsNullable -> Ok null
            // TODO: Capture position in the JSON document
            | Ok null -> createNullError ctx.OriginalTypeDef
            | Ok value when not ctx.IsNullable ->
                let ``type`` = value.GetType ()
                if
                    ``type``.IsValueType
                    && ``type``.FullName.StartsWith ReflectionHelper.ValueOptionTypeName
                    && value = Activator.CreateInstance ``type``
                then
                    createNullError ctx.OriginalTypeDef
                else
                    Ok value
            | result ->
                result
                |> Result.mapError (List.map (mapInputError ctx.VarDef ctx.InputObjectPath ctx.ObjectFieldErrorDetails))
    | Nullable (InputObject innerdef) ->
        if ctx.Input.ValueKind = JsonValueKind.Null then
            Ok null
        else
            let ctx' = {
                ctx with
                    IsNullable = true
                    ObjectFieldErrorDetails = ValueNone
                    OriginalTypeDef = ctx.TypeDef
                    TypeDef = innerdef :> InputDef
            }
            coerceVariableValue(ctx', inputContext)
    | Nullable (Input innerDef) ->
        if ctx.Input.ValueKind = JsonValueKind.Null then
            Ok null
        else
            let ctx' = {
                ctx with
                    IsNullable = true
                    ObjectFieldErrorDetails = ValueNone
                    OriginalTypeDef = ctx.TypeDef
                    TypeDef = innerDef
            }
            coerceVariableValue(ctx', inputContext)
    | List (Input innerDef) ->
        let cons, nil = ReflectionHelper.listOfType innerDef.Type

        match ctx.Input with
        | _ when ctx.Input.ValueKind = JsonValueKind.Null && ctx.IsNullable -> Ok null
        | _ when ctx.Input.ValueKind = JsonValueKind.Null -> createNullError ctx.TypeDef
        | _ -> result {
            let areItemsNullable =
                match innerDef with
                | Nullable _ -> true
                | _ -> false

            let! items =
                if ctx.Input.ValueKind = JsonValueKind.Array then
                    result {
                        let! items =
                            ctx.Input.EnumerateArray ()
                            |> Seq.mapi (fun i elem ->
                                let ctx' = {
                                    ctx with
                                        IsNullable = areItemsNullable
                                        InputObjectPath = (box i) :: ctx.InputObjectPath
                                        ObjectFieldErrorDetails = ValueNone
                                        TypeDef = innerDef
                                        Input = elem
                                }
                                coerceVariableValue(ctx', inputContext))
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
                        let ctx' = {
                            ctx with
                                IsNullable = areItemsNullable
                                ObjectFieldErrorDetails = ValueNone
                                OriginalTypeDef = innerDef
                                TypeDef = innerDef
                        }
                        let! single = coerceVariableValue(ctx', inputContext)

                        if areItemsNullable then
                            let some, none, _ = ReflectionHelper.optionOfType innerDef.Type.GenericTypeArguments[0]
                            return [
                                if single = null then yield none else yield some single
                            ]
                        else
                            return [ single ]
                    }

            let isArray = ctx.TypeDef.Type.IsArray
            if isArray then
                return ReflectionHelper.arrayOfList innerDef.Type items
            else
                return List.foldBack cons items nil
          }
    | InputObject objDef ->
        coerceVariableInputObject ({
            InputObjectPath = ctx.InputObjectPath
            OriginalObjectDef = ctx.OriginalTypeDef
            ObjectDef = objDef
            VarDef = ctx.VarDef
            Input = ctx.Input
        }, inputContext)
    | Enum enumDef ->
        match ctx.Input with
        | _ when ctx.Input.ValueKind = JsonValueKind.Null && ctx.IsNullable -> Ok null
        | _ when ctx.Input.ValueKind = JsonValueKind.Null ->
            createVariableCoercionError $"A variable '$%s{ctx.VarDef.Name}' expected value of type '%s{enumDef.Name}!', but no value was found."
        | _ when ctx.Input.ValueKind = JsonValueKind.String ->
            let value = ctx.Input.GetString ()
            match
                enumDef.Options
                |> Array.tryFind (fun o -> o.Name.Equals (value, StringComparison.InvariantCultureIgnoreCase))
            with
            | Some option -> Ok option.Value
            | None -> createVariableCoercionError $"A value '%s{value}' is not defined in Enum '%s{enumDef.Name}'."
        | _ -> createVariableCoercionError $"Enum values must be strings but got '%O{ctx.Input.ValueKind}'."
    | InputCustom customDef ->
        if ctx.Input.ValueKind = JsonValueKind.Null then
            createNullError ctx.OriginalTypeDef
        else
            match customDef.CoerceInput inputContext (InputParameterValue.Variable ctx.Input) ImmutableDictionary.Empty with
            | Ok null when ctx.IsNullable -> Ok null
            // TODO: Capture position in the JSON document
            | Ok null -> createNullError ctx.OriginalTypeDef
            | Ok value when not ctx.IsNullable ->
                let ``type`` = value.GetType ()
                if
                    ``type``.IsValueType
                    && ``type``.FullName.StartsWith ReflectionHelper.ValueOptionTypeName
                    && value = Activator.CreateInstance ``type``
                then
                    createNullError ctx.OriginalTypeDef
                else
                    Ok value
            | result ->
                result
                |> Result.mapError (List.map (mapInputError ctx.VarDef ctx.InputObjectPath ctx.ObjectFieldErrorDetails))
    | _ -> failwith $"Variable '$%s{ctx.VarDef.Name}': Only Scalars, Nullables, Lists, and InputObjects are valid type definitions."

and private coerceVariableInputObject (ctx : CoerceVariableInputContext, inputContext : InputExecutionContextProvider) =
    match ctx.Input.ValueKind with
    | JsonValueKind.Object -> result {
        let mappedResult =
            ctx.ObjectDef.Fields
            |> Seq.vchoose (fun field ->
                let inline coerce value =
                    let inputObjectPath' = (box field.Name) :: ctx.InputObjectPath
                    let objectFieldErrorDetails =
                        ValueSome
                        <| { ObjectDef = ctx.OriginalObjectDef; FieldDef = ValueSome field }
                    let fieldTypeDef = field.TypeDef
                    let value =
                        let ctx = {
                            IsNullable = false
                            InputObjectPath = inputObjectPath'
                            ObjectFieldErrorDetails = objectFieldErrorDetails
                            OriginalTypeDef = fieldTypeDef
                            TypeDef = fieldTypeDef
                            VarDef = ctx.VarDef
                            Input = value
                        }
                        coerceVariableValue(ctx, inputContext)
                    KeyValuePair (field.Name, value)
                match ctx.Input.TryGetProperty field.Name with
                | true, value -> coerce value |> ValueSome
                | false, _ when field.IsSkippable -> ValueNone
                | false, _ ->
                    match field.DefaultValue with
                    | Some value -> KeyValuePair (field.Name, Ok value)
                    | None -> coerce (JsonDocument.Parse("null").RootElement)
                    |> ValueSome)
            |> ImmutableDictionary.CreateRange

        let! mapped = mappedResult |> splitObjectErrorsList
        // TODO: Improve without creating a dictionary
        // This also causes incorrect error messages and extensions to be generated
        let variables =
            seq { KeyValuePair (ctx.VarDef.Name, mapped :> obj) }
            |> ImmutableDictionary.CreateRange

        return! ctx.ObjectDef.ExecuteInput inputContext (VariableName ctx.VarDef.Name) variables
      }
    | JsonValueKind.Null -> Ok null
    | valueKind ->
        Error [
            {
                InputSource = Variable ctx.VarDef
                Message = $"A variable '$%s{ctx.VarDef.Name}' expected to be '%O{JsonValueKind.Object}' but got '%O{valueKind}'."
                ErrorKind = InputCoercion
                Path = ctx.InputObjectPath
                FieldErrorDetails = ValueSome { ObjectDef = ctx.OriginalObjectDef; FieldDef = ValueNone }
            }
            :> IGQLError
        ]
