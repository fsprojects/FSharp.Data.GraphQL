// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Reflection
open System.Text.Json.Serialization

/// General helper functions and types.
module internal ReflectionHelper =

    open Microsoft.FSharp.Quotations.Patterns

    let getModuleType quotation =
        match quotation with
        | PropertyGet (_, propertyInfo, _) -> propertyInfo.DeclaringType
        | FieldGet (_, fieldInfo) -> fieldInfo.DeclaringType
        | _ -> failwith "Expression is no property."

    let private listGenericTypeInfo = typedefof<_ list>.GetTypeInfo()
    /// <summary>
    /// Returns pair of function constructors for `cons(head,tail)` and `nil`
    /// used to create list of type <paramref name="t"/> given at runtime.
    /// </summary>
    /// <param name="t">Type used for result list constructors as type param</param>
    let listOfType t =
        let listType = listGenericTypeInfo.MakeGenericType([|t|]).GetTypeInfo()
        let nil =
            let empty = listType.GetDeclaredProperty "Empty"
            empty.GetValue (null)
        let cons =
            let cons = listType.GetDeclaredMethod "Cons"
            fun item list -> cons.Invoke (null, [| item; list |])
        (cons, nil)

    /// <summary>
    /// used to create array from list of type <paramref name="t"/> given at runtime.
    /// </summary>
    /// <param name="t">runtime type for array type construction</param>
    /// <param name="l">input list</param>
    let arrayOfList (t: Type) (l : _ list) =
        let array = System.Array.CreateInstance(t, l.Length)
        l |> List.iteri (fun i v ->
            array.SetValue(v, i)
        )
        array :> obj

    let private optionGenericTypeInfo = typedefof<_ option>.GetTypeInfo()
    /// <summary>
    /// Returns pair of function constructors for `some(value)` and `none`
    /// used to create option of type <paramref name="t"/> given at runtime.
    /// </summary>
    /// <param name="t">Type used for result option constructors as type param</param>
    let optionOfType t =
        let optionType = optionGenericTypeInfo.MakeGenericType([|t|]).GetTypeInfo()
        let none =
            let x = optionType.GetDeclaredProperty "None"
            x.GetValue(null)
        let some =
            let createSome = optionType.GetDeclaredMethod "Some"
            fun value ->
                if not (isNull value)
                then
                    let valueType = value.GetType().GetTypeInfo()
                    if valueType = optionType
                    then value
                    elif t.GetTypeInfo().IsAssignableFrom(valueType)
                    then createSome.Invoke(null, [| value |])
                    else null
                else none
        let value =
            let x = optionType.GetDeclaredProperty "Value"
            fun input ->
                if not (isNull input)
                then
                    let valueType = input.GetType().GetTypeInfo()
                    if valueType = optionType
                    then x.GetValue(input)
                    else input
                else input
        (some, none, value)

    let private valueOptionGenericTypeInfo = typedefof<_ voption>.GetTypeInfo()
    /// <summary>
    /// Returns pair of function constructors for `some(value)` and `none`
    /// used to create option of type <paramref name="t"/> given at runtime.
    /// </summary>
    /// <param name="t">Type used for result option constructors as type param</param>
    let vOptionOfType t =
        let optionType = valueOptionGenericTypeInfo.MakeGenericType([|t|]).GetTypeInfo()
        let none =
            let x = optionType.GetDeclaredProperty "None"
            x.GetValue(null)
        let some =
            let createSome = optionType.GetDeclaredMethod "Some"
            fun value ->
                if not (isNull value)
                then
                    let valueType = value.GetType().GetTypeInfo()
                    if valueType = optionType
                    then value
                    elif t.GetTypeInfo().IsAssignableFrom(valueType)
                    then createSome.Invoke(null, [| value |])
                    else null
                else none
        let value =
            let x = optionType.GetDeclaredProperty "Value"
            fun input ->
                if not (isNull input)
                then
                    let valueType = input.GetType().GetTypeInfo()
                    if valueType = optionType
                    then x.GetValue(input)
                    else input
                else input
        (some, none, value)

    /// <summary>
    /// Returns pair of function constructors for `include(value)` and `skip`
    /// used to create option of type <paramref name="t"/> given at runtime.
    /// </summary>
    /// <param name="t">Type used for result option constructors as type param</param>
    let ofSkippable (skippableType : Type) =
        let skippableType = skippableType.GetTypeInfo ()
        let skip =
            let x = skippableType.GetDeclaredProperty "Skip"
            x.GetValue(null)
        let ``include`` =
            let createInclude = skippableType.GetDeclaredMethod "NewInclude"
            fun value ->
                let valueType =
                    match value with
                    | null -> null
                    | _ -> value.GetType().GetTypeInfo()
                if valueType = skippableType
                then value
                else createInclude.Invoke(null, [| value |])
        (``include``, skip)

    let skippableGenericTypeInfo = typedefof<_ Skippable>.GetTypeInfo()
    /// <summary>
    /// Returns pair of function constructors for `include(value)` and `skip`
    /// used to create option of type <paramref name="t"/> given at runtime.
    /// </summary>
    /// <param name="t">Type used for result option constructors as type param</param>
    let skippableOfType t =
        let skippableType = skippableGenericTypeInfo.MakeGenericType([|t|]).GetTypeInfo()
        let skip =
            let x = skippableType.GetDeclaredProperty "Skip"
            x.GetValue(null)
        let ``include`` =
            let createInclude = skippableType.GetDeclaredMethod "NewInclude"
            fun value ->
                let valueType =
                    match value with
                    | null -> null
                    | _ -> value.GetType().GetTypeInfo()
                if valueType = skippableType
                then value
                else createInclude.Invoke(null, [| value |])
        (``include``, skip)

    /// <summary>
    /// Whether <paramref name="t"/> is constructed from the generic type definition <paramref name="definition"/>.
    /// </summary>
    /// <remarks>
    /// The definitions are compared with <see cref="M:System.Type.op_Equality(System.Type,System.Type)"/>. It takes
    /// about a nanosecond, while the F# equality of types takes about ten, and the culture-sensitive comparison
    /// of the full name with the name of the definition, which it replaces, hundreds. Unlike that comparison,
    /// it does not take an array of the generic type, whose full name starts with the same name, for the generic type.
    /// </remarks>
    let isConstructedFrom (definition : Type) (t : Type) =
        t.IsGenericType && Type.(=) (t.GetGenericTypeDefinition (), definition)

    /// Whether the type is an F# option.
    let isOptionType (t : Type) = isConstructedFrom optionGenericTypeInfo t

    /// Whether the type is an F# value option.
    let isValueOptionType (t : Type) = isConstructedFrom valueOptionGenericTypeInfo t

    /// Whether the type is a skippable value.
    let isSkippableType (t : Type) = isConstructedFrom skippableGenericTypeInfo t

    /// Whether the type is an F# list.
    let isListType (t : Type) = isConstructedFrom listGenericTypeInfo t

module Helpers =

    let rec internal moduleType = ReflectionHelper.getModuleType <@ moduleType @>

    /// <summary>
    /// Casts a <see cref="System.Object"/> to a <see cref="voption{System.Object}"/>.
    /// </summary>
    let objectOptionCast (value: obj) =
        match value with
        | null -> ValueNone
        | _ ->
            let t = value.GetType()
            if ReflectionHelper.isOptionType t then
                let p = t.GetProperty("Value")
                ValueSome (p.GetValue(value, [||]))
            elif ReflectionHelper.isValueOptionType t then
                if value = Activator.CreateInstance t then ValueNone
                else
                    let p = t.GetProperty("Value")
                    ValueSome (p.GetValue(value, [||]))
            else ValueNone

    /// <summary>
    /// Matches a <see cref="System.Object"/> containing a boxed <see cref="Option{T}"/> or <see cref="ValueOption{T}"/>.
    /// Returns the wrapped value for <see cref="Some"/> and <see cref="ValueSome"/>, and does not match for <see cref="None"/>, <see cref="ValueNone"/>, or non-option values.
    /// </summary>
    [<return: Struct>]
    let (|ObjectOption|_|) (value : obj) = objectOptionCast value

    /// <summary>
    /// Lifts a <see cref="System.Object"/> to an <see cref="option{System.Object}"/>, unless it is already an <see cref="option{System.Object}"/>.
    /// </summary>
    let toOption x =
        match x with
        | null -> None
        | ObjectOption v
        | v -> Some v

    /// <summary>
    /// Lifts a <see cref="System.Object"/> to an <see cref="voption{System.Object}"/>, unless it is already an <see cref="voption{System.Object}"/>.
    /// </summary>
    let toValueOption x =
        match x with
        | null -> ValueNone
        | value ->
            let t = value.GetType()
            if ReflectionHelper.isOptionType t || ReflectionHelper.isValueOptionType t then
                objectOptionCast value
            else
                ValueSome value

    /// <summary>
    /// Unwraps a <see cref="System.Object"/> from an <see cref="option{System.Object}"/> or <see cref="voption{System.Object}"/>,
    /// unless it is not wrapped.
    /// </summary>
    let unwrap (value : objnull) =
        match value with
        | null -> null
        | value ->
            let t = value.GetType()
            if ReflectionHelper.isOptionType t then
                t.GetProperty("Value").GetValue (value, [||])
            elif ReflectionHelper.isValueOptionType t then
                if value = Activator.CreateInstance t then null
                else
                    t.GetProperty("Value").GetValue (value, [||])
            else value
