// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Reflection
open System.Text.Json.Serialization

/// General helper functions and types.
module Helpers =

    /// Casts a System.Object to a System.Object option.
    let optionCast (value: obj) =
        if isNull value then None
        else
            let t = value.GetType()
            if t.FullName.StartsWith  "Microsoft.FSharp.Core.FSharpOption`1" then
                let p = t.GetProperty("Value")
                Some (p.GetValue(value, [||]))
            elif t.FullName.StartsWith "Microsoft.FSharp.Core.FSharpValueOption`1" then
                if value = Activator.CreateInstance t then None
                else
                    let p = t.GetProperty("Value")
                    Some (p.GetValue(value, [||]))
            else None

    /// Matches a System.Object with an option.
    /// If the object is an Option, returns it as Some, otherwise, return None.
    let (|ObjectOption|_|) = optionCast

    /// Lifts a System.Object to an option, unless it is already an option.
    let toOption x =
        match x with
        | null -> None
        | ObjectOption v
        | v -> Some v

module internal ReflectionHelper =

    /// <summary>
    /// Returns pair of function constructors for `cons(head,tail)` and `nil`
    /// used to create list of type <paramref name="t"/> given at runtime.
    /// </summary>
    /// <param name="t">Type used for result list constructors as type param</param>
    let listOfType t =
        let listType = typedefof<_ list>.GetTypeInfo().MakeGenericType([|t|]).GetTypeInfo()
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

    /// <summary>
    /// Returns pair of function constructors for `some(value)` and `none`
    /// used to create option of type <paramref name="t"/> given at runtime.
    /// </summary>
    /// <param name="t">Type used for result option constructors as type param</param>
    let optionOfType t =
        let optionType = typedefof<_ option>.GetTypeInfo().MakeGenericType([|t|]).GetTypeInfo()
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
    /// Returns pair of function constructors for `some(value)` and `none`
    /// used to create option of type <paramref name="t"/> given at runtime.
    /// </summary>
    /// <param name="t">Type used for result option constructors as type param</param>
    let vOptionOfType t =
        let optionType = typedefof<_ voption>.GetTypeInfo().MakeGenericType([|t|]).GetTypeInfo()
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

    /// <summary>
    /// Returns pair of function constructors for `include(value)` and `skip`
    /// used to create option of type <paramref name="t"/> given at runtime.
    /// </summary>
    /// <param name="t">Type used for result option constructors as type param</param>
    let skippableOfType t =
        let skippableType = typedefof<_ Skippable>.GetTypeInfo().MakeGenericType([|t|]).GetTypeInfo()
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
