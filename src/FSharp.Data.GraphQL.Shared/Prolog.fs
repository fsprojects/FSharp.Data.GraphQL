/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open System.Reflection
open System.Collections.Generic

type GraphQLException(msg) =
    inherit Exception(msg)

type MalformedQueryException(msg) =
    inherit GraphQLException(msg)

/// General helper functions and types.
module Helpers =
    /// Executes a function that returns unit, and then return its parameter again.
    let tee f x = f x; x

    /// Casts a System.Object to an option to a System.Object option.
    let optionCast (value: obj) =
        let optionDef = typedefof<option<_>>
        if isNull value then None
        else
            let t = value.GetType()
            let p = t.GetProperty("Value")
            if t.IsGenericType && t.GetGenericTypeDefinition() = optionDef then
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

module internal Array =

    /// <summary>
    /// Returns a new array with unique elements. Uniqueness is determined by
    /// output of the <paramref name="keyf"/> function.
    /// </summary>
    /// <param name="keyf">Function, which output is used to determine uniqueness of input elements.</param>
    /// <param name="array">Array of elements.</param>
    let distinctBy keyf (array:'T[]) =
            let temp = Array.zeroCreate array.Length
            let mutable i = 0
            let hashSet = HashSet<_>(HashIdentity.Structural<_>)
            for v in array do
                if hashSet.Add(keyf v) then
                    temp.[i] <- v
                    i <- i + 1
            Array.sub temp 0 i

module internal List =

    /// <summary>
    /// Merges elements of two lists, returning a new list without duplicates.
    /// </summary>
    /// <param name="f">Function used to determine if any two given elements are considered equal.</param>
    /// <param name="listx">First list with elements to merge.</param>
    /// <param name="listy">Second list with elements to merge.</param>
    let mergeBy f listx listy =
        let uniqx =
            listx
            |> List.filter (fun x -> not <| List.exists(fun y -> f(x) = f(y)) listy)
        uniqx @ listy

module internal Set =

    /// <summary>
    /// Maps over each of the <paramref name="set"/> elements, applying function
    /// over each one of them to generate new Set. Sets generated this way are
    /// then flattened into single output set.
    /// </summary>
    /// <param name="f">Function used to generate Set from each of the input's elements.</param>
    /// <param name="set">Input set.</param>
    let collect f set = set |> Set.fold (fun acc e -> acc + f e) Set.empty

module internal Map =

    /// <summary>
    /// Merges the entries of two maps by their key, returning new map in result.
    /// </summary>
    /// <param name="mergeFn">
    /// Function, which takes key shared by entries in both maps, first entry's value,
    /// second entry's value to produce a result value used in newly generated map.
    /// </param>
    /// <param name="mapx">First map with elements to merge.</param>
    /// <param name="mapy">Second map with elements to merge.</param>
    let merge mergeFn mapx mapy =
        mapy
        |> Map.fold (fun acc ky vy ->
            match Map.tryFind ky acc with
            | Some vx -> Map.add ky (mergeFn ky vx vy) acc
            | None -> Map.add ky vy acc) mapx

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
                if value <> null
                then
                    let valueType = value.GetType().GetTypeInfo()
                    if valueType = optionType
                    then value
                    elif t.GetTypeInfo().IsAssignableFrom(valueType)
                    then createSome.Invoke(null, [| value |])
                    else null
                else none
        (some, none)