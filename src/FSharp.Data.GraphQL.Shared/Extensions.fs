// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

module internal FSharp.Data.GraphQL.Extensions

open System.Reflection
open System.Collections.Generic
open System.Text.Json.Serialization

type IDictionary<'TKey, 'TValue> with

    member x.TryFind(key : 'TKey) =
        match x.TryGetValue(key) with
        | (true, value) -> Some value
        | _ -> None

type TypeInfo with
    /// If no property is found with the specified name, it will try changing the case of the first letter
    member x.GetDeclaredProperty(propertyName: string, ignoreCase: bool) =
        match x.GetDeclaredProperty(propertyName), ignoreCase with
        | null, true ->
            let first =
                let first = propertyName.Substring(0,1)
                match first.ToUpper() with
                | upper when upper <> first -> upper
                | _ -> first.ToLower()
            x.GetDeclaredProperty(first + propertyName.Substring(1))
        | prop, _ -> prop

    /// If no method is found with the specified name, it will try changing the case of the first letter
    member x.GetDeclaredMethod(propertyName: string, ignoreCase: bool) =
        match x.GetDeclaredMethod(propertyName), ignoreCase with
        | null, true ->
            let first =
                let first = propertyName.Substring(0,1)
                match first.ToUpper() with
                | upper when upper <> first -> upper
                | _ -> first.ToLower()
            x.GetDeclaredMethod(first + propertyName.Substring(1))
        | prop, _ -> prop

module Option =

    let mergeWith (f: 'T -> 'T -> 'T) (o1 : 'T option) (o2 : 'T option) : 'T option =
        match (o1, o2) with
        | Some a, Some b -> Some (f a b)
        | Some a, _ -> Some a
        | _, Some b -> Some b
        | _, _ -> None

    let unwrap (defaultValue : 'U) (onSome : 'T -> 'U) (o : 'T option) : 'U =
        match o with
        | Some t -> onSome t
        | None -> defaultValue

module Skippable =

    let ofList list =
        match list with
        | [] -> Skip
        | list -> Include list

module Dictionary =

    let addWith (f : 'V -> 'V -> 'V) (key : 'K) (value : 'V) (dict : Dictionary<'K, 'V>) : unit =
        match dict.TryGetValue(key) with
        | true, v -> dict.[key] <- f value v
        | false, _ -> dict.Add(key, value)

module Array =

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

module List =

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

module Set =

    /// <summary>
    /// Maps over each of the <paramref name="set"/> elements, applying function
    /// over each one of them to generate new Set. Sets generated this way are
    /// then flattened into single output set.
    /// </summary>
    /// <param name="f">Function used to generate Set from each of the input's elements.</param>
    /// <param name="set">Input set.</param>
    let collect f set = set |> Set.fold (fun acc e -> acc + f e) Set.empty

module Map =

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
