/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module internal FSharp.Data.GraphQL.Extensions

open System.Reflection
open System.Collections.Generic

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

module Result =
    let catchError (handle : 'Err -> 'T) (r : Result<'T, 'Err>) : 'T =
        match r with
        | Ok x -> x
        | Error err -> handle err

    let fromOption (errValue : 'Err) (o : 'T option) : Result<'T, 'Err> =
        match o with
        | Some x -> Ok x
        | None -> Error errValue

module Dictionary =
    let adjust (f : 'V -> 'V) (key : 'K) (dict : Dictionary<'K, 'V>) : unit =
        match dict.TryGetValue(key) with
        | true, v -> dict.[key] <- f v
        | false, _ -> ()

    let addWith (f : 'V -> 'V -> 'V) (key : 'K) (value : 'V) (dict : Dictionary<'K, 'V>) : unit =
        match dict.TryGetValue(key) with
        | true, v -> dict.[key] <- f v value
        | false, _ -> dict.Add(key, value)
