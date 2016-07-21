/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open System.Reflection
open System.Collections.Generic

type GraphQLException(msg) = 
    inherit Exception(msg)

module Array =
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
    let mergeBy f listx listy =
        let uniqx = 
            listx
            |> List.filter (fun x -> not <| List.exists(fun y -> f(x) = f(y)) listy)
        uniqx @ listy

module Map =
    let merge mergeFn mapx mapy =
        mapy
        |> Map.fold (fun acc ky vy -> 
            match Map.tryFind ky acc with
            | Some vx -> Map.add ky (mergeFn ky vx vy) acc
            | None -> Map.add ky vy acc) mapx

module Option =
    let toObj value =  match value with None -> null | Some x -> x

module ReflectionHelper =
    /// Returns cons(head,tail)/nil pair for list type generated in runtime from type t.
    let listOfType t =
        let listType = typedefof<_ list>.GetTypeInfo().MakeGenericType([|t|]).GetTypeInfo()
        let nil = 
            let empty = listType.GetDeclaredProperty "Empty"
            empty.GetValue (null)
        let cons = 
            let cons = listType.GetDeclaredMethod "Cons"
            fun item list -> cons.Invoke (null, [| item; list |])
        (cons, nil)

    /// Returns some(value)/none pair for option type generated in runtime from type t
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
                    elif t .GetTypeInfo().IsAssignableFrom(valueType)
                    then createSome.Invoke(null, [| value |])
                    else null
                else none
        (some, none)