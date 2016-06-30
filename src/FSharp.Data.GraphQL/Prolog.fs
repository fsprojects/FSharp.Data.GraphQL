/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
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

module Option =
    let toObj value =  match value with None -> null | Some x -> x

module ReflectionHelper =
    /// Returns cons(head,tail)/nil pair for list type generated in runtime from type t.
    let listOfType t =
        let listType = typedefof<Microsoft.FSharp.Collections.List<_>>.MakeGenericType [| t |]
        let nil = 
            let empty = listType.GetProperty "Empty"
            empty.GetValue (null)
        let cons = 
            let cons = listType.GetMethod "Cons"
            fun item list -> cons.Invoke (null, [| item; list |])
        (cons, nil)

    /// Returns some(value)/none pair for option type generated in runtime from type t
    let optionOfType t =
        let optionType = typedefof<option<_>>.MakeGenericType [| t |]
        let none = 
            let x = optionType.GetProperty "None"
            x.GetValue(null)
        let some =
            let createSome = optionType.GetMethod "Some"
            fun value -> 
                if value <> null 
                then
                    let valueType = value.GetType()
                    if valueType = optionType then value
                    elif t.IsAssignableFrom(valueType) then createSome.Invoke(null, [| value |])
                    else null
                else none
        (some, none)