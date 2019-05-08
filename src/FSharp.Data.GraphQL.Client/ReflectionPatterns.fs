/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Client

open System
open System.Collections
open Microsoft.FSharp.Reflection

[<AutoOpen>]
module ReflectionPatterns =
    let private numericTypes =
        [| typeof<decimal>
           typeof<double>
           typeof<single>
           typeof<uint64>
           typeof<int64>
           typeof<uint32>
           typeof<int>
           typeof<uint16>
           typeof<int16>
           typeof<byte>
           typeof<sbyte> |]

    let isOption (t : Type) = 
        t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<_ option>

    let isMap (t : Type) =
       t = typeof<Map<string, obj>>

    let isList (t : Type) =
        if t.IsGenericType
        then
            let gtype = t.GetGenericTypeDefinition()
            if gtype = typedefof<_ list>
            then true
            else false
        else false

    let isSeq (t : Type) =
        if t.IsGenericType
        then
            let gtype = t.GetGenericTypeDefinition()
            if gtype = typedefof<seq<_>>
            then true
            else false
        else false

    let (|Option|_|) t =
        if isOption t then Some (Option (t.GetGenericArguments().[0]))
        else None

    let isType (expected : Type) (t : Type) =
        match t with
        | Option t -> t = expected
        | _ -> t = expected

    let isNumericType (t : Type) =
        numericTypes |> Array.exists (fun expected -> isType expected t)

    let (|Array|_|) (t : Type) =
        if t.IsArray then Some (Array (t.GetElementType()))
        else None

    let (|List|_|) (t : Type) =
        if isList t then Some (List (t.GetGenericArguments().[0]))
        else None

    let (|Seq|_|) (t : Type) =
        if isSeq t then Some (Seq (t.GetGenericArguments().[0]))
        else None

    let (|EnumerableValue|_|) (x : obj) =
        match x with
        | :? IEnumerable as x -> Some (EnumerableValue (Seq.cast<obj> x |> Array.ofSeq))
        | _ -> None

    let (|OptionValue|_|) (x : obj) =
        let xtype = x.GetType()
        if isOption xtype
        then
            match FSharpValue.GetUnionFields(x, xtype) with
            | (_, [|value|]) -> Some (OptionValue Some value)
            | _ -> Some (OptionValue None)
        else None

    let (|EnumValue|_|) (x : obj) =
        let xtype = x.GetType()
        if xtype.IsEnum
        then Some (x.ToString())
        else None
