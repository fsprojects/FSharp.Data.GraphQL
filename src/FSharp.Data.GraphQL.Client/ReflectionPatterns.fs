// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Client

open System
open System.Collections
open Microsoft.FSharp.Reflection

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
        then t.GetGenericTypeDefinition() = typedefof<_ list>
        else false

    let isSeq (t : Type) =
        if t.IsGenericType
        then t.GetGenericTypeDefinition() = typedefof<seq<_>>
        else false

    let getOptionCases (t: Type) =
        let otype = typedefof<_ option>.MakeGenericType(t)
        let cases = FSharpType.GetUnionCases(otype)
        let some = cases |> Array.find (fun c -> c.Name = "Some")
        let none = cases |> Array.find (fun c -> c.Name = "None")
        (some, none, otype)

    let makeArray (itype : Type) (items : obj []) =
        if Array.exists isNull items
        then failwith "Array is an array of non null items, but a null item was found."
        else
            let arr = Array.CreateInstance(itype, items.Length)
            items |> Array.iteri (fun i x -> arr.SetValue(x, i))
            box arr

    let makeOptionArray (itype : Type) (items : obj []) =
        let (some, none, otype) = getOptionCases(itype)
        let arr = Array.CreateInstance(otype, items.Length)
        let mapper (i : int) (x : obj) =
            if isNull x
            then arr.SetValue(FSharpValue.MakeUnion(none, [||]), i)
            else arr.SetValue(FSharpValue.MakeUnion(some, [|x|]), i)
        items |> Array.iteri mapper
        box arr

    let makeSome (value : obj) =
        let (some, _, _) = getOptionCases (value.GetType())
        FSharpValue.MakeUnion(some, [|value|])

    let makeNone (t : Type) =
        let (_, none, _) = getOptionCases t
        FSharpValue.MakeUnion(none, [||])

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

    let makeValue (t : Type) (value : obj) =
        let isOption = isOption t
        match value, isOption with
        | null, true -> makeNone t
        | OptionValue (Some null), true -> box (makeSome (Convert.ChangeType(null, t)))
        | OptionValue (Some value), true -> box (makeSome value)
        | OptionValue (Some value), false -> Convert.ChangeType(value, t)
        | OptionValue None, false -> Convert.ChangeType(null, t)
        | OptionValue None, true -> box (makeNone t)
        | value, true -> makeSome value
        | value, false -> Convert.ChangeType(value, t)
