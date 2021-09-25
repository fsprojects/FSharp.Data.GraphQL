/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Client

open System
open Microsoft.FSharp.Reflection

module ReflectionPatterns =

    let isOption (t : Type) =
        t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<_ option>

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

    let (|Array|_|) (t : Type) =
        if t.IsArray then Some (Array (t.GetElementType()))
        else None