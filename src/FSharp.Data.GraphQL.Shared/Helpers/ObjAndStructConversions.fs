namespace rec FSharp.Data.GraphQL

open System.Linq
open System.Collections.Generic
open FsToolkit.ErrorHandling

module internal ValueOption =

    let mapOption mapping option = Option.toValueOption option |> ValueOption.map mapping

module internal Option =

    let mapValueOption mapping voption = voption |> ValueOption.map mapping |> ValueOption.toOption

[<AutoOpen>]
module KeyValuePair =

    let inline kvp key value = KeyValuePair (key, value)
    let inline kvpObj key (value : obj) = KeyValuePair (key, value)

[<AutoOpen>]
module internal ValueTuple =

    let fstv struct (a, _) = a
    let sndv struct (_, b) = b

[<AutoOpen>]
module Seq =

    let vchoose mapping seq =
        seq
        |> Seq.map mapping
        |> Seq.where ValueOption.isSome
        |> Seq.map ValueOption.get

    let vtryFind predicate seq =
        seq
        |> Seq.where predicate
        |> Seq.map ValueSome
        |> _.FirstOrDefault()

    let vtryHead (source : 'T seq) =
        use enumerator = source.GetEnumerator ()
        if not (enumerator.MoveNext ()) then
            ValueNone
        else
            match enumerator.Current with
            | null -> ValueNone
            | head -> ValueSome head

    let vtryLast (source : 'T seq) =
        use enumerator = source.GetEnumerator ()
        if not (enumerator.MoveNext ()) then
            ValueNone
        else
            let mutable last = enumerator.Current
            while enumerator.MoveNext () do
                last <- enumerator.Current
            match last with
            | null -> ValueNone
            | last -> ValueSome last

module internal List =

    let vchoose mapping list = list |> Seq.ofList |> Seq.vchoose mapping |> List.ofSeq

    let vtryFind predicate list = list |> Seq.ofList |> Seq.vtryFind predicate

module internal Array =

    let vchoose mapping array = array |> Seq.vchoose mapping |> Array.ofSeq

module internal Map =

    let vtryFind key (map : Map<_, _>) =
        match map.TryGetValue key with
        | true, value -> ValueSome value
        | false, _ -> ValueNone
