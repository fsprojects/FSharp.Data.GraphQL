namespace rec FSharp.Data.GraphQL

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

    let vtryHead (source : 'T seq) =
        use enumerator = source.GetEnumerator ()
        if not (enumerator.MoveNext ()) then
            ValueNone
        else
            ValueSome enumerator.Current

    let vtryLast (source : 'T seq) =
        use enumerator = source.GetEnumerator ()
        if not (enumerator.MoveNext ()) then
            ValueNone
        else
            let mutable last = enumerator.Current
            while enumerator.MoveNext () do
                last <- enumerator.Current
            ValueSome last

    let vchoose mapping seq =
        seq
        |> Seq.map mapping
        |> Seq.where ValueOption.isSome
        |> Seq.map ValueOption.get

    let vtryFind predicate (source : 'T seq) = source |> Seq.where predicate |> Seq.vtryHead

    let vtryItem index (source : 'T seq) =
        if index < 0 then
            ValueNone
        else
            use enumerator = source.GetEnumerator ()
            let mutable currentIndex = 0
            let mutable result = ValueNone
            let mutable found = false

            while not found && enumerator.MoveNext () do
                if currentIndex = index then
                    result <- ValueSome enumerator.Current
                    found <- true
                else
                    currentIndex <- currentIndex + 1

            result

module internal List =

    let vchoose mapping list = list |> Seq.vchoose mapping |> Seq.toList

    let vtryFind predicate list = list |> Seq.where predicate |> Seq.vtryHead

    let vtryItem index list =
        let rec loop currentIndex list =
            match currentIndex, list with
            | _, [] -> ValueNone
            | 0, head :: _ -> ValueSome head
            | currentIndex, _ :: tail when currentIndex > 0 -> loop (currentIndex - 1) tail
            | _ -> ValueNone

        loop index list

module internal Array =

    let vchoose mapping array = array |> Seq.vchoose mapping |> Seq.toArray

    let vtryItem index (array : 'T array) =
        if index < 0 || index >= array.Length then
            ValueNone
        else
            ValueSome array[index]

module internal Map =

    let vtryFind key (map : Map<_, _>) =
        match map.TryGetValue key with
        | true, value -> ValueSome value
        | false, _ -> ValueNone
