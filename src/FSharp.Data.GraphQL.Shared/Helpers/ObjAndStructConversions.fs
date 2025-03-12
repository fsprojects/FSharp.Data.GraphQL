namespace rec FSharp.Data.GraphQL

open System.Linq
open FsToolkit.ErrorHandling

module internal ValueOption =

    let mapOption mapping option = Option.toValueOption option |> ValueOption.map mapping

module internal Option =

    let mapValueOption mapping voption = voption |> ValueOption.map mapping |> ValueOption.toOption

[<AutoOpen>]
module internal ValueTuple =

    let fstv struct (a, _) = a
    let sndv struct (_, b) = b

module internal Seq =

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
