namespace rec FSharp.Data.GraphQL

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

module internal List =

    let vchoose mapping list = list |> Seq.vchoose mapping |> List.ofSeq

module internal Array =

    let vchoose mapping array = array |> Seq.vchoose mapping |> Array.ofSeq
