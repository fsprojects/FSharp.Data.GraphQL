namespace rec FSharp.Data.GraphQL

module internal ValueOption =

    let toOption value =
        match value with | ValueSome v -> Some v | _ -> None

    let mapOption mapping option = Option.toVOption option |> ValueOption.map mapping

    let ofOption value = Option.toVOption value

module internal Option =

    let toVOption voption =
        match voption with | Some v -> ValueSome v | _ -> ValueNone

    let mapVOption mapping voption = voption |> ValueOption.map mapping |> ValueOption.toOption

    let ofVOption voption = voption |> ValueOption.toOption

[<AutoOpen>]
module internal ValueTuple =

    let fstv struct (a,_) =  a
    let sndv struct (_,b) =  b
