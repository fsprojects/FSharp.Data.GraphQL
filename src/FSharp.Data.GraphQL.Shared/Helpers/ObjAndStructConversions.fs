namespace rec FSharp.Data.GraphQL

open System.Text.Json.Serialization
open FsToolkit.ErrorHandling

module internal Option =

    let mergeWith (f : 'T -> 'T -> 'T) (o1 : 'T option) (o2 : 'T option) : 'T option =
        match (o1, o2) with
        | Some a, Some b -> Some (f a b)
        | Some a, _ -> Some a
        | _, Some b -> Some b
        | _, _ -> None

    let unwrap (defaultValue : 'U) (onSome : 'T -> 'U) (o : 'T option) : 'U =
        match o with
        | Some t -> onSome t
        | None -> defaultValue

    let mapValueOption mapping voption =
        voption
        |> ValueOption.map mapping
        |> ValueOption.toOption

module internal ValueOption =

    let mergeWith (f : 'T -> 'T -> 'T) (o1 : 'T voption) (o2 : 'T voption) : 'T voption =
        match (o1, o2) with
        | ValueSome a, ValueSome b -> ValueSome (f a b)
        | ValueSome a, _ -> ValueSome a
        | _, ValueSome b -> ValueSome b
        | _, _ -> ValueNone

    let unwrap (defaultValue : 'U) (onSome : 'T -> 'U) (o : 'T voption) : 'U =
        match o with
        | ValueSome t -> onSome t
        | ValueNone -> defaultValue

    let mapOption mapping option =
        option
        |> Option.toValueOption
        |> ValueOption.map mapping

module Skippable =

    let ofList list =
        match list with
        | [] -> Skip
        | list -> Include list

[<AutoOpen>]
module internal ValueTuple =

    let fstv struct (a, _) = a
    let sndv struct (_, b) = b
