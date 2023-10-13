// The MIT License (MIT)

[<AutoOpen>]
module FSharp.Data.GraphQL.Errors

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Collections.ObjectModel
open FsToolkit.ErrorHandling

let getObjectErrors (object: IReadOnlyDictionary<string, Result<'t, IGQLError>>) =
    object
    |> Seq.choose (fun kvp ->
        match kvp.Value with
        | Ok _ -> None
        | Error err -> Some err)
    |> Seq.toList

let getObjectValues (object: IReadOnlyDictionary<string, Result<'t, IGQLError>>) =
    object
    |> Seq.map (fun kvp ->
        match kvp.Value with
        | Ok value -> KeyValuePair(kvp.Key, value)
        | Error _ -> raise <| ArgumentException())
    |> Dictionary
    |> ReadOnlyDictionary
    :> IReadOnlyDictionary<_,_>

let splitObjectErrors (object: IReadOnlyDictionary<string, Result<'t, IGQLError>>) =
    let errors = object |> getObjectErrors

    if not <| List.isEmpty errors then
        Error errors
    else
        let values = object |> getObjectValues
        Ok values

let getObjectErrorsList (object: IReadOnlyDictionary<string, Result<'t, IGQLError list>>) =
    object
    |> Seq.choose (fun kvp ->
        match kvp.Value with
        | Ok _ -> None
        | Error err -> Some err)
    |> Seq.toList

let getObjectValuesList (object: IReadOnlyDictionary<string, Result<'t, IGQLError list>>) =
    object
    |> Seq.map (fun kvp ->
        match kvp.Value with
        | Ok value -> KeyValuePair(kvp.Key, value)
        | Error _ -> raise <| ArgumentException())
    |> Dictionary
    |> ReadOnlyDictionary
    :> IReadOnlyDictionary<_,_>

let splitObjectErrorsList (object: IReadOnlyDictionary<string, Result<'t, IGQLError list>>) =
    let errors = object |> getObjectErrorsList

    if not <| List.isEmpty errors then
        Error (errors |> List.collect id)
    else
        let values = object |> getObjectValuesList
        Ok values

let getSeqErrors (items: Result<'t, IGQLError> seq) =
    items
    |> Seq.choose (fun result ->
        match result with
        | Ok _ -> None
        | Error err -> Some err)
    |> Seq.toList

let getSeqValues (items: Result<'t, IGQLError> seq) =
    items
    |> Seq.map (fun result ->
        match result with
        | Ok value -> value
        | Error _ -> raise <| ArgumentException())
    |> Seq.toArray

let splitSeqErrors (items: Result<'t, IGQLError> seq) =
    let errors = items |> getSeqErrors

    if not <| List.isEmpty errors then
        Error errors
    else
        let values = items |> getSeqValues
        Ok values

let getSeqErrorsList (items: Result<'t, IGQLError list> seq) =
    items
    |> Seq.choose (fun result ->
        match result with
        | Ok _ -> None
        | Error err -> Some err)
    |> Seq.toList

let getSeqValuesList (items: Result<'t, IGQLError list> seq) =
    items
    |> Seq.map (fun result ->
        match result with
        | Ok value -> value
        | Error _ -> raise <| ArgumentException())
    |> Seq.toArray

let splitSeqErrorsList (items: Result<'t, IGQLError list> seq) =
    let errors = items |> getSeqErrorsList

    if not <| List.isEmpty errors then
        Error (errors |> List.collect id)
    else
        let values = items |> getSeqValuesList
        Ok values
