[<AutoOpen>]
module internal FSharp.Data.GraphQL.GraphQLWebSocketResponseExtensions

open System
open System.Collections.Generic
open System.Text.Json.Serialization
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Extensions

type GQLWebSocketResponse with

    static member Direct (data : Output) = {
        Data = Include data
        Path = Skip
        HasNext = Skip
        Errors = Skip
        Extensions = Skip
    }
    static member Direct (data : Output, errors) = {
        Data = Include data
        Path = Skip
        HasNext = Skip
        Errors = Skippable.ofList errors
        Extensions = Skip
    }
    static member Deferred (data, path) = {
        Data = Include data
        Path = Include path
        HasNext = Skip
        Errors = Skip
        Extensions = Skip
    }
    static member Deferred (data, path, hasNext) = {
        Data = Include data
        Path = Include path
        HasNext = Include hasNext
        Errors = Skip
        Extensions = Skip
    }
    static member Error (errors) = {
        Data = Skip
        Path = Skip
        HasNext = Skip
        Errors = Include errors
        Extensions = Skip
    }
    static member Error (data, errors) = {
        Data = data |> ValueOption.ofObj |> Skippable.ofValueOption
        Path = Skip
        HasNext = Skip
        Errors = Include errors
        Extensions = Skip
    }
    static member Error (data : objnull, path, errors) = {
        Data = data |> ValueOption.ofObj |> Skippable.ofValueOption
        Path = Include path
        HasNext = Skip
        Errors = Include errors
        Extensions = Skip
    }
