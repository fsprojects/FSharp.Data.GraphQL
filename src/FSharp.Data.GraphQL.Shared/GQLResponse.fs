namespace FSharp.Data.GraphQL

open System
open System.Collections.Generic
open System.Text.Json.Serialization
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Extensions

type Output = IDictionary<string, obj>

type GQLResponse = {
    Data : Output Skippable
    Errors : GQLProblemDetails list Skippable
    Extensions : Output Skippable
}

type GQLWebSocketResponse = {
    Data : objnull Skippable
    Path : FieldPath Skippable
    HasNext : bool Skippable
    Errors : GQLProblemDetails list Skippable
    Extensions : Output Skippable
} with

    static member OfData (data : Output) = {
        Data = Include data
        Path = Skip
        HasNext = Skip
        Errors = Skip
        Extensions = Skip
    }
    static member OfData (data : Output, errors) = {
        Data = Include data
        Path = Skip
        HasNext = Skip
        Errors = Skippable.ofList errors
        Extensions = Skip
    }
    static member OfDefered (data, path) = {
        Data = Include data
        Path = Include path
        HasNext = Skip
        Errors = Skip
        Extensions = Skip
    }
    static member OfDefered (data, path, hasNext) = {
        Data = Include data
        Path = Include path
        HasNext = Include hasNext
        Errors = Skip
        Extensions = Skip
    }
    static member OfErrors (errors) = {
        Data = Skip
        Path = Skip
        HasNext = Skip
        Errors = Include errors
        Extensions = Skip
    }
    static member OfErrors (data, errors) = {
        Data = data |> ValueOption.ofObj |> Skippable.ofValueOption
        Path = Skip
        HasNext = Skip
        Errors = Include errors
        Extensions = Skip
    }
    static member OfErrors (data : objnull, path, errors) = {
        Data = data |> ValueOption.ofObj |> Skippable.ofValueOption
        Path = Include path
        HasNext = Skip
        Errors = Include errors
        Extensions = Skip
    }
