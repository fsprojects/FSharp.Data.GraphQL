// The MIT License (MIT)
namespace  FSharp.Data.GraphQL

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Collections.ObjectModel
open FsToolkit.ErrorHandling

type internal CoerceVariableError =
    {
        InnerError : IGQLError
        VariableName : string
        TypeName : string
        Path : FieldPath voption
    }
    interface IGQLError with

        member this.Message = this.InnerError.Message

    interface IGQLErrorExtensions with

        member this.Extensions =

            [
                yield KeyValuePair("variableName", this.VariableName |> box)
                yield KeyValuePair("typeName", this.TypeName |> box)

                match this.Path with
                | ValueSome path -> yield KeyValuePair("path", path)
                | ValueNone -> ()

                match this.InnerError with
                | :? IGQLErrorExtensions as ext ->
                    match ext.Extensions with
                    | ValueSome extensions -> yield! extensions
                    | ValueNone -> ()
                | _ -> ()
            ]
            |> Dictionary<_,_>
            |> ReadOnlyDictionary<_,_>
            :> IReadOnlyDictionary<string, obj>
            |> ValueSome

type internal CoerceArgumentError =
    {
        InnerError : IGQLError
        ArgumentName : string
        TypeName : string
        Path : FieldPath voption
    }
    interface IGQLError with

        member this.Message = this.InnerError.Message

    interface IGQLErrorExtensions with

        member this.Extensions =

            [
                yield KeyValuePair("argumentName", this.ArgumentName |> box)
                yield KeyValuePair("typeName", this.TypeName |> box)

                match this.Path with
                | ValueSome path -> yield KeyValuePair("path", path)
                | ValueNone -> ()

                match this.InnerError with
                | :? IGQLErrorExtensions as ext ->
                    match ext.Extensions with
                    | ValueSome extensions -> yield! extensions
                    | ValueNone -> ()
                | _ -> ()
            ]
            |> Dictionary<_,_>
            |> ReadOnlyDictionary<_,_>
            :> IReadOnlyDictionary<string, obj>
            |> ValueSome

