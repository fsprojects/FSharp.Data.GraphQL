// The MIT License (MIT)
namespace FSharp.Data.GraphQL

open System
open System.Collections.Generic
open FsToolkit.ErrorHandling
open FSharp.Data.GraphQL.Types

type InputSource =
    | Variable of VarDef : VarDef
    | Argument of ArgDef : InputFieldDef
    | Unknown

[<Struct>]
type internal ObjectFieldErrorDetails = { ObjectDef : InputDef; FieldDef : InputFieldDef voption }

type internal IInputSourceError =
    inherit IGQLError
    abstract member InputSource : InputSource with get, set

type internal CoercionError = {
    mutable InputSource : InputSource
    Message : string
    ErrorKind : ErrorKind
    Path : FieldPath
    FieldErrorDetails : ObjectFieldErrorDetails voption
} with

    interface IGQLError with

        member this.Message = this.Message
        member this.Exception = None

    interface IGQLErrorExtensions with

        member this.Extensions =

            [
                yield KeyValuePair (CustomErrorFields.Kind, this.ErrorKind |> box)
                match this.Path with
                | [] -> ()
                | path -> yield KeyValuePair (CustomErrorFields.Path, path |> List.rev |> box)

                match this.InputSource with
                | Variable varDef ->
                    yield KeyValuePair (CustomErrorFields.VariableName, varDef.Name |> box)
                    yield KeyValuePair (CustomErrorFields.VariableType, (string varDef.TypeDef) |> box)
                | Argument argDef ->
                    yield KeyValuePair (CustomErrorFields.ArgumentName, argDef.Name |> box)
                    yield KeyValuePair (CustomErrorFields.ArgumentType, (string argDef.TypeDef) |> box)
                | Unknown -> ()

                match this.FieldErrorDetails with
                | ValueSome details ->
                    yield KeyValuePair (CustomErrorFields.ObjectType, (string details.ObjectDef) |> box)
                    match details.FieldDef with
                    | ValueSome fieldDef -> yield KeyValuePair (CustomErrorFields.FieldType, (string fieldDef.TypeDef) |> box)
                    | ValueNone -> ()
                | ValueNone -> ()
            ]
            |> Dictionary<_, _>
            :> IReadOnlyDictionary<string, obj>
            |> ValueSome

    interface IInputSourceError with

        member this.InputSource
            with get () = this.InputSource
            and set (value) = this.InputSource <- value

type internal CoercionErrorWrapper = {
    mutable InputSource : InputSource
    InnerError : IGQLError
    ErrorKind : ErrorKind
    Path : FieldPath
    FieldErrorDetails : ObjectFieldErrorDetails voption
} with

    interface IGQLError with

        member this.Message = this.InnerError.Message
        member this.Exception = this.InnerError.Exception

    interface IGQLErrorExtensions with

        member this.Extensions =

            [
                yield KeyValuePair (CustomErrorFields.Kind, this.ErrorKind |> box)
                match this.Path with
                | [] -> ()
                | path -> yield KeyValuePair (CustomErrorFields.Path, path |> List.rev |> box)

                match this.InputSource with
                | Variable varDef ->
                    yield KeyValuePair (CustomErrorFields.VariableName, varDef.Name |> box)
                    yield KeyValuePair (CustomErrorFields.VariableType, (string varDef.TypeDef) |> box)
                | Argument argDef ->
                    yield KeyValuePair (CustomErrorFields.ArgumentName, argDef.Name |> box)
                    yield KeyValuePair (CustomErrorFields.ArgumentType, (string argDef.TypeDef) |> box)
                | Unknown -> ()

                match this.FieldErrorDetails with
                | ValueSome details ->
                    yield KeyValuePair (CustomErrorFields.ObjectType, (string details.ObjectDef) |> box)
                    match details.FieldDef with
                    | ValueSome fieldDef -> yield KeyValuePair (CustomErrorFields.FieldType, (string fieldDef.TypeDef) |> box)
                    | ValueNone -> ()
                | ValueNone -> ()

                match this.InnerError with
                | :? IGQLErrorExtensions as ext ->
                    match ext.Extensions with
                    | ValueSome extensions -> yield! extensions
                    | ValueNone -> ()
                | _ -> ()
            ]
            |> Dictionary<_, _>
            :> IReadOnlyDictionary<string, obj>
            |> ValueSome

    interface IInputSourceError with

        member this.InputSource
            with get () = this.InputSource
            and set (value) = this.InputSource <- value
