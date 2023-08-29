// The MIT License (MIT)
namespace  FSharp.Data.GraphQL

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Collections.ObjectModel
open FsToolkit.ErrorHandling
open FSharp.Data.GraphQL.Types

type InputRoot =
    | Variable of VarDef: VarDef
    | Argument of ArgDef: InputFieldDef

type ErrorKind =
    | InputCoercion
    | InputObjectValidation
    | Execution

type internal CoercionError =
    {
        InnerError : IGQLError
        ErrorKind : ErrorKind
        ObjectType : string voption
        FieldType : string voption
        Path : FieldPath
    }
    interface IGQLError with

        member this.Message = this.InnerError.Message

    interface IGQLErrorExtensions with

        member this.Extensions =

            [
                yield KeyValuePair("kind", this.ErrorKind |> box)
                if this.ObjectType.IsSome then yield KeyValuePair("objectType", this.ObjectType.Value |> box)
                if this.FieldType.IsSome then yield KeyValuePair("fieldType", this.FieldType.Value |> box)

                match this.Path with
                | [] -> ()
                | path -> yield KeyValuePair("path", path |> Seq.rev |> box)

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

[<Struct>]
type internal ObjectFieldErrorDetails = {
    ObjectDef : InputObjectDef
    FieldDef : InputFieldDef voption
}

type internal CoerceVariableError =
    {
        Message : string
        ErrorKind : ErrorKind
        Variable : VarDef
        Path : FieldPath
        FieldErrorDetails : ObjectFieldErrorDetails voption
    }
    interface IGQLError with

        member this.Message = this.Message

    interface IGQLErrorExtensions with

        member this.Extensions =

            [
                yield KeyValuePair("kind", this.ErrorKind |> box)
                yield KeyValuePair("variableName", this.Variable.Name |> box)
                yield KeyValuePair("variableType", (this.Variable.TypeDef :?> NamedDef).Name |> box)
                match this.Path with
                | [] -> ()
                | path -> yield KeyValuePair("path", path |> Seq.rev |> box)

                match this.FieldErrorDetails with
                | ValueSome details ->
                    yield KeyValuePair("objectType", details.ObjectDef.Name |> box)
                    match details.FieldDef with
                    | ValueSome fieldDef ->
                        yield KeyValuePair("fieldType", (fieldDef.TypeDef :?> NamedDef).Name|> box)
                    | ValueNone -> ()
                | ValueNone -> ()
            ]
            |> Dictionary<_,_>
            |> ReadOnlyDictionary<_,_>
            :> IReadOnlyDictionary<string, obj>
            |> ValueSome

type internal CoerceVariableErrorWrapper =
    {
        InnerError : IGQLError
        ErrorKind : ErrorKind
        Variable: VarDef
        Path : FieldPath
        FieldErrorDetails : ObjectFieldErrorDetails voption
    }
    interface IGQLError with

        member this.Message = this.InnerError.Message

    interface IGQLErrorExtensions with

        member this.Extensions =

            [
                yield KeyValuePair("kind", this.ErrorKind |> box)
                yield KeyValuePair("variableName", this.Variable.Name |> box)
                yield KeyValuePair("variableType", (this.Variable.TypeDef :?> NamedDef).Name |> box)
                match this.Path with
                | [] -> ()
                | path -> yield KeyValuePair("path", path |> Seq.rev |> box)

                match this.FieldErrorDetails with
                | ValueSome details ->
                    yield KeyValuePair("objectType", details.ObjectDef.Name |> box)
                    match details.FieldDef with
                    | ValueSome fieldDef ->
                        yield KeyValuePair("fieldType", (fieldDef.TypeDef :?> NamedDef).Name|> box)
                    | ValueNone -> ()
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
        Path : FieldPath
    }
    interface IGQLError with

        member this.Message = this.InnerError.Message

    interface IGQLErrorExtensions with

        member this.Extensions =

            [
                yield KeyValuePair("argumentName", this.ArgumentName |> box)
                yield KeyValuePair("typeName", this.TypeName |> box)

                match this.Path with
                | [] -> ()
                | path -> yield KeyValuePair("path", path |> Seq.rev |> box)

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

