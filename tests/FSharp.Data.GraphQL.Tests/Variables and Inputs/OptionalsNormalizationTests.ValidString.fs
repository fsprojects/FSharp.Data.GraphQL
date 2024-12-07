// The MIT License (MIT)

namespace FSharp.Data.GraphQL.Tests.OptionalsNormalizationTests

open System
open FSharp.Data.GraphQL
open FsToolkit.ErrorHandling

[<Struct>]
type ValidString<'t> = internal ValidString of string
with
    static member internal CreateVOption<'t> (value: string option) : ValidString<'t> voption =
        value |> ValueOption.ofOption |> ValueOption.map ValidString

module ValidString =

    let value (ValidString text) = text

    let vOption strOption : string voption = strOption |> ValueOption.map value

    let vOptionValue strOption : string = strOption |> ValueOption.map value |> ValueOption.toObj

open Validus
open Validus.Operators

module String =

    let notStartsWithWhiteSpace fieldName (s: string) =
        if s.StartsWith ' ' then
            Error
            <| ValidationErrors.create fieldName [ $"'%s{fieldName}' field cannot start with whitespace" ]
        else
            Ok <| s

    let notEndsWithWhiteSpace fieldName (s: string) =
        if s.EndsWith ' ' then
            Error
            <| ValidationErrors.create fieldName [ $"'%s{fieldName}' field cannot end with whitespace" ]
        else
            Ok <| s

    let notContainsWhiteSpace fieldName (s: string) =
        if s.Contains ' ' then
            Error
            <| ValidationErrors.create fieldName [ $"'%s{fieldName}' field cannot contain whitespace" ]
        else
            Ok <| s

    let notContainsBacktick fieldName (s: string) =
        if s.Contains '`' then
            Error
            <| ValidationErrors.create fieldName [ $"'%s{fieldName}' field cannot contain backtick" ]
        else
            Ok <| s

    let notContainsTilde fieldName (s: string) =
        if s.Contains '~' then
            Error
            <| ValidationErrors.create fieldName [ $"'%s{fieldName}' field cannot contain tilde" ]
        else
            Ok <| s

    let notContainsDash fieldName (s: string) =
        if s.Contains '-' then
            Error
            <| ValidationErrors.create fieldName [ $"'%s{fieldName}' field cannot contain dash: '-'" ]
        else
            Ok <| s

    let notContainsUmlauts fieldName (s: string) =
        let umlauts = [ 'ä'; 'ö'; 'ü'; 'ß'; 'Ä'; 'Ö'; 'Ü' ] |> set

        let contains = s |> Seq.exists (fun c -> umlauts |> Set.contains c)

        if contains then
            Error
            <| ValidationErrors.create fieldName [
                $"'%s{fieldName}' field cannot contain umlauts: ä, ö, ü, ß, Ä, Ö, Ü"
            ]
        else
            Ok <| s

    open Validus.Operators
    open System.Text.Json.Serialization

    let allowEmpty = ValueOption.ofObj >> ValueOption.filter (not << String.IsNullOrWhiteSpace)

    let validateStringCharacters =
        notStartsWithWhiteSpace
        <+> notEndsWithWhiteSpace
        <+> notContainsTilde
        <+> notContainsUmlauts
        <+> notContainsBacktick

    module Uri =

        let isValid fieldName uriString =
            if Uri.IsWellFormedUriString(uriString, UriKind.Absolute) then
                Ok uriString
            else
                Error
                <| ValidationErrors.create fieldName [ $"'%s{fieldName}' field is not a valid URI" ]


//module VOptionString =

//    let allow (validator : Validator<string, string>) : Validator<string voption, string voption> =
//        fun fieldName (value : string voption) ->
//            match value with
//            | ValueNone -> Ok ValueNone
//            | ValueSome str -> (validator *|* ValueSome) fieldName str

//    let toValidationResult _ value : ValidationResult<string voption> =
//        let valueOption =
//            value
//            |> ValueOption.ofObj
//            |> ValueOption.filter (not << String.IsNullOrWhiteSpace)
//        match valueOption with
//        | ValueSome str -> ValueSome str |> Ok
//        | ValueNone -> ValueNone |> Ok

module ValidationErrors =

    let toIGQLErrors (errors: ValidationErrors) : IGQLError list =
        errors
        |> ValidationErrors.toList
        |> List.map (fun e -> { new IGQLError with member _.Message = e })

module Operators =

    let vOption (v1: 'a -> 'a voption) (v2: Validator<'a, 'b>) : Validator<'a, 'b voption> =
        fun x y ->
            let value = v1 y
            match value with
            | ValueSome value -> (v2 *|* ValueSome) x y
            | ValueNone -> Ok ValueNone

    let (?=>) v1 v2 = vOption v1 v2
    let (?=<) v2 v1 = vOption v1 v2

module Scalars =

    open System.Text.Json
    open FSharp.Data.GraphQL.Ast
    open FSharp.Data.GraphQL.Types
    open FSharp.Data.GraphQL.Types.SchemaDefinitions.Errors

    type Define with

        static member ValidStringScalar<'t>
            (typeName, createValid : Validator<string, 't>, toString : 't -> string, ?description : string)
            =
            let createValid : string -> ValidationResult<'t> = createValid typeName
            Define.WrappedScalar (
                name = typeName,
                coerceInput =
                    (function
                    | Variable e when e.ValueKind = JsonValueKind.String ->
                        e.GetString ()
                        |> createValid
                        |> Result.mapError ValidationErrors.toIGQLErrors
                    | InlineConstant (StringValue s) ->
                        s
                        |> createValid
                        |> Result.mapError ValidationErrors.toIGQLErrors
                    | Variable e -> e.GetDeserializeError typeName
                    | InlineConstant value -> value.GetCoerceError typeName),
                coerceOutput =
                    (function
                    | :? 't as x -> Some (toString x)
                    | :? string as s -> s |> Some
                    | _ -> raise <| System.NotSupportedException ()),
                ?description = description
            )

        static member ValidStringScalar<'t>(typeName, createValid : Validator<string, 't voption>, toString : 't -> string, ?description: string) =
            let createValid = createValid typeName
            Define.WrappedScalar
                (name = typeName,
                 coerceInput =
                    (function
                    | Variable e when e.ValueKind = JsonValueKind.String -> e.GetString() |> createValid |> Result.mapError ValidationErrors.toIGQLErrors
                    | InlineConstant (StringValue s) -> s |> createValid |> Result.mapError ValidationErrors.toIGQLErrors
                    | Variable e -> e.GetDeserializeError typeName
                    | InlineConstant value -> value.GetCoerceError typeName),
                 coerceOutput =
                    (function
                    | :? ('t voption) as x -> x |> ValueOption.map toString |> ValueOption.toOption
                    | :? 't as x -> Some (toString x)
                    | :? string as s -> s |> Some
                    | null -> None
                    | _ -> raise <| System.NotSupportedException ()),
                 ?description = description)
