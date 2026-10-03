namespace FSharp.Data.GraphQL.Validation

open System.Collections.Generic
open System.Text.Json.Serialization
open Microsoft.FSharp.Core.CompilerServices
open FsToolkit.ErrorHandling

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Extensions

[<Struct>]
type ValidationResult<'Err> =
    | Success
    | ValidationError of 'Err list

    member this.AsResult : Result<unit, 'Err list> =
        match this with
        | Success -> Ok ()
        | ValidationError errors -> Error errors

[<AutoOpen>]
module Result =

    type ResultBuilder with

        member inline _.Source (result : ValidationResult<'error>) = result.AsResult

module ValidationResult =

    let (@@) (res1 : ValidationResult<'Err>) (res2 : ValidationResult<'Err>) : ValidationResult<'Err> =
        match res1, res2 with
        | Success, Success -> Success
        | Success, _ -> res2
        | _, Success -> res1
        | ValidationError e1, ValidationError e2 -> ValidationError (e1 @ e2)

    /// Call the given sequence of validations, accumulating any errors, and return one ValidationResult.
    let collect (f : 'T -> ValidationResult<'Err>) (xs : 'T seq) : ValidationResult<'Err> =
        // Appending to the accumulated list with @@ copies it on every step,
        // which makes a validation reporting many errors quadratic
        let mutable errors = ListCollector<'Err> ()
        let mutable failed = false
        for x in xs do
            match f x with
            | Success -> ()
            | ValidationError e ->
                failed <- true
                errors.AddMany e
        if failed then ValidationError (errors.Close ()) else Success

    let mapErrors (f : 'Err1 -> 'Err2) (res : ValidationResult<'Err1>) : ValidationResult<'Err2> =
        match res with
        | Success -> Success
        | ValidationError errors -> ValidationError (List.map f errors)

type internal GQLValidator<'Val> = 'Val -> ValidationResult<IGQLError>

module GQLValidator =

    let empty = fun _ -> Success

[<AbstractClass; Sealed>]
type AstError =

    /// <summary>Creates a validation error.</summary>
    /// <param name="message">The message of the error.</param>
    /// <param name="path">The reversed path of the selection that the error is about.</param>
    static member Create (message : string, ?path : FieldPath) : GQLProblemDetails = {
        Message = message
        Exception = ValueNone
        Path = path |> Skippable.ofOption |> Skippable.map List.rev
        Locations = Skip
        Extensions =
            Include (
                Dictionary<string, obj> ()
                |> GQLProblemDetails.SetErrorKind ErrorKind.Validation
            )
    }

    static member AsResult (message : string, ?path : FieldPath) =
        [ AstError.Create (message, ?path = path) ]
        |> ValidationResult.ValidationError
