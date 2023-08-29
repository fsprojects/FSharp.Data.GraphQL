namespace FSharp.Data.GraphQL.Validation

open System.Text.Json.Serialization
open FSharp.Data.GraphQL
open FsToolkit.ErrorHandling

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

        member inline _.Source(result: ValidationResult<'error>) = result.AsResult

module ValidationResult =

    let (@@) (res1 : ValidationResult<'Err>) (res2 : ValidationResult<'Err>) : ValidationResult<'Err> =
        match res1, res2 with
        | Success, Success -> Success
        | Success, _ -> res2
        | _, Success -> res1
        | ValidationError e1, ValidationError e2 -> ValidationError (e1 @ e2)

    /// Call the given sequence of validations, accumulating any errors, and return one ValidationResult.
    let collect (f : 'T -> ValidationResult<'Err>) (xs : 'T seq) : ValidationResult<'Err> =
        // TODO: Use PSeq
        Seq.fold (fun acc t -> acc @@ (f t)) Success xs

    let mapErrors (f : 'Err1 -> 'Err2) (res : ValidationResult<'Err1>) : ValidationResult<'Err2> =
        match res with
        | Success -> Success
        | ValidationError errors -> ValidationError (List.map f errors)

type internal GQLValidator<'Val> = 'Val -> ValidationResult<IGQLError>

module GQLValidator =

    let empty = fun _ -> Success

[<AbstractClass; Sealed>]
type AstError =

    static member AsResult(message : string, ?path : FieldPath) =
        [ { Message = message; Path = path |> Skippable.ofOption |> Skippable.map List.rev; Locations = Skip; Extensions = Skip  } ]
        |> ValidationResult.ValidationError
