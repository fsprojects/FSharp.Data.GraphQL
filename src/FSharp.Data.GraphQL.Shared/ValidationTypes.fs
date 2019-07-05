namespace FSharp.Data.GraphQL.Validation

type ValidationResult<'Err> =
    | Success
    | ValidationError of 'Err list

[<AutoOpen>]
module ValidationResult =
    let (@@) (res1 : ValidationResult<'Err>) (res2 : ValidationResult<'Err>) : ValidationResult<'Err> =
        match res1, res2 with
        | Success, Success -> Success
        | Success, _ -> res2
        | _, Success -> res1
        | ValidationError e1, ValidationError e2 -> ValidationError (e1 @ e2)

    let collectResults (f : 'T -> ValidationResult<'Err>) (xs : 'T seq) : ValidationResult<'Err> =
        Seq.fold (fun acc t -> acc @@ (f t)) Success xs

type Path = string list

type AstError =
    { Message : string
      Path : Path option }
    static member AsResult(message : string, ?path : Path) =
        [ { Message = message; Path = path |> Option.map List.rev } ]
        |> ValidationResult.ValidationError