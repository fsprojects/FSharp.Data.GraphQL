namespace FSharp.Data.GraphQL.Types

open System.IO

type FileUpload =
    { Name : string
      ContentType : string
      Size : int
      Content : Stream
      Path : string
      Hash : string }

[<AutoOpen>]
module SchemaDefinitions =
    let FileUpload =
        Define.Scalar<FileUpload>(
            name = "Upload",
            description = "The `Upload` scalar type represents a file upload.",
            coerceValue = (fun value ->
                match value with
                | :? FileUpload as upload -> Some upload
                | _ -> None),
            coerceInput = fun value ->
                raise <| invalidOp("Upload cannot be coerced from  AST input.")
        )