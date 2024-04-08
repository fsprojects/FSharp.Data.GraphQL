namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open System.IO
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types

/// Represents a file in a GraphQL file upload.
type File =
      /// Gets the name of the file.
    { Name : string
      /// Gets the type of the content of the file.
      ContentType : string
      /// Gets a stream which returns the content of the file when read.
      Content : Stream }

/// Contains customized schema definitions for extensibility features.
[<AutoOpen>]
module SchemaDefinitions =

    let private coerceUploadInput (_ : InputParameterValue) : Result<File, IGQLError list> =
        Result.Error [
            { new IGQLError with member _.Message = "Cannot coerce upload input. The type `Upload` can only be passed as a variable through a multipart request." }
        ]

    let private coerceUploadValue (value : obj) =
        match value with
        | :? File as file -> Some file
        | _ -> None

    /// GraphQL type for binary data stream representation.
    let Upload : ScalarDefinition<File> =
        { Name = "Upload"
          Description = Some "The `Upload` type represents an upload of binary data."
          CoerceInput = coerceUploadInput
          CoerceOutput = coerceUploadValue }
