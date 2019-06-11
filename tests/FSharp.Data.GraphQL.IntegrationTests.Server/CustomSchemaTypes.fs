namespace FSharp.Data.GraphQL.IntegrationTests.Server

open System.IO
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Ast

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
    let private coerceUploadInput (_ : Value) : File option =
        failwith "Can not coerce upload input. The type `Upload` can only be passed as a variable through a multipart request."
    
    let private coerceUploadValue (value : obj) =
        match value with
        | :? File as file -> Some file
        | _ -> None

    /// GraphQL type for binary data stream representation.
    let Upload : ScalarDefinition<File> =
        { Name = "Upload"
          Description = Some "The `Upload` type represents an upload of binary data."
          CoerceInput = coerceUploadInput
          CoerceValue = coerceUploadValue }