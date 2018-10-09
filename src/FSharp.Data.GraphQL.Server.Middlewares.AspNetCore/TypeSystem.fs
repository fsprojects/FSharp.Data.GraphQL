namespace FSharp.Data.GraphQL.Server.Middlewares.AspNetCore

open System.IO
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types

/// Represents a file in a GraphQL file upload multipart request.
type File =
      /// Gets the name of the file.
    { Name : string
      /// Gets the type of the content of the file.
      ContentType : string
      /// Gets a stream which returns the content of the file when read.
      Content : Stream }

module Patterns =
    let private coerceUploadInput : Value -> File option = 
        fun value -> None

    let private coerceUploadValue (x : obj) : File option =
        None

    let Upload : ScalarDefinition<File> =
        { Name = "Upload"
          Description = Some "The `Upload` scalar type represents a stream which transfers a file."
          CoerceInput = coerceUploadInput
          CoerceValue = coerceUploadValue }