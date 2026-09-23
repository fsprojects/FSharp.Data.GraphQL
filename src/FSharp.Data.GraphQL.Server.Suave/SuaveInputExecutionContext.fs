namespace FSharp.Data.GraphQL.Server.Suave

open System.IO
open Suave.Http

open FSharp.Data.GraphQL

/// <summary>
/// <see cref="IInputExecutionContext"/> implementation that resolves uploaded files from a Suave <see cref="HttpRequest"/>,
/// following the <see href="https://github.com/jaydenseric/graphql-multipart-request-spec">GraphQL multipart request specification</see>.
/// </summary>
type SuaveInputExecutionContext (request : HttpRequest) =

    interface IInputExecutionContext with

        member _.GetFile (key) =
            match
                request.files
                |> List.tryFind (fun file -> file.fieldName = key)
            with
            | Some file ->
                try
                    let memoryStream = new MemoryStream ()
                    use fileStream = File.OpenRead file.tempFilePath
                    fileStream.CopyTo memoryStream
                    memoryStream.Seek (0L, SeekOrigin.Begin) |> ignore
                    Ok { FileName = file.fileName; Stream = memoryStream; ContentType = file.mimeType }
                with ex ->
                    Error ex.Message
            | None -> Error $"File with key '%s{key}' not found"
