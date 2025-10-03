namespace FSharp.Data.GraphQL.Server.AspNetCore

open System.IO
open FSharp.Data.GraphQL
open Microsoft.AspNetCore.Http

type HttpContextRequestExecutionContext (httpContext : HttpContext) =

    interface IInputExecutionContext with

        member this.GetFile (key) =
            if not httpContext.Request.HasFormContentType then
                Error "Request does not have form content type"
            else
                let form = httpContext.Request.Form
                match (form.Files |> Seq.vtryFind (fun f -> f.Name = key)) with
                | ValueSome file ->
                    let memoryStream = new MemoryStream ()
                    use fileStream = file.OpenReadStream ()
                    fileStream.CopyTo (memoryStream)
                    memoryStream.Seek (0L, SeekOrigin.Begin) |> ignore
                    Ok {
                        FileName = file.FileName
                        Stream = memoryStream
                        ContentType = file.ContentType
                    }
                | ValueNone -> Error $"File with key '{key}' not found"
