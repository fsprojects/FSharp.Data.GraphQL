namespace FSharp.Data.GraphQL.Server.AspNetCore

open FSharp.Data.GraphQL
open Microsoft.AspNetCore.Http

type HttpContextRequestExecutionContext (httpContext : HttpContext) =

    interface IInputExecutionContext with

        member this.GetFile(key) =
            if not httpContext.Request.HasFormContentType then
                Error "Request does not have form content type"
            else
                let form = httpContext.Request.Form
                match (form.Files |> Seq.vtryFind (fun f -> f.Name = key)) with
                | ValueSome file ->
                    let fileData = { Stream = file.OpenReadStream(); ContentType = file.ContentType }
                    Ok (fileData)
                | ValueNone -> Error $"File with key '{key}' not found"

