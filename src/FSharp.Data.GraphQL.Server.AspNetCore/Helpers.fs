namespace FSharp.Data.GraphQL.Server.AspNetCore

open System
open System.Text
open FSharp.Data.GraphQL
open Microsoft.AspNetCore.Http

[<AutoOpen>]
module Helpers =

    let tee f x =
        f x
        x


[<AutoOpen>]
module StringHelpers =

    let utf8String (bytes : byte seq) =
        bytes
        |> Seq.filter (fun i -> i > 0uy)
        |> Array.ofSeq
        |> Encoding.UTF8.GetString

    let utf8Bytes (str : string) = str |> Encoding.UTF8.GetBytes

    let isNullOrWhiteSpace (str : string) = String.IsNullOrWhiteSpace (str)


[<AutoOpen>]
module LoggingHelpers =

    open Microsoft.Extensions.DependencyInjection
    open Microsoft.Extensions.Logging

    type IServiceProvider with
        member serviceProvider.CreateLogger (``type`` : Type) =
            let loggerFactory = serviceProvider.GetRequiredService<ILoggerFactory>()
            loggerFactory.CreateLogger(``type``)


[<AutoOpen>]
module ReflectionHelpers =

    open Microsoft.FSharp.Quotations.Patterns

    let getModuleType quotation =
        match quotation with
        | PropertyGet (_, propertyInfo, _) -> propertyInfo.DeclaringType
        | FieldGet (_, fieldInfo) -> fieldInfo.DeclaringType
        | _ -> failwith "Expression is no property."

type HttpContextRequestExecutionContext (httpContext : HttpContext) =
    interface IInputExecutionContext with
        member this.GetFile(key) =
            if not httpContext.Request.HasFormContentType then
                Error "Request does not have form content type"
            else
                let form = httpContext.Request.Form

                match (form.Files |> Seq.vtryFind (fun f -> f.Name = key)) with
                | ValueSome file -> Ok (file.OpenReadStream())
                | ValueNone -> Error $"File with key '{key}' not found"
