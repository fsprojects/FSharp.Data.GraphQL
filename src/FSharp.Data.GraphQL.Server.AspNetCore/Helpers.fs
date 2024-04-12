namespace FSharp.Data.GraphQL.Server.AspNetCore

open System
open System.Text


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

    let getModuleType = function
        | PropertyGet (_, propertyInfo, _) -> propertyInfo.DeclaringType
        | _ -> failwith "Expression is no property."

