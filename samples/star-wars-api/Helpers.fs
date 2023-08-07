namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open System
open System.Text
open System.Collections.Generic

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
        member sericeProvider.CreateLogger (``type`` : Type) =
            let loggerFactory = sericeProvider.GetRequiredService<ILoggerFactory>()
            loggerFactory.CreateLogger(``type``)

[<AutoOpen>]
module ReflectionHelpers =

    open Microsoft.FSharp.Quotations.Patterns

    let getModuleType = function
        | PropertyGet (_, propertyInfo, _) -> propertyInfo.DeclaringType
        | _ -> failwith "Expression is no property."
