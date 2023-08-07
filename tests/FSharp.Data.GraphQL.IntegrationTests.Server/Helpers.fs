namespace FSharp.Data.GraphQL.IntegrationTests.Server

open System
open System.Collections.Generic
open System.Text
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Newtonsoft.Json.Serialization

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

    let utf8Bytes (str : string) =
        str |> Encoding.UTF8.GetBytes

    let isNullOrWhiteSpace (str : string) =
        String.IsNullOrWhiteSpace(str)

[<AutoOpen>]
module JsonHelpers =

    let tryGetJsonProperty (jobj: JObject) prop =
        match jobj.Property(prop) with
        | null -> None
        | p -> Some(p.Value.ToString())

    let getJsonSerializerSettings (converters : JsonConverter seq) =
        JsonSerializerSettings()
        |> tee (fun s ->
            s.Converters <- List<JsonConverter>(converters)
            s.ContractResolver <- CamelCasePropertyNamesContractResolver())

    let getJsonSerializer (converters : JsonConverter seq) =
        JsonSerializer()
        |> tee (fun c ->
            Seq.iter c.Converters.Add converters
            c.ContractResolver <- CamelCasePropertyNamesContractResolver())

    let private converters : JsonConverter [] = [| OptionConverter() |]

    let jsonSettings = getJsonSerializerSettings converters
    let jsonSerializer = getJsonSerializer converters

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
