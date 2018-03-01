namespace FSharp.Data.GraphQL.Samples.GiraffeServer

open System.Text
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Newtonsoft.Json.Serialization
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

    let utf8Bytes (str : string) =
        str |> Encoding.UTF8.GetBytes

[<AutoOpen>]
module JsonHelpers =
    let tryGetJsonProperty (jobj: JObject) prop =
        match jobj.Property(prop) with
        | null -> None
        | p -> Some(p.Value.ToString())

    let jsonSerializerSettings (converters : JsonConverter seq) =
        JsonSerializerSettings()
        |> tee (fun s ->
            s.Converters <- List<JsonConverter>(converters)
            s.ContractResolver <- CamelCasePropertyNamesContractResolver())