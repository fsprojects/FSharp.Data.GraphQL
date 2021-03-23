[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module FSharp.Data.GraphQL.Samples.StarWarsApi.Json

open System.Collections.Generic
open Newtonsoft.Json
open Newtonsoft.Json.Serialization

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

open System.Text.Json
open System.Text.Json.Serialization
open Dahomey.Json
open System

#nowarn "0058"
let getSerializerOptions ([<ParamArray>] additionalConverters: JsonConverter array) =
    let options = JsonSerializerOptions (PropertyNamingPolicy = JsonNamingPolicy.CamelCase)
    let converters = options.Converters
    converters.Add (JsonStringEnumConverter ())
    converters.Add (
        JsonFSharpConverter(JsonUnionEncoding.InternalTag
                        ||| JsonUnionEncoding.NamedFields
                        ||| JsonUnionEncoding.UnwrapSingleCaseUnions
                        ||| JsonUnionEncoding.UnwrapRecordCases
                        ||| JsonUnionEncoding.UnwrapOption))
    options.SetupExtensions() |> ignore // Use Dahomey.Json
    additionalConverters |> Array.iter converters.Add
    options
