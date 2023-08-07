[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module FSharp.Data.GraphQL.Samples.StarWarsApi.Json

open System.Text.Json
open System.Text.Json.Serialization

let [<Literal>] UnionTag = "kind"

#nowarn "0058"
let configureSerializerOptions (jsonFSharpOptions: JsonFSharpOptions) (additionalConverters: JsonConverter seq) (options : JsonSerializerOptions) =
    options.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
    options.PropertyNameCaseInsensitive <- true
    //options.DefaultIgnoreCondition <- JsonIgnoreCondition.WhenWritingNull
    let converters = options.Converters
    converters.Add (JsonStringEnumConverter ())
    additionalConverters |> Seq.iter converters.Add
    jsonFSharpOptions.AddToJsonSerializerOptions options
    options

let defaultJsonFSharpOptions =
    JsonFSharpOptions(
        JsonUnionEncoding.InternalTag ||| JsonUnionEncoding.AllowUnorderedTag ||| JsonUnionEncoding.NamedFields
        ||| JsonUnionEncoding.UnwrapSingleCaseUnions
        ||| JsonUnionEncoding.UnwrapRecordCases
        ||| JsonUnionEncoding.UnwrapOption, UnionTag)

let configureDefaultSerializerOptions = configureSerializerOptions defaultJsonFSharpOptions

let getSerializerOptions (additionalConverters: JsonConverter seq) =
    JsonSerializerOptions () |> configureDefaultSerializerOptions additionalConverters

let serializerOptions = getSerializerOptions Seq.empty
