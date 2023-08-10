[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module FSharp.Data.GraphQL.Samples.StarWarsApi.Json

open System.Text.Json
open System.Text.Json.Serialization

let [<Literal>] UnionTag = "kind"

#nowarn "0058"
let configureSerializerOptions (jsonFSharpOptions: JsonFSharpOptions) (additionalConverters: JsonConverter seq) (options : JsonSerializerOptions) =
    options.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
    options.PropertyNameCaseInsensitive <- true
    let converters = options.Converters
    converters.Add (JsonStringEnumConverter ())
    //converters.Add (JsonSerializerOptionsState (options)) // Dahomey.Json
    additionalConverters |> Seq.iter converters.Add
    jsonFSharpOptions.AddToJsonSerializerOptions options

let defaultJsonFSharpOptions =
    JsonFSharpOptions(
        JsonUnionEncoding.InternalTag
        ||| JsonUnionEncoding.AllowUnorderedTag
        ||| JsonUnionEncoding.NamedFields
        ||| JsonUnionEncoding.UnwrapSingleCaseUnions
        ||| JsonUnionEncoding.UnwrapRecordCases
        ||| JsonUnionEncoding.UnwrapOption
        ||| JsonUnionEncoding.UnwrapFieldlessTags,
        UnionTag,
        allowOverride = true)

let configureDefaultSerializerOptions = configureSerializerOptions defaultJsonFSharpOptions

let getSerializerOptions (additionalConverters: JsonConverter seq) =
    let options = JsonSerializerOptions ()
    options |> configureDefaultSerializerOptions additionalConverters
    options

let serializerOptions = getSerializerOptions Seq.empty
