[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module FSharp.Data.GraphQL.Server.AspNetCore.Json

open System.Text.Json
open System.Text.Json.Serialization

let [<Literal>] UnionTag = "kind"

#nowarn "0058"
let configureSerializerOptions (jsonFSharpOptions: JsonFSharpOptions) (additionalConverters: JsonConverter seq) (options : JsonSerializerOptions) =
    options.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
    options.PropertyNameCaseInsensitive <- true
    let converters = options.Converters
    converters.Add (new JsonStringEnumConverter ())
    //converters.Add (JsonSerializerOptionsState (options)) // Dahomey.Json
    additionalConverters |> Seq.iter converters.Add
    jsonFSharpOptions.AddToJsonSerializerOptions options

let configureWSSerializerOptions (jsonFSharpOptions: JsonFSharpOptions) (additionalConverters: JsonConverter seq) (options : JsonSerializerOptions) =
    let additionalConverters = seq {
        yield new ClientMessageConverter () :> JsonConverter
        yield new RawServerMessageConverter ()
        yield! additionalConverters
    }
    configureSerializerOptions jsonFSharpOptions additionalConverters options

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
let configureDefaultWSSerializerOptions = configureWSSerializerOptions defaultJsonFSharpOptions

let getSerializerOptions (additionalConverters: JsonConverter seq) =
    let options = JsonSerializerOptions ()
    options |> configureDefaultSerializerOptions additionalConverters
    options

let getWSSerializerOptions (additionalConverters: JsonConverter seq) =
    let options = JsonSerializerOptions ()
    options |> configureDefaultWSSerializerOptions additionalConverters
    options

let serializerOptions = getWSSerializerOptions Seq.empty
