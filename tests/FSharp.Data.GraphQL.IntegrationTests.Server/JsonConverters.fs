namespace FSharp.Data.GraphQL.IntegrationTests.Server

open System.Collections.Generic
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Microsoft.FSharp.Reflection

[<Sealed>]
type OptionConverter() =
    inherit JsonConverter()

    override __.CanConvert(t) =
        t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>

    override __.WriteJson(writer, value, serializer) =
        let value =
            if isNull value then null
            else
                let _,fields = Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(value, value.GetType())
                fields.[0]
        serializer.Serialize(writer, value)

    override __.ReadJson(reader, t, _, serializer) =
        let innerType = t.GetGenericArguments().[0]
        let innerType =
            if innerType.IsValueType then (typedefof<System.Nullable<_>>).MakeGenericType([|innerType|])
            else innerType
        let value = serializer.Deserialize(reader, innerType)
        let cases = FSharpType.GetUnionCases(t)
        if isNull value then FSharpValue.MakeUnion(cases.[0], [||])
        else FSharpValue.MakeUnion(cases.[1], [|value|])