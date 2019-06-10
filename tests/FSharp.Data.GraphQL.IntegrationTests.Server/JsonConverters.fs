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

[<Sealed>]
type IDictionaryConverter() =
    inherit JsonConverter()

    let fail() = raise <| JsonSerializationException("Unexpected end when reading a JSON object into an IDictionary<string, object>.")

    let rec writeObject (writer : JsonWriter) (value : obj) =
        writer.WriteStartObject()
        value :?> IDictionary<string, obj>
        |> Seq.iter (fun kvp ->
            writer.WritePropertyName(kvp.Key)
            writeValue writer kvp.Value)
        writer.WriteEndObject()

    and writeArray (writer : JsonWriter) (value : obj) =
        writer.WriteStartArray()
        value :?> IEnumerable<obj>
        |> Seq.iter (fun v -> writeValue writer v)
        writer.WriteEndArray()

    and writeValue (writer : JsonWriter) (value : obj) =
        let t = JToken.FromObject(value)
        match t.Type with
        | JTokenType.Object -> writeObject writer value
        | JTokenType.Array -> writeArray writer value
        | _ -> writer.WriteValue(value)

    let rec readValue (reader : JsonReader) =
        while reader.TokenType = JsonToken.Comment do
            if not (reader.Read()) then fail()
        match reader.TokenType with
        | JsonToken.StartObject -> readObject reader
        | JsonToken.StartArray -> readArray reader
        | _ -> reader.Value

    and readObject (reader : JsonReader) =
        let mutable value = Map.empty<string, obj>
        let mutable (endReached, failed) = false, false
        while (not endReached && not failed) do
            if not (reader.Read()) 
            then failed <- true
            else
                if reader.TokenType = JsonToken.PropertyName then
                    let propertyName = reader.Value.ToString()
                    if not (reader.Read()) then fail()
                    let propertyValue = readValue reader
                    value <- value.Add(propertyName, propertyValue)
                elif reader.TokenType = JsonToken.EndObject then
                    endReached <- true
        if not endReached then fail()
        upcast value

    and readArray (reader : JsonReader) =
        let list = List<obj>()
        let mutable (endReached, failed) = false, false
        while (not endReached && not failed) do
            if not (reader.Read()) 
            then failed <- true
            else
                if reader.TokenType <> JsonToken.Comment && reader.TokenType <> JsonToken.EndArray then
                    list.Add(readValue reader)
                elif reader.TokenType = JsonToken.EndArray then 
                    endReached <- true
        if not endReached then fail()
        upcast Array.ofSeq list

    override __.CanConvert(t) = t.IsAssignableFrom(typeof<IDictionary<string, obj>>)

    override __.WriteJson(writer, value, _) = writeValue writer value

    override __.ReadJson(reader, _, _, _) = readValue reader