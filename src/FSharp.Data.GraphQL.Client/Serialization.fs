// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Client

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Text
open System.Text.Json
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Client.ReflectionPatterns
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Introspection

/// Manual schema parser that uses JsonElement directly, enabling lenient handling
/// of missing fields (e.g., 'kind' is absent in queryType/mutationType references).
module private SchemaParser =

    let private tryGetString (element : JsonElement) (name : string) =
        match element.TryGetProperty name with
        | true, el when el.ValueKind = JsonValueKind.String -> Some (el.GetString ())
        | _ -> None

    let private tryGetBool (element : JsonElement) (name : string) (defaultValue : bool) =
        match element.TryGetProperty name with
        | true, el ->
            match el.ValueKind with
            | JsonValueKind.True -> true
            | JsonValueKind.False -> false
            | _ -> defaultValue
        | _ -> defaultValue

    let private parseTypeKind (s : string) =
        match s with
        | "SCALAR" -> TypeKind.SCALAR
        | "OBJECT" -> TypeKind.OBJECT
        | "INTERFACE" -> TypeKind.INTERFACE
        | "UNION" -> TypeKind.UNION
        | "ENUM" -> TypeKind.ENUM
        | "INPUT_OBJECT" -> TypeKind.INPUT_OBJECT
        | "LIST" -> TypeKind.LIST
        | "NON_NULL" -> TypeKind.NON_NULL
        | _ -> Unchecked.defaultof<TypeKind>

    let private parseDirectiveLocation (s : string) =
        match s with
        | "QUERY" -> DirectiveLocation.QUERY
        | "MUTATION" -> DirectiveLocation.MUTATION
        | "SUBSCRIPTION" -> DirectiveLocation.SUBSCRIPTION
        | "FIELD" -> DirectiveLocation.FIELD
        | "FRAGMENT_DEFINITION" -> DirectiveLocation.FRAGMENT_DEFINITION
        | "FRAGMENT_SPREAD" -> DirectiveLocation.FRAGMENT_SPREAD
        | "INLINE_FRAGMENT" -> DirectiveLocation.INLINE_FRAGMENT
        | "SCHEMA" -> DirectiveLocation.SCHEMA
        | "SCALAR" -> DirectiveLocation.SCALAR
        | "OBJECT" -> DirectiveLocation.OBJECT
        | "FIELD_DEFINITION" -> DirectiveLocation.FIELD_DEFINITION
        | "ARGUMENT_DEFINITION" -> DirectiveLocation.ARGUMENT_DEFINITION
        | "INTERFACE" -> DirectiveLocation.INTERFACE
        | "UNION" -> DirectiveLocation.UNION
        | "ENUM" -> DirectiveLocation.ENUM
        | "ENUM_VALUE" -> DirectiveLocation.ENUM_VALUE
        | "INPUT_OBJECT" -> DirectiveLocation.INPUT_OBJECT
        | "INPUT_FIELD_DEFINITION" -> DirectiveLocation.INPUT_FIELD_DEFINITION
        | _ -> Unchecked.defaultof<DirectiveLocation>

    let rec private parseTypeRef (element : JsonElement) : IntrospectionTypeRef =
        {
            Kind =
                match element.TryGetProperty "kind" with
                | true, el when el.ValueKind = JsonValueKind.String -> parseTypeKind (el.GetString ())
                | _ -> Unchecked.defaultof<TypeKind>
            Name = tryGetString element "name"
            Description = tryGetString element "description"
            OfType =
                match element.TryGetProperty "ofType" with
                | true, el when el.ValueKind = JsonValueKind.Object -> Some (parseTypeRef el)
                | _ -> None
        }

    let private parseInputVal (element : JsonElement) : IntrospectionInputVal =
        {
            Name = tryGetString element "name" |> Option.defaultValue ""
            Description = tryGetString element "description"
            Type =
                match element.TryGetProperty "type" with
                | true, el -> parseTypeRef el
                | _ -> { Kind = Unchecked.defaultof<TypeKind>; Name = None; Description = None; OfType = None }
            DefaultValue = tryGetString element "defaultValue"
        }

    let private parseEnumVal (element : JsonElement) : IntrospectionEnumVal =
        {
            Name = tryGetString element "name" |> Option.defaultValue ""
            Description = tryGetString element "description"
            IsDeprecated = tryGetBool element "isDeprecated" false
            DeprecationReason = tryGetString element "deprecationReason"
        }

    let private parseField (element : JsonElement) : IntrospectionField =
        {
            Name = tryGetString element "name" |> Option.defaultValue ""
            Description = tryGetString element "description"
            Args =
                match element.TryGetProperty "args" with
                | true, el when el.ValueKind = JsonValueKind.Array ->
                    el.EnumerateArray () |> Seq.map parseInputVal |> Array.ofSeq
                | _ -> [||]
            Type =
                match element.TryGetProperty "type" with
                | true, el -> parseTypeRef el
                | _ -> { Kind = Unchecked.defaultof<TypeKind>; Name = None; Description = None; OfType = None }
            IsDeprecated = tryGetBool element "isDeprecated" false
            DeprecationReason = tryGetString element "deprecationReason"
        }

    let private parseType (element : JsonElement) : IntrospectionType =
        let tryGetArrayOfTypeRef (name : string) =
            match element.TryGetProperty name with
            | true, el when el.ValueKind = JsonValueKind.Array ->
                Some (el.EnumerateArray () |> Seq.map parseTypeRef |> Array.ofSeq)
            | _ -> None
        {
            Kind =
                match element.TryGetProperty "kind" with
                | true, el when el.ValueKind = JsonValueKind.String -> parseTypeKind (el.GetString ())
                | _ -> Unchecked.defaultof<TypeKind>
            Name = tryGetString element "name" |> Option.defaultValue ""
            Description = tryGetString element "description"
            Fields =
                match element.TryGetProperty "fields" with
                | true, el when el.ValueKind = JsonValueKind.Array ->
                    Some (el.EnumerateArray () |> Seq.map parseField |> Array.ofSeq)
                | _ -> None
            Interfaces = tryGetArrayOfTypeRef "interfaces"
            PossibleTypes = tryGetArrayOfTypeRef "possibleTypes"
            EnumValues =
                match element.TryGetProperty "enumValues" with
                | true, el when el.ValueKind = JsonValueKind.Array ->
                    Some (el.EnumerateArray () |> Seq.map parseEnumVal |> Array.ofSeq)
                | _ -> None
            InputFields =
                match element.TryGetProperty "inputFields" with
                | true, el when el.ValueKind = JsonValueKind.Array ->
                    Some (el.EnumerateArray () |> Seq.map parseInputVal |> Array.ofSeq)
                | _ -> None
            OfType =
                match element.TryGetProperty "ofType" with
                | true, el when el.ValueKind = JsonValueKind.Object -> Some (parseTypeRef el)
                | _ -> None
        }

    let private parseDirective (element : JsonElement) : IntrospectionDirective =
        {
            Name = tryGetString element "name" |> Option.defaultValue ""
            Description = tryGetString element "description"
            Locations =
                match element.TryGetProperty "locations" with
                | true, el when el.ValueKind = JsonValueKind.Array ->
                    el.EnumerateArray ()
                    |> Seq.choose (fun e ->
                        if e.ValueKind = JsonValueKind.String then
                            Some (parseDirectiveLocation (e.GetString ()))
                        else
                            None)
                    |> Array.ofSeq
                | _ -> [||]
            Args =
                match element.TryGetProperty "args" with
                | true, el when el.ValueKind = JsonValueKind.Array ->
                    el.EnumerateArray () |> Seq.map parseInputVal |> Array.ofSeq
                | _ -> [||]
        }

    let parseSchema (element : JsonElement) : IntrospectionSchema =
        {
            QueryType =
                match element.TryGetProperty "queryType" with
                | true, el -> parseTypeRef el
                | _ -> { Kind = Unchecked.defaultof<TypeKind>; Name = None; Description = None; OfType = None }
            MutationType =
                match element.TryGetProperty "mutationType" with
                | true, el when el.ValueKind = JsonValueKind.Object -> Some (parseTypeRef el)
                | _ -> None
            SubscriptionType =
                match element.TryGetProperty "subscriptionType" with
                | true, el when el.ValueKind = JsonValueKind.Object -> Some (parseTypeRef el)
                | _ -> None
            Types =
                match element.TryGetProperty "types" with
                | true, el when el.ValueKind = JsonValueKind.Array ->
                    el.EnumerateArray () |> Seq.map parseType |> Array.ofSeq
                | _ -> [||]
            Directives =
                match element.TryGetProperty "directives" with
                | true, el when el.ValueKind = JsonValueKind.Array ->
                    el.EnumerateArray () |> Seq.map parseDirective |> Array.ofSeq
                | _ -> [||]
        }

module Serialization =

    let private isoDateFormat = "yyyy-MM-dd"
    let private isoDateTimeFormat = "O"

    /// Converts a JsonElement to an F# object recursively.
    let rec private deserializeElement (element : JsonElement) : obj =
        match element.ValueKind with
        | JsonValueKind.Object ->
            element.EnumerateObject ()
            |> Seq.map (fun prop -> prop.Name, deserializeElement prop.Value)
            |> Map.ofSeq
            |> box
        | JsonValueKind.Array ->
            element.EnumerateArray ()
            |> Seq.map deserializeElement
            |> Array.ofSeq
            |> box
        | JsonValueKind.String -> element.GetString () |> box
        | JsonValueKind.Number ->
            match element.TryGetInt32 () with
            | true, n -> box n
            | _ ->
                match element.TryGetInt64 () with
                | true, n -> box n
                | _ -> element.GetDouble () |> box
        | JsonValueKind.True -> box true
        | JsonValueKind.False -> box false
        | _ -> null

    let deserializeMap (values : (string * JsonElement) []) =
        Tracer.runAndMeasureExecutionTime "Deserialized JSON Record into FSharp Map" (fun _ ->
            values
            |> Array.map (fun (name, element) -> name, deserializeElement element)
            |> Map.ofArray)

    let private writeValue (writer : Utf8JsonWriter) =
        let rec write (value : obj) =
            match value with
            | null -> writer.WriteNullValue ()
            | OptionValue None -> writer.WriteNullValue ()
            | OptionValue (Some v) -> write v
            | :? bool as b -> writer.WriteBooleanValue b
            | :? int as n -> writer.WriteNumberValue n
            | :? float as f -> writer.WriteNumberValue f
            | :? decimal as d -> writer.WriteNumberValue d
            | :? int64 as n -> writer.WriteNumberValue n
            | :? uint64 as n -> writer.WriteNumberValue n
            | :? int16 as n -> writer.WriteNumberValue (int n)
            | :? uint16 as n -> writer.WriteNumberValue (uint32 n)
            | :? byte as n -> writer.WriteNumberValue (uint32 n)
            | :? sbyte as n -> writer.WriteNumberValue (int n)
            | :? string as s -> writer.WriteStringValue s
            | :? Guid as g -> writer.WriteStringValue (g.ToString ())
            | :? DateTime as d when d.Date = d -> writer.WriteStringValue (d.ToString isoDateFormat)
            | :? DateTime as d -> writer.WriteStringValue (d.ToString isoDateTimeFormat)
            | :? DateTimeOffset as d -> writer.WriteStringValue (d.ToString isoDateTimeFormat)
            | :? Uri as u -> writer.WriteStringValue (u.ToString ())
            | :? Upload as u -> writer.WriteStringValue u.Name
            | :? IDictionary<string, obj> as dict ->
                writer.WriteStartObject ()
                for kvp in dict do
                    writer.WritePropertyName (kvp.Key.FirstCharLower ())
                    write kvp.Value
                writer.WriteEndObject ()
            | EnumerableValue items ->
                writer.WriteStartArray ()
                Array.iter write items
                writer.WriteEndArray ()
            | EnumValue s -> writer.WriteStringValue s
            | _ ->
                let props = value.GetType().GetProperties (BindingFlags.Public ||| BindingFlags.Instance)
                writer.WriteStartObject ()
                for p in props do
                    writer.WritePropertyName (p.Name.FirstCharLower ())
                    write (p.GetValue value)
                writer.WriteEndObject ()
        write

    /// Builds the JSON body for a standard GraphQL request.
    let buildRequestJson (operationName : string option) (query : string) (variables : (string * obj) []) =
        Tracer.runAndMeasureExecutionTime "Built GraphQL request JSON" (fun _ ->
            use stream = new MemoryStream ()
            let writerOptions = JsonWriterOptions (Indented = false)
            use writer = new Utf8JsonWriter (stream, writerOptions)
            let write = writeValue writer
            writer.WriteStartObject ()
            writer.WritePropertyName "operationName"
            match operationName with
            | Some name -> writer.WriteStringValue name
            | None -> writer.WriteNullValue ()
            writer.WritePropertyName "query"
            writer.WriteStringValue query
            writer.WritePropertyName "variables"
            if variables = null || variables.Length = 0 then
                writer.WriteNullValue ()
            else
                writer.WriteStartObject ()
                for (name, value) in variables do
                    writer.WritePropertyName name
                    write value
                writer.WriteEndObject ()
            writer.WriteEndObject ()
            writer.Flush ()
            Encoding.UTF8.GetString (stream.ToArray ()))

    /// Builds the JSON body for the "map" part of a multipart GraphQL request.
    let buildMapJson (files : (string * Upload) []) =
        Tracer.runAndMeasureExecutionTime "Built GraphQL map JSON" (fun _ ->
            use stream = new MemoryStream ()
            let writerOptions = JsonWriterOptions (Indented = false)
            use writer = new Utf8JsonWriter (stream, writerOptions)
            writer.WriteStartObject ()
            files
            |> Array.iteri (fun ix (name, _) ->
                writer.WritePropertyName (ix.ToString ())
                writer.WriteStartArray ()
                writer.WriteStringValue ("variables." + name)
                writer.WriteEndArray ())
            writer.WriteEndObject ()
            writer.Flush ()
            Encoding.UTF8.GetString (stream.ToArray ()))

    let deserializeSchema (json : string) =
        Tracer.runAndMeasureExecutionTime "Deserialized schema" (fun _ ->
            use doc = JsonDocument.Parse json
            let root = doc.RootElement
            let errors =
                match root.TryGetProperty "errors" with
                | true, errorsEl when errorsEl.ValueKind = JsonValueKind.Array && errorsEl.GetArrayLength () > 0 ->
                    errorsEl.EnumerateArray ()
                    |> Seq.choose (fun e ->
                        match e.TryGetProperty "message" with
                        | true, msgEl when msgEl.ValueKind = JsonValueKind.String -> Some (msgEl.GetString ())
                        | _ -> None)
                    |> Seq.toArray
                | _ -> [||]
            if errors.Length > 0 then
                String.concat "\n" errors |> failwithf "%s"
            match root.TryGetProperty "data" with
            | true, dataEl ->
                match dataEl.TryGetProperty "__schema" with
                | true, schemaEl -> SchemaParser.parseSchema schemaEl
                | _ -> failwith "Expected \"__schema\" field in the response data."
            | _ -> failwith "Expected \"data\" field in the response.")
