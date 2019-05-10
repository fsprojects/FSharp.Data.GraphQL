/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open System.Globalization
open FSharp.Core
open FSharp.Data.GraphQL.Client
open FSharp.Data.GraphQL.Client.ReflectionPatterns
open FSharp.Data.GraphQL.Types.Introspection
open System.Text
open Microsoft.FSharp.Reflection

/// A type alias to represent a Type name.
type TypeName = string

/// Contains data about a GQL operation error.
type OperationError =
      /// The description of the error that happened in the operation.
    { Message : string
      /// The path to the field that produced the error while resolving its value.
      Path : obj [] }

/// Contains helpers to build HTTP header sequences to be used in GraphQLProvider Run methods.
module HttpHeaders =
    /// Builds a sequence of HTTP headers as a sequence from a pre-formatted header string.
    /// The input headers string should be a string containing headers in the same way they are
    /// organized in a HTTP request (each header in a line, names and values separated by commas).
    let ofString (headers : string) : seq<string * string> =
        upcast (headers.Replace("\r\n", "\n").Split('\n')
                |> Array.map (fun header -> 
                    let separatorIndex = header.IndexOf(':')
                    if separatorIndex = -1
                    then failwithf "Header \"%s\" has an invalid header format. Must provide a name and a value, both separated by a comma." header
                    else
                        let name = header.Substring(0, separatorIndex).Trim()
                        let value = header.Substring(separatorIndex + 1).Trim()
                        (name, value)))
        
    /// Builds a sequence of HTTP headers as a sequence from a header file.
    /// The input file should be a file containing headers in the same way they are
    /// organized in a HTTP request (each header in a line, names and values separated by commas).
    let ofFile (path : string) =
        System.IO.File.ReadAllText path |> ofString

    let internal load (location : StringLocation) : seq<string * string> =
        let headersString =
            match location with
            | String headers -> headers
            | File path -> System.IO.File.ReadAllText path
        if headersString = "" then upcast [||]
        else headersString |> ofString

/// The base type for all GraphQLProvider provided enum types.
type EnumBase (name : string, value : string) =
    /// Gets the name of the provided enum type.
    member __.GetName() = name

    /// Gets the value of the provided enum type.
    member __.GetValue() = value

    override x.ToString() = x.GetValue()

    member x.Equals(other : EnumBase) =
        x.GetName() = other.GetName() && x.GetValue() = other.GetValue()

    override x.Equals(other : obj) =
        match other with
        | :? EnumBase as other -> x.Equals(other)
        | _ -> false

    override x.GetHashCode() = x.GetName().GetHashCode() ^^^ x.GetValue().GetHashCode()

    interface IEquatable<EnumBase> with
        member x.Equals(other) = x.Equals(other)

/// Contains information about a GraphQLProvider record property.
type RecordProperty =
      /// Gets the name of the record property.
    { Name : string
      /// Gets the value of the record property.
      Value : obj }

/// The base type for all GraphQLProvider provided record types.
type RecordBase (name : string, properties : RecordProperty seq) =
    do
        if not (isNull properties)
        then
            let distinctCount = properties |> Seq.map (fun p -> p.Name) |> Seq.distinct |> Seq.length
            if distinctCount <> Seq.length properties
            then failwith "Duplicated property names were found. Record can not be created, because each property name must be distinct."

    let properties = 
        if not (isNull properties)
        then properties |> Seq.sortBy (fun x -> x.Name) |> List.ofSeq
        else []

    /// Gets the name of this provided record type.
    member __.GetName() = name

    /// Gets a list of this provided record properties.
    member __.GetProperties() = properties
    
    /// Produces a dictionary containing all the properties and names of this provided record type.
    member x.ToDictionary() =
        let rec mapper (v : obj) =
            match v with
            | :? RecordBase as v -> box (v.ToDictionary())
            | OptionValue v -> v |> Option.map mapper |> Option.toObj
            | _ -> v
        x.GetProperties()
        |> Seq.map (fun p -> p.Name, mapper p.Value)
        |> dict

    override x.ToString() =
        let getPropValue (prop : RecordProperty) = sprintf "%A" prop.Value
        let sb = StringBuilder()
        sb.Append("{") |> ignore
        let rec printProperties (properties : RecordProperty list) =
            match properties with
            | [] -> ()
            | [prop] -> sb.Append(sprintf "%s = %s;" prop.Name (getPropValue prop)) |> ignore
            | prop :: tail -> sb.AppendLine(sprintf "%s = %s;" prop.Name (getPropValue prop)) |> ignore; printProperties tail
        printProperties (x.GetProperties())
        sb.Append("}") |> ignore
        sb.ToString()

    member x.Equals(other : RecordBase) = 
        x.GetName() = other.GetName() && x.GetProperties() = other.GetProperties()

    override x.Equals(other : obj) =
        match other with
        | :? RecordBase as other -> x.Equals(other)
        | _ -> false

    override x.GetHashCode() = 
        x.GetName().GetHashCode() ^^^ x.GetProperties().GetHashCode()

    interface IEquatable<RecordBase> with
        member x.Equals(other) = x.Equals(other)

module internal Types =
    let scalar =
        [| "Int", typeof<int>
           "Boolean", typeof<bool>
           "Date", typeof<DateTime>
           "Float", typeof<float>
           "ID", typeof<string>
           "String", typeof<string>
           "URI", typeof<Uri> |]
        |> Map.ofArray

    let getSchemaTypes (introspection : IntrospectionSchema) =
        let schemaTypeNames =
            [| "__TypeKind"
               "__DirectiveLocation"
               "__Type"
               "__InputValue"
               "__Field"
               "__EnumValue"
               "__Directive"
               "__Schema" |]
        let isScalarType (name : string) =
            scalar |> Map.containsKey name
        let isIntrospectionType (name : string) =
            schemaTypeNames |> Array.contains name
        introspection.Types
        |> Array.filter (fun t -> not (isIntrospectionType t.Name) && not (isScalarType t.Name))
        |> Array.map (fun t -> t.Name, t)
        |> Map.ofArray

    let makeOption (t : Type) = typedefof<_ option>.MakeGenericType(t)

    let makeArray (t : Type) = t.MakeArrayType()

    let unwrapOption (t : Type) = 
        if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<_ option>
        then t.GetGenericArguments().[0]
        else failwithf "Expected type to be an Option type, but it is %s." t.Name

    let makeAsync (t : Type) = typedefof<Async<_>>.MakeGenericType(t)

module internal JsonValueHelper =
    let getResponseFields (responseJson : JsonValue) =
        match responseJson with
        | JsonValue.Record fields -> fields
        | _ -> failwithf "Expected root type to be a Record type, but type is %A." responseJson

    let getResponseDataFields (responseJson : JsonValue) =
        match getResponseFields responseJson |> Array.tryFind (fun (name, _) -> name = "data") with
        | Some (_, data) -> 
            match data with
            | JsonValue.Record fields -> Some fields
            | JsonValue.Null -> None
            | _ -> failwithf "Expected data field of root type to be a Record type, but type is %A." data
        | None -> None

    let getResponseErrors (responseJson : JsonValue) =
        match getResponseFields responseJson |> Array.tryFind (fun (name, _) -> name = "errors") with
        | Some (_, errors) ->
            match errors with
            | JsonValue.Array [||] | JsonValue.Null -> None
            | JsonValue.Array items -> Some items
            | _ -> failwithf "Expected error field of root type to be an Array type, but type is %A." errors
        | None -> None

    let getResponseCustomFields (responseJson : JsonValue) =
        getResponseFields responseJson
        |> Array.filter (fun (name, _) -> name <> "data" && name <> "errors")

    let private removeTypeNameField (fields : (string * JsonValue) []) =
        fields |> Array.filter (fun (name, _) -> name <> "__typename")

    let firstUpper (name : string, value) =
        name.FirstCharUpper(), value

    let getFields (schemaType : IntrospectionType) =
        match schemaType.Fields with
        | None -> Map.empty
        | Some fields -> fields |> Array.map (fun field -> field.Name.FirstCharUpper(), field.Type) |> Map.ofArray

    let getTypeName (fields : (string * JsonValue) seq) =
        fields
        |> Seq.tryFind (fun (name, _) -> name = "__typename")
        |> Option.map (fun (_, value) ->
            match value with
            | JsonValue.String x -> x
            | _ -> failwithf "Expected \"__typename\" field to be a string field, but it was %A." value)

    let rec getFieldValue (schemaTypes : Map<string, IntrospectionType>) (fieldType : IntrospectionTypeRef) (fieldName : string, fieldValue : JsonValue) =
        let getScalarType (typeRef : IntrospectionTypeRef) =
            let getType (typeName : string) =
                match Map.tryFind typeName Types.scalar with
                | Some t -> t
                | None -> failwithf "Unsupported scalar type \"%s\"." typeName
            match typeRef.Name with
            | Some name -> getType name
            | None -> failwith "Expected scalar type to have a name, but it does not have one."
        let rec helper (useOption : bool) (fieldType : IntrospectionTypeRef) (fieldValue : JsonValue) : obj =
            let makeSomeIfNeeded value =
                match fieldType.Kind with
                | TypeKind.NON_NULL | TypeKind.LIST -> value
                | _ when useOption -> makeSome value
                | _ -> value
            let makeNoneIfNeeded (t : Type) =
                match fieldType.Kind with
                | TypeKind.NON_NULL | TypeKind.LIST -> null
                | _ when useOption -> makeNone t
                | _ -> null
            match fieldValue with
            | JsonValue.Array items ->
                let itemType =
                    match fieldType.OfType with
                    | Some t when t.Kind = TypeKind.LIST && t.OfType.IsSome -> t.OfType.Value
                    | _ -> failwithf "Expected field to be a list type with an underlying item, but it is %A." fieldType.OfType
                let items = items |> Array.map (helper false itemType)
                match itemType.Kind with
                | TypeKind.NON_NULL -> 
                    match itemType.OfType with
                    | Some itemType ->
                        match itemType.Kind with
                        | TypeKind.NON_NULL -> failwith "Schema definition is not supported: a non null type of a non null type was specified."
                        | TypeKind.OBJECT | TypeKind.INTERFACE | TypeKind.UNION -> makeArray typeof<RecordBase> items
                        | TypeKind.ENUM -> makeArray typeof<EnumBase> items
                        | TypeKind.SCALAR -> makeArray (getScalarType itemType) items
                        | kind -> failwithf "Unsupported type kind \"%A\"." kind
                    | None -> failwith "Item type is a non null type, but no underlying type exists on the schema definition of the type."
                | TypeKind.OBJECT | TypeKind.INTERFACE | TypeKind.UNION -> makeOptionArray typeof<RecordBase> items |> makeSomeIfNeeded
                | TypeKind.ENUM -> makeOptionArray typeof<EnumBase> items |> makeSomeIfNeeded
                | TypeKind.SCALAR -> makeOptionArray (getScalarType itemType) items |> makeSomeIfNeeded
                | kind -> failwithf "Unsupported type kind \"%A\"." kind
            | JsonValue.Record props -> 
                let typeName =
                    match getTypeName props with
                    | Some typeName -> typeName
                    | None -> failwith "Expected type to have a \"__typename\" field, but it was not found."
                let schemaType =
                    match schemaTypes.TryFind(typeName) with
                    | Some tref -> tref
                    | None -> failwithf "Expected to find a type \"%s\" in the schema types, but it was not found." typeName
                let fields = getFields schemaType
                let mapper (name : string, value : JsonValue) =
                    match fields.TryFind(name) with
                    | Some fieldType -> 
                        let value = helper true fieldType value
                        { Name = name; Value = value }
                    | None -> failwithf "Expected to find a field named \"%s\" on the type %s, but found none." name schemaType.Name
                let props =
                    props
                    |> removeTypeNameField
                    |> Array.map (firstUpper >> mapper)
                RecordBase(typeName, props) |> makeSomeIfNeeded
            | JsonValue.Boolean b -> makeSomeIfNeeded b
            | JsonValue.Float f -> makeSomeIfNeeded f
            | JsonValue.Null ->
                match fieldType.Kind with
                | TypeKind.NON_NULL -> failwith "Expected a non null item from the schema definition, but a null item was found in the response."
                | TypeKind.OBJECT | TypeKind.INTERFACE | TypeKind.UNION -> makeNoneIfNeeded typeof<RecordBase>
                | TypeKind.ENUM -> makeNoneIfNeeded typeof<EnumBase>
                | TypeKind.SCALAR -> getScalarType fieldType |> makeNoneIfNeeded
                | kind -> failwithf "Unsupported type kind \"%A\"." kind
            | JsonValue.Integer n -> makeSomeIfNeeded n
            | JsonValue.String s -> 
                match fieldType.Kind with
                | TypeKind.NON_NULL ->
                    match fieldType.OfType with
                    | Some itemType ->
                        match itemType.Kind with
                        | TypeKind.NON_NULL -> failwith "Schema definition is not supported: a non null type of a non null type was specified."
                        | TypeKind.SCALAR -> 
                            match itemType.Name with
                            | Some "String" | Some "ID" -> box s
                            | Some "URI" -> Uri(s) |> box
                            | Some "Date" -> 
                                match DateTime.TryParseExact(s, Serialization.isoDateTimeFormats, CultureInfo.InvariantCulture, DateTimeStyles.None) with
                                | (true, d) -> box d
                                | _ -> failwith "A string was received in the query response, and the schema recognizes it as a date and time sring, but the conversion failed."
                            | _ -> failwith "A string type was received in the query response item, but the matching schema field is not a string based type."
                        | TypeKind.ENUM when itemType.Name.IsSome -> EnumBase(itemType.Name.Value, s) |> box
                        | _ -> failwith "A string type was received in the query response item, but the matching schema field is not a string or an enum type."
                    | None -> failwith "Item type is a non null type, but no underlying type exists on the schema definition of the type."
                | TypeKind.SCALAR ->
                    match fieldType.Name with
                    | Some "String" -> makeSomeIfNeeded s
                    | Some "URI" -> Uri(s) |> makeSomeIfNeeded
                    | Some "Date" -> 
                        match DateTime.TryParseExact(s, Serialization.isoDateTimeFormats, CultureInfo.InvariantCulture, DateTimeStyles.None) with
                        | (true, d) -> makeSomeIfNeeded d
                        | _ -> failwith "A string was received in the query response, and the schema recognizes it as a date and time sring, but the conversion failed."
                    | _ -> failwith "A string type was received in the query response item, but the matching schema field is not a string based type."
                | TypeKind.ENUM when fieldType.Name.IsSome -> EnumBase(fieldType.Name.Value, s) |> makeSomeIfNeeded
                | _ -> failwith "A string type was received in the query response item, but the matching schema field is not a string based type or an enum type."
        fieldName, (helper true fieldType fieldValue)

    let getFieldValues (schemaTypes : Map<string, IntrospectionType>) (schemaType : IntrospectionType) (fields : (string * JsonValue) []) =
        let fieldMap = getFields schemaType
        let mapper (name : string, value : JsonValue) =
            match fieldMap.TryFind(name) with
            | Some fieldType -> getFieldValue schemaTypes fieldType (name, value)
            | None -> failwithf "Expected to find a field named \"%s\" on the type %s, but found none." name schemaType.Name
        removeTypeNameField fields
        |> Array.map (firstUpper >> mapper)

    let getErrors (errors : JsonValue []) =
        let errorMapper = function
            | JsonValue.Record fields ->
                match fields |> Array.tryFind (fun (name, _) -> name = "message"), fields |> Array.tryFind (fun (name, _) -> name = "path") with
                | Some (_, JsonValue.String message), Some (_, JsonValue.Array path) ->
                    let pathMapper = function
                        | JsonValue.String x -> box x
                        | JsonValue.Integer x -> box x
                        | _ -> failwith "Error parsing response errors. A item in the path is neither a String or a Number."
                    { Message = message; Path = Array.map pathMapper path }
                | _ -> failwith "Error parsing response errors. Unsupported errors field format."
            | other -> failwithf "Error parsing response errors. Expected error to be a Record type, but it is %s." (other.ToString())
        Array.map errorMapper errors

/// The base type for all GraphQLProvider operation result provided types.
type OperationResultBase (responseJson : JsonValue, schemaTypes : Map<string, IntrospectionType>, operationTypeName : string) =
    let data = 
        let data = JsonValueHelper.getResponseDataFields responseJson
        let operationType =
            match schemaTypes.TryFind(operationTypeName) with
            | Some def -> def
            | _ -> failwithf "Operation type %s could not be found on the schema types." operationTypeName
        match data with
        | Some [||] | None -> None
        | Some dataFields ->
            let fieldValues = JsonValueHelper.getFieldValues schemaTypes operationType dataFields
            let props = fieldValues |> Array.map (fun (name, value) -> { Name = name; Value = value })
            Some (RecordBase(operationType.Name, props))

    let errors =
        let errors = JsonValueHelper.getResponseErrors responseJson
        match errors with
        | Some [||] | None -> None
        | Some errors -> Some (JsonValueHelper.getErrors errors)

    let customData = 
        let customData = JsonValueHelper.getResponseCustomFields responseJson
        match customData with
        | [||] -> None
        | customData -> Some (Serialization.deserializeMap customData)

    member private __.ResponseJson = responseJson

    /// Gets the data returned by the operation on the server.
    member __.Data = data

    /// Gets all the errors returned by the operation on the server.
    member __.Errors = errors
    
    /// Gets all the custom data returned by the operation on server as a map of names and values.
    member __.CustomData = customData

    member x.Equals(other : OperationResultBase) =
        x.ResponseJson = other.ResponseJson

    override x.Equals(other : obj) =
        match other with
        | :? OperationResultBase as other -> x.Equals(other)
        | _ -> false

    override x.GetHashCode() = x.ResponseJson.GetHashCode()

/// The base type for al GraphQLProvider operation provided types.
type OperationBase (query : string) =
    /// Gets the query string of the operation.
    member __.Query = query