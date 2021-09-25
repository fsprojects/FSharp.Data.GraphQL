/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Client

open System
open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open System.Reflection
open System.Collections.Concurrent
open System.Collections.Generic
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types.Introspection
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Ast.Extensions
open FSharp.Reflection
open FSharp.Quotations


type OptionConverter<'T> () =
    inherit JsonConverter<option<'T>>()
    override _.Read(reader, typeToConvert, options) =
        match reader.TokenType with
        | JsonTokenType.Null -> None
        | _ -> Some <| JsonSerializer.Deserialize<'T>(&reader, options)

    override _.Write(writer, value, options) =
        match value with
        | None -> writer.WriteNullValue()
        | Some x -> JsonSerializer.Serialize<'T>(writer, x, options)

type OptionConverterFactory () =
    inherit JsonConverterFactory()
    let optionTypeConverter = typedefof<OptionConverter<_>>
    override _.CanConvert(typeToConvert) =
        typeToConvert.IsGenericType && typeToConvert.GetGenericTypeDefinition() = typedefof<option<_>>

    override _.CreateConverter(typeToConvert, options) =
        optionTypeConverter
            .MakeGenericType(typeToConvert.GetGenericArguments())
            .GetConstructor([||])
            .Invoke([||]) :?> JsonConverter


type RecordConverter<'T> () =
    inherit JsonConverter<'T>()
    let isNullableField (propertyInfo: PropertyInfo) =
        let propType = propertyInfo.PropertyType
        propType.IsGenericType &&
            (let genericType = propType.GetGenericTypeDefinition()
             genericType = typedefof<option<_>> || genericType = typedefof<voption<_>>)

    let makeRecord = FSharpValue.PreComputeRecordConstructor(typeof<'T>, true)
    let readRecord = FSharpValue.PreComputeRecordReader(typeof<'T>, true)

    let recordFields =
        let flags = BindingFlags.Public ||| BindingFlags.NonPublic
        [| for field in FSharpType.GetRecordFields(typeof<'T>, flags) do
           (field.Name, isNullableField field, field.PropertyType) |]

    let fieldDefaults =
        [| for (_,_,fieldType) in recordFields do
            if fieldType.IsValueType
            then Activator.CreateInstance(fieldType)
            else null |]

    let tryGetFieldPosition =
        let cache = ConcurrentDictionary<bool * string, int option>()
        fun (caseInsensitive: bool) (propertyName: string) ->
            let key = (caseInsensitive, propertyName)
            match cache.TryGetValue key with
            | true, v ->
                v
            | false, _ ->
                let stringCompare = if caseInsensitive then StringComparison.InvariantCultureIgnoreCase else StringComparison.InvariantCulture
                let index = recordFields |> Array.tryFindIndex(fun (v, _, _) -> v.Equals(propertyName, stringCompare))
                cache.[key] <- index
                index

    override _.Read(reader, typeToConvert, options) =
        if reader.TokenType <> JsonTokenType.StartObject then
            failwith "Expected Json Start Object Token"
        let mutable cont = true
        let values = Array.copy fieldDefaults
        let tryGetPosition = tryGetFieldPosition options.PropertyNameCaseInsensitive
        while cont do
            match reader.Read(), reader.TokenType with
            | true, JsonTokenType.EndObject ->
                cont <- false
            | true, JsonTokenType.PropertyName ->
                let propertyName = reader.GetString()
                match tryGetPosition propertyName with
                | Some pos ->
                    let (fieldName, isNullable, propertyType) = recordFields.[pos]
                    let value = JsonSerializer.Deserialize(&reader, propertyType, options)
                    if (not isNullable) && isNull value then
                        failwithf "Field '%s' is required" fieldName
                    values.[pos] <- value
                | None ->
                    reader.Skip()
                    ()
            | false, _ ->
                failwith "Unexpected End of JSON Sequence"
            | _, token ->
                failwithf "Unexpected token '%A' in JSON Sequence" token
        let record = makeRecord values
        record :?> 'T


    override _.Write(writer, value, options) =
        writer.WriteStartObject()
        let values = readRecord value
        let normalizeName =
            if isNull options.PropertyNamingPolicy
            then id
            else options.PropertyNamingPolicy.ConvertName
        for i = 0 to values.Length - 1 do
            let (name, _, propertyType) = recordFields.[i]
            writer.WritePropertyName(normalizeName name)
            JsonSerializer.Serialize(writer, values.[i], propertyType, options)
        writer.WriteEndObject()

type RecordConverterFactory () =
    inherit JsonConverterFactory()
    let recordTypeConverter = typedefof<RecordConverter<_>>

    override _.CanConvert(typeToConvert) =
        FSharpType.IsRecord(typeToConvert, true)

    override _.CreateConverter(typeToConvert, options) =
        recordTypeConverter
            .MakeGenericType(typeToConvert)
            .GetConstructor([||])
            .Invoke([||]) :?> JsonConverter

// Converted from FSharp.SystemTextJson
type MapConverter<'V> () =
    inherit JsonConverter<Map<string, 'V>>()
    let rec read (acc: Map<string, 'V>) (reader: byref<Utf8JsonReader>) options =
        if not (reader.Read()) then acc else
        match reader.TokenType with
        | JsonTokenType.EndObject -> acc
        | JsonTokenType.PropertyName ->
            let key = reader.GetString()
            let value =
                // if the value type is obj, we want to default to
                // the map converter whenever we see an object
                if typeof<'V> = typeof<obj>
                    && (let peekReader = reader
                        peekReader.Read() |> ignore
                        peekReader.TokenType = JsonTokenType.StartObject)
                then JsonSerializer.Deserialize<Map<string, obj>>(&reader, options) :> obj :?> 'V
                else JsonSerializer.Deserialize<'V>(&reader, options)
            read (Map.add key value acc) &reader options
        | _ ->
            failwithf "Failed to serialize map"

    override _.Read(reader, _typeToConvert, options) =
        read Map.empty &reader options

    override _.Write(writer, value, options) =
        writer.WriteStartObject()
        for kv in value do
            let k =
                match options.DictionaryKeyPolicy with
                | null -> kv.Key
                | p -> p.ConvertName kv.Key
            writer.WritePropertyName(k)
            JsonSerializer.Serialize<'V>(writer, kv.Value, options)
        writer.WriteEndObject()

type MapConverterFactory () =
    inherit JsonConverterFactory()
    let mapTypeConverter = typedefof<MapConverter<_>>
    override _.CanConvert(typeToConvert) =
        if typeToConvert.IsGenericType && typeToConvert.GetGenericTypeDefinition() = typedefof<Map<_, _>> then
            let types = typeToConvert.GetGenericArguments()
            types.[0] = typeof<string>
        else
            false
    override _.CreateConverter(typeToConvert, options) =
        let valueType = typeToConvert.GetGenericArguments().[1]
        mapTypeConverter
            .MakeGenericType([|valueType|])
            .GetConstructor([||])
            .Invoke([||]) :?> JsonConverter

type UploadConverter () =
    inherit JsonConverter<Upload>()

    override _.Read(_, _, _) =
        invalidOp "The type 'Upload' is write-only"

    override _.Write(writer, _, _) =
        writer.WriteNullValue()

type RecordBaseConverter () =
    inherit JsonConverter<RecordBase>()

    override _.Read(reader, _typeToConvert, options) =
        invalidOp "The type 'RecordBase' is write-only"

    override _.Write(writer, value, options) =
        let properties = value.ToDictionary()
        JsonSerializer.Serialize(writer, properties, options)

type EnumBaseConverter () =
    inherit JsonConverter<EnumBase>()

    override _.Read(reader, _typeToConvert, options) =
        invalidOp "The type 'EnumBase' is write-only"

    override _.Write(writer, value, options) =
        JsonSerializer.Serialize(writer, value.GetValue(), options)


[<AutoOpen>]
module RuntimeSerialization =

    let options = JsonSerializerOptions(PropertyNameCaseInsensitive=true, PropertyNamingPolicy=JsonNamingPolicy.CamelCase)
    options.Converters.Add(OptionConverterFactory())
    options.Converters.Add(RecordConverterFactory())
    options.Converters.Add(JsonStringEnumConverter())
    options.Converters.Add(MapConverterFactory())
    options.Converters.Add(UploadConverter())
    options.Converters.Add(RecordBaseConverter())
    options.Converters.Add(EnumBaseConverter())

    let getTypeName (element: JsonElement) =
        match element.TryGetProperty("__typename") with
        | true, value -> value.GetString()
        | false, _ -> failwith "__typename not found in json"

    let tryReadString (element: JsonElement) =
        match element.GetString() with
        | null -> None
        | s -> Some s

    let tryReadBool (element: JsonElement) =
       match element.ValueKind with
       | JsonValueKind.True -> Some true
       | JsonValueKind.False -> Some false
       |  _ -> None

    let tryReadInt (element: JsonElement) =
        match element.TryGetInt32() with
        | true, value -> Some value
        | _ -> None

    let tryReadFloat (element: JsonElement) =
        match element.TryGetDouble() with
        | true, value -> Some value
        | _ -> None

    let tryReadDateTime (element: JsonElement) =
        match element.TryGetDateTime() with
        | true, value -> Some value
        | _ -> None

    let tryReadUri (element: JsonElement) =
        match Uri.TryCreate(element.GetString(), UriKind.RelativeOrAbsolute) with
        | true, value -> Some value
        | false, _ -> None

    let tryReadGuid (element: JsonElement) =
        match element.TryGetGuid() with
        | true, value -> Some value
        | false, _ -> None

    let tryReadProperty (name: string) (element: JsonElement) =
        match element.TryGetProperty(name) with
        | true, value -> ValueSome value
        | false, _ -> ValueNone

    let readProperty (name: string) (element: JsonElement) =
        match element.TryGetProperty(name) with
        | true, value -> value
        | false, _ -> failwithf "'%s' is required, but was not found in result" name

    let readString (element: JsonElement) =
        element.GetString()

    let readBool (element: JsonElement) =
        element.GetBoolean()

    let readInt (element: JsonElement) =
        element.GetInt32()

    let readFloat (element: JsonElement) =
        element.GetDouble()

    let readDateTime (element: JsonElement) =
        element.GetDateTime()

    let readUri (element: JsonElement) =
        Uri(element.GetString())

    let readGuid (element: JsonElement) =
        element.GetGuid()

    let getPossibleTypes (schemaTypes: SchemaTypes) (abstractType: IntrospectionType) (abstractTypeName: string) (selection: AstFieldInfo) =
        match abstractType.PossibleTypes with
        | Some possibleTypes ->
            let fieldsByType =
                selection.Fields
                |> List.groupBy (function | TypeField _ -> abstractTypeName | FragmentField field -> field.TypeCondition)
                |> Map.ofList
            let getFieldsByTypeName typeName =
                fieldsByType
                |> Map.tryFind typeName
                |> Option.defaultValue []
            let commonFields = getFieldsByTypeName abstractTypeName
            [ for possibleType in possibleTypes do
                let objectType = schemaTypes.FindByTypeRef(possibleType)
                let possibleFields = commonFields @ getFieldsByTypeName objectType.Name
                objectType, possibleFields ]
        | None ->
            [ abstractType, selection.Fields ]

    let readErrorPath (element: JsonElement) =
        match tryReadProperty "path" element with
        | ValueSome pathElement when pathElement.ValueKind = JsonValueKind.Array ->
            use pathEnumerator = pathElement.EnumerateArray()
            [| for element in pathEnumerator do
                 if element.ValueKind = JsonValueKind.String
                 then element.GetString() |> box
                 else element.GetInt32() |> box |]
        | _ ->
            [||]

    let readErrors (element: JsonElement) =
        use enumerator = element.EnumerateArray()
        [| for element in enumerator do
            let message = readString (readProperty "message" element)
            let path =  readErrorPath element
            (message, path) |]

    let readCustomData (element: JsonElement) =
        use enumerator = element.EnumerateObject()
        let customData =
            [| for element in enumerator do
                match element.Name with
                | "data" | "errors" ->
                    ()
                | name ->
                    let serializationType =
                        match element.Value.ValueKind with
                        | JsonValueKind.Object -> typeof<Map<string, obj>>
                        | _ -> typeof<obj>
                    name, JsonSerializer.Deserialize(element.Value.GetRawText(), serializationType, options) |]
        Map.ofArray customData

module DesignTimeSerialization =

    type IExprVisitor<'R> =
        abstract Visit<'T> : unit -> 'R

    type IExprActivator =
        abstract Accept : v: IExprVisitor<'R> -> 'R

    type ExprActivator<'T> () =
        interface IExprActivator with
            member _.Accept(v: IExprVisitor<'R>) =
                v.Visit<'T>()

    type Expr with
        member e.Accept (v: IExprVisitor<'R>) =
            let ty = typedefof<ExprActivator<_>>.MakeGenericType e.Type
            let inst = Activator.CreateInstance(ty) :?> IExprActivator
            inst.Accept(v)

        member e.AcceptLambda (v: IExprVisitor<'R>) =
            if FSharpType.IsFunction e.Type then
                let (_, c) = FSharpType.GetFunctionElements e.Type
                let ty = typedefof<ExprActivator<_>>.MakeGenericType c
                let inst = Activator.CreateInstance(ty) :?> IExprActivator
                inst.Accept(v)
            else
                invalidOp "expected Expr<JsonElement -> 'c>"


    let inline reader (f: (JsonElement -> 'a)) (elementExpr: Expr<JsonElement>) =
        <@ f %elementExpr @> :> Expr

    let inline optionalReader (f: (JsonElement -> 'a option)) (elementExpr: Expr<JsonElement>) =
        <@ match (%elementExpr).ValueKind with
           | JsonValueKind.Null | JsonValueKind.Undefined -> None
           | _ -> f (%elementExpr) @> :> Expr

    let makeArrayExpr (elementExpr: Expr<JsonElement>) (body: Expr<JsonElement -> 'T>) (isNullable: bool) =
        if isNullable then
            <@@ let element: JsonElement = %elementExpr
                match element.ValueKind with
                | JsonValueKind.Array ->
                    use enumerator = element.EnumerateArray()
                    Some [| for element in enumerator -> (%body) element |]
                | _ ->
                    None
            @@>
        else
            <@@ let element: JsonElement = %elementExpr
                match element.ValueKind with
                | JsonValueKind.Array ->
                    use enumerator = element.EnumerateArray()
                    [| for element in enumerator -> (%body) element |]
                | valueKind ->
                    failwithf "Expected array type but received type: %A" valueKind
            @@>


    let rec tryReadObject (schemaTypes: SchemaTypes) (introspectionType: IntrospectionType) (selections: list<AstFieldInfo>) (elementExpr: Expr<JsonElement>) =
        let fieldExprs = makeFieldExpressions schemaTypes introspectionType selections elementExpr
        <@@ match (%elementExpr).ValueKind with
            | JsonValueKind.Object ->
                let properties = Dictionary<string, obj>()
                let typeName = getTypeName %elementExpr
                (%%fieldExprs) properties
                Some(RecordBase(typeName, properties))
            | _ ->
                None @@>
    and readObject (schemaTypes: SchemaTypes) (introspectionType: IntrospectionType) (selections: list<AstFieldInfo>) (elementExpr: Expr<JsonElement>) =
        let fieldExprs = makeFieldExpressions schemaTypes introspectionType selections elementExpr
        <@@ let element = %elementExpr
            let properties = Dictionary<string, obj>()
            (%%fieldExprs) properties
            let typeName = getTypeName element
            RecordBase(typeName, properties) @@>
    and makeFieldExpressions (schemaTypes: SchemaTypes) (introspectionType: IntrospectionType) (selections: list<AstFieldInfo>) (elementExpr: Expr<JsonElement>) =
        let fields = Option.defaultValue [||] introspectionType.Fields
        let fieldsMap =
            fields
            |> Array.map(fun field -> field.Name, field)
            |> Map.ofArray
        let propertiesVar = Var("properties", typeof<Dictionary<string, obj>>)
        let propertiesExpr = Expr.Var(propertiesVar)
        let makeSelectionExpr (selection: AstFieldInfo) =
            let propertyName = selection.AliasOrName
            let readerExpr: Expr =
                let selectionField = fieldsMap.[selection.Name]
                let isNullable = match selectionField.Type with ContainerTypeRef(TypeKind.NON_NULL, _) -> false | _ -> true
                let elementExpr =
                    if isNullable then
                       <@ match tryReadProperty propertyName %elementExpr with
                          | ValueSome element -> element
                          | ValueNone -> Unchecked.defaultof<JsonElement> @>
                    else
                       <@ readProperty propertyName %elementExpr @>
                makeElementReader schemaTypes selectionField selection selectionField.Type true elementExpr
            readerExpr.Accept
                { new IExprVisitor<Expr> with
                    member _.Visit<'r> () =
                        <@@ let properties:Dictionary<string, obj> = %%propertiesExpr
                            properties.[propertyName] <- (%%readerExpr : 'r) @@>
                }
        let fieldExpressions =
            selections
            |> List.filter(fun s -> s.Name <> "__typename")
            |> List.map makeSelectionExpr
            |> List.reduce(fun e1 e2 -> Expr.Sequential(e1, e2))
        Expr.Lambda(propertiesVar, fieldExpressions)
    and makeElementReader (schemaTypes: SchemaTypes) (field: IntrospectionField) (selection: AstFieldInfo) (fieldType: IntrospectionTypeRef) (isNullable: bool) =
        match fieldType with
        | ContainerTypeRef(TypeKind.NON_NULL, innerTypeRef) ->
            makeElementReader schemaTypes field selection innerTypeRef false
        | ContainerTypeRef(TypeKind.LIST, innerTypeRef) ->
            let body =
                let elementVar = Var("element", typeof<JsonElement>)
                let elementExpr = Expr.Var(elementVar) |> Expr.Cast<JsonElement>
                let elementReader = makeElementReader schemaTypes field selection innerTypeRef true
                Expr.Lambda(elementVar, elementReader elementExpr)
            fun elementExpr ->
                body.AcceptLambda
                    { new IExprVisitor<Expr> with
                        member _.Visit<'r> () =
                            let expr = Expr.Cast<JsonElement -> 'r> body
                            makeArrayExpr elementExpr expr isNullable }
        | NamedTypeRef(TypeKind.OBJECT, name) ->
            let objectType = schemaTypes.FindType(name)
            if isNullable
            then tryReadObject schemaTypes objectType selection.Fields
            else readObject schemaTypes objectType selection.Fields
        | NamedTypeRef((TypeKind.UNION | TypeKind.INTERFACE), abstractTypeName) ->
            let abstractType = schemaTypes.FindType(abstractTypeName)
            let implementations = getPossibleTypes schemaTypes abstractType abstractTypeName selection
            let typeNameVar = Var("typeVar", typeof<string>)
            let typeNameExpr = Expr.Var(typeNameVar)
            fun elementExpr ->
                let objectReader = if isNullable then tryReadObject else readObject
                let readConcreteType =
                    let rec makeConditionalRecord (elements: list<IntrospectionType * list<AstFieldInfo>>) =
                        match elements with
                        | (objectType, fields)::tail ->
                            let typeName = objectType.Name
                            let readExpr = objectReader schemaTypes objectType fields elementExpr
                            Expr.IfThenElse(<@@ %%typeNameExpr = typeName @@>, readExpr,  makeConditionalRecord tail)
                        | [] when isNullable -> <@@ None : RecordBase option @@>
                        | [] -> <@@ failwithf "Unknown descriminator for type %s" abstractTypeName @@>
                    makeConditionalRecord implementations
                Expr.Let(typeNameVar, <@@ getTypeName (%elementExpr) @@>, readConcreteType)
        | NamedTypeRef(TypeKind.ENUM, typeName) ->
            fun expr ->
                if isNullable then
                    <@@ match tryReadString %expr with
                        | Some value ->
                            Some(EnumBase(typeName, value))
                        | None ->
                            None @@>
                else
                    <@@ EnumBase(typeName, readString %expr) @@>
        | NamedTypeRef(TypeKind.SCALAR, typeName) ->
            match typeName with
            | "String" | "ID" ->
                if isNullable then
                   fun elementExpr ->
                       <@ match (%elementExpr).ValueKind with
                           | JsonValueKind.Null | JsonValueKind.Undefined -> None
                           | _ -> tryReadString (%elementExpr) @> :> Expr
                else
                     fun elementExpr ->
                        <@ readString (%elementExpr) @> :> Expr
            | "Int" ->
                if isNullable then
                   fun elementExpr ->
                       <@ match (%elementExpr).ValueKind with
                           | JsonValueKind.Null | JsonValueKind.Undefined -> None
                           | _ -> tryReadInt (%elementExpr) @> :> Expr
                else
                     fun elementExpr ->
                        <@ readInt (%elementExpr) @> :> Expr
            | "Boolean" ->
                if isNullable then
                   fun elementExpr ->
                       <@ match (%elementExpr).ValueKind with
                           | JsonValueKind.Null | JsonValueKind.Undefined -> None
                           | _ -> tryReadBool (%elementExpr) @> :> Expr
                else
                     fun elementExpr ->
                        <@ readBool (%elementExpr) @> :> Expr
            | "Float" ->
                if isNullable then
                   fun elementExpr ->
                       <@ match (%elementExpr).ValueKind with
                           | JsonValueKind.Null | JsonValueKind.Undefined -> None
                           | _ -> tryReadFloat (%elementExpr) @> :> Expr
                else
                     fun elementExpr ->
                        <@ readFloat (%elementExpr) @> :> Expr
            | "Date" ->
                if isNullable then
                   fun elementExpr ->
                       <@ match (%elementExpr).ValueKind with
                           | JsonValueKind.Null | JsonValueKind.Undefined -> None
                           | _ -> tryReadDateTime (%elementExpr) @> :> Expr
                else
                     fun elementExpr ->
                        <@ readDateTime (%elementExpr) @> :> Expr
            | "Guid" ->
                if isNullable then
                   fun elementExpr ->
                       <@ match (%elementExpr).ValueKind with
                           | JsonValueKind.Null | JsonValueKind.Undefined -> None
                           | _ -> tryReadGuid (%elementExpr) @> :> Expr
                else
                     fun elementExpr ->
                        <@ readGuid (%elementExpr) @> :> Expr
            | "URI" ->
                if isNullable then
                   fun elementExpr ->
                       <@ match (%elementExpr).ValueKind with
                           | JsonValueKind.Null | JsonValueKind.Undefined -> None
                           | _ -> tryReadUri (%elementExpr) @> :> Expr
                else
                     fun elementExpr ->
                        <@ readUri (%elementExpr) @> :> Expr
            | scalar -> failwithf "%s is not supported" scalar
        | other ->
            failwithf "%A not supported" other



    let getOperationJsonReader (schemaTypes: SchemaTypes) (selections: list<AstFieldInfo>) (operation: OperationDefinition) =
        let introspection = schemaTypes.Introspection
        let operationTypeRef =
            let operationTypeRefOpt =
                match operation.OperationType with
                | Query -> Some introspection.QueryType
                | Mutation -> introspection.MutationType
                | Subscription -> introspection.SubscriptionType
            match operationTypeRefOpt with
            | Some op -> op
            | None -> invalidOp (sprintf "top-level %A type is not defined in the schema" operation.OperationType)
        let operationType = schemaTypes.FindByTypeRef(operationTypeRef)
        let elementVar = Var("element", typeof<JsonElement>)
        let elementExpr = Expr.Var(elementVar) |> Expr.Cast<JsonElement>
        let dataElementExpr =
            <@ match tryReadProperty "data" %elementExpr with
               | ValueSome element -> element
               | ValueNone -> Unchecked.defaultof<JsonElement> @>
        let resultExpr = tryReadObject schemaTypes operationType selections dataElementExpr
        let bodyExpr =
            <@@
                let element = %elementExpr
                let result: RecordBase option = %%resultExpr
                let errors: OperationError [] =
                    match tryReadProperty "errors" element with
                    | ValueSome errorElement when errorElement.ValueKind = JsonValueKind.Array ->
                        element
                        |> readErrors
                        |> Array.map(fun (m, p) -> { Message = m; Path = p })
                    | _ ->
                        [||]
                let customData = readCustomData element
                { CustomData = customData; Data = result; Errors = errors  }
            @@>
        Expr.Lambda(elementVar, bodyExpr)


module Serialization =

    let deserializeStream<'T> (stream: Stream) =
        let vtask = JsonSerializer.DeserializeAsync<'T>(stream, RuntimeSerialization.options)
        vtask.AsTask() |> Async.AwaitTask

    let deserializeSchema (stream: Stream) =
        let introspectionSchema =
            stream
            |> deserializeStream<GraphQLResponse<IntrospectionResult>>
            |> Async.RunSynchronously
        introspectionSchema.Data.__schema

    let serializeToStream (value: 'T) (stream: Stream) = async {
        do! JsonSerializer.SerializeAsync(stream, value, RuntimeSerialization.options) |> Async.AwaitTask
        stream.Position <- 0L
    }