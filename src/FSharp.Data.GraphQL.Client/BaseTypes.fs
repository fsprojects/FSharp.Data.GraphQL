/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open System.Security.Cryptography
open System.Globalization
open FSharp.Data
open FSharp.Core
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Client
open System.Collections.Generic
open FSharp.Data.GraphQL.Types.Introspection
open FSharp.Data.GraphQL.Ast.Extensions
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Quotations
open System.Reflection
open System.Text
open Microsoft.FSharp.Reflection
open System.Collections
open System.Net

/// A type alias to represent a Type name.
type TypeName = string

/// Contains data about a GQL operation error.
type OperationError =
      /// The description of the error that happened in the operation.
    { Message : string
      /// The path to the field that produced the error while resolving its value.
      Path : obj [] }

module internal QuotationHelpers = 
    let rec coerceValues fieldTypeLookup fields = 
        let arrayExpr (arrayType : Type) (v : obj) =
            let typ = arrayType.GetElementType()
            let instance =
                match v with
                | :? IEnumerable as x -> Seq.cast<obj> x |> Array.ofSeq
                | _ -> failwith "Unexpected array value."
            let exprs = coerceValues (fun _ -> typ) instance
            Expr.NewArray(typ, exprs)
        let tupleExpr (tupleType : Type) (v : obj) =
            let typ = FSharpType.GetTupleElements tupleType |> Array.mapi (fun i t -> i, t) |> Map.ofArray
            let fieldTypeLookup i = typ.[i]
            let fields = FSharpValue.GetTupleFields v
            let exprs = coerceValues fieldTypeLookup fields
            Expr.NewTuple(exprs)
        Array.mapi (fun i v ->
                let expr = 
                    if isNull v then simpleTypeExpr v
                    else
                        let tpy = v.GetType()
                        if tpy.IsArray then arrayExpr tpy v
                        elif FSharpType.IsTuple tpy then tupleExpr tpy v
                        elif FSharpType.IsUnion tpy then unionExpr v |> snd
                        elif FSharpType.IsRecord tpy then recordExpr v |> snd
                        else simpleTypeExpr v
                Expr.Coerce(expr, fieldTypeLookup i)
        ) fields |> List.ofArray
    
    and simpleTypeExpr instance = Expr.Value(instance)

    and unionExpr instance = 
        let caseInfo, fields = FSharpValue.GetUnionFields(instance, instance.GetType())    
        let fieldInfo = caseInfo.GetFields()
        let fieldTypeLookup indx = fieldInfo.[indx].PropertyType
        caseInfo.DeclaringType, Expr.NewUnionCase(caseInfo, coerceValues fieldTypeLookup fields)

    and recordExpr instance = 
        let tpy = instance.GetType()
        let fields = FSharpValue.GetRecordFields(instance)
        let fieldInfo = FSharpType.GetRecordFields(tpy)
        let fieldTypeLookup indx = fieldInfo.[indx].PropertyType
        tpy, Expr.NewRecord(instance.GetType(), coerceValues fieldTypeLookup fields)

    and arrayExpr (instance : 'a array) =
        let typ = typeof<'a>
        let arrayType = instance.GetType()
        let exprs = coerceValues (fun _ -> typ) (instance |> Array.map box)
        arrayType, Expr.NewArray(typ, exprs)

    let createLetExpr varType instance body args = 
        let var = Var("instance", varType)  
        Expr.Let(var, instance, body args (Expr.Var(var)))

    let quoteUnion instance = 
        let func instance = unionExpr instance ||> createLetExpr
        Tracer.runAndMeasureExecutionTime "Quoted union type" (fun _ -> func instance)

    let quoteRecord instance = 
        let func instance = recordExpr instance ||> createLetExpr
        Tracer.runAndMeasureExecutionTime "Quoted record type" (fun _ -> func instance)

    let quoteArray instance = 
        let func instance = arrayExpr instance ||> createLetExpr
        Tracer.runAndMeasureExecutionTime "Quoted array type" (fun _ -> func instance)

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

    static member internal MakeProvidedType(name, items : string seq) =
        let tdef = ProvidedTypeDefinition(name, Some typeof<EnumBase>, nonNullable = true, isSealed = true)
        tdef.AddMembersDelayed(fun _ ->
            items
            |> Seq.map (fun item ->
                let getterCode (_ : Expr list) =
                    Expr.NewObject(EnumBase.Constructor, [ <@@ name @@>; <@@ item @@> ])
                ProvidedProperty(item, tdef, getterCode, isStatic = true))
            |> Seq.cast<MemberInfo>
            |> List.ofSeq)
        tdef

    static member internal Constructor = typeof<EnumBase>.GetConstructors().[0]

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

type internal ProvidedTypeMetadata =
    { Name : string
      Description : string option }

/// The base type for all GraphQLProvider provided interface types.
type InterfaceBase private () =
    static member internal MakeProvidedType(metadata : ProvidedTypeMetadata) =
        let tdef = ProvidedTypeDefinition("I" + metadata.Name.FirstCharUpper(), None, nonNullable = true, isInterface = true)
        metadata.Description |> Option.iter tdef.AddXmlDoc
        tdef

type internal RecordPropertyMetadata =
    { Name : string
      Description : string option
      DeprecationReason : string option
      Type : Type }

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

    /// Gets the name of this provided record type.
    member __.GetName() = name

    /// Gets a list of this provided record properties.
    member __.GetProperties() = List.ofSeq properties
    
    /// Produces a dictionary containing all the properties and names of this provided record type.
    member x.ToDictionary() =
        let mapper (v : obj) =
            match v with
            | :? RecordBase as v -> box (v.ToDictionary())
            | _ -> v
        x.GetProperties()
        |> Seq.map (fun p -> p.Name, mapper p.Value)
        |> dict

    static member internal MakeProvidedType(tdef : ProvidedTypeDefinition, properties : RecordPropertyMetadata list) =
        let name = tdef.Name
        let propertyMapper (metadata : RecordPropertyMetadata) : MemberInfo =
            let pname = metadata.Name.FirstCharUpper()
            let getterCode (args : Expr list) =
                <@@ let this = %%args.[0] : RecordBase
                    match this.GetProperties() |> List.tryFind (fun prop -> prop.Name = pname) with
                    | Some prop -> prop.Value
                    | None -> failwithf "Expected to find property \"%s\", but the property was not found." pname @@>
            let pdef = ProvidedProperty(pname, metadata.Type, getterCode)
            metadata.Description |> Option.iter pdef.AddXmlDoc
            metadata.DeprecationReason |> Option.iter pdef.AddObsoleteAttribute
            upcast pdef
        tdef.AddMembersDelayed(fun _ -> List.map propertyMapper properties)
        let addConstructorDelayed (propertiesGetter : unit -> (string * Type) list) =
            tdef.AddMemberDelayed(fun _ ->
                let properties = propertiesGetter ()
                let prm = properties |> List.map (fun (name, t) -> ProvidedParameter(name, t))
                let invoker (args : Expr list) = 
                    let properties =
                        let args = 
                            let names = properties |> List.map (fun (name, _) -> name.FirstCharUpper())
                            let mapper (name : string, value : Expr) =
                                let value = Expr.Coerce(value, typeof<obj>)
                                <@@ { Name = name; Value = %%value } @@>
                            List.zip names args |> List.map mapper
                        Expr.NewArray(typeof<RecordProperty>, args)
                    Expr.NewObject(RecordBase.Constructor, [Expr.Value(name); properties])
                ProvidedConstructor(prm, invoker))
        match tdef.BaseType with
        | :? ProvidedTypeDefinition as bdef ->
            bdef.AddMembersDelayed(fun _ ->
                let asType = 
                    let invoker (args : Expr list) =
                        <@@ let this = %%args.[0] : RecordBase
                            if this.GetName() = name then this
                            else failwithf "Expected type to be \"%s\", but it is \"%s\". Make sure to check the type by calling \"Is%s\" method before calling \"As%s\" method." name (this.GetName()) name name @@>
                    ProvidedMethod("As" + name, [], tdef, invoker)
                let tryAsType =
                    let invoker (args : Expr list) =
                        <@@ let this = %%args.[0] : RecordBase
                            if this.GetName() = name then Some this
                            else None @@>
                    ProvidedMethod("TryAs" + name, [], typedefof<_ option>.MakeGenericType(tdef), invoker)
                let isType =
                    let invoker (args : Expr list) =
                        <@@ let this = %%args.[0] : RecordBase
                            this.GetName() = name @@>
                    ProvidedMethod("Is" + name, [], typeof<bool>, invoker)
                let members : MemberInfo list = [asType; tryAsType; isType]
                members)
            let propertiesGetter() =
                let bprops = bdef.GetConstructors().[0].GetParameters() |> Array.map (fun p -> p.Name, p.ParameterType) |> List.ofArray
                let props = properties |> List.map (fun p -> p.Name, p.Type)
                bprops @ props
            addConstructorDelayed propertiesGetter
        | _ -> 
            let propertiesGetter() = properties |> List.map (fun p -> p.Name, p.Type)
            addConstructorDelayed propertiesGetter
        tdef

    static member internal PreBuildProvidedType(metadata : ProvidedTypeMetadata, baseType : Type option) =
        let baseType = Option.defaultValue typeof<RecordBase> baseType
        let name = metadata.Name.FirstCharUpper()
        let tdef = ProvidedTypeDefinition(name, Some baseType, nonNullable = true, isSealed = true)
        tdef

    static member internal Constructor = typeof<RecordBase>.GetConstructors().[0]

    static member internal NewObjectExpr(properties : (string * obj) list) =
        let names = properties |> List.map fst
        let values = properties |> List.map snd
        Expr.NewObject(RecordBase.Constructor, [ <@@ List.zip names values @@> ])

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
        let xprops = x.GetProperties() |> List.sortBy (fun x -> x.Name)
        let yprops = other.GetProperties() |> List.sortBy (fun x -> x.Name)
        x.GetName() = other.GetName() && xprops = yprops


    override x.Equals(other : obj) =
        match other with
        | :? RecordBase as other -> x.Equals(other)
        | _ -> false

    override x.GetHashCode() = x.GetName().GetHashCode() ^^^ x.GetProperties().GetHashCode()

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
        let getOptionCases (t: Type) =
            let otype = typedefof<_ option>.MakeGenericType(t)
            let cases = FSharpType.GetUnionCases(otype)
            let some = cases |> Array.find (fun c -> c.Name = "Some")
            let none = cases |> Array.find (fun c -> c.Name = "None")
            (some, none, otype)
        let makeSome (value : obj) =
            let (some, _, _) = getOptionCases (value.GetType())
            FSharpValue.MakeUnion(some, [|value|])
        let makeNone (t : Type) =
            let (_, none, _) = getOptionCases t
            FSharpValue.MakeUnion(none, [||])
        let makeArray (itype : Type) (items : obj []) =
            if Array.exists (fun x -> isNull x) items
            then failwith "Array is an array of non null items, but a null item was found."
            else
                let arr = Array.CreateInstance(itype, items.Length)
                items |> Array.iteri (fun i x -> arr.SetValue(x, i))
                box arr
        let makeOptionArray (itype : Type) (items : obj []) =
            let (some, none, otype) = getOptionCases(itype)
            let arr = Array.CreateInstance(otype, items.Length)
            let mapper (i : int) (x : obj) =
                if isNull x
                then arr.SetValue(FSharpValue.MakeUnion(none, [||]), i)
                else arr.SetValue(FSharpValue.MakeUnion(some, [|x|]), i)
            items |> Array.iteri mapper
            box arr
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
            | JsonValue.Number n -> makeSomeIfNeeded (float n)
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
                        | JsonValue.Float x -> box (int x)
                        | JsonValue.Number x -> box (int x)
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

    static member internal MakeProvidedType(operationType : Type) =
        let tdef = ProvidedTypeDefinition("OperationResult", Some typeof<OperationResultBase>, nonNullable = true)
        tdef.AddMemberDelayed(fun _ ->
            let getterCode (args : Expr list) =
                <@@ let this = %%args.[0] : OperationResultBase
                    this.Data @@>
            let prop = ProvidedProperty("Data", operationType, getterCode)
            prop.AddXmlDoc("Contains the data returned by the operation on the server.")
            prop)
        tdef

    member x.Equals(other : OperationResultBase) =
        x.ResponseJson = other.ResponseJson

    override x.Equals(other : obj) =
        match other with
        | :? OperationResultBase as other -> x.Equals(other)
        | _ -> false

    override x.GetHashCode() = x.ResponseJson.GetHashCode()

type internal GraphQLContextInfo =
    { ServerUrl : string
      HttpHeaders : seq<string * string> }

/// A context for running operations using the GraphQLProvider in runtime.
type GraphQLProviderRuntimeContext =
      /// Gets the URL of the server that this context refers to.
    { ServerUrl : string
      /// Gets the HTTP headers used for calls to the server that this context refers to.
      HttpHeaders : seq<string * string> }
    /// Gets the connection component used to make calls to the server.
    member __.Connection = new GraphQLConnection()
    member x.Dispose() = x.Connection.Dispose()
    interface IDisposable with
        member x.Dispose() = x.Dispose()

/// The base type for al GraphQLProvider operation provided types.
type OperationBase (query : string) =
    /// Gets the query string of the operation.
    member __.Query = query

    static member internal MakeProvidedType(userQuery,
                                            operationDefinition : OperationDefinition,
                                            operationTypeName : string, 
                                            schemaTypes : Expr,
                                            schemaProvidedTypes : Map<string, ProvidedTypeDefinition>,
                                            operationType : Type,
                                            contextInfo : GraphQLContextInfo option) =
        let query = 
            let ast = Parser.parse userQuery
            ast.ToQueryString(QueryStringPrintingOptions.IncludeTypeNames).Replace("\r\n", "\n")
        let className = 
            let hash = 
                Encoding.UTF8.GetBytes(query)
                |> MD5.Create().ComputeHash
                |> Array.map (fun x -> x.ToString("x2"))
                |> Array.reduce (+)
            "Operation" + hash
        let tdef = ProvidedTypeDefinition(className, Some typeof<OperationBase>)
        tdef.AddXmlDoc("Represents a GraphQL operation on the server.")
        tdef.AddMembersDelayed(fun _ ->
            let rtdef = OperationResultBase.MakeProvidedType(operationType)
            let variables =
                let rec mapVariable (variableName : string) (vartype : InputType) =
                    match vartype with
                    | NamedType typeName ->
                        match Types.scalar.TryFind(typeName) with
                        | Some t -> (variableName, Types.makeOption t)
                        | None ->
                            match schemaProvidedTypes.TryFind(typeName) with
                            | Some t -> (variableName, Types.makeOption t)
                            | None -> failwithf "Unable to find variable type \"%s\" on the schema definition." typeName
                    | ListType itype -> 
                        let (name, t) = mapVariable variableName itype
                        (name, t |> Types.makeArray |> Types.makeOption)
                    | NonNullType itype -> 
                        let (name, t) = mapVariable variableName itype
                        (name, Types.unwrapOption t)
                operationDefinition.VariableDefinitions |> List.map (fun vdef -> mapVariable vdef.VariableName vdef.Type)
            let varExprMapper (variables : (string * Type) list) (args : Expr list) =
                let exprMapper (name : string, value : Expr) =
                    let value = Expr.Coerce(value, typeof<obj>)
                    <@@ let value = 
                            match (%%value : obj) with
                            | :? RecordBase as v -> box (v.ToDictionary())
                            | v -> v
                        (name, value) @@>
                let args =
                    let names = variables |> List.map fst
                    let args = 
                        match args with
                        | _ :: tail when tail.Length > 0 -> List.take (tail.Length - 1) tail
                        | _ -> []
                    List.zip names args |> List.map exprMapper
                Expr.NewArray(typeof<string * obj>, args)
            let defaultContextExpr = 
                match contextInfo with
                | Some info -> 
                    let serverUrl = info.ServerUrl
                    let headerNames = info.HttpHeaders |> Seq.map fst |> Array.ofSeq
                    let headerValues = info.HttpHeaders |> Seq.map snd |> Array.ofSeq
                    <@@ { ServerUrl = serverUrl; HttpHeaders = Array.zip headerNames headerValues } @@>
                | None -> <@@ Unchecked.defaultof<GraphQLProviderRuntimeContext> @@>
            let rundef = 
                let invoker (args : Expr list) =
                    let operationName = Option.toObj operationDefinition.Name
                    let variables = varExprMapper variables args
                    <@@ let argsContext = %%args.[args.Length - 1] : GraphQLProviderRuntimeContext
                        let isDefaultContext = Object.ReferenceEquals(argsContext, null)
                        let context = if isDefaultContext then %%defaultContextExpr else argsContext
                        let request =
                            { ServerUrl = context.ServerUrl
                              HttpHeaders = context.HttpHeaders
                              OperationName = Option.ofObj operationName
                              Query = query
                              Variables = %%variables }
                        let response = Tracer.runAndMeasureExecutionTime "Ran a GraphQL query" (fun _ -> GraphQLClient.sendRequest context.Connection request)
                        let responseJson = Tracer.runAndMeasureExecutionTime "Parsed a GraphQL response to a JsonValue" (fun _ -> JsonValue.Parse response)
                        if isDefaultContext then context.Dispose() // If the user does not provide a context, we should dispose the default one after running the query
                        OperationResultBase(responseJson, %%schemaTypes, operationTypeName) @@>
                let varprm = variables |> List.map (fun (name, t) -> ProvidedParameter(name, t))
                let ctxprm =
                    match contextInfo with
                    | Some _ -> ProvidedParameter("runtimeContext", typeof<GraphQLProviderRuntimeContext>, optionalValue = null)
                    | None -> ProvidedParameter("runtimeContext", typeof<GraphQLProviderRuntimeContext>)
                let mdef = ProvidedMethod("Run", varprm @ [ctxprm], rtdef, invoker)
                mdef.AddXmlDoc("Executes the operation on the server and fetch its results.")
                mdef
            let arundef = 
                let invoker (args : Expr list) =
                    let operationName = Option.toObj operationDefinition.Name
                    let variables = varExprMapper variables args
                    <@@ let argsContext = %%args.[args.Length - 1] : GraphQLProviderRuntimeContext
                        let isDefaultContext = Object.ReferenceEquals(argsContext, null)
                        let context = if isDefaultContext then %%defaultContextExpr else argsContext
                        let request =
                            { ServerUrl = context.ServerUrl
                              HttpHeaders = context.HttpHeaders
                              OperationName = Option.ofObj operationName
                              Query = query
                              Variables = %%variables }
                        async {
                            let! response = Tracer.asyncRunAndMeasureExecutionTime "Ran a GraphQL query asynchronously" (fun _ -> GraphQLClient.sendRequestAsync context.Connection request)
                            let responseJson = Tracer.runAndMeasureExecutionTime "Parsed a GraphQL response to a JsonValue" (fun _ -> JsonValue.Parse response)
                            if isDefaultContext then context.Dispose() // If the user does not provide a context, we should dispose the default one after running the query
                            return OperationResultBase(responseJson, %%schemaTypes, operationTypeName)
                        } @@>
                let varprm = variables |> List.map (fun (name, t) -> ProvidedParameter(name, t))
                let ctxprm =
                    match contextInfo with
                    | Some _ -> ProvidedParameter("runtimeContext", typeof<GraphQLProviderRuntimeContext>, optionalValue = null)
                    | None -> ProvidedParameter("runtimeContext", typeof<GraphQLProviderRuntimeContext>)
                let mdef = ProvidedMethod("AsyncRun", varprm @ [ctxprm], Types.makeAsync rtdef, invoker)
                mdef.AddXmlDoc("Executes the operation asynchronously on the server and fetch its results.")
                mdef
            let members : MemberInfo list = [rtdef; rundef; arundef]
            members)
        tdef

/// The base type for the GraphQLProvider.
type ProviderBase private () =
    static member private GetOperationProvidedTypes(schemaTypes : Map<TypeName, IntrospectionType>, enumProvidedTypes : Map<TypeName, ProvidedTypeDefinition>, operationAstFields, operationTypeRef) =
        let providedTypes = ref Map.empty<Path * TypeName, ProvidedTypeDefinition>
        let rec getProvidedType (providedTypes : Map<Path * TypeName, ProvidedTypeDefinition> ref) (schemaTypes : Map<TypeName, IntrospectionType>) (path : Path) (astFields : AstFieldInfo list) (tref : IntrospectionTypeRef) : Type =
            match tref.Kind with
            | TypeKind.NON_NULL when tref.Name.IsNone && tref.OfType.IsSome -> getProvidedType providedTypes schemaTypes path astFields tref.OfType.Value |> Types.unwrapOption
            | TypeKind.LIST when tref.Name.IsNone && tref.OfType.IsSome -> getProvidedType providedTypes schemaTypes path astFields tref.OfType.Value |> Types.makeArray |> Types.makeOption
            | TypeKind.SCALAR when tref.Name.IsSome ->
                if Types.scalar.ContainsKey(tref.Name.Value)
                then Types.scalar.[tref.Name.Value] |> Types.makeOption
                else Types.makeOption typeof<string>
            | TypeKind.ENUM when tref.Name.IsSome ->
                match enumProvidedTypes.TryFind(tref.Name.Value) with
                | Some providedEnum -> Types.makeOption providedEnum
                | None -> failwithf "Could not find a enum type based on a type reference. The reference is an \"%s\" enum, but that enum was not found in the introspection schema." tref.Name.Value
            | (TypeKind.OBJECT | TypeKind.INTERFACE | TypeKind.UNION) when tref.Name.IsSome ->
                if (!providedTypes).ContainsKey(path, tref.Name.Value)
                then upcast (!providedTypes).[path, tref.Name.Value]
                else
                    let ifields typeName =
                        if schemaTypes.ContainsKey(typeName)
                        then schemaTypes.[typeName].Fields |> Option.defaultValue [||]
                        else failwithf "Could not find a schema type based on a type reference. The reference is to a \"%s\" type, but that type was not found in the schema types." typeName
                    let getPropertyMetadata typeName (info : AstFieldInfo) : RecordPropertyMetadata =
                        let ifield =
                            match ifields typeName |> Array.tryFind(fun f -> f.Name = info.Name) with
                            | Some ifield -> ifield
                            | None -> failwithf "Could not find field \"%s\" of type \"%s\". The schema type does not have a field with the specified name." info.Name tref.Name.Value
                        let path = info.AliasOrName :: path
                        let astFields = info.Fields
                        let ftype = getProvidedType providedTypes schemaTypes path astFields ifield.Type
                        { Name = info.Name; Description = ifield.Description; DeprecationReason = ifield.DeprecationReason; Type = ftype }
                    let baseType =
                        let metadata : ProvidedTypeMetadata = { Name = tref.Name.Value; Description = tref.Description }
                        let tdef = RecordBase.PreBuildProvidedType(metadata, None)
                        providedTypes := (!providedTypes).Add((path, tref.Name.Value), tdef)
                        let properties =
                            astFields
                            |> List.filter (function | TypeField _ -> true | _ -> false)
                            |> List.map (getPropertyMetadata tref.Name.Value)
                        RecordBase.MakeProvidedType(tdef, properties)
                    let fragmentProperties =
                        astFields
                        |> List.choose (function FragmentField f -> Some f | _ -> None)
                        |> List.groupBy (fun field -> field.TypeCondition)
                        |> List.map (fun (typeCondition, fields) -> typeCondition, List.map (getPropertyMetadata typeCondition) (List.map FragmentField fields))
                    let fragmentTypes =
                        let createFragmentType (typeName, properties) =
                            let itype =
                                if schemaTypes.ContainsKey(typeName)
                                then schemaTypes.[typeName]
                                else failwithf "Could not find schema type based on the query. Type \"%s\" does not exist on the schema definition." typeName
                            let metadata : ProvidedTypeMetadata = { Name = itype.Name; Description = itype.Description }
                            let tdef = RecordBase.PreBuildProvidedType(metadata, Some (upcast baseType))
                            RecordBase.MakeProvidedType(tdef, properties)
                        fragmentProperties
                        |> List.map createFragmentType
                    fragmentTypes |> List.iter (fun fragmentType -> providedTypes := (!providedTypes).Add((path, fragmentType.Name), fragmentType))
                    Types.makeOption baseType
            | _ -> failwith "Could not find a schema type based on a type reference. The reference has an invalid or unsupported combination of Name, Kind and OfType fields."
        (getProvidedType providedTypes schemaTypes [] operationAstFields operationTypeRef), !providedTypes

    static member internal GetSchemaProvidedTypes(schema : IntrospectionSchema) =
        let providedTypes = ref Map.empty<TypeName, ProvidedTypeDefinition>
        let schemaTypes = Types.getSchemaTypes(schema)
        let getSchemaType (tref : IntrospectionTypeRef) =
            match tref.Name with
            | Some name ->
                match schemaTypes.TryFind(name) with
                | Some itype -> itype
                | None -> failwithf "Type \"%s\" was not found on the schema custom types." name
            | None -> failwith "Expected schema type to have a name, but it does not have one."
        let typeModifier (modifier : Type -> Type) (metadata : RecordPropertyMetadata) = { metadata with Type = modifier metadata.Type }
        let makeOption = typeModifier Types.makeOption
        let makeArrayOption = typeModifier (Types.makeArray >> Types.makeOption)
        let unwrapOption = typeModifier Types.unwrapOption
        let ofFieldType (field : IntrospectionField) = { field with Type = field.Type.OfType.Value }
        let ofInputFieldType (field : IntrospectionInputVal) = { field with Type = field.Type.OfType.Value }
        let rec resolveFieldMetadata (field : IntrospectionField) : RecordPropertyMetadata =
            match field.Type.Kind with
            | TypeKind.NON_NULL when field.Type.Name.IsNone && field.Type.OfType.IsSome -> ofFieldType field |> resolveFieldMetadata |> unwrapOption
            | TypeKind.LIST when field.Type.Name.IsNone && field.Type.OfType.IsSome ->  ofFieldType field |> resolveFieldMetadata |> makeArrayOption
            | TypeKind.SCALAR when field.Type.Name.IsSome ->
                let providedType =
                    // Unknown scalar types will be mapped to a string type.
                    if Types.scalar.ContainsKey(field.Type.Name.Value)
                    then Types.scalar.[field.Type.Name.Value]
                    else typeof<string>
                { Name = field.Name
                  Description = field.Description
                  DeprecationReason = field.DeprecationReason
                  Type = providedType }
                |> makeOption
            | (TypeKind.OBJECT | TypeKind.INTERFACE | TypeKind.INPUT_OBJECT | TypeKind.UNION | TypeKind.ENUM) when field.Type.Name.IsSome ->
                let itype = getSchemaType field.Type
                let providedType = resolveProvidedType itype
                { Name = field.Name
                  Description = field.Description
                  DeprecationReason = field.DeprecationReason
                  Type = providedType }
                |> makeOption
            | _ -> failwith "Could not find a schema type based on a type reference. The reference has an invalid or unsupported combination of Name, Kind and OfType fields."
        and resolveInputFieldMetadata (field : IntrospectionInputVal) : RecordPropertyMetadata =
            match field.Type.Kind with
            | TypeKind.NON_NULL when field.Type.Name.IsNone && field.Type.OfType.IsSome -> ofInputFieldType field |> resolveInputFieldMetadata |> unwrapOption
            | TypeKind.LIST when field.Type.Name.IsNone && field.Type.OfType.IsSome ->  ofInputFieldType field |> resolveInputFieldMetadata |> makeArrayOption
            | TypeKind.SCALAR when field.Type.Name.IsSome ->
                let providedType =
                    if Types.scalar.ContainsKey(field.Type.Name.Value)
                    then Types.scalar.[field.Type.Name.Value]
                    else Types.makeOption typeof<string>
                { Name = field.Name
                  Description = field.Description
                  DeprecationReason = None
                  Type = providedType }
                |> makeOption
            | (TypeKind.OBJECT | TypeKind.INTERFACE | TypeKind.INPUT_OBJECT | TypeKind.UNION | TypeKind.ENUM) when field.Type.Name.IsSome ->
                let itype = getSchemaType field.Type
                let providedType = resolveProvidedType itype
                { Name = field.Name
                  Description = field.Description
                  DeprecationReason = None
                  Type = providedType }
                |> makeOption
            | _ -> failwith "Could not find a schema type based on a type reference. The reference has an invalid or unsupported combination of Name, Kind and OfType fields."
        and resolveProvidedType (itype : IntrospectionType) : ProvidedTypeDefinition =
            if (!providedTypes).ContainsKey(itype.Name)
            then (!providedTypes).[itype.Name]
            else
                let metadata = { Name = itype.Name; Description = itype.Description }
                match itype.Kind with
                | TypeKind.OBJECT ->
                    let tdef = RecordBase.PreBuildProvidedType(metadata, None)
                    providedTypes := (!providedTypes).Add(itype.Name, tdef)
                    let properties = 
                        itype.Fields
                        |> Option.defaultValue [||]
                        |> Array.map resolveFieldMetadata
                        |> List.ofArray
                    RecordBase.MakeProvidedType(tdef, properties)
                | TypeKind.INPUT_OBJECT ->
                    let tdef = RecordBase.PreBuildProvidedType(metadata, None)
                    providedTypes := (!providedTypes).Add(itype.Name, tdef)
                    let properties = 
                        itype.InputFields
                        |> Option.defaultValue [||]
                        |> Array.map resolveInputFieldMetadata
                        |> List.ofArray
                    RecordBase.MakeProvidedType(tdef, properties)
                | TypeKind.INTERFACE | TypeKind.UNION ->
                    let bdef = InterfaceBase.MakeProvidedType(metadata)
                    providedTypes := (!providedTypes).Add(itype.Name, bdef)
                    bdef
                | TypeKind.ENUM ->
                    let items =
                        match itype.EnumValues with
                        | Some values -> values |> Array.map (fun value -> value.Name)
                        | None -> [||]
                    let tdef = EnumBase.MakeProvidedType(itype.Name, items)
                    providedTypes := (!providedTypes).Add(itype.Name, tdef)
                    tdef
                | _ -> failwithf "Type \"%s\" is not a Record, Union, Enum, Input Object, or Interface type." itype.Name
        let ignoredKinds = [TypeKind.SCALAR; TypeKind.LIST; TypeKind.NON_NULL]
        schemaTypes |> Map.iter (fun _ itype -> if not (List.contains itype.Kind ignoredKinds) then resolveProvidedType itype |> ignore)
        let possibleTypes (itype : IntrospectionType) =
            match itype.PossibleTypes with
            | Some trefs -> trefs |> Array.map (getSchemaType >> resolveProvidedType)
            | None -> [||]
        let getProvidedType typeName =
            match (!providedTypes).TryFind(typeName) with
            | Some ptype -> ptype
            | None -> failwithf "Expected to find a type \"%s\" on the schema type map, but it was not found." typeName
        schemaTypes
        |> Seq.map (fun kvp -> kvp.Value)
        |> Seq.filter (fun itype -> itype.Kind = TypeKind.INTERFACE || itype.Kind = TypeKind.UNION)
        |> Seq.map (fun itype -> getProvidedType itype.Name, (possibleTypes itype))
        |> Seq.iter (fun (itype, ptypes) -> ptypes |> Array.iter (fun ptype -> ptype.AddInterfaceImplementation(itype)))
        !providedTypes

    static member internal MakeProvidedType(asm : Assembly, ns : string, resolutionFolder : string) =
        let generator = ProvidedTypeDefinition(asm, ns, "GraphQLProvider", None)
        let prm = 
            [ ProvidedStaticParameter("introspection", typeof<string>)
              ProvidedStaticParameter("httpHeaders", typeof<string>, parameterDefaultValue = "")  
              ProvidedStaticParameter("resolutionFolder", typeof<string>, parameterDefaultValue = resolutionFolder) ]
        generator.DefineStaticParameters(prm, fun tname args ->
            let introspectionLocation = IntrospectionLocation.Create(downcast args.[0], downcast args.[2])
            let httpHeadersLocation = StringLocation.Create(downcast args.[1], resolutionFolder)
            let maker =
                lazy
                    let tdef = ProvidedTypeDefinition(asm, ns, tname, None)
                    tdef.AddXmlDoc("A type provider for GraphQL operations.")
                    tdef.AddMembersDelayed (fun _ ->
                        let httpHeaders = HttpHeaders.load httpHeadersLocation
                        let schemaJson =
                            match introspectionLocation with
                            | Uri serverUrl -> 
                                use connection = new GraphQLConnection()
                                GraphQLClient.sendIntrospectionRequest connection serverUrl httpHeaders
                            | IntrospectionFile path ->
                                System.IO.File.ReadAllText path
                        let schema = Serialization.deserializeSchema schemaJson
                        let schemaProvidedTypes = ProviderBase.GetSchemaProvidedTypes(schema)
                        let typeWrapper = ProvidedTypeDefinition("Types", None, isSealed = true)
                        typeWrapper.AddMembers(schemaProvidedTypes |> Seq.map (fun kvp -> kvp.Value) |> List.ofSeq)
                        let ctxmdef =
                            let prm =
                                let serverUrl =
                                    match introspectionLocation with
                                    | Uri serverUrl -> ProvidedParameter("serverUrl", typeof<string>, optionalValue = serverUrl)
                                    | _ -> ProvidedParameter("serverUrl", typeof<string>)
                                let httpHeaders = ProvidedParameter("httpHeaders", typeof<seq<string * string>>, optionalValue = null)
                                [httpHeaders; serverUrl]
                            let defaultHttpHeadersExpr =
                                let names = httpHeaders |> Seq.map fst |> Array.ofSeq
                                let values = httpHeaders |> Seq.map snd |> Array.ofSeq
                                Expr.Coerce(<@@ Array.zip names values @@>, typeof<seq<string * string>>)
                            let invoker (args : Expr list) =
                                let serverUrl = args.[1]
                                <@@ let httpHeaders =
                                        match %%args.[0] : seq<string * string> with
                                        | null -> %%defaultHttpHeadersExpr
                                        | argHeaders -> argHeaders
                                    { ServerUrl = %%serverUrl; HttpHeaders = httpHeaders } @@>
                            ProvidedMethod("GetContext", prm, typeof<GraphQLProviderRuntimeContext>, invoker, isStatic = true)
                        let omdef =
                            let sprm = 
                                [ ProvidedStaticParameter("query", typeof<string>)
                                  ProvidedStaticParameter("resolutionFolder", typeof<string>, parameterDefaultValue = resolutionFolder)
                                  ProvidedStaticParameter("operationName", typeof<string>, parameterDefaultValue = "") ]
                            let smdef = ProvidedMethod("Operation", [], typeof<OperationBase>, isStatic = true)
                            let genfn (mname : string) (args : obj []) =
                                let queryLocation = StringLocation.Create(downcast args.[0], downcast args.[1])
                                let query = 
                                    match queryLocation with
                                    | String query -> query
                                    | File path -> System.IO.File.ReadAllText(path)
                                let queryAst = Parser.parse query
                                let operationName : OperationName option = 
                                    match args.[2] :?> string with
                                    | "" -> 
                                        match queryAst.Definitions with
                                        | [] -> failwith "Error parsing query. Can not choose a default operation: query document has no definitions."
                                        | _ -> queryAst.Definitions.Head.Name
                                    | x -> Some x
                                let operationDefinition =
                                    queryAst.Definitions
                                    |> List.choose (function OperationDefinition odef -> Some odef | _ -> None)
                                    |> List.find (fun d -> d.Name = operationName)
                                let operationAstFields =
                                    let infoMap = queryAst.GetInfoMap()
                                    match infoMap.TryFind(operationName) with
                                    | Some fields -> fields
                                    | None -> failwith "Error parsing query. Could not find field information for requested operation."
                                let operationTypeRef =
                                    let tref =
                                        match operationDefinition.OperationType with
                                        | Query -> schema.QueryType
                                        | Mutation -> 
                                            match schema.MutationType with
                                            | Some tref -> tref
                                            | None -> failwith "The operation is a mutation operation, but the schema does not have a mutation type."
                                        | Subscription -> 
                                            match schema.SubscriptionType with
                                            | Some tref -> tref
                                            | None -> failwithf "The operation is a subscription operation, but the schema does not have a subscription type."
                                    let tinst =
                                        match tref.Name with
                                        | Some name -> schema.Types |> Array.tryFind (fun t -> t.Name = name)
                                        | None -> None
                                    match tinst with
                                    | Some t -> { tref with Kind = t.Kind }
                                    | None -> failwith "The operation was found in the schema, but it does not have a name."
                                let schemaTypes = Types.getSchemaTypes(schema)
                                let enumProvidedTypes = schemaProvidedTypes |> Map.filter (fun _ t -> t.BaseType = typeof<EnumBase>)
                                let (operationType, operationTypes) = ProviderBase.GetOperationProvidedTypes(schemaTypes, enumProvidedTypes, operationAstFields, operationTypeRef)
                                let generateWrapper name = ProvidedTypeDefinition(name, None, isSealed = true)
                                let wrappersByPath = Dictionary<string list, ProvidedTypeDefinition>()
                                let rootWrapper = generateWrapper "Types"
                                wrappersByPath.Add([], rootWrapper)
                                let rec getWrapper (path : string list) =
                                    if wrappersByPath.ContainsKey path
                                    then wrappersByPath.[path]
                                    else
                                        let wrapper = generateWrapper (path.Head.FirstCharUpper())
                                        let upperWrapper =
                                            let path = path.Tail
                                            if wrappersByPath.ContainsKey(path)
                                            then wrappersByPath.[path]
                                            else getWrapper path
                                        upperWrapper.AddMember(wrapper)
                                        wrappersByPath.Add(path, wrapper)
                                        wrapper
                                let includeType (path : string list) (t : ProvidedTypeDefinition) =
                                    let wrapper = getWrapper path
                                    wrapper.AddMember(t)
                                operationTypes |> Map.iter (fun (path, _) t -> includeType path t)
                                let operationTypeName : TypeName =
                                    match operationTypeRef.Name with
                                    | Some name -> name
                                    | None -> failwith "Error parsing query. Operation type does not have a name."
                                // Every time we run the query, we will need the schema type map as an expression.
                                // To avoid creating the type map expression every time we call Run method, we cache it here.
                                let schemaTypes =
                                    let schemaTypeNames = schemaTypes |> Seq.map (fun x -> x.Key) |> Array.ofSeq
                                    let schemaTypes = schemaTypes |> Seq.map (fun x -> x.Value) |> Array.ofSeq |> QuotationHelpers.arrayExpr |> snd
                                    <@@ Array.zip schemaTypeNames (%%schemaTypes : IntrospectionType []) |> Map.ofArray @@>
                                let contextInfo : GraphQLContextInfo option =
                                    match introspectionLocation with
                                    | Uri serverUrl -> Some { ServerUrl = serverUrl; HttpHeaders = httpHeaders }
                                    | _ -> None
                                let odef = OperationBase.MakeProvidedType(query, operationDefinition, operationTypeName, schemaTypes, schemaProvidedTypes, operationType, contextInfo)
                                odef.AddMember(rootWrapper)
                                let invoker (_ : Expr list) = <@@ new OperationBase(query) @@>
                                let mdef = ProvidedMethod(mname, [], odef, invoker, isStatic = true)
                                mdef.AddXmlDoc("Creates an operation to be executed on the server and provide its return types.")
                                let members : MemberInfo list = [odef; mdef]
                                tdef.AddMembers(members)
                                mdef
                            smdef.DefineStaticParameters(sprm, genfn)
                            smdef
                        let schemapdef = 
                            let getterCode = QuotationHelpers.quoteRecord schema (fun (_ : Expr list) schema -> schema)
                            ProvidedProperty("Schema", typeof<IntrospectionSchema>, getterCode, isStatic = true)
                        let members : MemberInfo list = [typeWrapper; ctxmdef; omdef; schemapdef]
                        members)
                    tdef
            let providerKey = { IntrospectionLocation = introspectionLocation; CustomHttpHeadersLocation = httpHeadersLocation }
            DesignTimeCache.getOrAdd providerKey maker.Force)
        generator