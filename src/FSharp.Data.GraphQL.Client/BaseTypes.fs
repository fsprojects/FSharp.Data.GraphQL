namespace FSharp.Data.GraphQL

open System
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

type TypeName = string

type OperationError =
    { Message : string
      Path : obj [] }

module QuotationHelpers = 
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

    let quoteUnion instance = unionExpr instance ||> createLetExpr
    let quoteRecord instance = recordExpr instance ||> createLetExpr
    let quoteArray instance = arrayExpr instance ||> createLetExpr

type EnumBase (name : string, value : string) =
    member __.Name = name

    member __.Value = value

    static member internal MakeProvidedType(name, items : string seq) =
        let tdef = ProvidedTypeDefinition(name, Some typeof<EnumBase>, nonNullable = true, isSealed = true)
        for item in items do
            let getterCode (_ : Expr list) =
                Expr.NewObject(EnumBase.Constructor, [ <@@ name @@>; <@@ item @@> ])
            let idef = ProvidedProperty(item, tdef, getterCode, isStatic = true)
            tdef.AddMember(idef)
        tdef

    static member internal Constructor = typeof<EnumBase>.GetConstructors().[0]

    override x.ToString() = x.Value

    member x.Equals(other : EnumBase) =
        x.Name = other.Name && x.Value = other.Value

    override x.Equals(other : obj) =
        match other with
        | :? EnumBase as other -> x.Equals(other)
        | _ -> false

    override x.GetHashCode() = x.Name.GetHashCode() ^^^ x.Value.GetHashCode()

    interface IEquatable<EnumBase> with
        member x.Equals(other) = x.Equals(other)

type ProvidedTypeMetadata =
    { Name : string
      Description : string option }

type InterfaceBase private () =
    static member internal MakeProvidedType(metadata : ProvidedTypeMetadata) =
        let tdef = ProvidedTypeDefinition("I" + metadata.Name.FirstCharUpper(), None, nonNullable = true, isInterface = true)
        metadata.Description |> Option.iter tdef.AddXmlDoc
        tdef

type RecordPropertyMetadata =
    { Name : string
      Description : string option
      DeprecationReason : string option
      Type : Type }

type RecordProperty =
    { Name : string
      Value : obj }

type RecordBase (name : string, properties : RecordProperty seq) =
    member private __.Name = name

    member private __.Properties = List.ofSeq properties

    static member internal MakeProvidedType(metadata : ProvidedTypeMetadata, properties : RecordPropertyMetadata list, baseType : Type option) =
        let baseType = Option.defaultValue typeof<RecordBase> baseType
        let name = metadata.Name.FirstCharUpper()
        let tdef = ProvidedTypeDefinition(name, Some baseType, nonNullable = true, isSealed = true)
        metadata.Description |> Option.iter tdef.AddXmlDoc
        let propertyMapper (metadata : RecordPropertyMetadata) : MemberInfo =
            let pname = metadata.Name.FirstCharUpper()
            let getterCode (args : Expr list) =
                <@@ let this = %%args.[0] : RecordBase
                    let propdef = typeof<RecordBase>.GetProperty("Properties", BindingFlags.NonPublic ||| BindingFlags.Instance)
                    let props = propdef.GetValue(this) :?> RecordProperty list
                    match props |> List.tryFind (fun prop -> prop.Name = pname) with
                    | Some prop -> prop.Value
                    | None -> failwithf "Expected to find property \"%s\", but the property was not found." pname @@>
            let pdef = ProvidedProperty(pname, metadata.Type, getterCode)
            metadata.Description |> Option.iter pdef.AddXmlDoc
            metadata.DeprecationReason |> Option.iter pdef.AddObsoleteAttribute
            upcast pdef
        let pdefs = properties |> List.map propertyMapper
        tdef.AddMembers(pdefs)
        match baseType with
        | :? ProvidedTypeDefinition as bdef ->
            let asType = 
                let invoker (args : Expr list) =
                    <@@ let this = %%args.[0] : RecordBase
                        let propdef = typeof<RecordBase>.GetProperty("Name", BindingFlags.NonPublic ||| BindingFlags.Instance)
                        let baseName = propdef.GetValue(this) :?> string
                        if baseName = name then this
                        else failwithf "Expected type to be \"%s\", but it is \"%s\". Make sure to check the type by calling \"Is%s\" method before calling \"As%s\" method." name baseName name name @@>
                ProvidedMethod("As" + name, [], tdef, invoker)
            let tryAsType =
                let invoker (args : Expr list) =
                    <@@ let this = %%args.[0] : RecordBase
                        let propdef = typeof<RecordBase>.GetProperty("Name", BindingFlags.NonPublic ||| BindingFlags.Instance)
                        let baseName = propdef.GetValue(this) :?> string
                        if baseName = name then Some this
                        else None @@>
                ProvidedMethod("TryAs" + name, [], typedefof<_ option>.MakeGenericType(tdef), invoker)
            let isType =
                let invoker (args : Expr list) =
                    <@@ let this = %%args.[0] : RecordBase
                        let propdef = typeof<RecordBase>.GetProperty("Name", BindingFlags.NonPublic ||| BindingFlags.Instance)
                        let baseName = propdef.GetValue(this) :?> string
                        baseName = name @@>
                ProvidedMethod("Is" + name, [], typeof<bool>, invoker)
            let members : MemberInfo list = [asType; tryAsType; isType]
            bdef.AddMembers(members)
        | _ ->
            let ctdef =
                let prm = properties |> List.map (fun prop -> ProvidedParameter(prop.Name, prop.Type))
                let invoker (args : Expr list) = 
                    let properties =
                        let args = 
                            let names = properties |> List.map (fun prop -> prop.Name.FirstCharUpper())
                            let mapper (name : string, value : Expr) =
                                let value = Expr.Coerce(value, typeof<obj>)
                                <@@ { Name = name; Value = %%value } @@>
                            List.zip names args |> List.map mapper
                        Expr.NewArray(typeof<RecordProperty>, args)
                    Expr.NewObject(RecordBase.Constructor, [Expr.Value(name); properties])
                ProvidedConstructor(prm, invoker)
            tdef.AddMember(ctdef)
        tdef

    static member internal Constructor = typeof<RecordBase>.GetConstructors().[0]

    static member internal NewObjectExpr(properties : (string * obj) list) =
        let names = properties |> List.map fst
        let values = properties |> List.map snd
        Expr.NewObject(RecordBase.Constructor, [ <@@ List.zip names values @@> ])

    override x.ToString() =
        let sb = StringBuilder()
        sb.Append("{") |> ignore
        let rec printProperties (properties : RecordProperty list) =
            match properties with
            | [] -> ()
            | [prop] -> sb.Append(sprintf "%s = %A;" prop.Name prop.Value) |> ignore
            | prop :: tail -> sb.AppendLine(sprintf "%s = %A;" prop.Name prop.Value) |> ignore; printProperties tail
        printProperties x.Properties
        sb.Append("}") |> ignore
        sb.ToString()

    member x.Equals(other : RecordBase) = x.Name = other.Name && x.Properties = other.Properties

    override x.Equals(other : obj) =
        match other with
        | :? RecordBase as other -> x.Equals(other)
        | _ -> false

    override x.GetHashCode() = x.Name.GetHashCode() ^^^ x.Properties.GetHashCode()

    interface IEquatable<RecordBase> with
        member x.Equals(other) = x.Equals(other)

module Types =
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

module JsonValueHelper =
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

    let private removeTypeNameField (fields : (string * JsonValue) list) =
        fields |> List.filter (fun (name, _) -> name <> "__typename")

    let firstUpper (name : string, value) =
        name.FirstCharUpper(), value

    let getFields (schemaType : IntrospectionType) =
        match schemaType.Fields with
        | None -> Map.empty
        | Some fields -> fields |> Array.map (fun field -> field.Name.FirstCharUpper(), field.Type) |> Map.ofSeq

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
                    |> List.ofArray
                    |> removeTypeNameField
                    |> List.map (firstUpper >> mapper)
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

    let getFieldValues (schemaTypes : Map<string, IntrospectionType>) (schemaType : IntrospectionType) (fields : (string * JsonValue) list) =
        let mapper (name : string, value : JsonValue) =
            let fields = getFields schemaType
            match fields.TryFind(name) with
            | Some fieldType -> getFieldValue schemaTypes fieldType (name, value)
            | None -> failwithf "Expected to find a field named \"%s\" on the type %s, but found none." name schemaType.Name
        removeTypeNameField fields
        |> List.map (firstUpper >> mapper)

    let getErrors (errors : JsonValue list) =
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
        List.map errorMapper errors

type OperationResultProvidingInformation =
    { SchemaTypeNames : string []
      SchemaTypes : IntrospectionType []
      OperationTypeName : string }

type OperationResultBase (responseJson : string) =
    member private __.ResponseJson = JsonValue.Parse responseJson

    member private x._DataFields = JsonValueHelper.getResponseDataFields x.ResponseJson |> Option.map List.ofArray

    member private x._Errors = JsonValueHelper.getResponseErrors x.ResponseJson |> Option.map List.ofArray
    
    member private x._CustomFields = JsonValueHelper.getResponseCustomFields x.ResponseJson |> List.ofArray

    static member internal MakeProvidedType(providingInformation : OperationResultProvidingInformation, operationType : Type) =
        let tdef = ProvidedTypeDefinition("OperationResult", Some typeof<OperationResultBase>, nonNullable = true)
        let ddef = 
            let getterCode =
                QuotationHelpers.quoteRecord providingInformation (fun (args : Expr list) var ->
                    <@@ let this = %%args.[0] : OperationResultBase
                        let info = %%var : OperationResultProvidingInformation
                        let schemaTypes = Map.ofArray (Array.zip info.SchemaTypeNames info.SchemaTypes)
                        let operationType =
                            match schemaTypes.TryFind(info.OperationTypeName) with
                            | Some def -> def
                            | _ -> failwithf "Operation type %s could not be found on the schema types." info.OperationTypeName
                        let dfdef = typeof<OperationResultBase>.GetProperty("_DataFields", BindingFlags.NonPublic ||| BindingFlags.Instance)
                        let dataFields = dfdef.GetValue(this) :?> (string * JsonValue) list option
                        match dataFields with
                        | Some [] | None -> None
                        | Some dataFields ->
                            let fieldValues = JsonValueHelper.getFieldValues schemaTypes operationType dataFields
                            let props = fieldValues |> List.map (fun (name, value) -> { Name = name; Value = value })
                            Some (RecordBase(operationType.Name, props)) @@>)
            let prop = ProvidedProperty("Data", operationType, getterCode)
            prop.AddXmlDoc("Contains the data returned by the operation on the server.")
            prop
        let edef =
            let getterCode (args : Expr list) =
                <@@ let this = %%args.[0] : OperationResultBase
                    let efdef = typeof<OperationResultBase>.GetProperty("_Errors", BindingFlags.NonPublic ||| BindingFlags.Instance)
                    let errors = efdef.GetValue(this) :?> JsonValue list option
                    match errors with
                    | Some [] | None -> None
                    | Some errors -> Some (JsonValueHelper.getErrors errors) @@>
            let prop = ProvidedProperty("Errors", typeof<OperationError list option>, getterCode)
            prop.AddXmlDoc("Contains erros returned by the operation on the server.")
            prop
        let members : MemberInfo list = [ddef; edef]
        tdef.AddMembers(members)
        tdef

    member x.Equals(other : OperationResultBase) =
        x.ResponseJson = other.ResponseJson

    override x.Equals(other : obj) =
        match other with
        | :? OperationResultBase as other -> x.Equals(other)
        | _ -> false

    override x.GetHashCode() = x.ResponseJson.GetHashCode()

type OperationBase (serverUrl : string) =
    member __.ServerUrl = serverUrl

    static member internal MakeProvidedType(userQuery,
                                            operationDefinition : OperationDefinition,
                                            operationTypeName : string, 
                                            schemaTypes : Map<string, IntrospectionType>,
                                            schemaProvidedTypes : Map<string, ProvidedTypeDefinition>,
                                            operationType : Type) =
        let query = 
            let ast = Parser.parse userQuery
            ast.ToQueryString(QueryStringPrintingOptions.IncludeTypeNames)
        let className = "Operation" + query.GetHashCode().ToString("x2")
        let tdef = ProvidedTypeDefinition(className, Some typeof<OperationBase>)
        tdef.AddXmlDoc("Represents a GraphQL operation on the server.")
        let schemaTypes = schemaTypes |> Seq.map (|KeyValue|)
        let info = 
            { SchemaTypeNames = schemaTypes |> Seq.map (fst >> (fun name -> name.FirstCharUpper())) |> Array.ofSeq
              SchemaTypes = schemaTypes |> Seq.map snd |> Array.ofSeq
              OperationTypeName = operationTypeName }
        let rtdef = OperationResultBase.MakeProvidedType(info, operationType)
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
        let rundef = 
            let invoker (args : Expr list) =
                let operationName = Option.toObj operationDefinition.Name
                let variables = 
                    let args =
                        let names = variables |> List.map fst
                        let args = 
                            match args with
                            | _ :: tail when tail.Length > 0 -> List.take (tail.Length - 1) tail
                            | _ -> []
                        let mapper (name : string, value : Expr) =
                            let value = Expr.Coerce(value, typeof<obj>)
                            <@@ (name, %%value) @@>
                        List.zip names args |> List.map mapper
                    Expr.NewArray(typeof<string * obj>, args)
                let customHttpHeaders =
                    match args with
                    | _ :: tail when tail.Length > 0 -> tail.[tail.Length - 1]
                    | _ -> <@@ null @@>
                <@@ let this = %%args.[0] : OperationBase
                    let customHttpHeaders = 
                        let headers = %%customHttpHeaders : seq<string * string>
                        match headers with
                        | null -> [||]
                        | _ -> Array.ofSeq headers
                    let request =
                        { ServerUrl = this.ServerUrl
                          CustomHeaders = customHttpHeaders
                          OperationName = Option.ofObj operationName
                          Query = query
                          Variables = %%variables }
                    let responseJson = GraphQLClient.sendRequest request
                    OperationResultBase(responseJson) @@>
            let varprm = variables |> List.map (fun (name, t) -> ProvidedParameter(name, t))
            let prm = varprm @ [ProvidedParameter("customHttpHeaders", typeof<seq<string * string>>, optionalValue = None)]
            let mdef = ProvidedMethod("Run", prm, rtdef, invoker)
            mdef.AddXmlDoc("Executes the operation on the server and fetch its results.")
            mdef
        let arundef = 
            let invoker (args : Expr list) =
                let operationName = Option.toObj operationDefinition.Name
                let variables = 
                    let args =
                        let names = variables |> List.map fst
                        let args = 
                            match args with
                            | _ :: tail when tail.Length > 0 -> List.take (tail.Length - 1) tail
                            | _ -> []
                        let mapper (name : string, value : Expr) =
                            let value = Expr.Coerce(value, typeof<obj>)
                            <@@ (name, %%value) @@>
                        List.zip names args |> List.map mapper
                    Expr.NewArray(typeof<string * obj>, args)
                let customHttpHeaders =
                    match args with
                    | _ :: tail when tail.Length > 0 -> tail.[tail.Length - 1]
                    | _ -> <@@ null @@>
                <@@ let this = %%args.[0] : OperationBase
                    let customHttpHeaders = 
                        let headers = %%customHttpHeaders : seq<string * string>
                        match headers with
                        | null -> [||]
                        | _ -> Array.ofSeq headers
                    let request =
                        { ServerUrl = this.ServerUrl
                          CustomHeaders = customHttpHeaders
                          OperationName = Option.ofObj operationName
                          Query = query
                          Variables = %%variables }
                    async {
                        let! responseJson = GraphQLClient.sendRequestAsync request
                        return OperationResultBase(responseJson)
                    } @@>
            let varprm = variables |> List.map (fun (name, t) -> ProvidedParameter(name, t))
            let prm = varprm @ [ProvidedParameter("customHttpHeaders", typeof<seq<string * string>>, optionalValue = None)]
            let mdef = ProvidedMethod("AsyncRun", prm, Types.makeAsync rtdef, invoker)
            mdef.AddXmlDoc("Executes the operation asynchronously on the server and fetch its results.")
            mdef
        let members : MemberInfo list = [rtdef; rundef; arundef]
        tdef.AddMembers(members)
        tdef

type ContextBase (serverUrl : string, schema : IntrospectionSchema) =
    member __.ServerUrl = serverUrl

    member __.Schema = schema

    static member private GetOperationProvidedTypes(schema, enumProvidedTypes : Map<TypeName, ProvidedTypeDefinition>, operationAstFields, operationTypeRef) =
        let providedTypes = Dictionary<Path * TypeName, ProvidedTypeDefinition>()
        let schemaTypes = Types.getSchemaTypes(schema)
        let rec getProvidedType (providedTypes : Dictionary<Path * TypeName, ProvidedTypeDefinition>) (schemaTypes : Map<TypeName, IntrospectionType>) (path : Path) (astFields : AstFieldInfo list) (tref : IntrospectionTypeRef) : Type =
            match tref.Kind with
            | TypeKind.NON_NULL when tref.Name.IsNone && tref.OfType.IsSome -> getProvidedType providedTypes schemaTypes path astFields tref.OfType.Value |> Types.unwrapOption
            | TypeKind.LIST when tref.Name.IsNone && tref.OfType.IsSome -> getProvidedType providedTypes schemaTypes path astFields tref.OfType.Value |> Types.makeArray |> Types.makeOption
            | TypeKind.SCALAR when tref.Name.IsSome ->
                if Types.scalar.ContainsKey(tref.Name.Value)
                then Types.scalar.[tref.Name.Value] |> Types.makeOption
                else failwithf "Could not find a schema type based on a type reference. The reference is a scalar type \"%s\", but that type is not supported by the client provider." tref.Name.Value
            | TypeKind.ENUM when tref.Name.IsSome ->
                match enumProvidedTypes.TryFind(tref.Name.Value) with
                | Some providedEnum -> Types.makeOption providedEnum
                | None -> failwithf "Could not find a enum type based on a type reference. The reference is an \"%s\" enum, but that enum was not found in the introspection schema." tref.Name.Value
            | (TypeKind.OBJECT | TypeKind.INTERFACE | TypeKind.UNION) when tref.Name.IsSome ->
                if providedTypes.ContainsKey(path, tref.Name.Value)
                then upcast providedTypes.[path, tref.Name.Value]
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
                        let properties =
                            astFields
                            |> List.filter (function | TypeField _ -> true | _ -> false)
                            |> List.map (getPropertyMetadata tref.Name.Value)
                        let metadata : ProvidedTypeMetadata = { Name = tref.Name.Value; Description = tref.Description }
                        RecordBase.MakeProvidedType(metadata, properties, None)
                    providedTypes.Add((path, baseType.Name), baseType)
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
                            RecordBase.MakeProvidedType(metadata, properties, Some (upcast baseType))
                        fragmentProperties
                        |> List.map createFragmentType
                    fragmentTypes |> List.iter (fun fragmentType -> providedTypes.Add((path, fragmentType.Name), fragmentType))
                    Types.makeOption baseType
            | _ -> failwith "Could not find a schema type based on a type reference. The reference has an invalid or unsupported combination of Name, Kind and OfType fields."
        (getProvidedType providedTypes schemaTypes [] operationAstFields operationTypeRef), (providedTypes |> Seq.map (|KeyValue|) |> Map.ofSeq)

    static member internal MakeProvidedType(schema : IntrospectionSchema, schemaProvidedTypes : Map<TypeName, ProvidedTypeDefinition>) =
        let tdef = ProvidedTypeDefinition("Context", Some typeof<ContextBase>)
        tdef.AddXmlDoc("Represents a connection to a GraphQL server. A context groups all operations and their types from a specific server connection.")
        let mdef =
            let sprm = 
                [ ProvidedStaticParameter("queryString", typeof<string>)
                  ProvidedStaticParameter("operationName", typeof<string>, "") ]
            let smdef = ProvidedMethod("Operation", [], typeof<OperationBase>)
            let genfn (mname : string) (args : obj []) =
                let query = args.[0] :?> string
                let queryAst = Parser.parse query
                let operationName : OperationName option = 
                    match args.[1] :?> string with
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
                let schemaTypes = Types.getSchemaTypes(schema)
                let enumProvidedTypes = schemaProvidedTypes |> Map.filter (fun _ t -> t.BaseType = typeof<EnumBase>)
                let (operationType, operationTypes) = ContextBase.GetOperationProvidedTypes(schema, enumProvidedTypes, operationAstFields, operationTypeRef)
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
                operationTypes
                |> Seq.map ((|KeyValue|) >> (fun ((path, _), t) -> path, t))
                |> Seq.iter (fun (path, t) -> includeType path t)
                let operationTypeName : TypeName =
                    match operationTypeRef.Name with
                    | Some name -> name
                    | None -> failwith "Error parsing query. Operation type does not have a name."
                let odef = OperationBase.MakeProvidedType(query, operationDefinition, operationTypeName, schemaTypes, schemaProvidedTypes, operationType)
                odef.AddMember(rootWrapper)
                let invoker (args : Expr list) =
                    <@@ let this = %%args.[0] : ContextBase
                        OperationBase(this.ServerUrl) @@>
                let mdef = ProvidedMethod(mname, [], odef, invoker)
                mdef.AddXmlDoc("Creates an operation to be executed on the server and provide its return types.")
                let members : MemberInfo list = [odef; mdef]
                tdef.AddMembers(members)
                mdef
            smdef.DefineStaticParameters(sprm, genfn)
            smdef
        tdef.AddMember(mdef)
        tdef

    static member internal Constructor = typeof<ContextBase>.GetConstructors().[0]

type ProviderBase private () =
    static member private GetSchemaProvidedTypes(schema : IntrospectionSchema) =
        let providedTypes = Dictionary<TypeName, ProvidedTypeDefinition>()
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
        let ofType (field : IntrospectionField) = { field with Type = field.Type.OfType.Value }
        let rec getPropertyMetadata (field : IntrospectionField) : RecordPropertyMetadata =
            match field.Type.Kind with
            | TypeKind.NON_NULL when field.Type.Name.IsNone && field.Type.OfType.IsSome -> ofType field |> getPropertyMetadata |> unwrapOption
            | TypeKind.LIST when field.Type.Name.IsNone && field.Type.OfType.IsSome ->  ofType field |> getPropertyMetadata |> makeArrayOption
            | TypeKind.SCALAR when field.Type.Name.IsSome ->
                let providedType =
                    if Types.scalar.ContainsKey(field.Type.Name.Value)
                    then Types.scalar.[field.Type.Name.Value]
                    else failwithf "Could not find a schema type based on a type reference. The reference is a scalar type \"%s\", but that type is not supported by the client provider." field.Type.Name.Value
                { Name = field.Name
                  Description = field.Description
                  DeprecationReason = field.DeprecationReason
                  Type = providedType }
                |> makeOption
            | (TypeKind.OBJECT | TypeKind.INTERFACE | TypeKind.UNION | TypeKind.ENUM) when field.Type.Name.IsSome ->
                let itype = getSchemaType field.Type
                let providedType = getProvidedType itype
                { Name = field.Name
                  Description = field.Description
                  DeprecationReason = field.DeprecationReason
                  Type = providedType }
                |> makeOption
            | _ -> failwith "Could not find a schema type based on a type reference. The reference has an invalid or unsupported combination of Name, Kind and OfType fields."
        and getProvidedType (itype : IntrospectionType) : ProvidedTypeDefinition =
            if providedTypes.ContainsKey(itype.Name)
            then providedTypes.[itype.Name]
            else
                let metadata = { Name = itype.Name; Description = itype.Description }
                match itype.Kind with
                | TypeKind.OBJECT ->
                    let properties = 
                        itype.Fields
                        |> Option.defaultValue [||]
                        |> Array.map getPropertyMetadata
                        |> List.ofArray
                    let tdef = RecordBase.MakeProvidedType(metadata, properties, None)
                    providedTypes.Add(itype.Name, tdef)
                    tdef
                | TypeKind.INTERFACE | TypeKind.UNION ->
                    let bdef = InterfaceBase.MakeProvidedType(metadata)
                    providedTypes.Add(itype.Name, bdef)
                    bdef
                | TypeKind.ENUM ->
                    let items =
                        match itype.EnumValues with
                        | Some values -> values |> Array.map (fun value -> value.Name)
                        | None -> [||]
                    let tdef = EnumBase.MakeProvidedType(itype.Name, items)
                    providedTypes.Add(itype.Name, tdef)
                    tdef
                | _ -> failwithf "Type \"%s\" is not a Record, Union, Enum or Interface type." itype.Name
        schemaTypes |> Map.iter (fun _ itype -> getProvidedType itype |> ignore)
        let possibleTypes (itype : IntrospectionType) =
            match itype.PossibleTypes with
            | Some trefs -> trefs |> Array.map (getSchemaType >> getProvidedType)
            | None -> [||]
        let getProvidedType typeName =
            match providedTypes.TryGetValue(typeName) with
            | (true, ptype) -> ptype
            | _ -> failwithf "Expected to find a type \"%s\" on the schema type map, but it was not found." typeName
        schemaTypes
        |> Seq.map (fun kvp -> kvp.Value)
        |> Seq.filter (fun itype -> itype.Kind = TypeKind.INTERFACE || itype.Kind = TypeKind.UNION)
        |> Seq.map (fun itype -> getProvidedType itype.Name, (possibleTypes itype))
        |> Seq.iter (fun (itype, ptypes) -> ptypes |> Array.iter (fun ptype -> ptype.AddInterfaceImplementation(itype)))
        providedTypes |> Seq.map(|KeyValue|) |> Map.ofSeq

    static member internal MakeProvidedType(asm : Assembly, ns : string) =
        let generator = ProvidedTypeDefinition(asm, ns, "GraphQLProvider", None)
        let prm = [ProvidedStaticParameter("serverUrl", typeof<string>)]
        generator.DefineStaticParameters(prm, fun tname args ->
            let serverUrl = args.[0] :?> string
            let introspectionJson = GraphQLClient.sendIntrospectionRequest serverUrl
            let tdef = ProvidedTypeDefinition(asm, ns, tname, None)
            tdef.AddXmlDoc("A type provider for GraphQL operations.")
            let schema = Serialization.deserializeSchema introspectionJson
            let schemaProvidedTypes = ProviderBase.GetSchemaProvidedTypes(schema)
            let typeWrapper = ProvidedTypeDefinition("Types", None, isSealed = true)
            schemaProvidedTypes
            |> Seq.map (fun kvp -> kvp.Value)
            |> Seq.iter typeWrapper.AddMember
            tdef.AddMember(typeWrapper)
            let ctxdef = ContextBase.MakeProvidedType(schema, schemaProvidedTypes)
            let schemaExpr = <@@ Serialization.deserializeSchema introspectionJson @@>
            let ctxmdef =
                let prm = [ProvidedParameter("serverUrl", typeof<string>, optionalValue = serverUrl)]
                let invoker (args : Expr list) =
                    let serverUrl = args.[0]
                    Expr.NewObject(ContextBase.Constructor, [serverUrl; schemaExpr])
                let mdef = ProvidedMethod("GetContext", prm, ctxdef, invoker, true)
                mdef.AddXmlDoc("Builds a connection to a GraphQL server. If no server URL is passed as an argument, the context will connect to the provider design-time URL.")
                mdef
            let schemapdef = 
                let getterCode (_ : Expr list) = schemaExpr
                ProvidedProperty("Schema", typeof<IntrospectionSchema>, getterCode, isStatic = true)
            let members : MemberInfo list = [ctxdef; ctxmdef; schemapdef]
            tdef.AddMembers(members)
            tdef)
        generator