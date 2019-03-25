namespace FSharp.Data.GraphQL

open System
open FSharp.Core
open FSharp.Data
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Client
open System.Collections.Generic
open FSharp.Data.GraphQL.Types.Introspection
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Ast.Extensions
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Quotations
open System.Reflection
open System.Text
open Microsoft.FSharp.Reflection
open System.Collections
open System.Globalization

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

type InterfaceBase private () =
    static member internal MakeProvidedType(name : string) =
        ProvidedTypeDefinition("I" + name, None, nonNullable = true, isInterface = true)

type RecordBase (name : string, properties : (string * obj) seq) =
    member private __.Name = name

    member private __.Properties = List.ofSeq properties

    static member internal MakeProvidedType(name : string, properties : (string * Type) list, baseType : Type option) =
        let baseType = Option.defaultValue typeof<RecordBase> baseType
        let name = name.FirstCharUpper()
        let tdef = ProvidedTypeDefinition(name, Some baseType, nonNullable = true, isSealed = true)
        let propertyMapper (pname : string, ptype : Type) : MemberInfo =
            let pname = pname.FirstCharUpper()
            let getterCode (args : Expr list) =
                <@@ let this = %%args.[0] : RecordBase
                    let propdef = typeof<RecordBase>.GetProperty("Properties", BindingFlags.NonPublic ||| BindingFlags.Instance)
                    let props = propdef.GetValue(this) :?> (string * obj) list
                    match props |> List.tryFind (fun (name, _) -> name = pname) with
                    | Some (_, value) -> value
                    | None -> failwithf "Expected to find property \"%s\" under properties %A, but the property was not found." pname (List.map snd props) @@>
            upcast ProvidedProperty(pname, ptype, getterCode)
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
                let prm = properties |> List.map (fun (name, t) -> ProvidedParameter(name, t))
                let invoker (args : Expr list) = 
                    let fields =
                        let args = 
                            let names = properties |> List.map (fst >> (fun name -> name.FirstCharUpper()))
                            List.zip names args |> List.map (fun (name, arg) -> Expr.NewTuple([Expr.Value(name); Expr.Coerce(arg, typeof<obj>)]))
                        Expr.NewArray(typeof<string * obj>, args)
                    Expr.NewObject(RecordBase.Constructor, [Expr.Value(name); fields])
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
        let rec printProperties (properties : (string * obj) list) =
            match properties with
            | [] -> ()
            | [name, value] -> sb.Append(sprintf "%s = %A;" name value) |> ignore
            | (name, value) :: tail -> sb.AppendLine(sprintf "%s = %A;" name value) |> ignore; printProperties tail
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

    let schema =
        [| "__TypeKind"
           "__DirectiveLocation"
           "__Type"
           "__InputValue"
           "__Field"
           "__EnumValue"
           "__Directive"
           "__Schema" |]

    let getSchemaTypes (introspection : IntrospectionSchema) =
        let isScalarType (name : string) =
            scalar |> Map.containsKey name
        let isIntrospectionType (name : string) =
            schema |> Array.contains name
        introspection.Types
        |> Array.filter (fun t -> not (isIntrospectionType t.Name) && not (isScalarType t.Name))
        |> Array.map (fun t -> t.Name, t)
        |> Map.ofArray

    let getScalarType (typeName : string) =
        match scalar |> Map.tryFind typeName with
        | Some t -> t
        | None -> failwithf "Scalar type %s is not supported." typeName

    let makeOption (name : string, t : Type) = name, typedefof<_ option>.MakeGenericType(t)
    let makeArray (name : string, t : Type) = name, t.MakeArrayType()
    let unwrapOption (name: string, t : Type) = 
        if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<_ option>
        then name, t.GetGenericArguments().[0]
        else failwithf "Expected native type of property \"%s\" to be an option type, but it is %s." name t.Name
    let unwrapOptionArray (name : string, t : Type) =
        if t.IsArray
        then (name, t.GetElementType()) |> unwrapOption |> makeArray
        else failwithf "Expected type of property \"%s\" to be an array, but it is %s" name t.Name

module JsonValueHelper =
    let getResponseFields (responseJson : JsonValue) =
        match responseJson with
        | JsonValue.Record fields -> fields
        | _ -> failwithf "Expected root type to be a Record type, but type is %A." responseJson

    let getResponseDataFields (responseJson : JsonValue) =
        match getResponseFields responseJson |> Array.tryFind (fun (name, _) -> name = "data") with
        | Some (_, data) -> 
            match data with
            | JsonValue.Record fields -> fields
            | _ -> failwithf "Expected data field of root type to be a Record type, but type is %A." data
        | None -> failwith "Expected root type to have a \"data\" field, but it was not found."

    let getResponseCustomFields (responseJson : JsonValue) =
        getResponseFields responseJson
        |> Array.filter (fun (name, _) -> name <> "data")

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
                    | Some fieldType -> name, (helper true fieldType value)
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

type OperationResultProvidingInformation =
    { SchemaTypeNames : string []
      SchemaTypes : IntrospectionType []
      OperationTypeName : string }

type OperationResultBase (responseJson : string) =
    member private __.ResponseJson = JsonValue.Parse responseJson

    member private x._DataFields = JsonValueHelper.getResponseDataFields x.ResponseJson |> List.ofArray
    
    member private x._CustomFields = JsonValueHelper.getResponseCustomFields x.ResponseJson |> List.ofArray

    static member internal MakeProvidedType(providingInformation : OperationResultProvidingInformation, operationOutputType : ProvidedTypeDefinition) =
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
                        let dataFields = dfdef.GetValue(this) :?> (string * JsonValue) list
                        let fieldValues = JsonValueHelper.getFieldValues schemaTypes operationType dataFields
                        RecordBase(operationType.Name, fieldValues) @@>)
            ProvidedProperty("Data", operationOutputType, getterCode)
        
        let members : MemberInfo list = [ddef]
        tdef.AddMembers(members)
        tdef

    member x.Equals(other : OperationResultBase) =
        x.ResponseJson = other.ResponseJson

    override x.Equals(other : obj) =
        match other with
        | :? OperationResultBase as other -> x.Equals(other)
        | _ -> false

    override x.GetHashCode() = x.ResponseJson.GetHashCode()

type OperationBase (serverUrl : string, customHttpHeaders : seq<string * string> option) =
    member __.ServerUrl = serverUrl

    member __.CustomHttpHeaders = customHttpHeaders |> Option.map Array.ofSeq

    static member internal MakeProvidedType(requestHashCode : int, query, operationName : string option, operationTypeName : string, schemaTypes : Map<string, IntrospectionType>, outputTypes : Map<string list * string, ProvidedTypeDefinition>) =
        let className = "Operation" + requestHashCode.ToString("x2")
        let tdef = ProvidedTypeDefinition(className, Some typeof<OperationBase>)
        let operationOutputType =
            match outputTypes.TryFind([], operationTypeName) with
            | Some tdef -> tdef
            | _ -> failwithf "Operation type \"%s\" could not be found on the provided types." operationTypeName
        let schemaTypes = schemaTypes |> Seq.map (|KeyValue|)
        let info = 
            { SchemaTypeNames = schemaTypes |> Seq.map (fst >> (fun name -> name.FirstCharUpper())) |> Array.ofSeq
              SchemaTypes = schemaTypes |> Seq.map snd |> Array.ofSeq
              OperationTypeName = operationTypeName }
        let rtdef = OperationResultBase.MakeProvidedType(info, operationOutputType)
        // TODO : Parse query variables in the method parameters.
        let invoker (args : Expr list) =
            let operationName = Option.toObj operationName
            <@@ let this = %%args.[0] : OperationBase
                let request =
                    { ServerUrl = this.ServerUrl
                      CustomHeaders = this.CustomHttpHeaders
                      OperationName = Option.ofObj operationName
                      Query = query
                      Variables = None }
                let responseJson = GraphQLClient.sendRequest request
                OperationResultBase(responseJson) @@>
        let mdef = ProvidedMethod("Run", [], rtdef, invoker)
        let members : MemberInfo list = [rtdef; mdef]
        tdef.AddMembers(members)
        tdef

type ContextBase (serverUrl : string, schema : IntrospectionSchema) =
    static member private BuildOutputTypes(schemaTypes : Map<string, IntrospectionType>, enumProvidedTypes : Map<string, ProvidedTypeDefinition>, operationName : string option, queryAst : Document, responseJson : string) =
        let responseJson = JsonValue.Parse responseJson
        let providedTypes = Dictionary<string list * string, ProvidedTypeDefinition>()
        let astInfoMap = 
            match queryAst.GetInfoMap() |> Map.tryFind operationName with
            | Some info -> info
            | None ->
                match operationName with
                | Some name -> failwithf "Operation \"%s\" was not found in query document." name
                | None -> failwith "No unamed operation was found in query document."
        let getAstInfo (path : string list) =
            match astInfoMap |> Map.tryFind path with
            | Some ast -> 
                let typeFields = 
                    ast
                    |> List.choose (function | TypeField name -> Some name | _ -> None)
                let fragmentFields = 
                    ast
                    |> List.choose (function | TypeField _ -> None | FragmentField (tc, name) -> Some (tc, name))
                    |> List.groupBy fst
                    |> List.map (fun (key, items) -> key, (items |> List.map snd |> List.rev))
                    |> Map.ofList
                typeFields, fragmentFields
            | None -> failwithf "Property \"%s\" is a union or interface type, but no inheritance information could be determined from the input query." path.Head
        let getTypeFields (path : string list) (fields : (string * JsonValue) []) =
            let astInfo = getAstInfo path
            let typeFields = fst astInfo
            fields |> Array.filter (fun (name, _) -> List.contains name typeFields || name = "__typename")
        let buildOutputTypes (fields : (string * JsonValue) []) =
            let getEnumType (typeName : string) =
                if enumProvidedTypes.ContainsKey(typeName)
                then enumProvidedTypes.[typeName]
                else failwithf "Enum type %s was not found in the schema." typeName
            let rec getRecordOrInterfaceType (path : string list) (typeName : string option) (baseType : Type option) (fields : (string * JsonValue) []) =
                let helper (path : string list) typeName (fields : (string * JsonValue) []) (schemaType : IntrospectionType) =
                    let key = path, typeName
                    if providedTypes.ContainsKey(key)
                    then providedTypes.[key]
                    else
                        let properties =
                            let fields =
                                match schemaType.Kind with
                                | TypeKind.OBJECT -> fields
                                | TypeKind.INTERFACE | TypeKind.UNION -> getTypeFields path fields
                                | _ -> failwithf "Type \"%s\" is not a Record, Union or Interface type." schemaType.Name
                            match fields with
                            | [||] -> []
                            | _ -> getProperties path fields schemaType
                        let outputType =
                            match schemaType.Kind with
                            | TypeKind.OBJECT -> RecordBase.MakeProvidedType(typeName, properties, baseType)
                            | TypeKind.INTERFACE | TypeKind.UNION -> RecordBase.MakeProvidedType(typeName, properties, None)
                            | _ -> failwithf "Type \"%s\" is not a Record, Union or Interface type." schemaType.Name
                        providedTypes.Add(key, outputType)
                        outputType
                match typeName |> Option.orElse (JsonValueHelper.getTypeName fields) with
                | Some typeName ->
                    match schemaTypes |> Map.tryFind typeName with
                    | Some schemaType -> helper path typeName fields schemaType
                    | None -> failwithf "Expected to find a type \"%s\" on schema, but it was not found." typeName
                | None -> failwith "Expected type to have a \"__typename\" field, but it was not found."
            and getProperties (path : string list) (fields : (string * JsonValue) []) (schemaType : IntrospectionType) =
                let getFieldType (name : string) =
                    match schemaType.Fields with
                    | Some fields ->
                        match fields |> Array.tryFind (fun f -> f.Name = name) with
                        | Some field -> field.Type
                        | None -> failwithf "Expected type \"%s\" to have a field \"%s\", but it was not found in schema." schemaType.Name name
                    | None -> failwithf "Expected type \"%s\" to have fields, but it does not have any field." schemaType.Name
                let rec getListProperty (name : string, items : JsonValue [], tref : IntrospectionTypeRef) =
                    let path = name :: path
                    match tref.Kind with
                    | TypeKind.NON_NULL ->
                        match tref.OfType with
                        | Some tref when tref.Kind <> TypeKind.NON_NULL -> getListProperty (name, items, tref) |> Types.unwrapOptionArray
                        | _ -> failwithf "Property \"%s\" is a list of a non-null type, but it does not have an underlying type, or its underlying type is no supported." name
                    | TypeKind.UNION | TypeKind.INTERFACE ->
                        if tref.Name.IsSome
                        then
                            let baseType : Type =
                                match items |> Array.tryHead with
                                | Some (JsonValue.Record fields) -> upcast getRecordOrInterfaceType path tref.Name None fields
                                | _ -> upcast getRecordOrInterfaceType path tref.Name None [||]
                            let itemTypeMapper = function
                                | JsonValue.Record fields -> 
                                    match JsonValueHelper.getTypeName fields with
                                    | Some typeName -> getRecordOrInterfaceType path (Some typeName) (Some baseType) fields |> ignore
                                    | None -> failwith "Expected type to have a \"__typename\" field, but it was not found."
                                | other -> failwithf "Expected property \"%s\" to be a Union or Interface type, but it is %A." name other
                            items |> Array.iter itemTypeMapper
                            (name, baseType) |> Types.makeOption |> Types.makeArray
                        else failwithf "Property \"%s\" is an union or interface type, but it does not have a type name, or its base type does not have a name." name
                    | TypeKind.OBJECT ->
                        let itemType : Type =
                            match JsonValueHelper.getTypeName fields with
                            | Some typeName -> upcast getRecordOrInterfaceType path (Some typeName) None fields
                            | None -> failwith "Expected type to have a \"__typename\" field, but it was not found."
                        (name, itemType) |> Types.makeOption |> Types.makeArray
                    | TypeKind.ENUM ->
                        match tref.Name with
                        | Some typeName -> (name, getEnumType typeName) |> Types.makeOption |> Types.makeArray
                        | None -> failwith "Expected enum type to have a name, but it does not have one."
                    | kind -> failwithf "Unsupported type kind \"%A\"." kind
                let rec getProperty (name : string, value : JsonValue, tref : IntrospectionTypeRef) =
                    match tref.Kind with
                    | TypeKind.NON_NULL ->
                        match tref.OfType with
                        | Some tref when tref.Kind <> TypeKind.NON_NULL -> getProperty (name, value, tref) |> Types.unwrapOption
                        | _ -> failwithf "Property \"%s\" is a non-null type, but it does not have an underlying type, or its underlying type is no supported." name
                    | TypeKind.LIST ->
                        match tref.OfType, value with
                        | Some tref, JsonValue.Array items -> getListProperty (name, items, tref) |> Types.makeOption
                        | _ -> failwithf "Property \"%s\" is a list type, but it does not have an underlying type, or its combination of type and the response value is not supported." name
                    | TypeKind.OBJECT | TypeKind.INTERFACE | TypeKind.UNION ->
                        match value with
                        | JsonValue.Record fields -> (name, getRecordOrInterfaceType (name :: path) None None fields) |> Types.makeOption
                        | _ -> failwithf "Expected property \"%s\" to be a Record type, but it is %A." name value
                    | TypeKind.SCALAR -> 
                        match tref.Name with
                        | Some typeName -> (name, Types.getScalarType typeName) |> Types.makeOption
                        | None -> failwith "Expected scalar type to have a name, but it does not have one."
                    | TypeKind.ENUM ->
                        match tref.Name with
                        | Some typeName -> (name, getEnumType typeName) |> Types.makeOption
                        | None -> failwith "Expected enum type to have a name, but it does not have one."
                    | kind -> failwithf "Unsupported type kind \"%A\"." kind
                fields
                |> Array.filter (fun (name, _) -> name <> "__typename")
                |> Array.map ((fun (name, value) -> name, value, getFieldType name) >> getProperty)
                |> List.ofArray
            getRecordOrInterfaceType [] None None fields |> ignore
        JsonValueHelper.getResponseDataFields responseJson |> buildOutputTypes
        providedTypes |> Seq.map (|KeyValue|) |> Map.ofSeq

    member __.ServerUrl = serverUrl

    member __.Schema = schema

    static member internal MakeProvidedType(schema : IntrospectionSchema, enumProvidedTypes : Map<string, ProvidedTypeDefinition>, serverUrl : string) =
        let tdef = ProvidedTypeDefinition("Context", Some typeof<ContextBase>)
        let mdef =
            let sprm = 
                [ ProvidedStaticParameter("queryString", typeof<string>)
                  ProvidedStaticParameter("operationName", typeof<string>, "") ]
            let smdef = ProvidedMethod("Operation", [], typeof<OperationBase>)
            let genfn (mname : string) (args : obj []) =
                let originalQuery = args.[0] :?> string
                let queryAst = parse originalQuery
                let query = queryAst.ToQueryString(QueryStringPrintingOptions.IncludeTypeNames)
                let operationName = 
                    match args.[1] :?> string with
                    | "" -> 
                        match queryAst.Definitions with
                        | [] -> failwith "Error parsing query. Can not choose a default operation: query document has no definitions."
                        | _ -> queryAst.Definitions.Head.Name
                    | x -> Some x
                let request =
                    { ServerUrl = serverUrl
                      CustomHeaders = None
                      OperationName = operationName
                      Query = query
                      Variables = None }
                let responseJson = GraphQLClient.sendRequest request
                let schemaTypes = Types.getSchemaTypes(schema)
                let outputTypes = ContextBase.BuildOutputTypes(schemaTypes, enumProvidedTypes, operationName, queryAst, responseJson)
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
                outputTypes
                |> Seq.map ((|KeyValue|) >> (fun ((path, _), t) -> path, t))
                |> Seq.iter (fun (path, t) -> includeType path t)
                let operationTypeName =
                    let odef = 
                        queryAst.Definitions
                        |> List.choose (function OperationDefinition odef -> Some odef | _ -> None)
                        |> List.find (fun d -> d.Name = operationName)
                    match odef.OperationType with
                    | Query -> 
                        match schema.QueryType.Name with
                        | Some name -> name
                        | None -> failwith "Query type does not have a name in the introspection schema."
                    | Mutation ->
                        match schema.MutationType with
                        | Some m when m.Name.IsSome -> m.Name.Value
                        | _ -> failwith "Mutation type does not have a name in the introspection schema, or the schema does not provide a mutation type."
                    | Subscription ->
                        match schema.SubscriptionType with
                        | Some s when s.Name.IsSome -> s.Name.Value
                        | _ -> failwith "Subscription type does not have a name in the introspection schema, or the schema does not provide a subscription type."
                let odef = OperationBase.MakeProvidedType(request.GetHashCode(), query, operationName, operationTypeName, schemaTypes, outputTypes)
                odef.AddMember(rootWrapper)
                let invoker (args : Expr list) =
                    <@@ let this = %%args.[0] : ContextBase
                        let customHttpHeaders = (%%args.[1] : seq<string * string>) |> Option.ofObj
                        OperationBase(this.ServerUrl, customHttpHeaders) @@>
                let prm = [ProvidedParameter("customHttpHeaders", typeof<seq<string * string>>, optionalValue = None)]
                let mdef = ProvidedMethod(mname, prm, odef, invoker)
                let members : MemberInfo list = [odef; mdef]
                tdef.AddMembers(members)
                mdef
            smdef.DefineStaticParameters(sprm, genfn)
            smdef
        tdef.AddMember(mdef)
        tdef

    static member internal Constructor = typeof<ContextBase>.GetConstructors().[0]

type ProviderBase private () =
    static member private BuildOutputTypes(schema : IntrospectionSchema) =
        let providedTypes = Dictionary<string, ProvidedTypeDefinition>()
        let schemaTypes = Types.getSchemaTypes(schema)
        let getSchemaType (tref : IntrospectionTypeRef) =
            match tref.Name with
            | Some name ->
                match schemaTypes.TryFind(name) with
                | Some itype -> itype
                | None -> failwithf "Type \"%s\" was not found on the schema custom types." name
            | None -> failwith "Expected schema type to have a name, but it does not have one."
        let rec getProperty (name : string) (tref : IntrospectionTypeRef) : string * Type =
            match tref.Kind with
            | TypeKind.NON_NULL ->
                match tref.OfType with
                | Some tref ->
                    match tref.Kind with
                    | TypeKind.NON_NULL -> failwith "Schema parsing error. A non null type has another non null type as underlying type."
                    | TypeKind.LIST -> getProperty name tref |> Types.unwrapOption
                    | _ -> getProperty name tref |> Types.unwrapOption
                | None -> failwith "Schema parsing error. A non null type has no underlying type."
            | TypeKind.OBJECT | TypeKind.UNION | TypeKind.INTERFACE | TypeKind.ENUM ->
                let itype = getSchemaType tref
                (name, getProvidedType itype) |> Types.makeOption
            | TypeKind.LIST ->
                match tref.OfType with
                | Some tref -> getProperty name tref |> Types.makeArray |> Types.makeOption
                | None -> failwith "Schema parsing error. A non null type has no underlying type."
            | TypeKind.SCALAR ->
                match tref.Name with
                | Some typeName -> (name, Types.getScalarType typeName) |> Types.makeOption
                | None -> failwith "Schema parsing error. Expected scalar type to have a name, but it does not have one."
            | kind -> failwithf "Unsupported type kind \"%A\"." kind
        and getProvidedType (itype : IntrospectionType) : ProvidedTypeDefinition =
            if providedTypes.ContainsKey(itype.Name)
            then providedTypes.[itype.Name]
            else
                let properties =
                    match itype.Fields with
                    | Some fields -> fields |> Array.map (fun field -> getProperty field.Name field.Type) |> List.ofArray
                    | None -> []
                match itype.Kind with
                | TypeKind.OBJECT -> 
                    let tdef = RecordBase.MakeProvidedType(itype.Name, properties, None)
                    providedTypes.Add(itype.Name, tdef)
                    tdef
                | TypeKind.INTERFACE | TypeKind.UNION ->
                    let bdef = InterfaceBase.MakeProvidedType(itype.Name)
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
            let schema = Serialization.deserializeSchema introspectionJson
            let outputTypes = ProviderBase.BuildOutputTypes(schema)
            let typeWrapper = ProvidedTypeDefinition("Types", None, isSealed = true)
            outputTypes
            |> Seq.map (fun kvp -> kvp.Value)
            |> Seq.iter typeWrapper.AddMember
            tdef.AddMember(typeWrapper)
            let enumOutputTypes = outputTypes |> Map.filter (fun _ ptype -> ptype.BaseType = typeof<EnumBase>)
            let ctxdef = ContextBase.MakeProvidedType(schema, enumOutputTypes, serverUrl)
            let schemaExpr = <@@ Serialization.deserializeSchema introspectionJson @@>
            let ctxmdef =
                let prm = [ProvidedParameter("serverUrl", typeof<string>, optionalValue = serverUrl)]
                let invoker (args : Expr list) =
                    let serverUrl = args.[0]
                    Expr.NewObject(ContextBase.Constructor, [serverUrl; schemaExpr])
                ProvidedMethod("GetContext", prm, ctxdef, invoker, true)
            let schemapdef = 
                let getterCode (_ : Expr list) = schemaExpr
                ProvidedProperty("Schema", typeof<IntrospectionSchema>, getterCode, isStatic = true)
            let members : MemberInfo list = [ctxdef; ctxmdef; schemapdef]
            tdef.AddMembers(members)
            tdef)
        generator