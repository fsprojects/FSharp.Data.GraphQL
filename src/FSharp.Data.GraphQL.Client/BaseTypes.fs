namespace FSharp.Data.GraphQL

open System
open System.Security.Cryptography
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
        Array.mapi (fun i v ->
                let expr = 
                    if v = null then simpleTypeExpr v
                    else
                        let tpy = v.GetType()
                        if tpy.IsArray then arrayExpr tpy v
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

[<AutoOpen>]
module internal ErrorHelper =
    let fail : string -> 'T = fun msg -> failwith ("Failure deserializing query response. " + msg)

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

type InterfaceBase private () =
    static member internal MakeProvidedType((name : string), properties : (string * Type) list) =
        let tdef = ProvidedTypeDefinition("I" + name.FirstCharUpper(), None, nonNullable = true, isInterface = true)
        let propertyMapper (pname : string, ptype : Type) : MemberInfo =
            upcast ProvidedProperty(pname, ptype)
        let pdefs = properties |> List.map propertyMapper
        tdef.AddMembers(pdefs)
        tdef

type RecordBase (properties : (string * obj) list) =
    member internal __.Properties = properties

    static member internal MakeProvidedType((name : string), properties : (string * Type) list) =
        let tdef = ProvidedTypeDefinition(name.FirstCharUpper(), Some typeof<RecordBase>, nonNullable = true, isSealed = true)
        let propertyMapper (pname : string, ptype : Type) : MemberInfo =
            let pname = pname.FirstCharUpper()
            let getterCode (args : Expr list) =
                <@@ let this = %%args.[0] : RecordBase
                    let propdef = typeof<RecordBase>.GetProperty("Properties", BindingFlags.NonPublic ||| BindingFlags.Instance)
                    let props = propdef.GetValue(this) :?> (string * obj) list
                    match props |> List.tryFind (fun (name, _) -> name = pname) with
                    | Some (_, value) -> value
                    | None -> failwithf "Expected to find property \"%s\" under properties %A, but was not found." pname (List.map snd props) @@>
            upcast ProvidedProperty(pname, ptype, getterCode)
        let pdefs = properties |> List.map propertyMapper
        tdef.AddMembers(pdefs)
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
            | [name, value] -> sb.Append(sprintf "%s = %O;" name value) |> ignore
            | (name, value) :: tail -> sb.AppendLine(sprintf "%s = %O;" name value) |> ignore; printProperties tail
        printProperties x.Properties
        sb.Append("}") |> ignore
        sb.ToString()

    member x.Equals(other : RecordBase) =
        x.Properties = other.Properties

    override x.Equals(other : obj) =
        match other with
        | :? RecordBase as other -> x.Equals(other)
        | _ -> false
    override x.GetHashCode() = x.Properties.GetHashCode()

    interface IEquatable<RecordBase> with
        member x.Equals(other) = x.Equals(other)

module JsonValueHelper =
    let getResponseFields (responseJson : JsonValue) =
        match responseJson with
        | JsonValue.Record fields -> fields
        | _ -> fail (sprintf "Expected root type to be a Record type, but type is %A." responseJson)

    let getResponseDataFields (responseJson : JsonValue) =
        match getResponseFields responseJson |> Array.tryFind (fun (name, _) -> name = "data") with
        | Some (_, data) -> 
            match data with
            | JsonValue.Record fields -> fields
            | _ -> fail (sprintf "Expected data field of root type to be a Record type, but type is %A." data)
        | None -> fail "Expected root type to have a \"data\" field, but it was not found."

    let getResponseCustomFields (responseJson : JsonValue) =
        getResponseFields responseJson
        |> Array.filter (fun (name, _) -> name <> "data")

    let private removeTypeNameField (fields : (string * JsonValue) list) =
        fields |> List.filter (fun (name, _) -> name <> "__typename")

    let rec getFieldValue (schemaType : IntrospectionType) (fieldName : string, fieldValue : JsonValue) =
        let rec helper (path : string list) (fieldValue : JsonValue) : obj =
            match fieldValue with
            | JsonValue.Array items -> box (items |> Array.map (helper path))
            | JsonValue.Record props -> 
                props
                |> List.ofArray
                |> removeTypeNameField
                |> List.map ((fun (name, value) -> name.FirstCharUpper(), value) >> (fun (name, value) -> name, (helper (name::path) value)))
                |> RecordBase
                |> box
            | JsonValue.Boolean b -> box b
            | JsonValue.Float f -> box f
            | JsonValue.Null -> null
            | JsonValue.Number n -> box (float n)
            | JsonValue.String s -> box s
        fieldName, (helper [fieldName] fieldValue)

    and getFieldValues (schemaType : IntrospectionType) (fields : (string * JsonValue) list) =
        removeTypeNameField fields |> List.map ((fun (name, value) -> name.FirstCharUpper(), value) >> getFieldValue schemaType)

type ProvidedTypeKind =
    | EnumType of name : string
    | OutputType of path : string list * name : string

type OperationResultBase (responseJson : string) =
    member __.ResponseJson = JsonValue.Parse responseJson

    member this.DataFields = JsonValueHelper.getResponseDataFields this.ResponseJson |> List.ofArray
    
    member this.CustomFields = JsonValueHelper.getResponseCustomFields this.ResponseJson |> List.ofArray

    static member internal MakeProvidedType(outputQueryType : ProvidedTypeDefinition, schemaQueryType : IntrospectionType) =
        let tdef = ProvidedTypeDefinition("OperationResult", Some typeof<OperationResultBase>, nonNullable = true)
        let qpdef = 
            let getterCode =
                QuotationHelpers.quoteRecord schemaQueryType (fun (args : Expr list) schemaQueryType ->
                    <@@ let this = %%args.[0] : OperationResultBase
                        let fieldValues = JsonValueHelper.getFieldValues %%schemaQueryType this.DataFields
                        RecordBase(fieldValues) @@>)
            ProvidedProperty("Data", outputQueryType, getterCode)
        tdef.AddMember(qpdef)
        tdef

type OperationBase (serverUrl : string, customHttpHeaders : seq<string * string> option, operationName : string option) =
    member __.ServerURl = serverUrl

    member __.CustomHttpHeaders = customHttpHeaders

    member __.OperationName = operationName

    static member internal MakeProvidedType(requestHashCode : int, serverUrl, operationName, query, queryTypeName : string, schemaTypes : Map<string, IntrospectionType>, outputTypes : Map<ProvidedTypeKind, ProvidedTypeDefinition>) =
        let hashString = 
            let md5 = MD5.Create()
            let bytes = md5.ComputeHash(requestHashCode.ToString() |> Encoding.UTF8.GetBytes)
            let sb = StringBuilder()
            for b in bytes do sb.Append(b.ToString("x2")) |> ignore
            sb.ToString()
        let className = sprintf "Operation_%s" hashString
        let tdef = ProvidedTypeDefinition(className, Some typeof<OperationBase>)
        // We need to convert the operation name to a nullable string instead of an option here,
        // because we are going to use it inside a quotation, and quotations have issues with options as constant values.
        let operationName = Option.toObj operationName
        let outputQueryType =
            match outputTypes.TryGetValue(OutputType ([], queryTypeName)) with
            | (true, tdef) -> tdef
            | _ -> fail (sprintf "Query type %s could not be found on the provided types. This could be a internal bug. Please report the author." queryTypeName)
        let schemaQueryType =
            match schemaTypes.TryGetValue(queryTypeName) with
            | (true, def) -> def
            | _ -> fail (sprintf "Query type %s could not be found on the schema types. This could be a internal bug. Please report the author." queryTypeName)
        let rtdef = OperationResultBase.MakeProvidedType(outputQueryType, schemaQueryType)
        // TODO : Parse query parameters in the method args
        let invoker (args : Expr list) =
            <@@ let request =
                    { ServerUrl = serverUrl
                      CustomHeaders = None
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
    static member private ScalarTypes =
        [| "Int", typeof<int>
           "Boolean", typeof<bool>
           "Date", typeof<DateTime>
           "Float", typeof<float>
           "ID", typeof<string>
           "String", typeof<string>
           "URI", typeof<Uri> |]
        |> Map.ofArray
    static member private GetSchemaTypes (schema : IntrospectionSchema) =
        let isScalarType (name : string) =
            ContextBase.ScalarTypes |> Map.containsKey name
        let isIntrospectionType (name : string) =
            [| "__TypeKind"
               "__DirectiveLocation"
               "__Type"
               "__InputValue"
               "__Field"
               "__EnumValue"
               "__Directive"
               "__Schema" |]
            |> Array.contains name
        schema.Types
        |> Array.filter (fun t -> not (isIntrospectionType t.Name) && not (isScalarType t.Name))
        |> Array.map (fun t -> t.Name, t)
        |> Map.ofArray
    static member private BuildOutputTypes(schemaTypes : Map<string, IntrospectionType>, operationName : string option, queryAst : Document, responseJson : string) =
        let responseJson = JsonValue.Parse responseJson
        let providedTypes = Dictionary<ProvidedTypeKind, ProvidedTypeDefinition>()
        let createEnumType (t : IntrospectionType) =
            match t.EnumValues with
            | Some enumValues -> 
                let edef = EnumBase.MakeProvidedType(t.Name, enumValues |> Array.map (fun x -> x.Name))
                providedTypes.Add(EnumType t.Name, edef)
            | None -> fail (sprintf "Type %s is a enum type, but no enum values were found for this type." t.Name)
        schemaTypes
        |> Map.filter (fun _ t -> t.Kind = TypeKind.ENUM)
        |> Map.iter (fun _ t -> createEnumType t)
        let astInfoMap = 
            match queryAst.GetInfoMap() |> Map.tryFind operationName with
            | Some info -> info
            | None ->
                match operationName with
                | Some name -> fail (sprintf "Operation \"%s\" was not found in query document." name)
                | None -> fail "No unamed operation was found in query document."
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
            | None -> fail (sprintf "Property \"%s\" is a union or interface type, but no inheritance information could be determined from the input query." path.Head)
        let getFragmentFields (typeName : string) (path : string list) (fields : (string * JsonValue) []) =
                match getAstInfo path |> snd |> Map.tryFind typeName with
                | Some fragmentFields -> fields |> Array.filter (fun (name, _) -> List.contains name fragmentFields || name = "__typename")
                | None -> fail (sprintf "Property \%s\" is a union or interface type, but type %s does not inherit it." path.Head typeName)
        let getTypeFields (path : string list) (fields : (string * JsonValue) []) =
            let typeFields = getAstInfo path |> fst
            fields |> Array.filter (fun (name, _) -> List.contains name typeFields || name = "__typename")
        let buildOutputTypes (fields : (string * JsonValue) []) =
            let getTypeName (fields : (string * JsonValue) []) =
                fields
                |> Array.tryFind (fun (name, _) -> name = "__typename")
                |> Option.map (fun (_, value) ->
                    match value with
                    | JsonValue.String x -> x
                    | _ -> fail (sprintf "Expected \"__typename\" field to be a string field, but it was %A." value))
            let getScalarType (typeName : string) =
                match ContextBase.ScalarTypes |> Map.tryFind typeName with
                | Some t -> t
                | None -> fail (sprintf "Scalar type %s is not supported." typeName)
            let getEnumType (typeName : string) =
                let key = EnumType typeName
                if providedTypes.ContainsKey(key)
                then providedTypes.[key]
                else fail (sprintf "Enum type %s was not found in the schema." typeName)
            let rec getRecordOrInterfaceType (path : string list) (typeName : string option) (interfaces : Type list) (fields : (string * JsonValue) []) =
                let rec helper (path : string list) typeName (fields : (string * JsonValue) []) (schemaType : IntrospectionType) =
                    let key = OutputType (path, typeName)
                    if providedTypes.ContainsKey(key)
                    then providedTypes.[key]
                    else
                        let properties =
                            let fields =
                                match schemaType.Kind with
                                | TypeKind.OBJECT -> 
                                    if interfaces.Length > 0
                                    then getFragmentFields typeName path fields
                                    else getTypeFields path fields
                                | TypeKind.INTERFACE | TypeKind.UNION -> getTypeFields path fields
                                | _ -> fail (sprintf "Type \"%s\" is not a Record, Union or Interface type." schemaType.Name)
                            match fields with
                            | [||] -> []
                            | _ -> getProperties path fields schemaType
                        let outputType =
                            match schemaType.Kind with
                            | TypeKind.OBJECT -> RecordBase.MakeProvidedType(typeName, properties)
                            | TypeKind.INTERFACE | TypeKind.UNION -> InterfaceBase.MakeProvidedType(typeName, properties)
                            | _ -> fail (sprintf "Type \"%s\" is not a Record, Union or Interface type." schemaType.Name)
                        interfaces |> List.iter outputType.AddInterfaceImplementation
                        providedTypes.Add(key, outputType)
                        outputType
                match typeName |> Option.orElse (getTypeName fields) with
                | Some typeName ->
                    match schemaTypes |> Map.tryFind typeName with
                    | Some schemaType -> helper path typeName fields schemaType
                    | None -> fail (sprintf "Expected to find a type \"%s\" on schema, but it was not found." typeName)
                | None -> fail "Expected type to have a \"__typename\" field, but it was not found."
            and getProperties (path : string list) (fields : (string * JsonValue) []) (schemaType : IntrospectionType) =
                let getFieldType (name : string) =
                    match schemaType.Fields with
                    | Some fields ->
                        match fields |> Array.tryFind (fun f -> f.Name = name) with
                        | Some field -> field.Type
                        | None -> fail (sprintf "Expected type \"%s\" to have a field \"%s\", but it was not found in schema." schemaType.Name name)
                    | None -> fail (sprintf "Expected type \"%s\" to have fields, but it does not have any field." schemaType.Name)
                let makeOption (name : string, t : Type) = name, typedefof<_ option>.MakeGenericType(t)
                let makeArray (name : string, t : Type) = name, t.MakeArrayType()
                let unwrapOption (name: string, t : Type) = 
                    if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<_ option>
                    then name, t.GetGenericArguments().[0]
                    else fail (sprintf "Expected native type of property \"%s\" to be an option type, but it is %s." name t.Name)
                let unwrapOptionArray (name : string, t : Type) =
                    if t.IsArray
                    then (name, t.GetElementType()) |> unwrapOption |> makeArray
                    else fail (sprintf "Expected type of property \"%s\" to be an array, but it is %s" name t.Name)
                let rec getListProperty (name : string, items : JsonValue [], tref : IntrospectionTypeRef) =
                    match tref.Kind with
                    | TypeKind.NON_NULL ->
                        match tref.OfType with
                        | Some tref when tref.Kind <> TypeKind.NON_NULL -> getListProperty (name, items, tref) |> unwrapOptionArray
                        | _ -> fail (sprintf "Property \"%s\" is a list of a non-null type, but it does not have an underlying type, or its underlying type is no supported." name)
                    | TypeKind.UNION | TypeKind.INTERFACE ->
                        if tref.Name.IsSome
                        then
                            let path = name :: path
                            let btype = getRecordOrInterfaceType path tref.Name [] fields
                            let itemMapper = function
                                | JsonValue.Record fields ->
                                    match getTypeName fields with
                                    | Some typeName -> getRecordOrInterfaceType path (Some typeName) [btype] fields |> ignore
                                    | None -> fail "Expected type to have a \"__typename\" field, but it was not found."
                                | other -> fail (sprintf "Expected property \"%s\" to be a Record type, but it is %A." name other)
                            items |> Array.iter itemMapper
                            (name, btype) |> makeOption |> makeArray
                        else fail (sprintf "Property \"%s\" is an union or interface type, but it does not have a type name, or its base type does not have a name." name)
                    | TypeKind.ENUM ->
                        match tref.Name with
                        | Some typeName -> (name, getEnumType typeName) |> makeOption |> makeArray
                        | None -> fail "Expected enum type to have a name, but it does not have one."
                    | kind -> fail (sprintf "Unsupported type kind \"%O\"." kind)
                let rec getProperty (name : string, value : JsonValue, tref : IntrospectionTypeRef) =
                    match tref.Kind with
                    | TypeKind.NON_NULL ->
                        match tref.OfType with
                        | Some tref when tref.Kind <> TypeKind.NON_NULL -> getProperty (name, value, tref) |> unwrapOption
                        | _ -> fail (sprintf "Property \"%s\" is a non-null type, but it does not have an underlying type, or its underlying type is no supported." name)
                    | TypeKind.LIST ->
                        match tref.OfType, value with
                        | Some tref, JsonValue.Array items -> getListProperty (name, items, tref) |> makeOption
                        | _ -> fail (sprintf "Property \"%s\" is a list type, but it does not have an underlying type, or its combination of type and the response value is not supported." name)
                    | TypeKind.OBJECT | TypeKind.INTERFACE | TypeKind.UNION ->
                        match value with
                        | JsonValue.Record fields -> (name, getRecordOrInterfaceType (name :: path) None [] fields) |> makeOption
                        | _ -> fail (sprintf "Expected property \"%s\" to be a Record type, but it is %A." name value)
                    | TypeKind.SCALAR -> 
                        match tref.Name with
                        | Some typeName -> (name, getScalarType typeName) |> makeOption
                        | None -> fail "Expected scalar type to have a name, but it does not have one."
                    | TypeKind.ENUM ->
                        match tref.Name with
                        | Some typeName -> (name, getEnumType typeName) |> makeOption
                        | None -> fail "Expected enum type to have a name, but it does not have one."
                    | kind -> fail (sprintf "Unsupported type kind \"%O\"." kind)
                fields
                |> Array.filter (fun (name, _) -> name <> "__typename")
                |> Array.map ((fun (name, value) -> name, value, getFieldType name) >> getProperty)
                |> List.ofArray
            getRecordOrInterfaceType [] None [] fields |> ignore
        JsonValueHelper.getResponseDataFields responseJson |> buildOutputTypes
        providedTypes |> Seq.map (|KeyValue|) |> Map.ofSeq

    member __.ServerUrl = serverUrl

    member __.Schema = schema

    static member internal MakeProvidedType(schema : IntrospectionSchema, serverUrl : string) =
        let tdef = ProvidedTypeDefinition("Context", Some typeof<ContextBase>)
        let mdef =
            let sprm = 
                [ ProvidedStaticParameter("queryString", typeof<string>)
                  ProvidedStaticParameter("operationName", typeof<string>, "") ]
            let smdef = ProvidedMethod("Query", [], typeof<OperationBase>)
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
                let schemaTypes = ContextBase.GetSchemaTypes(schema)
                let outputTypes = ContextBase.BuildOutputTypes(schemaTypes, operationName, queryAst, responseJson)
                let generateWrapper name = ProvidedTypeDefinition(name, None, isSealed = true)
                let contextWrapper = generateWrapper "Types"
                outputTypes
                |> Seq.map (|KeyValue|)
                |> Seq.choose (fun (kind, tdef) -> match kind with | EnumType _ -> Some tdef | _ -> None)
                |> Seq.iter (contextWrapper.AddMember)
                tdef.AddMember(contextWrapper)
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
                |> Seq.map (|KeyValue|)
                |> Seq.choose (fun (kind, t) -> match kind with | OutputType (path, _) -> Some (path, t) | _ -> None)
                |> Seq.iter (fun (path, t) -> includeType path t)
                let queryTypeName =
                    match schema.QueryType.Name with
                    | Some name -> name
                    | None -> fail "Query type does not have a name in the introspection."
                let odef = OperationBase.MakeProvidedType(request.GetHashCode(), serverUrl, operationName, query, queryTypeName, schemaTypes, outputTypes)
                odef.AddMember(rootWrapper)
                let invoker (args : Expr list) =
                    let operationName = Option.toObj operationName
                    <@@ let this = %%args.[0] : ContextBase
                        let customHttpHeaders = (%%args.[1] : seq<string * string>) |> Option.ofObj
                        OperationBase(this.ServerUrl, customHttpHeaders, Option.ofObj operationName) @@>
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