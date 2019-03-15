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
            let getterCode (args : Expr list) =
                <@@ let this = %%args.[0] : RecordBase
                    let propdef = typeof<RecordBase>.GetProperty("Properties", BindingFlags.NonPublic ||| BindingFlags.Instance)
                    let props = propdef.GetValue(this) :?> (string * obj) list
                    props |> List.find (fun (name, _) -> name = pname) |> snd @@>
            upcast ProvidedProperty(pname.FirstCharUpper(), ptype, getterCode)
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

type ContextBase (serverUrl : string, schema : IntrospectionSchema) =
    static member private BuildOutputTypes(schema : IntrospectionSchema, operationName : string option, queryAst : Document, responseJson : string) =
        let fail : string -> 'T = fun msg -> failwith ("Failure deserializing query response. " + msg)
        let outputTypes = Dictionary<string list * string, ProvidedTypeDefinition>()
        let scalarTypes =
            [| "Int", typeof<int>
               "Boolean", typeof<bool>
               "Date", typeof<DateTime>
               "Float", typeof<float>
               "ID", typeof<string>
               "String", typeof<string>
               "URI", typeof<Uri> |]
            |> Map.ofArray
        let isScalarType (name : string) =
            scalarTypes |> Map.containsKey name
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
        let schemaTypes =
            schema.Types
            |> Array.filter (fun t -> not (isIntrospectionType t.Name) && not (isScalarType t.Name))
            |> Array.map (fun t -> t.Name, t)
            |> Map.ofArray
        let enumTypes =
            let createType (t : IntrospectionType) =
                match t.EnumValues with
                | Some enumValues -> EnumBase.MakeProvidedType(t.Name, enumValues |> Array.map (fun x -> x.Name))
                | None -> fail (sprintf "Type %s is a enum type, but no enum values were found for this type." t.Name)
            schemaTypes
            |> Map.filter (fun _ t -> t.Kind = TypeKind.ENUM)
            |> Map.map (fun _ t -> createType t)
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
        let responseJson = JsonValue.Parse responseJson
        let buildOutputTypes (fields : (string * JsonValue) []) =
            let getTypeName (fields : (string * JsonValue) []) =
                fields
                |> Array.tryFind (fun (name, _) -> name = "__typename")
                |> Option.map (fun (_, value) ->
                    match value with
                    | JsonValue.String x -> x
                    | _ -> fail (sprintf "Expected \"__typename\" field to be a string field, but it was %A." value))
            let getScalarType (typeName : string) =
                match scalarTypes |> Map.tryFind typeName with
                | Some t -> t
                | None -> fail (sprintf "Scalar type %s is not supported." typeName)
            let getEnumType (typeName : string) =
                match enumTypes |> Map.tryFind typeName with
                | Some t -> outputTypes.Add(([], typeName), t); t
                | None -> fail (sprintf "Enum type %s was not found in the schema." typeName)
            let rec getRecordOrInterfaceType (path : string list) (typeName : string option) (interfaces : Type list) (fields : (string * JsonValue) []) =
                let rec helper (path : string list) typeName (fields : (string * JsonValue) []) (schemaType : IntrospectionType) =
                    if outputTypes.ContainsKey(path, typeName)
                    then outputTypes.[path, typeName]
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
                        outputTypes.Add((path, typeName), outputType)
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
        match responseJson with
        | JsonValue.Record fields ->
            let dataField = fields |> Array.tryFind (fun (name, _) -> name = "data")
            match dataField with
            | Some (_, data) ->
                match data with
                | JsonValue.Record fields -> buildOutputTypes fields
                | _ -> fail (sprintf "Expected data field of root type to be a Record type, but type is %A." data)
            | None -> fail "Expected root type to have a \"data\" field, but it was not found."
        | _ -> fail (sprintf "Expected root type to be a Record type, but type is %A." responseJson)
        outputTypes

    member __.ServerUrl = serverUrl

    member __.Schema = schema

    static member internal MakeProvidedType(schema : IntrospectionSchema, serverUrl : string, customHeaders : seq<string * string>) =
        let tdef = ProvidedTypeDefinition("Context", Some typeof<ContextBase>)
        let mdef =
            let sprm = 
                [ ProvidedStaticParameter("query", typeof<string>)
                  ProvidedStaticParameter("operationName", typeof<string>, "")
                  ProvidedStaticParameter("variables", typeof<seq<string * obj>>, Seq.empty<string * obj>) ]
            let smdef = ProvidedMethod("Query", [], typeof<obj>)
            let genfn (mname : string) (args : obj []) =
                let originalQuery = args.[0] :?> string
                let queryAst = parse originalQuery
                let query = queryAst.ToQueryString(QueryStringPrintingOptions.IncludeTypeNames)
                let operationName = 
                    match args.[1] :?> string with
                    | "" -> 
                        match queryAst.Definitions with
                        | [] -> failwith "Error parsing query. No definition exists in the document."
                        | _ -> queryAst.Definitions.Head.Name
                    | x -> Some x
                let variables = 
                    let vars = args.[2] :?> seq<string * obj>
                    if Seq.length vars = 0 then None else Some vars
                let request =
                    { ServerUrl = serverUrl
                      CustomHeaders = Some customHeaders
                      OperationName = operationName
                      Query = query
                      Variables = variables }
                // TODO: Confirm if this is the best query type name generation strategy
                let className = sprintf "Query<%i>" (request.GetHashCode())
                let qdef = ProvidedTypeDefinition(className, None)
                let outputTypes =
                    let responseJson = GraphQLClient.sendRequest request
                    ContextBase.BuildOutputTypes(schema, operationName, queryAst, responseJson)
                let wrappersByPath = Dictionary<string list, ProvidedTypeDefinition>()
                let rootWrapper = ProvidedTypeDefinition("Types", None, isSealed = true)
                wrappersByPath.Add([], rootWrapper)
                let rec getWrapper (path : string list) =
                    if wrappersByPath.ContainsKey path
                    then wrappersByPath.[path]
                    else
                        let wrapper = ProvidedTypeDefinition(path.Head.FirstCharUpper(), None, isSealed = true)
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
                |> Seq.iter (fun ((path, _), t) -> includeType path t)
                qdef.AddMember(rootWrapper)
                let invoker (args : Expr list) =
                    <@@ let queryAst = parse query
                        let operationName =
                            match queryAst.Definitions with
                            | [] -> failwith "Error parsing query. No definition exists in the document."
                            | _ -> queryAst.Definitions.Head.Name
                        let this = %%args.[0] : ContextBase
                        let request =
                            { ServerUrl = this.ServerUrl
                              CustomHeaders = Option.ofObj %%args.[1]
                              OperationName = Option.ofObj %%args.[2] |> Option.orElse operationName
                              Query = query
                              Variables = Option.ofObj %%args.[3] }
                        let responseJson = GraphQLClient.sendRequest request
                        obj() @@>
                let prm = 
                    [ ProvidedParameter("customHeaders", typeof<seq<string * string>>, optionalValue = None)
                      ProvidedParameter("operationName", typeof<string>, optionalValue = None)
                      ProvidedParameter("variables", typeof<seq<string * obj>>, optionalValue = None) ]
                let mdef = ProvidedMethod(mname, prm, qdef, invoker)
                let members : MemberInfo list = [qdef; mdef]
                tdef.AddMembers(members)
                mdef
            smdef.DefineStaticParameters(sprm, genfn)
            smdef
        tdef.AddMember(mdef)
        tdef

    static member internal Constructor = typeof<ContextBase>.GetConstructors().[0]