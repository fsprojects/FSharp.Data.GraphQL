/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Provider

open System
open FSharp.Core
open System.Reflection
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Client
open FSharp.Data.GraphQL.Client.ReflectionPatterns
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Validation
open System.Collections.Generic
open FSharp.Data.GraphQL.Types.Introspection
open FSharp.Data.GraphQL.Ast.Extensions
open FSharp.Data.GraphQL.ProvidedSchema
open FSharp.Data.GraphQL.ProvidedOperation
open ProviderImplementation.ProvidedTypes
open FSharp.Quotations

#nowarn "10001"

type internal ProvidedOperationMetadata =
    { OperationType : Type
      UploadInputTypeName : string option
      TypeWrapper : ProvidedTypeDefinition }

let uploadTypeIsNotScalar uploadTypeName =
    failwithf "Upload type \"%s\" was found on the schema, but it is not a Scalar type. Upload types can only be used if they are defined as scalar types." uploadTypeName

let private getOperationMetadata (schemaTypes : Map<TypeName, IntrospectionType>, uploadInputTypeName : string option, enumProvidedTypes : Map<TypeName, ProvidedTypeDefinition>, operationAstFields, operationTypeRef, explicitOptionalParameters: bool) =
    let generateWrapper name =
        let rec resolveWrapperName actual =
            if schemaTypes.ContainsKey(actual)
            then resolveWrapperName (actual + "Fields")
            else actual
        ProvidedTypeDefinition(resolveWrapperName name, None, isSealed = true)
    let wrappersByPath = Dictionary<string list, ProvidedTypeDefinition>()
    let rootWrapper = generateWrapper "Types"
    wrappersByPath.Add([], rootWrapper)
    let rec getWrapper (path : string list) =
        if wrappersByPath.ContainsKey path
        then wrappersByPath.[path]
        else
            let wrapper = generateWrapper (path.Head.FirstCharUpper() + "Fields")
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
    let providedTypes = Dictionary<Path * TypeName, ProvidedTypeDefinition>()
    let rec getProvidedType (providedTypes : Dictionary<Path * TypeName, ProvidedTypeDefinition>) (schemaTypes : Map<TypeName, IntrospectionType>) (path : Path) (astFields : AstFieldInfo list) (tref : IntrospectionTypeRef) : Type =
        match tref.Kind with
        | TypeKind.SCALAR when tref.Name.IsSome -> TypeMapping.mapScalarType uploadInputTypeName tref.Name.Value |> TypeMapping.makeOption
        | _ when uploadInputTypeName.IsSome && tref.Name.IsSome && uploadInputTypeName.Value = tref.Name.Value -> uploadTypeIsNotScalar uploadInputTypeName.Value
        | TypeKind.NON_NULL when tref.Name.IsNone && tref.OfType.IsSome -> getProvidedType providedTypes schemaTypes path astFields tref.OfType.Value |> TypeMapping.unwrapOption
        | TypeKind.LIST when tref.Name.IsNone && tref.OfType.IsSome -> getProvidedType providedTypes schemaTypes path astFields tref.OfType.Value |> TypeMapping.makeArray |> TypeMapping.makeOption
        | TypeKind.ENUM when tref.Name.IsSome ->
            match enumProvidedTypes.TryFind(tref.Name.Value) with
            | Some providedEnum -> TypeMapping.makeOption providedEnum
            | None -> failwithf "Could not find a enum type based on a type reference. The reference is an \"%s\" enum, but that enum was not found in the introspection schema." tref.Name.Value
        | (TypeKind.OBJECT | TypeKind.INTERFACE | TypeKind.UNION) when tref.Name.IsSome ->
            if providedTypes.ContainsKey(path, tref.Name.Value)
            then TypeMapping.makeOption providedTypes.[path, tref.Name.Value]
            else
                let getIntrospectionFields typeName =
                    if schemaTypes.ContainsKey(typeName)
                    then schemaTypes.[typeName].Fields |> Option.defaultValue [||]
                    else failwithf "Could not find a schema type based on a type reference. The reference is to a \"%s\" type, but that type was not found in the schema types." typeName
                let getPropertyMetadata typeName (info : AstFieldInfo) : RecordPropertyMetadata =
                    let ifield =
                        match getIntrospectionFields typeName |> Array.tryFind(fun f -> f.Name = info.Name) with
                        | Some ifield -> ifield
                        | None -> failwithf "Could not find field \"%s\" of type \"%s\". The schema type does not have a field with the specified name." info.Name tref.Name.Value
                    let path = info.AliasOrName :: path
                    let astFields = info.Fields
                    let ftype = getProvidedType providedTypes schemaTypes path astFields ifield.Type
                    { Name = info.Name; Alias = info.Alias; Description = ifield.Description; DeprecationReason = ifield.DeprecationReason; Type = ftype }
                let fragmentProperties =
                    astFields
                    |> List.choose (function FragmentField f when f.TypeCondition <> tref.Name.Value -> Some f | _ -> None)
                    |> List.groupBy (fun field -> field.TypeCondition)
                    |> List.map (fun (typeCondition, fields) ->
                        let conditionFields = fields |> List.distinctBy (fun x -> x.AliasOrName) |> List.map FragmentField
                        typeCondition, List.map (getPropertyMetadata typeCondition) conditionFields)
                let baseProperties =
                    astFields
                    |> List.choose (fun x ->
                        match x with
                        | TypeField _ -> Some x
                        | FragmentField f when f.TypeCondition = tref.Name.Value -> Some x
                        | _ -> None)
                    |> List.distinctBy (fun x -> x.AliasOrName)
                    |> List.map (getPropertyMetadata tref.Name.Value)
                let baseType =
                    let metadata : ProvidedTypeMetadata = { Name = tref.Name.Value; Description = tref.Description }
                    let tdef = preBuildProvidedRecordType(metadata, None)
                    providedTypes.Add((path, tref.Name.Value), tdef)
                    includeType path tdef
                    makeProvidedRecordType(tdef, baseProperties, explicitOptionalParameters)
                let createFragmentType (typeName, properties) =
                    let itype =
                        if schemaTypes.ContainsKey(typeName)
                        then schemaTypes.[typeName]
                        else failwithf "Could not find schema type based on the query. Type \"%s\" does not exist on the schema definition." typeName
                    let metadata : ProvidedTypeMetadata = { Name = itype.Name; Description = itype.Description }
                    let tdef = preBuildProvidedRecordType(metadata, Some (upcast baseType))
                    providedTypes.Add((path, typeName), tdef)
                    includeType path tdef
                    makeProvidedRecordType(tdef, properties, explicitOptionalParameters) |> ignore
                fragmentProperties |> List.iter createFragmentType
                TypeMapping.makeOption baseType
        | _ -> failwith "Could not find a schema type based on a type reference. The reference has an invalid or unsupported combination of Name, Kind and OfType fields."
    let operationType = getProvidedType providedTypes schemaTypes [] operationAstFields operationTypeRef
    { OperationType = operationType
      UploadInputTypeName = uploadInputTypeName
      TypeWrapper = rootWrapper }

let getSchemaProvidedTypes(schema : IntrospectionSchema, uploadInputTypeName : string option, explicitOptionalParameters: bool) =
    let providedTypes = ref Map.empty<TypeName, ProvidedTypeDefinition>
    let schemaTypes = TypeMapping.getSchemaTypes schema
    let getSchemaType (tref : IntrospectionTypeRef) =
        match tref.Name with
        | Some name ->
            match schemaTypes.TryFind(name) with
            | Some itype -> itype
            | None -> failwithf "Type \"%s\" was not found on the schema custom types." name
        | None -> failwith "Expected schema type to have a name, but it does not have one."
    let typeModifier (modifier : Type -> Type) (metadata : RecordPropertyMetadata) = { metadata with Type = modifier metadata.Type }
    let makeOption = typeModifier TypeMapping.makeOption
    let makeArrayOption = typeModifier (TypeMapping.makeArray >> TypeMapping.makeOption)
    let unwrapOption = typeModifier TypeMapping.unwrapOption
    let ofFieldType (field : IntrospectionField) = { field with Type = field.Type.OfType.Value }
    let ofInputFieldType (field : IntrospectionInputVal) = { field with Type = field.Type.OfType.Value }
    let rec resolveFieldMetadata (field : IntrospectionField) : RecordPropertyMetadata =
        match field.Type.Kind with
        | TypeKind.SCALAR when field.Type.Name.IsSome ->
            let providedType = TypeMapping.mapScalarType uploadInputTypeName field.Type.Name.Value
            { Name = field.Name
              Alias = None
              Description = field.Description
              DeprecationReason = field.DeprecationReason
              Type = providedType }
            |> makeOption
        | _ when uploadInputTypeName.IsSome && field.Type.Name.IsSome && uploadInputTypeName.Value = field.Type.Name.Value -> uploadTypeIsNotScalar uploadInputTypeName.Value
        | TypeKind.NON_NULL when field.Type.Name.IsNone && field.Type.OfType.IsSome -> ofFieldType field |> resolveFieldMetadata |> unwrapOption
        | TypeKind.LIST when field.Type.Name.IsNone && field.Type.OfType.IsSome ->  ofFieldType field |> resolveFieldMetadata |> makeArrayOption
        | (TypeKind.OBJECT | TypeKind.INTERFACE | TypeKind.INPUT_OBJECT | TypeKind.UNION | TypeKind.ENUM) when field.Type.Name.IsSome ->
            let itype = getSchemaType field.Type
            let providedType = resolveProvidedType itype
            { Name = field.Name
              Alias = None
              Description = field.Description
              DeprecationReason = field.DeprecationReason
              Type = providedType }
            |> makeOption
        | _ -> failwith "Could not find a schema type based on a type reference. The reference has an invalid or unsupported combination of Name, Kind and OfType fields."
    and resolveInputFieldMetadata (field : IntrospectionInputVal) : RecordPropertyMetadata =
        match field.Type.Kind with
        | TypeKind.SCALAR when field.Type.Name.IsSome ->
            let providedType = TypeMapping.mapScalarType uploadInputTypeName field.Type.Name.Value
            { Name = field.Name
              Alias = None
              Description = field.Description
              DeprecationReason = None
              Type = providedType }
            |> makeOption
        | _ when uploadInputTypeName.IsSome && field.Type.Name.IsSome && uploadInputTypeName.Value = field.Type.Name.Value -> uploadTypeIsNotScalar uploadInputTypeName.Value
        | TypeKind.NON_NULL when field.Type.Name.IsNone && field.Type.OfType.IsSome -> ofInputFieldType field |> resolveInputFieldMetadata |> unwrapOption
        | TypeKind.LIST when field.Type.Name.IsNone && field.Type.OfType.IsSome ->  ofInputFieldType field |> resolveInputFieldMetadata |> makeArrayOption
        | (TypeKind.OBJECT | TypeKind.INTERFACE | TypeKind.INPUT_OBJECT | TypeKind.UNION | TypeKind.ENUM) when field.Type.Name.IsSome ->
            let itype = getSchemaType field.Type
            let providedType = resolveProvidedType itype
            { Name = field.Name
              Alias = None
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
                let tdef = preBuildProvidedRecordType(metadata, None)
                providedTypes := (!providedTypes).Add(itype.Name, tdef)
                let properties =
                    itype.Fields
                    |> Option.defaultValue [||]
                    |> Array.map resolveFieldMetadata
                    |> List.ofArray
                upcast makeProvidedRecordType(tdef, properties, explicitOptionalParameters)
            | TypeKind.INPUT_OBJECT ->
                let tdef = preBuildProvidedRecordType(metadata, None)
                providedTypes := (!providedTypes).Add(itype.Name, tdef)
                let properties =
                    itype.InputFields
                    |> Option.defaultValue [||]
                    |> Array.map resolveInputFieldMetadata
                    |> List.ofArray
                upcast makeProvidedRecordType(tdef, properties, explicitOptionalParameters)
            | TypeKind.INTERFACE | TypeKind.UNION ->
                let bdef = makeProvidedInterfaceType(metadata)
                providedTypes := (!providedTypes).Add(itype.Name, bdef)
                bdef
            | TypeKind.ENUM ->
                let items =
                    match itype.EnumValues with
                    | Some values -> values |> Array.map (fun value -> value.Name)
                    | None -> [||]
                let tdef = makeProvidedEnumType(itype.Name, items)
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
    |> Seq.iter (fun kvp ->
        if kvp.Value.Kind = TypeKind.INTERFACE || kvp.Value.Kind = TypeKind.UNION then
            let itype = getProvidedType kvp.Value.Name
            let ptypes = possibleTypes kvp.Value
            ptypes |> Array.iter (fun ptype -> ptype.AddInterfaceImplementation(itype)))
    !providedTypes

let private loadSchema (introspectionLocation: IntrospectionLocation) (httpHeaders: seq<string * string>)=
    let schemaJson =
        match introspectionLocation with
        | Uri serverUrl ->
            use connection = new GraphQLClientConnection()
            GraphQLClient.sendIntrospectionRequest connection serverUrl httpHeaders
        | IntrospectionFile path ->
            System.IO.File.ReadAllText path
    Serialization.deserializeSchema schemaJson

#if IS_DESIGNTIME
let throwExceptionIfValidationFailed (validationResult : ValidationResult<AstError>) =
    let rec formatValidationExceptionMessage(errors : AstError list) =
        match errors with
        | [] -> "Query validation resulted in invalid query, but no validation messages were produced."
        | errors ->
            errors
            |> List.map (fun err ->
                match err.Path with
                | Some path -> sprintf "%s Path: %A" err.Message path
                | None -> err.Message)
            |> List.reduce (fun x y -> x + Environment.NewLine + y)
    match validationResult with
    | ValidationError msgs -> failwith (formatValidationExceptionMessage msgs)
    | Success -> ()
#endif

let rec private getKind (tref : IntrospectionTypeRef) =
    match tref.Kind with
    | TypeKind.NON_NULL | TypeKind.LIST  when tref.OfType.IsSome -> getKind tref.OfType.Value
    | _ -> tref.Kind

let rec private getTypeName (tref: IntrospectionTypeRef) =
    match tref.Kind with
    | TypeKind.NON_NULL | TypeKind.LIST when tref.OfType.IsSome -> getTypeName tref.OfType.Value
    | _ ->
        match tref.Name with
        | Some tname -> tname
        | None -> failwithf "Expected type kind \"%s\" to have a name, but it does not have a name." (tref.Kind.ToString())

let rec private getIntrospectionType (schemaTypes: Map<string,IntrospectionType>) (tref: IntrospectionTypeRef) =
    match tref.Kind with
    | TypeKind.NON_NULL | TypeKind.LIST when tref.OfType.IsSome -> getIntrospectionType schemaTypes tref.OfType.Value
    | _ ->
        let typeName = getTypeName tref
        match schemaTypes.TryFind(typeName) with
        | Some t -> t
        | None -> failwithf "Type \"%s\" was not found in the introspection schema." typeName

let private getOperationFields (schemaTypes: Map<string,IntrospectionType>) (operationAstFields : AstFieldInfo list) (operationType : IntrospectionType) =
    let rec helper (acc : SchemaFieldInfo list) (astFields : AstFieldInfo list) (introspectionType : IntrospectionType) =
        match introspectionType.Kind with
        | TypeKind.OBJECT | TypeKind.INTERFACE | TypeKind.UNION ->
            match astFields with
            | [] -> acc
            | field :: tail ->
                let throw typeName = failwithf "Field \"%s\" of type \"%s\" was not found in the introspection schema." field.Name typeName
                let tref =
                    match field with
                    | FragmentField fragf ->
                        let fragmentType =
                            let tref =
                                Option.defaultValue [||] introspectionType.PossibleTypes
                                |> Array.map (getIntrospectionType schemaTypes)
                                |> Array.append [|introspectionType|]
                                |> Array.tryFind (fun pt -> pt.Name = fragf.TypeCondition)
                            match tref with
                            | Some t -> t
                            | None -> failwithf "Fragment field defines a type condition \"%s\", but that type was not found in the schema definition." fragf.TypeCondition
                        let field =
                            fragmentType.Fields
                            |> Option.map (Array.tryFind (fun f -> f.Name = fragf.Name))
                            |> Option.flatten
                        match field with
                        | Some f -> f.Type
                        | None -> throw fragmentType.Name
                    | TypeField typef ->
                        let field =
                            introspectionType.Fields
                            |> Option.map (Array.tryFind (fun f -> f.Name = typef.Name))
                            |> Option.flatten
                        match field with
                        | Some f -> f.Type
                        | None -> throw introspectionType.Name
                let fields =
                    match getKind tref with
                    | TypeKind.OBJECT | TypeKind.INTERFACE | TypeKind.UNION ->
                        let schemaType = getIntrospectionType schemaTypes tref
                        helper [] field.Fields schemaType
                    | _ -> []
                let info = { AliasOrName = field.AliasOrName.FirstCharUpper(); SchemaTypeRef = tref; Fields = Array.ofList fields }
                helper (info :: acc) tail introspectionType
        | _ -> []
    helper [] operationAstFields operationType |> Array.ofList

let private makeOperationMethodDef
    (providerSettings: ProviderSettings) (schema: IntrospectionSchema)
    (schemaProvidedTypes: Map<TypeName, ProvidedTypeDefinition>)
    (httpHeaders: seq<string * string>) (operationWrapper: ProvidedTypeDefinition) =
    let staticParams =
        [ ProvidedStaticParameter("query", typeof<string>)
          ProvidedStaticParameter("resolutionFolder", typeof<string>, parameterDefaultValue = providerSettings.ResolutionFolder)
          ProvidedStaticParameter("operationName", typeof<string>, parameterDefaultValue = "")
          ProvidedStaticParameter("typeName", typeof<string>, parameterDefaultValue = "") ]
    let staticMethodDef = ProvidedMethod("Operation", [], typeof<OperationBase>, isStatic = true)
    let instanceBuilder (methodName : string) (args : obj []) =
        let queryLocation = StringLocation.Create(downcast args.[0], downcast args.[1])
        let query =
            match queryLocation with
            | String query -> query
            | File path -> System.IO.File.ReadAllText(path)
        let queryAst = Parser.parse query
#if IS_DESIGNTIME
        let key = { DocumentId = queryAst.GetHashCode(); SchemaId = schema.GetHashCode() }
        let refMaker = lazy Validation.Ast.validateDocument schema queryAst
        if providerSettings.ClientQueryValidation then
            refMaker.Force
            |> QueryValidationDesignTimeCache.getOrAdd key
            |> throwExceptionIfValidationFailed
#endif
        let operationName : OperationName option =
            match args.[2] :?> string with
            | null | "" ->
                let operationDefinitions = queryAst.Definitions |> List.filter (function OperationDefinition _ -> true | _ -> false)
                match operationDefinitions with
                | opdef :: _ -> opdef.Name
                | _ -> failwith "Error parsing query. Can not choose a default operation: query document has no operation definitions."
            | x -> Some x
        let explicitOperationTypeName : TypeName option =
            match args.[3] :?> string with
            | null | "" -> None
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
        let schemaTypes = TypeMapping.getSchemaTypes schema
        let enumProvidedTypes = schemaProvidedTypes |> Map.filter (fun _ t -> t.BaseType = typeof<EnumBase>)
        let actualQuery = queryAst.ToQueryString(QueryStringPrintingOptions.IncludeTypeNames).Replace("\r\n", "\n")
        let className =
            match explicitOperationTypeName, operationDefinition.Name with
            | Some name, _ -> name.FirstCharUpper()
            | None, Some name -> name.FirstCharUpper()
            | None, None -> "Operation" + actualQuery.MD5Hash()
        let metadata = getOperationMetadata(schemaTypes, providerSettings.UploadInputTypeName, enumProvidedTypes, operationAstFields, operationTypeRef, providerSettings.ExplicitOptionalParameters)
        let operationTypeName : TypeName =
            match operationTypeRef.Name with
            | Some name -> name
            | None -> failwith "Error parsing query. Operation type does not have a name."

        // Every time we run the query, we will need the schema types information as an expression.
        // To avoid creating the type map expression every time we call Run method, we cache it here.
        let introspectionType = getIntrospectionType schemaTypes operationTypeRef
        let operationFieldsExpr =
            let fields = getOperationFields schemaTypes operationAstFields introspectionType
            fields |> QuotationHelpers.arrayExpr |> snd
        let contextInfo : GraphQLRuntimeContextInfo option =
            match providerSettings.IntrospectionLocation with
            | Uri serverUrl -> Some { ServerUrl = serverUrl; HttpHeaders = httpHeaders }
            | _ -> None
        let operationDef = makeProvidedOperationType(actualQuery, operationDefinition, operationTypeName, operationFieldsExpr, schemaTypes, schemaProvidedTypes, metadata.OperationType, contextInfo, metadata.UploadInputTypeName, className, providerSettings.ExplicitOptionalParameters)
        operationDef.AddMember(metadata.TypeWrapper)
        let invoker (_ : Expr list) = <@@ OperationBase(query) @@>
        let methodDef = ProvidedMethod(methodName, [], operationDef, invoker, isStatic = true)
        methodDef.AddXmlDoc("Creates an operation to be executed on the server and provide its return types.")
        operationWrapper.AddMember(operationDef)
        operationDef.AddMember(methodDef)
        methodDef
    staticMethodDef.DefineStaticParameters(staticParams, instanceBuilder)
    staticMethodDef

let private makeRootType (asm: Assembly) (ns:string) (tname:string) (providerSettings: ProviderSettings) =
    let tdef = ProvidedTypeDefinition(asm, ns, tname, None)
    tdef.AddXmlDoc("A type provider for GraphQL operations.")
    tdef.AddMembersDelayed (fun _ ->
        let httpHeaders = HttpHeaders.load providerSettings.CustomHttpHeadersLocation
        let schema = loadSchema providerSettings.IntrospectionLocation httpHeaders
        let schemaProvidedTypes = getSchemaProvidedTypes(schema, providerSettings.UploadInputTypeName, providerSettings.ExplicitOptionalParameters)
        let typeWrapper = ProvidedTypeDefinition("Types", None, isSealed = true)
        typeWrapper.AddMembers(schemaProvidedTypes |> Seq.map (fun kvp -> kvp.Value) |> List.ofSeq)
        let operationWrapper = ProvidedTypeDefinition("Operations", None, isSealed = true)
        let getContextMethodDef =
            let methodParameters =
                let serverUrl =
                    match providerSettings.IntrospectionLocation with
                    | Uri serverUrl -> ProvidedParameter("serverUrl", typeof<string>, optionalValue = serverUrl)
                    | _ -> ProvidedParameter("serverUrl", typeof<string>)
                let httpHeaders = ProvidedParameter("httpHeaders", typeof<seq<string * string>>, optionalValue = null)
                let connectionFactory = ProvidedParameter("connectionFactory", typeof<unit -> GraphQLClientConnection>, optionalValue = null)
                [serverUrl; httpHeaders; connectionFactory]
            let defaultHttpHeadersExpr =
                let names = httpHeaders |> Seq.map fst |> Array.ofSeq
                let values = httpHeaders |> Seq.map snd |> Array.ofSeq
                Expr.Coerce(<@@ Array.zip names values @@>, typeof<seq<string * string>>)
            let invoker (args : Expr list) =
                let serverUrl = args.[0]
                <@@ let httpHeaders =
                        match %%args.[1] : seq<string * string> with
                        | null -> %%defaultHttpHeadersExpr
                        | argHeaders -> argHeaders
                    let connectionFactory =
                        match %%args.[2] : unit -> GraphQLClientConnection with
                        | argHeaders when obj.Equals(argHeaders, null) -> fun () -> new GraphQLClientConnection()
                        | argHeaders -> argHeaders
                    { ServerUrl = %%serverUrl; HttpHeaders = httpHeaders; Connection = connectionFactory() } @@>
            ProvidedMethod("GetContext", methodParameters, typeof<GraphQLProviderRuntimeContext>, invoker, isStatic = true)
        let operationMethodDef =
            makeOperationMethodDef providerSettings schema schemaProvidedTypes httpHeaders operationWrapper
        let schemaPropertyDef =
            let getter = QuotationHelpers.quoteRecord schema (fun (_ : Expr list) schema -> schema)
            ProvidedProperty("Schema", typeof<IntrospectionSchema>, getter, isStatic = true)
        let members : MemberInfo list = [typeWrapper; operationWrapper; getContextMethodDef; operationMethodDef; schemaPropertyDef]
        members)
    tdef

let makeGraphQLProvider(asm : Assembly, ns : string, resolutionFolder : string) =
    let generator = ProvidedTypeDefinition(asm, ns, "GraphQLProvider", None)
    let staticParams =
        [ ProvidedStaticParameter("introspection", typeof<string>)
          ProvidedStaticParameter("httpHeaders", typeof<string>, parameterDefaultValue = "")
          ProvidedStaticParameter("resolutionFolder", typeof<string>, parameterDefaultValue = resolutionFolder)
          ProvidedStaticParameter("uploadInputTypeName", typeof<string>, parameterDefaultValue = "")
          ProvidedStaticParameter("clientQueryValidation", typeof<bool>, parameterDefaultValue = true)
          ProvidedStaticParameter("explicitOptionalParameters", typeof<bool>, parameterDefaultValue = false) ]
    generator.DefineStaticParameters(staticParams, fun tname args ->
        let clientQueryValidation : bool = downcast args.[4]
        let explicitOptionalParameters : bool = downcast args.[5]
        let introspectionLocation = IntrospectionLocation.Create(downcast args.[0], downcast args.[2])
        let httpHeadersLocation = StringLocation.Create(downcast args.[1], resolutionFolder)
        let uploadInputTypeName =
            let name : string = unbox args.[3]
            match name with
            | null | "" -> None
            | _ -> Some name
        let providerSettings =
            { IntrospectionLocation = introspectionLocation
              CustomHttpHeadersLocation = httpHeadersLocation
              UploadInputTypeName = uploadInputTypeName
              ResolutionFolder = resolutionFolder
              ClientQueryValidation = clientQueryValidation
              ExplicitOptionalParameters = explicitOptionalParameters }
        let maker = lazy makeRootType asm ns tname providerSettings
#if IS_DESIGNTIME
        ProviderDesignTimeCache.getOrAdd providerSettings maker.Force
#else
        maker.Force()
#endif
    )
    generator