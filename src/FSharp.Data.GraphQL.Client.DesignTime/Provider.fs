/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Provider

open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions
open FSharp.Core
open System.Reflection
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Client
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types.Introspection
open ProviderImplementation.ProvidedTypes
open FSharp.Quotations

#nowarn "10001"

let private loadSchema (introspectionLocation: IntrospectionLocation) (httpHeaders: seq<string * string>)=
    let schemaJson =
        match introspectionLocation with
        | Uri serverUrl ->
            use connection = new GraphQLClientConnection()
            GraphQLClient.sendIntrospectionRequest connection serverUrl httpHeaders
        | IntrospectionFile path ->
            System.IO.File.ReadAllText path
    Serialization.deserializeSchema schemaJson

let importsRegex = Regex(@"^\s*#import\s+(?<file>.*)",  RegexOptions.Compiled ||| RegexOptions.Multiline)
let parseImports (query: string) =
    let matches = importsRegex.Matches(query)
    [ for import in matches do
         let file = import.Groups.["file"]
         file.Value.Trim(' ', '\'', '"') ]

let loadFragmentFromPath (file: string) =
    if File.Exists file then
        let content = File.ReadAllText file
        let imports = parseImports content
        let document = Parser.parse content
        imports, document
    else
        failwithf "Fragment file does not exist on disk %s" file

type ProvidedFragmentMetadata = unit

let rec buildFragmentType (selectionSet: Selection list) (fragment: ProvidedTypeDefinition) (parent: ProvidedTypeDefinition) =
    for selection in selectionSet do
        match selection with
        | InlineFragment ast ->
            match ast.TypeCondition with
            | Some typeCondition ->
                let subFragmentInterface = ProvidedTypeDefinition(typeCondition, Some(fragment :> _))
                fragment.AddMember subFragmentInterface
            | None ->
                ()
        | Field field ->
            ()
            //fragmentInterface.AddMember(ProvidedProperty("Foo", typeof<string>, isStatic=false))
            //.AddMember(ProvidedProperty(property.Name, property.PropertyType))
        | FragmentSpread fragmentSpread ->
            ()
    parent.AddMember(fragment)

let rec makeFragmentInterface
    (file: string) (schema: IntrospectionSchema)
    (schemaProvidedTypes: Map<TypeName, ProvidedTypeDefinition>)
    (fragmentWrapper: ProvidedTypeDefinition)
    (cache: Map<string, ProvidedFragmentMetadata>) =
    let imports, document = loadFragmentFromPath file
    [ for node in document.Definitions do
        match node with
        | FragmentDefinition definition ->
            match definition.Name, definition.TypeCondition with
            | Some name, Some typeCondition ->
                let fragmentType = ProvidedTypeDefinition(name, None, isSealed=false)
                let providedType =
                    match schemaProvidedTypes.TryFind typeCondition with
                    | Some providedType -> providedType
                    | None -> failwithf "Fragment \"%s\" defined in \"%s\" refers to a type \"%s\" that does not exist in the introspection schema." name file typeCondition
                ()
            | _, _ ->
                ()
        | OperationDefinition op ->
            ()
    ]

let private makeFragmentInterfaces
    (schema: IntrospectionSchema)
    (schemaProvidedTypes: Map<TypeName, ProvidedTypeDefinition>)
    (fragmentWrapper: ProvidedTypeDefinition)
    (providerSettings: ProviderSettings) =
    match providerSettings.FragmentsFolder with
    | Some folder ->
        let fragmentFiles = Directory.GetFiles(folder, "*.graphql")
        for file in fragmentFiles do
            makeFragmentInterface file schema schemaProvidedTypes fragmentWrapper Map.empty |> ignore
    | None ->
        ()

let private makeOperationMethodDef
    (providerSettings: ProviderSettings) (schema: IntrospectionSchema)
    (schemaProvidedTypes: IDictionary<TypeName, ProvidedTypeDefinition>)
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
        let enumProvidedTypes =
            dict [ for KeyValue(name, providedType) in schemaProvidedTypes do
                    if providedType.BaseType = typeof<EnumBase> then
                        name, providedType ]

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


let private makeRootType (asm: Assembly) (ns: string) (tname: string) (providerSettings: ProviderSettings) =
    let tdef = ProvidedTypeDefinition(asm, ns, tname, Some typeof<obj>)
    tdef.AddXmlDoc("A type provider for GraphQL operations.")
    tdef.AddMembersDelayed (fun () ->
        let httpHeaders = HttpHeaders.load providerSettings.CustomHttpHeadersLocation
        let schema = loadSchema providerSettings.IntrospectionLocation httpHeaders
        let schemaCompiler = SchemaGenerator(schema,
                                ?uploadInputTypeName = providerSettings.UploadInputTypeName,
                                explicitOptionalParameters = providerSettings.ExplicitOptionalParameters)
        let schemaProvidedTypes = schemaCompiler.GetProvidedTypes()
        let schemaProvidedTypesList = schemaProvidedTypes |> Seq.map (fun kvp -> kvp.Value) |> List.ofSeq
        let typeWrapper = ProvidedTypeDefinition("Types", Some typeof<obj>, isSealed = true)
        typeWrapper.AddMembers schemaProvidedTypesList


        let fragmentsWrapper = ProvidedTypeDefinition("Fragments", None, isSealed = true)
        //makeFragmentInterfaces schema schemaProvidedTypes fragmentsWrapper providerSettings

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
        let members : MemberInfo list = [typeWrapper; operationWrapper; fragmentsWrapper; getContextMethodDef; operationMethodDef; schemaPropertyDef]
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
          ProvidedStaticParameter("explicitOptionalParameters", typeof<bool>, parameterDefaultValue = false)
          ProvidedStaticParameter("fragmentsFolder", typeof<string>, parameterDefaultValue="") ]
    generator.DefineStaticParameters(staticParams, fun tname args ->
        let clientQueryValidation : bool = downcast args.[4]
        let explicitOptionalParameters : bool = downcast args.[5]
        let introspectionLocation = IntrospectionLocation.Create(downcast args.[0], downcast args.[2])
        let httpHeadersLocation = StringLocation.Create(downcast args.[1], resolutionFolder)
        let uploadInputTypeName =
            match args.[3] with
            | :? string as name
                when not(String.IsNullOrEmpty name) -> Some name
            | _ -> None
        let fragmentsFolder =
            match args.[6] with
            | :? string as folder
                when not(String.IsNullOrEmpty folder) ->
                    Some(Path.Combine(resolutionFolder, folder))
            | _ -> None
        let providerSettings =
            { IntrospectionLocation = introspectionLocation
              CustomHttpHeadersLocation = httpHeadersLocation
              UploadInputTypeName = uploadInputTypeName
              ResolutionFolder = resolutionFolder
              ClientQueryValidation = clientQueryValidation
              ExplicitOptionalParameters = explicitOptionalParameters
              FragmentsFolder = fragmentsFolder }
        let maker = lazy makeRootType asm ns tname providerSettings
#if IS_DESIGNTIME
        ProviderDesignTimeCache.getOrAdd providerSettings maker.Force
#else
        maker.Force()
#endif
    )
    generator