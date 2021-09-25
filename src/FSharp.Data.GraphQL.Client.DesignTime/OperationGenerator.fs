namespace FSharp.Data.GraphQL

open System
open System.IO
open System.Text
open FSharp.Core
open System.Reflection
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Client
open FSharp.Data.GraphQL.Client.DesignTimeSerialization
open FSharp.Data.GraphQL.Client.ReflectionPatterns
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Validation
open System.Collections.Generic
open FSharp.Data.GraphQL.Types.Introspection
open FSharp.Data.GraphQL.Ast.Extensions
open FSharp.Data.GraphQL.ProvidedSchema
open ProviderImplementation.ProvidedTypes
open FSharp.Quotations

#nowarn "10001"

type internal ProvidedOperationMetadata =
    { OperationType : Type
      UploadInputTypeName : string option
      TypeWrapper : ProvidedTypeDefinition }

[<AutoOpen>]
module private Operations =

    let makeProvidedOperationResultType (operationType : Type) =
        let tdef = ProvidedTypeDefinition("OperationResult", Some typeof<OperationResultBase>, nonNullable = true)
        tdef.AddMemberDelayed(fun _ ->
            let getterCode (args : Expr list) =
                <@@ let this = %%args.[0] : OperationResultBase
                    this.RawData @@>
            let prop = ProvidedProperty("Data", operationType, getterCode)
            prop.AddXmlDoc("Contains the data returned by the operation on the server.")
            prop)
        tdef

    let convertToClrTypes (schemaGenerator: SchemaGenerator) (variables: VariableDefinition list) =
        let schemaTypes = schemaGenerator.SchemaTypes
        let isCustomScalar (typeName: string) =
            match schemaTypes.TryFindType typeName with
            | Some introspectionType -> introspectionType.Kind = TypeKind.SCALAR
            | None -> false
        let getSchemaType (typeName: string) =
            match schemaTypes.UploadInputType with
            | Some uploadInputTypeName when typeName = uploadInputTypeName.Name ->
                typeof<Upload>
            | _ ->
                match TypeMapping.scalar.TryFind(typeName) with
                | Some t -> t
                | None when isCustomScalar typeName -> typeof<string>
                | None ->
                    match schemaGenerator.ProvidedTypes.TryGetValue(typeName) with
                    | true, t -> t :> Type
                    | false, _ -> failwithf "Unable to find variable type \"%s\" in the schema definition." typeName
        let rec mapVariable (variableType : InputType) (isNullable: bool) =
            match variableType with
            | NamedType typeName when isNullable -> getSchemaType typeName |> TypeMapping.makeOption
            | NamedType typeName -> getSchemaType typeName
            | ListType itype when isNullable -> mapVariable itype true |> TypeMapping.makeArray |> TypeMapping.makeOption
            | ListType itype -> mapVariable itype true |> TypeMapping.makeArray
            | NonNullType itype -> mapVariable itype false
        List.map (fun vdef -> vdef.VariableName, mapVariable vdef.Type true) variables


    // We need to use the combination strategy to generate overloads for variables in the Run/AsyncRun methods.
    // The strategy follows the same principle with ProvidedRecord constructor overloads,
    // except that we also must create one additional overload for the runtime context, for each already existent overload,
    // if no default context is provided.
    let createRunMethodOverloads contextInfo explicitOptionalParameters clrVariables =
        let overloadsWithoutContext =
            let optionalVariables, requiredVariables =
                clrVariables |> List.partition (fun (_, t) -> isOption t)
            if explicitOptionalParameters then
                [requiredVariables @ optionalVariables]
            else
                optionalVariables
                |> List.combinations
                |> List.map (fun (optionalVariables, _) ->
                    let optionalVariables = optionalVariables |> List.map (fun (name, t) -> name, TypeMapping.unwrapOption t)
                    requiredVariables @ optionalVariables)
        let overloadsWithContext =
            overloadsWithoutContext
            |> List.map (fun var -> ("runtimeContext", typeof<GraphQLProviderRuntimeContext>) :: var)
        match contextInfo with
        | Some _ -> overloadsWithoutContext @ overloadsWithContext
        | None -> overloadsWithContext

    let rec existsUploadType (foundTypes : ProvidedTypeDefinition list) (t : Type) =
        match t with
        | :? ProvidedTypeDefinition as tdef when not (List.contains tdef foundTypes) ->
            tdef.DeclaredProperties |> Seq.exists ((fun p -> p.PropertyType) >> existsUploadType (tdef :: foundTypes))
        | Option t ->
            existsUploadType foundTypes t
        | Array t ->
            existsUploadType foundTypes t
        | _ ->
            t = typeof<Upload>

    let makeProvidedOperationType
        (actualQuery: string, operationDefinition: OperationDefinition, operationTypeName: string,
         schemaGenerator: SchemaGenerator, operationType: Type, selections: list<AstFieldInfo>,
         contextInfo: GraphQLRuntimeContextInfo option, className: string, explicitOptionalParameters: bool) =

        let tdef = ProvidedTypeDefinition(className, Some typeof<OperationBase>)
        tdef.AddXmlDoc("Represents a GraphQL operation on the server.")
        tdef.AddMembersDelayed(fun _ ->
            let operationResultDef = makeProvidedOperationResultType operationType
            let clrVariables = convertToClrTypes schemaGenerator operationDefinition.VariableDefinitions
            let defaultContextExpr =
                match contextInfo with
                | Some info ->
                    let serverUrl = info.ServerUrl
                    let headerNames = info.HttpHeaders |> Seq.map fst |> Array.ofSeq
                    let headerValues = info.HttpHeaders |> Seq.map snd |> Array.ofSeq
                    <@@ { ServerUrl = serverUrl; HttpHeaders = Array.zip headerNames headerValues; Connection = new GraphQLClientConnection() } @@>
                | None ->
                    <@@ Unchecked.defaultof<GraphQLProviderRuntimeContext> @@>

            let methodOverloadDefinitions = createRunMethodOverloads contextInfo explicitOptionalParameters clrVariables
            // Multipart requests should only be used when the user specifies a upload type name AND the type
            // is present in the query as an input value. If not, we fallback to classic requests.
            let shouldUseMultipartRequest = clrVariables |> Seq.exists (snd >> existsUploadType [])
            let jsonParser = getOperationJsonReader schemaGenerator.SchemaTypes selections operationDefinition
            let runMethodOverloads : MemberInfo list =
                let operationName = Option.toObj operationDefinition.Name
                [ for overloadParameters in methodOverloadDefinitions do
                    let variableNames =
                        [ for (name, _) in overloadParameters do
                            if name <> "runtimeContext" then
                                name ]
                    let invoker (isAsync: bool) (args : Expr list) =
                        // First arg is the operation instance, second should be the context, if the overload asks for one.
                        // We determine it by seeing if the variable names have one less item than the arguments without the instance.
                        let argsWithoutInstance = args.Tail
                        let variableArgs, isDefaultContext, context =
                            if argsWithoutInstance.Length - variableNames.Length = 1
                            then argsWithoutInstance.Tail, false, argsWithoutInstance.Head
                            else argsWithoutInstance, true, defaultContextExpr
                        let variables =
                            let elements =
                                variableArgs
                                |> List.skip (variableArgs.Length - clrVariables.Length)
                                |> List.map2(fun name value ->
                                    let expr = Expr.Coerce(value, typeof<obj>)
                                    <@@ name, %%expr @@>) variableNames
                            Expr.NewArray(typeof<string * obj>, elements)
                        let runExpr =
                            <@ let context = %%context : GraphQLProviderRuntimeContext
                               let request =
                                    { ServerUrl = context.ServerUrl; HttpHeaders = context.HttpHeaders
                                      OperationName = Option.ofObj operationName; Query = actualQuery
                                      Variables = %%variables }
                               async {
                                    let! response =
                                        if shouldUseMultipartRequest
                                        then Tracer.asyncRunAndMeasureExecutionTime "Ran a multipart GraphQL query request asynchronously" (fun _ -> GraphQLClient.sendMultipartRequestAsync context.Connection request)
                                        else Tracer.asyncRunAndMeasureExecutionTime "Ran a GraphQL query request asynchronously" (fun _ -> GraphQLClient.sendRequestAsync context.Connection request)
                                    // If the user does not provide a context, we should dispose the default one after running the query
                                    if isDefaultContext then (context :> IDisposable).Dispose()
                                    return OperationResultBase(response, %%jsonParser, operationTypeName)
                               } @>
                        if isAsync then
                            runExpr :> Expr
                        else
                            <@@ %runExpr |> Async.RunSynchronously @@>

                    let methodParameters = overloadParameters |> List.map (fun (name, t) -> ProvidedParameter(name, t, ?optionalValue = if isOption t then Some null else None))

                    let asyncMethodDef = ProvidedMethod("AsyncRun", methodParameters, TypeMapping.makeAsync operationResultDef, invoker true)
                    asyncMethodDef.AddXmlDoc("Executes the operation asynchronously on the server and fetch its results.")
                    asyncMethodDef

                    let methodDef = ProvidedMethod("Run", methodParameters, operationResultDef, invoker false)
                    methodDef.AddXmlDoc("Executes the operation synchronously on the server and fetch its results.")
                    methodDef ]

            let parseResultDef =
                let invoker (args : Expr list) =
                    <@@ let bytes = Encoding.UTF8.GetBytes(%%args.[1] : string)
                        let stream = new MemoryStream(bytes)
                        OperationResultBase(stream , %%jsonParser, operationTypeName) @@>
                let parameters = [ProvidedParameter("responseJson", typeof<string>)]
                let methodDef = ProvidedMethod("ParseResult", parameters, operationResultDef, invoker)
                methodDef.AddXmlDoc("Parses a JSON response that matches the response pattern of the current operation into a OperationResult type.")
                methodDef

            [ yield! runMethodOverloads
              operationResultDef
              parseResultDef ])
        tdef


    let makeProvidedType (getWrapper: SchemaTypes -> Path -> ProvidedTypeDefinition) (schemaGenerator: SchemaGenerator) (rootAstFields: AstFieldInfo list) (rootTypeRef: IntrospectionTypeRef) (explicitOptionalParameters: bool): Type =
        let providedTypes = Dictionary<_,_>()
        let includeType (path: string list) (t: ProvidedTypeDefinition) =
            let wrapper = getWrapper schemaGenerator.SchemaTypes path
            wrapper.AddMember(t)
        let uploadInputTypeName = schemaGenerator.SchemaTypes.UploadInputType |> Option.map(fun n -> n.Name)
        let rec makeType (path: string list) (astFields: AstFieldInfo list) (tref: IntrospectionTypeRef) (isNullable: bool) =
            match tref with
            | NamedTypeRef(TypeKind.SCALAR, name) ->
                let scalarType = TypeMapping.mapScalarType uploadInputTypeName name
                if isNullable
                then TypeMapping.makeOption scalarType
                else scalarType
            | NamedTypeRef(TypeKind.ENUM, name) ->
                match schemaGenerator.EnumProvidedTypes.TryGetValue(name) with
                | true, enumType ->
                    if isNullable
                    then TypeMapping.makeOption enumType
                    else enumType :> Type
                | false, _ -> failwithf "Could not find a enum type based on a type reference. The reference is an \"%s\" enum, but that enum was not found in the introspection schema." tref.Name.Value
            | NamedTypeRef((TypeKind.OBJECT | TypeKind.INTERFACE | TypeKind.UNION), name) ->
                match providedTypes.TryGetValue ((path, name)) with
                | true, providedType ->
                    if isNullable
                    then TypeMapping.makeOption providedType
                    else providedType :> Type
                | false, _ ->
                    let getIntrospectionFields typeName =
                        match schemaGenerator.SchemaTypes.TryFindType(typeName) with
                        | Some def -> Option.defaultValue [||] def.Fields
                        | None -> failwithf "Could not find a schema type based on a type reference. The reference is to a \"%s\" type, but that type was not found in the schema types." typeName
                    let getPropertyMetadata typeName (info : AstFieldInfo) : RecordPropertyMetadata =
                        let ifield =
                            match getIntrospectionFields typeName |> Array.tryFind(fun f -> f.Name = info.Name) with
                            | Some ifield -> ifield
                            | None -> failwithf "Could not find field \"%s\" of type \"%s\". The schema type does not have a field with the specified name." info.Name tref.Name.Value
                        let path = info.AliasOrName :: path
                        let astFields = info.Fields
                        let ftype = makeType path astFields ifield.Type true
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

                    for (typeName, properties) in fragmentProperties do
                        match schemaGenerator.SchemaTypes.TryFindType(typeName) with
                        | Some tp ->
                            let metadata = { Name = tp.Name; Description = tp.Description }
                            let tdef = preBuildProvidedRecordType(metadata, Some (upcast baseType))
                            providedTypes.Add((path, typeName), tdef)
                            includeType path tdef
                            makeProvidedRecordType(tdef, properties, explicitOptionalParameters) |> ignore
                        | None ->
                            failwithf "Could not find schema type based on the query. Type \"%s\" does not exist on the schema definition." typeName

                    if isNullable
                    then TypeMapping.makeOption baseType
                    else baseType :> Type
            | ContainerTypeRef(TypeKind.LIST, innerTypeRef) ->
                let elementType = makeType path astFields innerTypeRef true
                let arrayType = TypeMapping.makeArray elementType
                if isNullable
                then TypeMapping.makeOption arrayType
                else arrayType
            | ContainerTypeRef(TypeKind.NON_NULL, innerTypeRef) ->
                makeType path astFields innerTypeRef false
            | typeRef ->
                failwithf "Unsupported Type ref encountered while generating Operation '%A'" typeRef
        makeType [] rootAstFields rootTypeRef true

    let rec getKind (tref : IntrospectionTypeRef) =
        match tref with
        | ContainerTypeRef(_, inner) -> getKind inner
        | _ -> tref.Kind

    let rec getTypeName (tref: IntrospectionTypeRef) =
        match tref with
        | ContainerTypeRef(_, inner) -> getTypeName inner
        | NamedTypeRef(_, name) -> name

    let rec getIntrospectionType (schemaTypes: SchemaTypes) (tref: IntrospectionTypeRef) =
        match tref with
        | ContainerTypeRef(_, inner) ->
            getIntrospectionType schemaTypes inner
        | NamedTypeRef(_, name) ->
            match schemaTypes.TryFindType name with
            | Some t -> t
            | None -> failwithf "Type \"%s\" was not found in the introspection schema." name

type GeneratedOperation =
    { Query: string
      ActualQuery: string
      OperationType: ProvidedTypeDefinition }

type internal OperationGenerator (providerSettings: ProviderSettings, schemaGenerator: SchemaGenerator, httpHeaders: seq<string * string>, operationWrapper: ProvidedTypeDefinition) =
    let wrappersByPath = Dictionary<string list, ProvidedTypeDefinition>()
    let rootWrapper = ProvidedTypeDefinition("Types", Some typeof<obj>, isSealed = true)
    let getWrapper (schemaTypes: SchemaTypes) (path: string list) =
        let rec resolveWrapperName actual =
            if schemaTypes.ContainsType actual
            then resolveWrapperName (actual + "Fields")
            else actual
        let rec getWrapperForPath (path : string list) =
            match wrappersByPath.TryGetValue(path) with
            | true, wrapper ->
                wrapper
            | false, _ ->
                match path with
                | hd::tail ->
                    let typeName = hd.ToTitleCase() + "Fields"
                    let wrapper = ProvidedTypeDefinition(resolveWrapperName typeName, Some typeof<obj>, isSealed = true)
                    let upperWrapper: ProvidedTypeDefinition =
                        if wrappersByPath.ContainsKey(tail)
                        then wrappersByPath.[tail]
                        else getWrapperForPath tail
                    upperWrapper.AddMember(wrapper)
                    wrappersByPath.Add(path, wrapper)
                    wrapper
                | [] ->
                    rootWrapper
        getWrapperForPath path


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

    let tryPickOperation (selectedOperationName: string option) (items: Definition list) =
        match selectedOperationName with
        | Some _ ->
            List.tryPick(fun definition ->
                match definition with
                | OperationDefinition op when op.Name = selectedOperationName ->
                    Some op
                | _ ->
                    None
            ) items
        | None ->
            List.tryPick(fun definition ->
                match definition with
                | OperationDefinition op ->
                    Some op
                | _ ->
                    None
            ) items

    let getRootType (schema: IntrospectionSchema) (operation: OperationDefinition) =
        let tref =
            match operation.OperationType with
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

    let generateOperationDefinition (schema: IntrospectionSchema) (query: string) (queryAst: Document) (operation: OperationDefinition) (explicitOperationTypeName: string option) =
#if IS_DESIGNTIME
        if providerSettings.ClientQueryValidation then
            let key = { DocumentId = queryAst.GetHashCode(); SchemaId = schema.GetHashCode() }
            fun () -> Ast.validateDocument schema queryAst
            |> QueryValidationDesignTimeCache.getOrAdd key
            |> throwExceptionIfValidationFailed
#endif
        let infoMap = queryAst.GetInfoMap()
        let astFields =
            match infoMap.TryFind(operation.Name) with
            | Some fields -> fields
            | None -> failwith "Error parsing query. Could not find field information for requested operation."
        let rootType = getRootType schema operation
        let actualQuery = queryAst.ToQueryString(QueryStringPrintingOptions.IncludeTypeNames).Replace("\r\n", "\n")
        let className =
            match explicitOperationTypeName, operation.Name with
            | Some name, _
            | None, Some name -> name.ToTitleCase()
            | None, None -> "Operation" + actualQuery.MD5Hash()
        let metadata =
            let operationType = makeProvidedType getWrapper schemaGenerator astFields rootType providerSettings.ExplicitOptionalParameters
            let uploadInputTypeName = schemaGenerator.SchemaTypes.UploadInputType |> Option.map (fun t -> t.Name)
            let rootWrapper = getWrapper schemaGenerator.SchemaTypes []
            { OperationType = operationType
              UploadInputTypeName = uploadInputTypeName
              TypeWrapper = rootWrapper }
        let operationTypeName : string =
            match rootType.Name with
            | Some name -> name
            | None -> failwith "Error parsing query. Operation type does not have a name."

        let contextInfo : GraphQLRuntimeContextInfo option =
            match providerSettings.IntrospectionLocation with
            | Uri serverUrl -> Some { ServerUrl = serverUrl; HttpHeaders = httpHeaders }
            | _ -> None
        let operationDef = makeProvidedOperationType(actualQuery, operation, operationTypeName, schemaGenerator, metadata.OperationType, astFields, contextInfo, className, providerSettings.ExplicitOptionalParameters)
        operationDef.AddMember(metadata.TypeWrapper)
        { Query = query
          ActualQuery = actualQuery
          OperationType = operationDef }

    member _.Generate (queryOrPath: string, resolutionFolder: string, ?operationName: string, ?typeName: string): GeneratedOperation =
        let queryLocation = StringLocation.Create(queryOrPath, resolutionFolder)
        let query =
            match queryLocation with
            | String query -> query
            | File path -> File.ReadAllText(path)
        let queryAst = Parser.parse query
        let schema = schemaGenerator.SchemaTypes.Introspection
        let operationDefinition = tryPickOperation operationName queryAst.Definitions
        match operationDefinition with
        | Some operation ->
            generateOperationDefinition schema query queryAst operation typeName
        | None ->
            failwith "Expected an operation definition"