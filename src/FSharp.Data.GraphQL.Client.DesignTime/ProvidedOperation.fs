module internal FSharp.Data.GraphQL.ProvidedOperation

open System
open FSharp.Core
open System.Reflection
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Client
open FSharp.Data.GraphQL.Client.ReflectionPatterns
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types.Introspection
open ProviderImplementation.ProvidedTypes
open FSharp.Quotations

#nowarn "10001"

let private makeProvidedOperationResultType(operationType : Type) =
    let tdef = ProvidedTypeDefinition("OperationResult", Some typeof<OperationResultBase>, nonNullable = true)
    tdef.AddMemberDelayed(fun _ ->
        let getterCode (args : Expr list) =
            <@@ let this = %%args.[0] : OperationResultBase
                this.RawData @@>
        let prop = ProvidedProperty("Data", operationType, getterCode)
        prop.AddXmlDoc("Contains the data returned by the operation on the server.")
        prop)
    tdef

let makeProvidedOperationType
    (actualQuery: string, operationDefinition: OperationDefinition, operationTypeName: string,
     operationFieldsExpr: Expr, schemaTypes: Map<string,IntrospectionType>,
     schemaProvidedTypes: Map<string, ProvidedTypeDefinition>, operationType: Type,
     contextInfo: GraphQLRuntimeContextInfo option, uploadInputTypeName : string option,
     className: string, explicitOptionalParameters: bool) =
    let tdef = ProvidedTypeDefinition(className, Some typeof<OperationBase>)
    tdef.AddXmlDoc("Represents a GraphQL operation on the server.")
    tdef.AddMembersDelayed(fun _ ->
        let operationResultDef = makeProvidedOperationResultType(operationType)
        let isScalar (typeName: string) =
            match schemaTypes.TryFind typeName with
            | Some introspectionType -> introspectionType.Kind = TypeKind.SCALAR
            | None -> false
        let variables =
            let rec mapVariable (variableName : string) (variableType : InputType) =
                match variableType with
                | NamedType typeName ->
                    match uploadInputTypeName with
                    | Some uploadInputTypeName when typeName = uploadInputTypeName ->
                        variableName, TypeMapping.makeOption typeof<Upload>
                    | _ ->
                        match TypeMapping.scalar.TryFind(typeName) with
                        | Some t -> variableName, TypeMapping.makeOption t
                        | None when isScalar typeName -> variableName, typeof<string option>
                        | None ->
                            match schemaProvidedTypes.TryFind(typeName) with
                            | Some t -> variableName, TypeMapping.makeOption t
                            | None -> failwithf "Unable to find variable type \"%s\" in the schema definition." typeName
                | ListType itype ->
                    let name, t = mapVariable variableName itype
                    name, t |> TypeMapping.makeArray |> TypeMapping.makeOption
                | NonNullType itype ->
                    let name, t = mapVariable variableName itype
                    name, TypeMapping.unwrapOption t
            operationDefinition.VariableDefinitions |> List.map (fun vdef -> mapVariable vdef.VariableName vdef.Type)
        let buildVariablesExprFromArgs (varNames : string list) (args : Expr list) =
            let mapVariableExpr (name : string) (value : Expr) =
                let value = Expr.Coerce(value, typeof<obj>)
                <@@ let rec mapVariableValue (value : obj) =
                        match value with
                        | null -> null
                        | :? string -> value // We need this because strings are enumerables, and we don't want to enumerate them recursively as an object
                        | :? EnumBase as v -> v.GetValue() |> box
                        | :? RecordBase as v -> v.ToDictionary() |> box
                        | OptionValue v -> v |> Option.map mapVariableValue |> box
                        | EnumerableValue v -> v |> Array.map mapVariableValue |> box
                        | v -> v
                    (name, mapVariableValue %%value) @@>
            let args =
                let varArgs = List.skip (args.Length - variables.Length) args
                (varNames, varArgs) ||> List.map2 mapVariableExpr
            Expr.NewArray(typeof<string * obj>, args)
        let defaultContextExpr =
            match contextInfo with
            | Some info ->
                let serverUrl = info.ServerUrl
                let headerNames = info.HttpHeaders |> Seq.map fst |> Array.ofSeq
                let headerValues = info.HttpHeaders |> Seq.map snd |> Array.ofSeq
                <@@ { ServerUrl = serverUrl; HttpHeaders = Array.zip headerNames headerValues; Connection = new GraphQLClientConnection() } @@>
            | None -> <@@ Unchecked.defaultof<GraphQLProviderRuntimeContext> @@>
        // We need to use the combination strategy to generate overloads for variables in the Run/AsyncRun methods.
        // The strategy follows the same principle with ProvidedRecord constructor overloads,
        // except that we also must create one additional overload for the runtime context, for each already existent overload,
        // if no default context is provided.
        let methodOverloadDefinitions =
            let overloadsWithoutContext =
                let optionalVariables, requiredVariables =
                    variables |> List.partition (fun (_, t) -> isOption t)
                if explicitOptionalParameters then
                    [requiredVariables @ optionalVariables]
                else
                    List.combinations optionalVariables
                    |> List.map (fun (optionalVariables, _) ->
                        let optionalVariables = optionalVariables |> List.map (fun (name, t) -> name, (TypeMapping.unwrapOption t))
                        requiredVariables @ optionalVariables)
            let overloadsWithContext =
                overloadsWithoutContext
                |> List.map (fun var -> ("runtimeContext", typeof<GraphQLProviderRuntimeContext>) :: var)
            match contextInfo with
            | Some _ -> overloadsWithoutContext @ overloadsWithContext
            | None -> overloadsWithContext
        // Multipart requests should only be used when the user specifies a upload type name AND the type
        // is present in the query as an input value. If not, we fallback to classic requests.
        let shouldUseMultipartRequest =
            let rec existsUploadType (foundTypes : ProvidedTypeDefinition list) (t : Type) =
                match t with
                | :? ProvidedTypeDefinition as tdef when not (List.contains tdef foundTypes) -> tdef.DeclaredProperties |> Seq.exists ((fun p -> p.PropertyType) >> existsUploadType (tdef :: foundTypes))
                | Option t -> existsUploadType foundTypes t
                | Array t -> existsUploadType foundTypes t
                | _ -> t = typeof<Upload>
            variables |> Seq.exists (snd >> existsUploadType [])
        let runMethodOverloads : MemberInfo list =
            let operationName = Option.toObj operationDefinition.Name
            methodOverloadDefinitions |> List.map (fun overloadParameters ->
                let variableNames = overloadParameters |> List.map fst |> List.filter (fun name -> name <> "runtimeContext")
                let invoker (args : Expr list) =
                    // First arg is the operation instance, second should be the context, if the overload asks for one.
                    // We determine it by seeing if the variable names have one less item than the arguments without the instance.
                    let argsWithoutInstance = args.Tail
                    let variableArgs, isDefaultContext, context =
                        if argsWithoutInstance.Length - variableNames.Length = 1
                        then argsWithoutInstance.Tail, false, argsWithoutInstance.Head
                        else argsWithoutInstance, true, defaultContextExpr
                    let variables = buildVariablesExprFromArgs variableNames variableArgs
                    let variables =
                        if explicitOptionalParameters then
                            <@@ (%%variables: (string * obj) [])
                                |> Array.filter (fun (_, value) ->
                                    match value with
                                    | :? Option<obj> as option -> option.IsSome
                                    | _ -> true) @@>
                        else
                            variables
                    <@@ let context = %%context : GraphQLProviderRuntimeContext
                        let request =
                            { ServerUrl = context.ServerUrl
                              HttpHeaders = context.HttpHeaders
                              OperationName = Option.ofObj operationName
                              Query = actualQuery
                              Variables = %%variables }
                        let response =
                            if shouldUseMultipartRequest
                            then Tracer.runAndMeasureExecutionTime "Ran a multipart GraphQL query request" (fun _ -> GraphQLClient.sendMultipartRequest context.Connection request)
                            else Tracer.runAndMeasureExecutionTime "Ran a GraphQL query request" (fun _ -> GraphQLClient.sendRequest context.Connection request)
                        let responseJson = Tracer.runAndMeasureExecutionTime "Parsed a GraphQL response to a JsonValue" (fun _ -> JsonValue.Parse response)
                        // If the user does not provide a context, we should dispose the default one after running the query
                        if isDefaultContext then (context :> IDisposable).Dispose()
                        OperationResultBase(responseJson, %%operationFieldsExpr, operationTypeName) @@>
                let methodParameters = overloadParameters |> List.map (fun (name, t) -> ProvidedParameter(name, t, ?optionalValue = if isOption t then Some null else None))
                let methodDef = ProvidedMethod("Run", methodParameters, operationResultDef, invoker)
                methodDef.AddXmlDoc("Executes the operation on the server and fetch its results.")
                upcast methodDef)
        let asyncRunMethodOverloads : MemberInfo list =
            let operationName = Option.toObj operationDefinition.Name
            methodOverloadDefinitions |> List.map (fun overloadParameters ->
                let variableNames = overloadParameters |> List.map fst |> List.filter (fun name -> name <> "runtimeContext")
                let invoker (args : Expr list) =
                    // First arg is the operation instance, second should be the context, if the overload asks for one.
                    // We determine it by seeing if the variable names have one less item than the arguments without the instance.
                    let argsWithoutInstance = args.Tail
                    let variableArgs, isDefaultContext, context =
                        if argsWithoutInstance.Length - variableNames.Length = 1
                        then argsWithoutInstance.Tail, false, argsWithoutInstance.Head
                        else argsWithoutInstance, true, defaultContextExpr
                    let variables = buildVariablesExprFromArgs variableNames variableArgs
                    let variables =
                        if explicitOptionalParameters then
                            <@@ (%%variables: (string * obj) [])
                                |> Array.filter (fun (_, value) ->
                                    match value with
                                    | :? Option<obj> as option -> option.IsSome
                                    | _ -> true) @@>
                        else
                            variables
                    <@@ let context = %%context : GraphQLProviderRuntimeContext
                        let request =
                            { ServerUrl = context.ServerUrl
                              HttpHeaders = context.HttpHeaders
                              OperationName = Option.ofObj operationName
                              Query = actualQuery
                              Variables = %%variables }
                        async {
                            let! response =
                                if shouldUseMultipartRequest
                                then Tracer.asyncRunAndMeasureExecutionTime "Ran a multipart GraphQL query request asynchronously" (fun _ -> GraphQLClient.sendMultipartRequestAsync context.Connection request)
                                else Tracer.asyncRunAndMeasureExecutionTime "Ran a GraphQL query request asynchronously" (fun _ -> GraphQLClient.sendRequestAsync context.Connection request)
                            let responseJson = Tracer.runAndMeasureExecutionTime "Parsed a GraphQL response to a JsonValue" (fun _ -> JsonValue.Parse response)
                            // If the user does not provide a context, we should dispose the default one after running the query
                            if isDefaultContext then (context :> IDisposable).Dispose()
                            return OperationResultBase(responseJson, %%operationFieldsExpr, operationTypeName)
                        } @@>
                let methodParameters = overloadParameters |> List.map (fun (name, t) -> ProvidedParameter(name, t, ?optionalValue = if isOption t then Some null else None))
                let methodDef = ProvidedMethod("AsyncRun", methodParameters, TypeMapping.makeAsync operationResultDef, invoker)
                methodDef.AddXmlDoc("Executes the operation asynchronously on the server and fetch its results.")
                upcast methodDef)
        let parseResultDef =
            let invoker (args : Expr list) = <@@ OperationResultBase(JsonValue.Parse %%args.[1], %%operationFieldsExpr, operationTypeName) @@>
            let parameters = [ProvidedParameter("responseJson", typeof<string>)]
            let methodDef = ProvidedMethod("ParseResult", parameters, operationResultDef, invoker)
            methodDef.AddXmlDoc("Parses a JSON response that matches the response pattern of the current operation into a OperationResult type.")
            methodDef
        let members : MemberInfo list = [operationResultDef; parseResultDef] @ runMethodOverloads @ asyncRunMethodOverloads
        members)
    tdef
