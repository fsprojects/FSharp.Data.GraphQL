namespace FSharp.Data.GraphQL

open System.Collections.Immutable
open System.Collections.Generic
open System.Runtime.InteropServices
open System.Text.Json
open FsToolkit.ErrorHandling

open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Validation
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Planning

/// A function signature that represents a middleware for schema compile phase.
/// I takes two arguments: A schema compile context, containing all the data used for the
/// compilation phase, and another function that can be called to pass
/// the execution for the next middleware.
type SchemaCompileMiddleware =
    SchemaCompileContext -> (SchemaCompileContext -> unit) -> unit

/// A function signature that represents a middleware for the post-schema compilation phase.
type SchemaPostCompileMiddleware =
    ISchema -> (ISchema -> unit) -> unit

/// A function signature that represents a middleware for operation planning phase.
/// I takes two arguments: A planning context, containing all the data used for the
/// planning phase, and another function that can be called to pass
/// the execution for the next middleware.
type OperationPlanningMiddleware =
    PlanningContext -> (PlanningContext -> ExecutionPlan) -> ExecutionPlan

/// A function signature that represents a middleware for query execution phase.
/// I takes two arguments: An execution context, containing all the data used for the
/// execution phase, and another function that can be called to pass
/// the execution for the next middleware.
type OperationExecutionMiddleware =
    ExecutionContext -> (ExecutionContext -> AsyncVal<GQLExecutionResult>) -> AsyncVal<GQLExecutionResult>

/// An interface to implement Executor middlewares.
/// A middleware can have one to three sub-middlewares, one for each phase of the query execution.
type IExecutorMiddleware =
    /// Defines the sub-middleware that intercepts the schema compile process of the Executor.
    abstract CompileSchema : SchemaCompileMiddleware option
    /// Defines the sub-middleware that executes after the schema compilation phase of the Executor is complete.
    abstract PostCompileSchema : SchemaPostCompileMiddleware option
    /// Defines the sub-middleware that intercepts the operation planning phase of the Executor.
    abstract PlanOperation : OperationPlanningMiddleware option
    /// Defines the sub-middleware that intercepts the operation execution phase of the Executor.
    abstract ExecuteOperationAsync : OperationExecutionMiddleware option

/// A simple, concrete implementation for the IExecutorMiddleware interface.
type ExecutorMiddleware(?compile, ?postCompile, ?plan, ?execute) =
    interface IExecutorMiddleware with
        member _.CompileSchema = compile
        member _.PostCompileSchema = postCompile
        member _.PlanOperation = plan
        member _.ExecuteOperationAsync = execute

/// The standard schema executor.
/// It compiles the schema and offers an interface for planning and executing queries.
/// The execution process can be customized through usage of middlewares.
/// An optional pre-existing validation cache can be supplied.  If not, one is created and used internally.
type Executor<'Root>(schema: ISchema<'Root>, middlewares : IExecutorMiddleware seq, [<Optional>] validationCache : IValidationResultCache voption) =
    let validationCache = validationCache |> ValueOption.defaultWith (fun () -> upcast MemoryValidationResultCache())

    let fieldExecuteMap = FieldExecuteMap(compileField)

    // FIXME: for some reason static do or do invocation in module doesn't work
    // for this reason we're compiling executors as part of identifier evaluation
    let __done =
        // We don't need to know possible types at this point
        fieldExecuteMap.SetExecute(SchemaMetaFieldDef)
        fieldExecuteMap.SetExecute(TypeMetaFieldDef)
        fieldExecuteMap.SetExecute(TypeNameMetaFieldDef)

    let middlewaresList = Seq.toList middlewares

    let rec runMiddlewares (phaseSel : IExecutorMiddleware -> ('ctx -> ('ctx -> 'res) -> 'res) option)
                           (initialCtx : 'ctx)
                           (onComplete : 'ctx -> 'res)
                           : 'res =
        let rec go ctx (middlewares: IExecutorMiddleware list) =
            match middlewares with
            | [] -> onComplete ctx
            | m :: ms ->
                match (phaseSel m) with
                | Some f -> f ctx (fun ctx' -> go ctx' ms)
                | None -> go ctx ms
        go initialCtx middlewaresList

    do
        let compileCtx = { Schema = schema; TypeMap = schema.TypeMap; FieldExecuteMap = fieldExecuteMap }
        runMiddlewares (fun x -> x.CompileSchema) compileCtx compileSchema
        runMiddlewares (fun x -> x.PostCompileSchema) (upcast schema) ignore
        match Validation.Types.validateTypeMap schema.TypeMap with
        | Success -> ()
        | ValidationError errors -> raise (GQLMessageException (System.String.Join("\n", errors)))

    let eval (executionPlan: ExecutionPlan, data: 'Root option, variables: ImmutableDictionary<string, JsonElement>): Async<GQLExecutionResult> =
        let documentId = executionPlan.DocumentId
        let prepareOutput res =
            match res with
            | RequestError errs -> GQLExecutionResult.Error (documentId, errs, res.Metadata)
            | Direct (data, errors) -> GQLExecutionResult.Direct (documentId, data, errors, res.Metadata)
            | Deferred (data, errors, deferred) -> GQLExecutionResult.Deferred (documentId, data, errors, deferred, res.Metadata)
            | Stream (stream) -> GQLExecutionResult.Stream (documentId, stream, res.Metadata)
        async {
            try
                let root = data |> Option.map box |> Option.toObj
                match coerceVariables executionPlan.Variables variables with
                | Error errs -> return prepareOutput (GQLExecutionResult.Error (documentId, errs, executionPlan.Metadata))
                | Ok variables ->
                    let executionCtx =
                        { Schema = schema
                          RootValue = root
                          ExecutionPlan = executionPlan
                          Variables = variables
                          FieldExecuteMap = fieldExecuteMap
                          Metadata = executionPlan.Metadata }
                    let! res = runMiddlewares (fun x -> x.ExecuteOperationAsync) executionCtx executeOperation |> AsyncVal.toAsync
                    return prepareOutput res
            with
            | :? GQLMessageException as ex -> return prepareOutput(GQLExecutionResult.Error (documentId, ex, executionPlan.Metadata))
            | ex -> return prepareOutput (GQLExecutionResult.Error(documentId, ex.ToString(), executionPlan.Metadata)) // TODO: Handle better
        }

    let execute (executionPlan: ExecutionPlan, data: 'Root option, variables: ImmutableDictionary<string, JsonElement> option) =
        let variables = defaultArg variables ImmutableDictionary.Empty
        eval (executionPlan, data, variables)

    let createExecutionPlan (ast: Document, operationName: string option, meta : Metadata) =
        let documentId = ast.GetHashCode()
        result {
            match findOperation ast operationName with
            | Some operation ->
                let! rootDef =
                    match operation.OperationType with
                    | Query -> Ok schema.Query
                    | Mutation ->
                        match schema.Mutation with
                        | Some m -> Ok m
                        | None -> Error <| [ GQLProblemDetails.CreateWithKind (
                            "Operation to be executed is of type mutation, but no mutation root object was defined in current schema",
                            ErrorKind.Validation
                        )]
                    | Subscription ->
                        match schema.Subscription with
                        | Some s -> Ok <| upcast s
                        | None -> Error <| [ GQLProblemDetails.CreateWithKind (
                            "Operation to be executed is of type subscription, but no subscription root object was defined in the current schema",
                            ErrorKind.Validation
                        )]
                do!
                    let schemaId = schema.Introspected.GetHashCode()
                    let key = { DocumentId = documentId; SchemaId = schemaId }
                    let producer = fun () -> Validation.Ast.validateDocument schema.Introspected ast
                    validationCache.GetOrAdd producer key
                let planningCtx =
                    { Schema = schema
                      RootDef = rootDef
                      Document = ast
                      Metadata = meta
                      Operation = operation
                      DocumentId = documentId }
                return runMiddlewares (fun x -> x.PlanOperation) planningCtx planOperation
            | None -> return! Error <| [ GQLProblemDetails.CreateWithKind (
                "No operation with specified name has been found for provided document",
                ErrorKind.Validation
            )]
        }
        |> Result.mapError (fun errs -> struct (documentId, errs))

    new(schema) =
        Executor(schema, middlewares = Seq.empty)

    /// <summary>
    /// Asynchronously executes a provided execution plan. In case of repetitive queries, execution plan may be preprocessed
    /// and cached using `documentId` as an identifier.
    /// Returned value is a readonly dictionary consisting of following top level entries:
    /// 'documentId' (unique identifier of current document's AST, it can be used as a key/identifier of ExecutionPlan as well),
    /// 'data' (GraphQL response matching the structure provided in GraphQL query string), and
    /// 'errors' (optional, contains a list of errors that occurred while executing a GraphQL operation).
    /// </summary>
    /// <param name="executionPlan">Execution plan for the operation.</param>
    /// <param name="data">Optional object provided as a root to all top level field resolvers</param>
    /// <param name="variables">Map of all variable values provided by the client request.</param>
    member _.AsyncExecute(executionPlan: ExecutionPlan, ?data: 'Root, ?variables: ImmutableDictionary<string, JsonElement>): Async<GQLExecutionResult> =
        execute (executionPlan, data, variables)

    /// <summary>
    /// Asynchronously executes parsed GraphQL query AST. Returned value is a readonly dictionary consisting of following top level entries:
    /// 'documentId' (unique identifier of current document's AST, it can be used as a key/identifier of ExecutionPlan as well),
    /// 'data' (GraphQL response matching the structure provided in GraphQL query string), and
    /// 'errors' (optional, contains a list of errors that occurred while executing a GraphQL operation).
    /// </summary>
    /// <param name="ast">Parsed GraphQL query string.</param>
    /// <param name="data">Optional object provided as a root to all top level field resolvers</param>
    /// <param name="variables">Map of all variable values provided by the client request.</param>
    /// <param name="operationName">In case when document consists of many operations, this field describes which of them to execute.</param>
    /// <param name="meta">A plain dictionary of metadata that can be used through execution customizations.</param>
    member _.AsyncExecute(ast: Document, ?data: 'Root, ?variables: ImmutableDictionary<string, JsonElement>, ?operationName: string, ?meta : Metadata): Async<GQLExecutionResult> =
        let meta = defaultArg meta Metadata.Empty
        match createExecutionPlan (ast, operationName, meta) with
        | Ok executionPlan -> execute (executionPlan, data, variables)
        | Error (documentId, errors) -> async.Return <| GQLExecutionResult.Invalid(documentId, errors, meta)

    /// <summary>
    /// Asynchronously executes unparsed GraphQL query AST. Returned value is a readonly dictionary consisting of following top level entries:
    /// 'documentId' (unique identifier of current document's AST, it can be used as a key/identifier of ExecutionPlan as well),
    /// 'data' (GraphQL response matching the structure provided in GraphQL query string), and
    /// 'errors' (optional, contains a list of errors that occurred while executing a GraphQL operation).
    /// </summary>
    /// <param name="queryOrMutation">GraphQL query string.</param>
    /// <param name="data">Optional object provided as a root to all top level field resolvers</param>
    /// <param name="variables">Map of all variable values provided by the client request.</param>
    /// <param name="operationName">In case when document consists of many operations, this field describes which of them to execute.</param>
    /// <param name="meta">A plain dictionary of metadata that can be used through execution customizations.</param>
    member _.AsyncExecute(queryOrMutation: string, ?data: 'Root, ?variables: ImmutableDictionary<string, JsonElement>, ?operationName: string, ?meta : Metadata): Async<GQLExecutionResult> =
        let meta = defaultArg meta Metadata.Empty
        let ast = parse queryOrMutation
        match createExecutionPlan (ast, operationName, meta) with
        | Ok executionPlan -> execute (executionPlan, data, variables)
        | Error (documentId, errors) -> async.Return <| GQLExecutionResult.Invalid(documentId, errors, meta)

    /// Creates an execution plan for provided GraphQL document AST without
    /// executing it. This is useful in cases when you have the same query executed
    /// multiple times with different parameters. In that case, query can be used
    /// to construct execution plan, which then is cached (using DocumentId as a key) and reused when needed.
    /// <param name="ast">The parsed GraphQL query string.</param>
    /// <param name="operationName">The name of the operation that should be executed on the parsed document.</param>
    /// <param name="meta">A plain dictionary of metadata that can be used through execution plan customizations.</param>
    member _.CreateExecutionPlan(ast: Document, ?operationName: string, ?meta : Metadata) =
        let meta = defaultArg meta Metadata.Empty
        createExecutionPlan (ast, operationName, meta)

    /// Creates an execution plan for provided GraphQL query string without
    /// executing it. This is useful in cases when you have the same query executed
    /// multiple times with different parameters. In that case, query can be used
    /// to construct execution plan, which then is cached (using DocumentId as a key) and reused when needed.
    /// <param name="queryOrMutation">The GraphQL query string.</param>
    /// <param name="operationName">The name of the operation that should be executed on the parsed document.</param>
    /// <param name="meta">A plain dictionary of metadata that can be used through execution plan customizations.</param>
    member _.CreateExecutionPlan(queryOrMutation: string, ?operationName: string, ?meta : Metadata) =
        let meta = defaultArg meta Metadata.Empty
        let ast = parse queryOrMutation
        createExecutionPlan (ast, operationName, meta)
