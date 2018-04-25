namespace FSharp.Data.GraphQL

open System.Collections.Generic
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Planning

type CompileSchemaFunc = 
    SchemaCompileContext -> (SchemaCompileContext -> unit) -> unit

type PlanOperationFunc = 
    PlanningContext -> (PlanningContext -> ExecutionPlan) -> ExecutionPlan

type ExecuteOperationFunc =
    ExecutionContext -> (ExecutionContext -> AsyncVal<GQLResponse>) -> AsyncVal<GQLResponse>

type ISchemaCompileMiddleware =
    abstract member CompileSchema : CompileSchemaFunc

type IOperationPlanningMiddleware =
    abstract member PlanOperation : PlanOperationFunc

type IOperationExecutionMiddleware =
    abstract member ExecuteOperationAsync : ExecuteOperationFunc

type IExecutorMiddleware =
    abstract member CompileSchema : ISchemaCompileMiddleware option
    abstract member PlanOperation : IOperationPlanningMiddleware option
    abstract member ExecuteOperationAsync : IOperationExecutionMiddleware option

type Executor<'Root>(schema: ISchema<'Root>, middlewares : IExecutorMiddleware seq) =
    let fieldExecuteMap = FieldExecuteMap()

    // FIXME: for some reason static do or do invocation in module doesn't work
    // for this reason we're compiling executors as part of identifier evaluation
    let __done =
        // We don't need to know possible types at this point
        fieldExecuteMap.SetExecute("",
                                   "__schema",
                                   compileField SchemaMetaFieldDef)

        fieldExecuteMap.SetExecute("",
                                   "__type",
                                   compileField TypeMetaFieldDef )

        fieldExecuteMap.SetExecute("",
                                   "__typename",
                                   compileField TypeNameMetaFieldDef )

    let rec runSchemaCompilingMiddlewares (ctx : SchemaCompileContext) (middlewares : CompileSchemaFunc list) =
        match middlewares with
        | [] -> compileSchema ctx
        | x :: xs -> x ctx (fun ctx -> runSchemaCompilingMiddlewares ctx xs)

    let compileSchemaWithMiddlewares (ctx : SchemaCompileContext) =
        middlewares
        |> Seq.map (fun m -> m.CompileSchema)
        |> Seq.choose id
        |> Seq.map (fun m -> m.CompileSchema)
        |> List.ofSeq
        |> runSchemaCompilingMiddlewares ctx

    do
        let compileCtx = { Schema = schema; FieldExecuteMap = fieldExecuteMap }
        compileSchemaWithMiddlewares compileCtx
        match Validation.validate schema.TypeMap with
        | Validation.Success -> ()
        | Validation.Error errors -> raise (GraphQLException (System.String.Join("\n", errors)))

    let rec runOperationExecutionMiddlewares (ctx : ExecutionContext) (middlewares : ExecuteOperationFunc list) =
        match middlewares with
        | [] -> executeOperation ctx
        | x :: xs -> x ctx (fun ctx -> runOperationExecutionMiddlewares ctx xs)

    let executeOperationWithMiddlewares (ctx : ExecutionContext) =
        middlewares
        |> Seq.map (fun m -> m.ExecuteOperationAsync)
        |> Seq.choose id
        |> Seq.map (fun m -> m.ExecuteOperationAsync)
        |> List.ofSeq
        |> runOperationExecutionMiddlewares ctx

    let eval(executionPlan: ExecutionPlan, data: 'Root option, variables: Map<string, obj>, meta : Metadata): Async<GQLResponse> =
        let prepareOutput res =
            let prepareData data (errs: Error list) =
                let parsedErrors =
                    errs
                    |> List.map(fun (message, path) -> NameValueLookup.ofList [ "message", upcast message; "path", upcast path])
                match parsedErrors with
                | [] -> NameValueLookup.ofList [ "documentId", box executionPlan.DocumentId ; "data", upcast data ] :> Output
                | errors -> NameValueLookup.ofList [ "documentId", box executionPlan.DocumentId ; "data", upcast data ; "errors", upcast errors ] :> Output
            match res with
            | Direct (data, errors) -> Direct (prepareData data errors, errors)
            | Deferred (data, errors, deferred) -> Deferred (prepareData data errors, errors, deferred)
            | Stream (stream) -> Stream(stream)
        async {
            try
                let errors = System.Collections.Concurrent.ConcurrentBag<exn>()
                let root = data |> Option.map box |> Option.toObj
                let variables = coerceVariables executionPlan.Variables variables
                let executionCtx = 
                    { Schema = schema
                      ExecutionPlan = executionPlan
                      RootValue = root
                      Variables = variables
                      Errors = errors
                      FieldExecuteMap = fieldExecuteMap
                      Metadata = meta }
                let! res = executeOperationWithMiddlewares executionCtx
                return prepareOutput res
            with
            | ex ->
                let msg = ex.ToString()
                return prepareOutput(Direct(new Dictionary<string, obj>() :> Output, [msg, []]))
        }

    let execute (executionPlan: ExecutionPlan, data: 'Root option, variables: Map<string, obj> option, meta : Metadata) =
        let variables = defaultArg variables Map.empty
        eval(executionPlan, data, variables, meta)

    let rec runOperationPlanningMiddlewares (ctx : PlanningContext) (middlewares : PlanOperationFunc list) =
        match middlewares with
        | [] -> planOperation ctx
        | x :: xs -> x ctx (fun ctx -> runOperationPlanningMiddlewares ctx xs)

    let planOperationWithMiddlewares (ctx : PlanningContext) =
        middlewares
        |> Seq.map (fun m -> m.PlanOperation)
        |> Seq.choose id
        |> Seq.map (fun m -> m.PlanOperation)
        |> List.ofSeq
        |> runOperationPlanningMiddlewares ctx

    let createExecutionPlan (ast: Document, operationName: string option, meta : Metadata) =
        match findOperation ast operationName with
        | Some operation -> 
            let rootDef = 
                match operation.OperationType with
                | Query -> schema.Query
                | Mutation -> 
                    match schema.Mutation with
                    | Some m -> m
                    | None -> raise (GraphQLException "Operation to be executed is of type mutation, but no mutation root object was defined in current schema")
                | Subscription ->
                    match schema.Subscription with
                    | Some s -> upcast s
                    | None -> raise (GraphQLException "Operations to be executed is of type subscription, but no subscription root object was defined in the current schema") 
            let planningCtx = 
                { Schema = schema
                  RootDef = rootDef
                  Document = ast
                  Metadata = meta
                  Operation = operation
                  DocumentId = ast.GetHashCode() }
            planOperationWithMiddlewares planningCtx
        | None -> raise (GraphQLException "No operation with specified name has been found for provided document")

    new(schema) = Executor(schema, middlewares = Seq.empty)

    /// <summary>
    /// Asynchronously executes a provided execution plan. In case of repetitive queries, execution plan may be preprocessed 
    /// and cached using `documentId` as an identifier.
    /// Returned value is a readonly dictionary consisting of following top level entries:
    /// - `documentId`: unique identifier of current document's AST, it can be used as a key/identifier of ExecutionPlan as well
    /// - `data`: GraphQL response matching the structure provided in GraphQL query string
    /// - `errors (oprional): contains a list of errors that occurred while executing a GraphQL operation
    /// </summary>
    /// <param name="ast">Parsed GraphQL query string.</param>
    /// <param name="data">Optional object provided as a root to all top level field resolvers</param>
    /// <param name="variables">Map of all variable values provided by the client request.</param>
    /// <param name="operationName">In case when document consists of many operations, this field describes which of them to execute.</param>
    /// <param name="meta">A plain dictionary of metadata that can be used through execution customizations.</param>
    member __.AsyncExecute(executionPlan: ExecutionPlan, ?data: 'Root, ?variables: Map<string, obj>, ?meta : Metadata): Async<GQLResponse> =
        let meta = defaultArg meta Metadata.Empty
        execute (executionPlan, data, variables, meta)
    
    /// <summary>
    /// Asynchronously executes parsed GraphQL query AST. Returned value is a readonly dictionary consisting of following top level entries:
    /// - `documentId`: unique identifier of current document's AST
    /// - `data`: GraphQL response matching the structure provided in GraphQL query string
    /// - `errors` (oprional): contains a list of errors that occurred while executing a GraphQL operation
    /// </summary>
    /// <param name="ast">Parsed GraphQL query string.</param>
    /// <param name="data">Optional object provided as a root to all top level field resolvers</param>
    /// <param name="variables">Map of all variable values provided by the client request.</param>
    /// <param name="operationName">In case when document consists of many operations, this field describes which of them to execute.</param>
    /// <param name="meta">A plain dictionary of metadata that can be used through execution customizations.</param>
    member __.AsyncExecute(ast: Document, ?data: 'Root, ?variables: Map<string, obj>, ?operationName: string, ?meta : Metadata): Async<GQLResponse> =
        let meta = defaultArg meta Metadata.Empty
        let executionPlan = createExecutionPlan (ast, operationName, meta)
        execute (executionPlan, data, variables, meta)
        
    /// <summary>
    /// Asynchronously executes unparsed GraphQL query AST. Returned value is a readonly dictionary consisting of following top level entries:
    /// - `documentId`: unique identifier of current document's AST
    /// - `data`: GraphQL response matching the structure provided in GraphQL query string
    /// - `errors (oprional): contains a list of errors that occurred while executing a GraphQL operation
    /// </summary>
    /// <param name="ast">Parsed GraphQL query string.</param>
    /// <param name="data">Optional object provided as a root to all top level field resolvers</param>
    /// <param name="variables">Map of all variable values provided by the client request.</param>
    /// <param name="operationName">In case when document consists of many operations, this field describes which of them to execute.</param>
    /// <param name="meta">A plain dictionary of metadata that can be used through execution customizations.</param>
    member __.AsyncExecute(queryOrMutation: string, ?data: 'Root, ?variables: Map<string, obj>, ?operationName: string, ?meta : Metadata): Async<GQLResponse> =
        let meta = defaultArg meta Metadata.Empty
        let ast = parse queryOrMutation
        let executionPlan = createExecutionPlan (ast, operationName, meta)
        execute (executionPlan, data, variables, meta)

    /// Creates an execution plan for provided GraphQL document AST without 
    /// executing it. This is useful in cases when you have the same query executed 
    /// multiple times with different parameters. In that case, query can be used 
    /// to construct execution plan, which then is cached (using DocumentId as a key) and reused when needed.
    /// <param name="meta">A plain dictionary of metadata that can be used through execution customizations.</param>
    member __.CreateExecutionPlan(ast: Document, ?operationName: string, ?meta : Metadata): ExecutionPlan =
        let meta = defaultArg meta Metadata.Empty
        createExecutionPlan (ast, operationName, meta)
        
    /// Creates an execution plan for provided GraphQL query string without 
    /// executing it. This is useful in cases when you have the same query executed 
    /// multiple times with different parameters. In that case, query can be used 
    /// to construct execution plan, which then is cached (using DocumentId as a key) and reused when needed.
    /// <param name="meta">A plain dictionary of metadata that can be used through execution customizations.</param>
    member __.CreateExecutionPlan(queryOrMutation: string, ?operationName: string, ?meta : Metadata) =
        let meta = defaultArg meta Metadata.Empty
        let ast = parse queryOrMutation
        createExecutionPlan (ast, operationName, meta)