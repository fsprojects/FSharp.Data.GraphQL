namespace FSharp.Data.GraphQL

open System.Collections.Generic
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Planning

/// Arguments used to execute a GraphQL query.
type ExecutionArgs<'Root> =
    ExecutionPlan * 'Root option * Map<string, obj> option

/// Represents the function used to execute a parsed query.
type ExecutionFunc<'Root> =
    ExecutionArgs<'Root> -> Async<GQLResponse>

module ExecutionFunc =
    let error msg path : ExecutionFunc<'Root> = fun _ -> GQLResponse.directErrorAsync msg path

/// Represents the interception function used by a query execution middleware.
type ExecutionMiddlewareFunc<'Root> =
    ExecutionArgs<'Root> -> ExecutionFunc<'Root> -> Async<GQLResponse>

/// Interface to implement query execution middlewares.
type IExecutionMiddleware<'Root> =
    /// Contains the function used to execute a query execution middleware.
    abstract member ExecuteAsync : ExecutionMiddlewareFunc<'Root>

type Executor<'Root>(schema: ISchema<'Root>, middlewares : IExecutionMiddleware<'Root> seq) =
    let fieldExecuteMap = FieldExecuteMap()

    //FIXME: for some reason static do or do invocation in module doesn't work
    // for this reason we're compiling executors as part of identifier evaluation
    let __done =
    //     we don't need to know possible types at this point
        fieldExecuteMap.SetExecute("",
                                   "__schema",
                                   compileField SchemaMetaFieldDef)

        fieldExecuteMap.SetExecute("",
                                   "__type",
                                   compileField TypeMetaFieldDef )

        fieldExecuteMap.SetExecute("",
                                   "__typename",
                                   compileField TypeNameMetaFieldDef )

    do
        compileSchema schema.TypeMap fieldExecuteMap schema.SubscriptionProvider
        match Validation.validate schema.TypeMap with
        | Validation.Success -> ()
        | Validation.Error errors -> raise (GraphQLException (System.String.Join("\n", errors)))

    let eval(executionPlan: ExecutionPlan, data: 'Root option, variables: Map<string, obj>): Async<GQLResponse> =
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
                let rootObj = data |> Option.map box |> Option.toObj
                let! res = evaluate schema executionPlan variables rootObj errors fieldExecuteMap
                return prepareOutput res
            with
            | ex ->
                let msg = ex.ToString()
                return prepareOutput(Direct(new Dictionary<string, obj>() :> Output, [msg, []]))
        }

    let execute (executionPlan: ExecutionPlan, data: 'Root option, variables: Map<string, obj> option) =
        eval(executionPlan, data, defaultArg variables Map.empty)

    let rec runMiddlewares executionPlan data variables (middlewares : ExecutionMiddlewareFunc<'Root> list) =
        match middlewares with
        | [] -> execute (executionPlan, data, variables)
        | x :: xs -> x (executionPlan, data, variables) (fun (plan, data, variables) -> runMiddlewares plan data variables xs)

    let executeWithMiddlewares (executionPlan: ExecutionPlan, data: 'Root option, variables: Map<string, obj> option) =
        middlewares
        |> Seq.map (fun m -> m.ExecuteAsync)
        |> List.ofSeq
        |> runMiddlewares executionPlan data variables

    new(schema) = Executor(schema, middlewares = Seq.empty)

    new(schema, middlewareFuncs : ExecutionMiddlewareFunc<'Root> seq) = 
        let middlewares = 
            middlewareFuncs 
            |> Seq.map (fun m -> { new IExecutionMiddleware<'Root> with member __.ExecuteAsync = m })
        Executor(schema, middlewares = middlewares)

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
    member __.AsyncExecute(executionPlan: ExecutionPlan, ?data: 'Root, ?variables: Map<string, obj>): Async<GQLResponse> =
        executeWithMiddlewares (executionPlan, data, variables)
    
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
    member this.AsyncExecute(ast: Document, ?data: 'Root, ?variables: Map<string, obj>, ?operationName: string): Async<GQLResponse> =
        let executionPlan = 
            match operationName with
            | Some opname -> this.CreateExecutionPlan(ast, opname)
            | None -> this.CreateExecutionPlan(ast)
        executeWithMiddlewares (executionPlan, data, variables)
        
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
    member this.AsyncExecute(queryOrMutation: string, ?data: 'Root, ?variables: Map<string, obj>, ?operationName: string): Async<GQLResponse> =
        let ast = parse queryOrMutation
        let executionPlan = 
            match operationName with
            | Some opname -> this.CreateExecutionPlan(ast, opname)
            | None -> this.CreateExecutionPlan(ast)
        executeWithMiddlewares (executionPlan, data, variables)

    /// Creates an execution plan for provided GraphQL document AST without 
    /// executing it. This is useful in cases when you have the same query executed 
    /// multiple times with different parameters. In that case, query can be used 
    /// to construct execution plan, which then is cached (using DocumentId as a key) and reused when needed.
    member __.CreateExecutionPlan(ast: Document, ?operationName: string): ExecutionPlan =
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
            let planningCtx = { Schema = schema; RootDef = rootDef; Document = ast }
            let result = planOperation (ast.GetHashCode()) planningCtx operation
            { result with Metadata = schema.Metadata }
        | None -> raise (GraphQLException "No operation with specified name has been found for provided document")
        
    /// Creates an execution plan for provided GraphQL query string without 
    /// executing it. This is useful in cases when you have the same query executed 
    /// multiple times with different parameters. In that case, query can be used 
    /// to construct execution plan, which then is cached (using DocumentId as a key) and reused when needed.
    member this.CreateExecutionPlan(queryOrMutation: string, ?operationName: string) =
        match operationName with
        | None -> this.CreateExecutionPlan(parse queryOrMutation)
        | Some o -> this.CreateExecutionPlan(parse queryOrMutation, o)