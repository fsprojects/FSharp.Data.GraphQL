namespace FSharp.Data.GraphQL

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
    ExecutionContext -> (ExecutionContext -> AsyncVal<GQLResponse>) -> AsyncVal<GQLResponse>

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
        member __.CompileSchema = compile
        member __.PostCompileSchema = postCompile
        member __.PlanOperation = plan
        member __.ExecuteOperationAsync = execute

/// The standard schema executor.
/// It compiles the schema and offers an interface for planning and executing queries.
/// The execution process can be customized through usage of middlewares.
type Executor<'Root>(schema: ISchema<'Root>, middlewares : IExecutorMiddleware seq, ?validationCache : IValidationResultCache) =
    let validationCache = defaultArg validationCache (upcast MemoryValidationResultCache())

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
        let rec go (ctx : 'ctx) (middlewares : IExecutorMiddleware list) : 'res =
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
        | ValidationError errors -> raise (GraphQLException (System.String.Join("\n", errors)))

    let eval (executionPlan: ExecutionPlan, data: 'Root option, variables: Map<string, Value>): Async<GQLResponse> =
        let prepareOutput res =
            let prepareData data (errs: Error list) =
                let parsedErrors =
                    errs
                    |> List.map(fun (message, path) -> NameValueLookup.ofList [ "message", upcast message; "path", upcast path])
                match parsedErrors with
                | [] -> NameValueLookup.ofList [ "documentId", box executionPlan.DocumentId ; "data", upcast data ] :> Output
                | errors -> NameValueLookup.ofList [ "documentId", box executionPlan.DocumentId ; "data", upcast data ; "errors", upcast errors ] :> Output
            match res with
            | Direct (data, errors) -> GQLResponse.Direct(prepareData data errors, errors, res.Metadata)
            | Deferred (data, errors, deferred) -> GQLResponse.Deferred(prepareData data errors, errors, deferred, res.Metadata)
            | Stream (stream) -> GQLResponse.Stream(stream, res.Metadata)
        async {
            match executionPlan.ValidationResult with
            | Validation.Success ->
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
                          Metadata = executionPlan.Metadata }
                    let! res = runMiddlewares (fun x -> x.ExecuteOperationAsync) executionCtx executeOperation
                    return prepareOutput res
                with
                | ex -> return prepareOutput(GQLResponse.Error(ex.ToString(), executionPlan.Metadata))
            | Validation.ValidationError errors ->
                let errors = errors |> List.map (fun err ->
                    let path = err.Path |> Option.defaultValue [] |> List.map box
                    err.Message, path)
                return GQLResponse.Invalid(errors, executionPlan.Metadata)
        }

    let execute (executionPlan: ExecutionPlan, data: 'Root option, variables: Map<string, Value> option) =
        let variables = defaultArg variables Map.empty
        eval (executionPlan, data, variables)

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
                    | None -> raise (GraphQLException "Operation to be executed is of type subscription, but no subscription root object was defined in the current schema")
            let documentId = ast.GetHashCode()
            let validationResult =
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
                  DocumentId = documentId
                  ValidationResult = validationResult }
            runMiddlewares (fun x -> x.PlanOperation) planningCtx planOperation
        | None -> raise (GraphQLException "No operation with specified name has been found for provided document")

    new(schema) = Executor(schema, middlewares = Seq.empty)

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
    member __.AsyncExecute(executionPlan: ExecutionPlan, ?data: 'Root, ?variables: Map<string, Value>): Async<GQLResponse> =
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
    member __.AsyncExecute(ast: Document, ?data: 'Root, ?variables: Map<string, Value>, ?operationName: string, ?meta : Metadata): Async<GQLResponse> =
        let meta = defaultArg meta Metadata.Empty
        let executionPlan = createExecutionPlan (ast, operationName, meta)
        execute (executionPlan, data, variables)

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
    member __.AsyncExecute(queryOrMutation: string, ?data: 'Root, ?variables: Map<string, Value>, ?operationName: string, ?meta : Metadata): Async<GQLResponse> =
        let meta = defaultArg meta Metadata.Empty
        let ast = parse queryOrMutation
        let executionPlan = createExecutionPlan (ast, operationName, meta)
        execute (executionPlan, data, variables)

    /// Creates an execution plan for provided GraphQL document AST without
    /// executing it. This is useful in cases when you have the same query executed
    /// multiple times with different parameters. In that case, query can be used
    /// to construct execution plan, which then is cached (using DocumentId as a key) and reused when needed.
    /// <param name="ast">The parsed GraphQL query string.</param>
    /// <param name="operationName">The name of the operation that should be executed on the parsed document.</param>
    /// <param name="meta">A plain dictionary of metadata that can be used through execution plan customizations.</param>
    member __.CreateExecutionPlan(ast: Document, ?operationName: string, ?meta : Metadata): ExecutionPlan =
        let meta = defaultArg meta Metadata.Empty
        createExecutionPlan (ast, operationName, meta)

    /// Creates an execution plan for provided GraphQL query string without
    /// executing it. This is useful in cases when you have the same query executed
    /// multiple times with different parameters. In that case, query can be used
    /// to construct execution plan, which then is cached (using DocumentId as a key) and reused when needed.
    /// <param name="queryOrMutation">The GraphQL query string.</param>
    /// <param name="operationName">The name of the operation that should be executed on the parsed document.</param>
    /// <param name="meta">A plain dictionary of metadata that can be used through execution plan customizations.</param>
    member __.CreateExecutionPlan(queryOrMutation: string, ?operationName: string, ?meta : Metadata) =
        let meta = defaultArg meta Metadata.Empty
        let ast = parse queryOrMutation
        createExecutionPlan (ast, operationName, meta)

[<AutoOpen>]
module CompatabilityExtensions =

    open System

    module internal Helpers =

        open System.Reflection
        open FSharp.Reflection
        open FSharp.Data.GraphQL.Ast

        let private (| FSharpListType |) obj =
            let t = obj.GetType()
            if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<obj list> then
                // Crafty trick since we can always do `List<'t> :> seq<obj>`
                // Perhaps not the most efficient way, involves copying
                let xs = box obj :?> seq<obj> |> List.ofSeq
                Some xs
            else
                None

        let private (| RecordType |) obj =
            let t = obj.GetType()
            if FSharpType.IsRecord t then
                Some t
            else
                None

        let private (| UnionType |) obj =
            let t = obj.GetType()
            if FSharpType.IsUnion t then
                Some t
            else
                None

        let private (| EnumType |) obj =
            let t = obj.GetType()
            if t.IsEnum then
                Some t
            else
                None

        let private (| TupleType |) obj =
            let t = obj.GetType()
            if FSharpType.IsTuple t then
                Some (FSharpValue.GetTupleFields obj)
            else
                None

        let private (| FSharpOptionType |) obj =
            let t = obj.GetType()
            if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<obj option> then
                let _, values = FSharpValue.GetUnionFields(obj, t)
                let opt = values |> Seq.tryHead // Some has one field and None has zero
                Some opt
            else
                None

        let rec private objToValue (x : obj) =
            match box x with
            | null -> NullValue
            | :? Value as v -> v
            | :? bool as b -> BooleanValue b
            | :? int64 as i -> IntValue i
            | :? int as i -> IntValue (int64 i)
            | :? string as s -> StringValue s
            | :? Guid as g -> StringValue (string g)
            | :? Map<string, obj> as m ->
                m
                |> Map.map (fun _ v -> objToValue v)
                |> ObjectValue
            | FSharpOptionType (Some opt) ->
                match opt with
                | Some x -> objToValue x
                | None -> NullValue
            | TupleType (Some fields) ->
                fields
                |> Seq.map objToValue
                |> Seq.toList
                |> ListValue
            | FSharpListType (Some xs) ->
                xs |> List.map objToValue |> ListValue
            | RecordType (Some t) ->
                let ts = FSharpType.GetRecordFields(t)
                let vs = FSharpValue.GetRecordFields(x)

                Map.ofSeq [
                    for pi, v in Seq.zip ts vs do
                        let value = objToValue v
                        pi.Name, value
                ]
                |> ObjectValue
            | UnionType (Some t) ->
                let uci, _ = FSharpValue.GetUnionFields(x, t)
                EnumValue uci.Name
            | EnumType (Some t) ->
                StringValue (t.GetEnumName(x))
            | _ -> failwithf "Error converting %A (type: %s) to an input value" x (x.GetType().FullName)

        let variablesToValues (vars : Map<string, obj>) : Map<string, Value> =
            vars
            |> Map.map (fun _ v -> objToValue v)

    open Helpers

    type Executor<'Root> with

        [<Obsolete>]
        member this.AsyncExecute(ast : Document, ?data : 'Root, ?variables : Map<string, obj>, ?operationName : string, ?meta : Metadata): Async<GQLResponse> =
            let variables = variables |> Option.map variablesToValues
            this.AsyncExecute(ast, ?data=data, ?variables=variables, ?operationName=operationName, ?meta=meta)
