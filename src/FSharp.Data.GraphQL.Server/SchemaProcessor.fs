namespace FSharp.Data.GraphQL

open System.Collections.Generic
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Planning

type SchemaProcessor (schema: ISchema) = 
    
    member private this.Eval(executionPlan: ExecutionPlan, data: 'Root option, variables: Map<string, obj>): Async<IDictionary<string,obj>> =
        let inline prepareOutput (errors: System.Collections.Concurrent.ConcurrentBag<exn>) (result: NameValueLookup) =
            if errors.IsEmpty 
            then [ "documentId", box executionPlan.DocumentId ; "data", upcast result ] 
            else [ "documentId", box executionPlan.DocumentId ; "data", box result ; "errors", upcast (errors.ToArray() |> schema.ParseErrors) ]
        async {
            try
                let errors = System.Collections.Concurrent.ConcurrentBag<exn>()
                let rootObj = data |> Option.map box |> Option.toObj
                let res = evaluate schema executionPlan variables rootObj errors schema.FieldExecuteMap
                let! result = res |> AsyncVal.map (fun x -> NameValueLookup.ofList (prepareOutput errors x))
                return result :> IDictionary<string,obj>
            with 
            | ex -> 
                let msg = ex.ToString()
                return upcast NameValueLookup.ofList [ "errors", upcast [ msg ]]
        }
    
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
    member this.AsyncExecute(ast: Document, ?data: 'Root, ?variables: Map<string, obj>, ?operationName: string): Async<IDictionary<string,obj>> =
        let executionPlan = 
            match operationName with
            | Some opname -> this.CreateExecutionPlan(ast, opname)
            | None -> this.CreateExecutionPlan(ast)
        this.Eval(executionPlan, data, defaultArg variables Map.empty)
        
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
    member this.AsyncExecute(queryOrMutation: string, ?data: 'Root, ?variables: Map<string, obj>, ?operationName: string): Async<IDictionary<string,obj>> =
        let ast = parse queryOrMutation
        let executionPlan = 
            match operationName with
            | Some opname -> this.CreateExecutionPlan(ast, opname)
            | None -> this.CreateExecutionPlan(ast)
        this.Eval(executionPlan, data, defaultArg variables Map.empty)
        
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
    member this.AsyncExecute(executionPlan: ExecutionPlan, ?data: 'Root, ?variables: Map<string, obj>): Async<IDictionary<string,obj>> =
        this.Eval(executionPlan, data, defaultArg variables Map.empty)

    /// Creates an execution plan for provided GraphQL document AST without 
    /// executing it. This is useful in cases when you have the same query executed 
    /// multiple times with different parameters. In that case, query can be used 
    /// to construct execution plan, which then is cached (using DocumentId as a key) and reused when needed.
    member this.CreateExecutionPlan(ast: Document, ?operationName: string): ExecutionPlan =
        match findOperation ast operationName with
        | Some operation -> 
            let rootDef = 
                match operation.OperationType with
                | Query -> schema.Query
                | Mutation -> 
                    match schema.Mutation with
                    | Some m -> m
                    | None -> raise (GraphQLException "Operation to be executed is of type mutation, but no mutation root object was defined in current schema")
            let planningCtx = { Schema = schema; RootDef = rootDef; Document = ast }
            planOperation (ast.GetHashCode()) planningCtx operation
        | None -> raise (GraphQLException "No operation with specified name has been found for provided document")
        
    /// Creates an execution plan for provided GraphQL query string without 
    /// executing it. This is useful in cases when you have the same query executed 
    /// multiple times with different parameters. In that case, query can be used 
    /// to construct execution plan, which then is cached (using DocumentId as a key) and reused when needed.
    member this.CreateExecutionPlan(queryOrMutation: string, ?operationName: string) =
        match operationName with
        | None -> this.CreateExecutionPlan(parse queryOrMutation)
        | Some o -> this.CreateExecutionPlan(parse queryOrMutation, o)