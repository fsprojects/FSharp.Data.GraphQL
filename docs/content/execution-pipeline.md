
Technical overview over execution pipeline
======================

The major work done by FSharp.Data.GraphQL server can be split into several phases:

1. Compilation phase, which is executed once when a new `Schema` is being created.
2. Planning phase, when GraphQL query string has been supplied.
3. Execution phase, when query is being executed and result is produced.

## Compilation phase

Compilation phase is called, when we're creating a new schema instance. From this point, all types defined within that schema should remain unchanged. During this step, server will invoke all operations that can be done ahead of time, before actual queries can be handled. This also means that creating a new schema is expensive process, and **should not** be done on each request.

Compilation phase performs several operations:

- Validates, if all type definitions conform GraphQL specification. 
- Creates an [introspection](https://facebook.github.io/graphql/#sec-Introspection)-oriented representation of the schema.
- Prepares value resolution pipeline for each of the fields defined in the schema.

## Planning phase

In many cases planning and execution phases are called together. This however can be changed by calling `schema.CreateExecutionPlan(graphQlQueryString)`, which produces so-called `ExecutionPlan` without actually executing a query.

As the name suggests, `ExecutionPlan` and its components (a tree of objects known as `ExecutionInfo`s) describes how the query is going to be executed. This includes:

- Inlining GraphQL query fragments.
- Defining fields resolution strategy - serial or parallel.
- Combining information from query AST (resolved fields / aliases) with server-side information about them (field and type definitions).
- Preparation of the hooks in the execution chain, that will be supplied with potential variables upon execution.

Spliting planning and execution phases is a good idea, when you have the same GraphQL query requested many times (with potentially different variables). This way you can compute execution plan once and cache it. You can use `executionPlan.DocumentId` as a cache identifier. DocumentId is also returned as one of the top level fields in GraphQL response, so it can be used from the client side. Other GraphQL implementations describe that technique as **persistent queries**.

## Execution phase 

Execution phase is performed, when we want to resolve response from GraphQL server given query execution plan and optional set of variables provided by the client. Usually it's called together with planning phase descibed above.

During execution phase, we perform a traversal over execution plan tree, create an execution context objects for each of the fields and supply them along with data resolved from previous higher level resolver responses recrusively.

Field resolution context contains all of the necessary metadata about things such as:

- Executing schema itself.
- Variables provided with the request.
- Field parameters (in case if field defined any arguments).
- Type definition of both parent and expected return type.
- ExecutionInfo which is fragment of the execution plan itself related to current field and all of its subsequent fields.

Execution phase can be performed using one of the two strategies:

- **Parallel**, where asynchronous fields can be resolved in unordered manner. This is default option in case of GraphQL queries due to fact, that they are readonly operations.
- **Sequential** where asynchronous fields must be resolved in correct order (as it was defined in query string). This is default mode for GraphQL mutations.

As result of GraphQL query execution, a dictionary with resolved values will be returned. It contains following fields:

- `documentId` which is query AST document hash code - it can be used to implement execution plan caching.
- `data` with a formated GraphQL response matching the requested query string.
- `errors` entry is optional. If it has been provided, it will contain list of errors, that occured during query execution.

This result can be serialized directly and returned to the client.