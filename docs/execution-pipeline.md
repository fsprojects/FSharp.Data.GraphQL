
Server execution pipeline
======================

The major work done by FSharp.Data.GraphQL.Server consists of the following phases:

1. Compilation phase: when a new `Schema` is being created;
2. Planning phase: when a GraphQL query as a raw string has been supplied;
3. Execution phase: when a query is being executed and a result is produced

## Compilation phase

The compilation phase is when we're creating a new schema instance. After the schema is created, all types defined within that schema will remain unchanged. Also during this phase, the server will invoke all operations that can be done independent of any future query. Creating a new schema is an expensive process which is independent of any request and therefore **should not** be done on each request.

During the compilation phase, basically the following operations are performed:

- Validation that all type definitions conform to the GraphQL specification;
- Creation of an [introspection](https://facebook.github.io/graphql/#sec-Introspection)-oriented representation of the schema;
- Preparation of a value resolution pipeline for each of the fields defined in the schema.

## Planning phase

In many cases, it makes sense to handle planning and execution phases sequentially as one big phase. However, one can handle them separately by calling `schema.CreateExecutionPlan(graphQlQueryString)`, which produces a so-called `ExecutionPlan` without actually executing a query.

As the name suggests, `ExecutionPlan` and its components (a tree of objects known as `ExecutionInfo`s) describes how the query is going to be executed. This includes:

- Inlining GraphQL query fragments;
- Defining fields resolution strategy - sequential or parallel;
- Combining information from the query AST (resolved fields / aliases) with server-side information about them (field and type definitions);
- Preparation of the hooks in the execution chain that will be supplied with potential variables upon execution.

Spliting planning and execution phases is a good idea when you have the same GraphQL query requested many times (with potentially different variables). This way you can compute the execution plan once and cache it. You can use `executionPlan.DocumentId` as a cache identifier. DocumentId is also returned as one of the top level fields in GraphQL response, so it can be used from the client side. Other GraphQL implementations describe that technique as **persistent queries**.

## Execution phase 

The execution phase is performed when we want to resolve a response from an GraphQL server given a query execution plan and an optional set of variables provided by the client. Usually it's called together with the planning phase descibed above.

During execution phase, we perform a traversal of the execution plan tree; create execution context objects for each of the fields and supply them along with data resolved from previous higher level resolver responses recursively.

A field resolution context contains metadata such as:

- Information about the execution of the schema itself;
- Variables provided with the request;
- Field parameters (in case the corresponding field defined any arguments);
- Type definition of both parent and expected return type;
- `ExecutionInfo` which is a fragment of the execution plan itself related to the current field and all of its subsequent fields.

The execution phase can be performed using one of the two strategies:

- **Parallel**: in which asynchronous fields can be resolved in any order. This is the default mode for GraphQL queries due to the fact that they are readonly operations.
- **Sequential**: in which asynchronous fields must be resolved in a specific order: as it was defined in the query. This is default mode for GraphQL mutations.

The result of a GraphQL query execution is a dictionary with resolved values. This dictionary contains the following fields:

- `documentId`: which is the hash code of the query's AST document - it can be used to implement execution plan caching (persistent queries).
- `data`: with a formated GraphQL response matching the requested query.
- `errors`: optional. If it has been provided, it will contain a list of errors that occured during query execution.

This result can then be serialized and returned to the client.