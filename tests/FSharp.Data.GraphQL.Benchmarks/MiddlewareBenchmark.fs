// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc
module FSharp.Data.GraphQL.MiddlewaresBenchmark

#nowarn "40"

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.ExecutionBenchmark
open FSharp.Data.GraphQL.Shared
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open BenchmarkDotNet.Attributes
open FSharp.Data.GraphQL.Benchmarks
open FSharp.Data.GraphQL.Server.Middleware

[<Config(typeof<GraphQLBenchConfig>)>]
[<SimpleJob>]
type SimpleExecutionWithMiddlewaresBenchmark() =
    let mutable schema : Schema<unit> = Unchecked.defaultof<Schema<unit>>
    let mutable middlewares : IExecutorMiddleware list = []
    let mutable schemaProcessor : Executor<unit> = Unchecked.defaultof<Executor<unit>>
    let mutable simpleAst : Ast.Document = Unchecked.defaultof<Ast.Document>
    let mutable flatAst : Ast.Document = Unchecked.defaultof<Ast.Document>
    let mutable nestedAst : Ast.Document = Unchecked.defaultof<Ast.Document>
    let mutable simpleExecutionPlan : ExecutionPlan = Unchecked.defaultof<ExecutionPlan>
    let mutable flatExecutionPlan : ExecutionPlan = Unchecked.defaultof<ExecutionPlan>
    let mutable nestedExecutionPlan : ExecutionPlan = Unchecked.defaultof<ExecutionPlan>
    let mutable filteredExecutionPlan : ExecutionPlan = Unchecked.defaultof<ExecutionPlan>
    let mutable filteredAst : Ast.Document = Unchecked.defaultof<Ast.Document>
    let inputContext = fun () -> MockInputExecutionContext() :> IInputExecutionContext

    [<GlobalSetup>]
    member _.Setup() =
        schema <- Schema(SchemaDefinition.Query)
        middlewares <- [ Define.QueryWeightMiddleware(20.0); Define.ObjectListFilterMiddleware<Person, Person option>() ]
        schemaProcessor <- Executor(schema, middlewares)
        simpleAst <- parse QueryStrings.simple
        flatAst <- parse QueryStrings.flat
        nestedAst <- parse QueryStrings.nested
        simpleExecutionPlan <- schemaProcessor.CreateExecutionPlanOrFail(simpleAst)
        flatExecutionPlan <- schemaProcessor.CreateExecutionPlanOrFail(flatAst)
        nestedExecutionPlan <- schemaProcessor.CreateExecutionPlanOrFail(nestedAst)
        filteredAst <- parse QueryStrings.filtered
        filteredExecutionPlan <- schemaProcessor.CreateExecutionPlanOrFail(filteredAst)

    [<Benchmark>]
    member _.BenchmarkSimpleQueryUnparsed() = schemaProcessor.AsyncExecute(QueryStrings.simple, inputContext) |> Async.RunSynchronously

    [<Benchmark>]
    member _.BenchmarkSimpleQueryParsed() = schemaProcessor.AsyncExecute(simpleAst, inputContext) |> Async.RunSynchronously

    [<Benchmark>]
    member _.BenchmarkSimpleQueryPlanned() = schemaProcessor.AsyncExecute(simpleExecutionPlan, inputContext) |> Async.RunSynchronously

    [<Benchmark>]
    member _.BenchmarkFlatQueryUnparsed() = schemaProcessor.AsyncExecute(QueryStrings.flat, inputContext) |> Async.RunSynchronously

    [<Benchmark>]
    member _.BenchmarkFlatQueryParsed() = schemaProcessor.AsyncExecute(flatAst, inputContext) |> Async.RunSynchronously

    [<Benchmark>]
    member _.BenchmarkFlatQueryPlanned() = schemaProcessor.AsyncExecute(flatExecutionPlan, inputContext) |> Async.RunSynchronously

    [<Benchmark>]
    member _.BenchmarkNestedQueryUnparsed() = schemaProcessor.AsyncExecute(QueryStrings.nested, inputContext) |> Async.RunSynchronously

    [<Benchmark>]
    member _.BenchmarkNestedQueryParsed() = schemaProcessor.AsyncExecute(nestedAst, inputContext) |> Async.RunSynchronously

    [<Benchmark>]
    member _.BenchmarkNestedQueryPlanned() = schemaProcessor.AsyncExecute(nestedExecutionPlan, inputContext) |> Async.RunSynchronously

    [<Benchmark>]
    member _.BenchmarkFilteredQueryUnparsed() = schemaProcessor.AsyncExecute(QueryStrings.filtered, inputContext) |> Async.RunSynchronously

    [<Benchmark>]
    member _.BenchmarkFilteredQueryParsed() = schemaProcessor.AsyncExecute(filteredAst, inputContext) |> Async.RunSynchronously

    [<Benchmark>]
    member _.BenchmarkFilteredQueryPlanned() = schemaProcessor.AsyncExecute(filteredExecutionPlan, inputContext) |> Async.RunSynchronously
