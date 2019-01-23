namespace FSharp.Data.GraphQL.Benchmarks.Sql

open BenchmarkDotNet.Attributes
open FSharp.Data.GraphQL
open Helpers

[<MemoryDiagnoser>]
[<Config(typeof<JobConfig>); CoreJob; MonoJob>]
type QueryBenchmarks() =
    let mutable schema = Unchecked.defaultof<Schema<Root>>
    let mutable executor = Unchecked.defaultof<Executor<Root>>

    [<GlobalSetup>]
    member __.Setup() =
        schema <- Schema(SchemaDefinition.Query, config = SchemaConfig.Default)
        executor <- Executor(schema)

    [<Benchmark>]
    member __.SimpleQuery() = executeDirect executor Queries.simple

    [<Benchmark>]
    member __.FlatQuery() = executeDirect executor Queries.flat

    [<Benchmark>]
    member __.LongListQuery() = executeDirect executor Queries.longList

    [<Benchmark>]
    member __.LongStreamQuery() = executeDeferred executor Queries.longStream