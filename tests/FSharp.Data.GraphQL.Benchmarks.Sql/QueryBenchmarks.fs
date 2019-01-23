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
    member __.MovieQuery() = executeDirect executor Queries.movie

    [<Benchmark>]
    member __.MovieUserRatingQuery() = executeDirect executor Queries.movieUserRating

    [<Benchmark>]
    member __.MovieRatingsDirectQuery() = executeDirect executor Queries.movieRatingsDirect

    [<Benchmark>]
    member __.MovieRatingsStreamed() = executeDeferred executor Queries.movieRatingsStreamed

    [<Benchmark>]
    member __.MovieRatingsDeferred() = executeDeferred executor Queries.movieRatingsDeferred