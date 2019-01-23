namespace FSharp.Data.GraphQL.Benchmarks.Sql

open BenchmarkDotNet.Attributes
open FSharp.Data.GraphQL

[<MemoryDiagnoser>]
[<Config(typeof<JobConfig>); CoreJob; MonoJob>]
type QueryBenchmarks() =
    let mutable schema = Unchecked.defaultof<Schema<Root>>
    let mutable executor = Unchecked.defaultof<Executor<Root>>
    let executeDirect query = Helpers.executeDirect executor query
    let executeDeferred query = Helpers.executeDeferred executor query

    [<GlobalSetup>]
    member __.Setup() =
        schema <- Schema(SchemaDefinition.Query, config = SchemaConfig.Default)
        executor <- Executor(schema)

    [<Benchmark>]
    member __.SingleMovie() = executeDirect Queries.singleMovie

    [<Benchmark>]
    member __.SingleMovieSingleUserRating() = executeDirect Queries.singleMovieSingleUserRating

    [<Benchmark>]
    member __.MovieTagsDirect() = executeDirect (Queries.movieTags Direct)

    [<Benchmark>]
    member __.MovieTagsStreamed() = executeDeferred (Queries.movieTags Streamed)

    [<Benchmark>]
    member __.MovieTagsDeferred() = executeDeferred (Queries.movieTags Deferred)

    [<Benchmark>]
    member __.MovieTagsDirectAndLinksDirect() = executeDirect (Queries.movieTagsAndLinks Direct Direct)

    [<Benchmark>]
    member __.MovieTagsStreamedAndLinksDirect() = executeDeferred (Queries.movieTagsAndLinks Streamed Direct)

    [<Benchmark>]
    member __.MovieTagsStreamedAndLinksStreamed() = executeDeferred (Queries.movieTagsAndLinks Streamed Streamed)

    [<Benchmark>]
    member __.MovieTagsDeferredAndLinksDeferred() = executeDeferred (Queries.movieTagsAndLinks Deferred Deferred)

    [<Benchmark>]
    member __.MovieTagsStreamedAndLinksDeferred() = executeDeferred (Queries.movieTagsAndLinks Streamed Deferred)