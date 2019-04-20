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

    [<GlobalCleanup>]
    member __.Cleanup() =
        Database.connection.Dispose()

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
    member __.MovieTagsDirectAndRatingsDirect() = executeDirect (Queries.movieTagsAndRatings Direct Direct)

    [<Benchmark>]
    member __.MovieTagsStreamedAndRatingsDirect() = executeDeferred (Queries.movieTagsAndRatings Streamed Direct)

    [<Benchmark>]
    member __.MovieTagsStreamedAndRatingsStreamed() = executeDeferred (Queries.movieTagsAndRatings Streamed Streamed)

    [<Benchmark>]
    member __.MovieTagsDeferredAndRatingsDeferred() = executeDeferred (Queries.movieTagsAndRatings Deferred Deferred)

    [<Benchmark>]
    member __.MovieTagsStreamedAndRatingsDeferred() = executeDeferred (Queries.movieTagsAndRatings Streamed Deferred)

    [<Benchmark>]
    member __.AllMoviesDirect() = executeDirect (Queries.allMovies Direct)

    [<Benchmark>]
    member __.AllMoviesDeferred() = executeDeferred (Queries.allMovies Deferred)

    [<Benchmark>]
    member __.AllMoviesStreamed() = executeDeferred (Queries.allMovies Streamed)

    [<Benchmark>]
    member __.FiftyDirectMoviesWithDirectLinks() = executeDirect (Queries.fiftyMoviesWithLinks Direct Direct)

    [<Benchmark>]
    member __.FiftyDirectMoviesWithDeferredLinks() = executeDeferred (Queries.fiftyMoviesWithLinks Direct Deferred)

    [<Benchmark>]
    member __.FiftyDirectMoviesWithStreamedLinks() = executeDeferred (Queries.fiftyMoviesWithLinks Direct Streamed)

    [<Benchmark>]
    member __.FiftyDeferredMoviesWithDirectLinks() = executeDeferred (Queries.fiftyMoviesWithLinks Deferred Direct)

    [<Benchmark>]
    member __.FiftyStreamedMoviesWithDirectLinks() = executeDeferred (Queries.fiftyMoviesWithLinks Streamed Direct)

    [<Benchmark>]
    member __.FiftyDeferredMoviesWithDeferredLinks() = executeDeferred (Queries.fiftyMoviesWithLinks Deferred Deferred)

    [<Benchmark>]
    member __.FiftyDeferredMoviesWithStreamedLinks() = executeDeferred (Queries.fiftyMoviesWithLinks Deferred Streamed)

    [<Benchmark>]
    member __.FiftyStreamedMoviesWithDeferredLinks() = executeDeferred (Queries.fiftyMoviesWithLinks Streamed Deferred)

    [<Benchmark>]
    member __.FiftyStreamedMoviesWithStreamedLinks() = executeDeferred (Queries.fiftyMoviesWithLinks Streamed Streamed)