module FSharp.Data.GraphQL.Benchmarks.Sql.Program

open BenchmarkDotNet.Running

let [<Literal>] private SuccessCode = 0

let connectionString = "Data Source=.;Integrated Security=True"

[<EntryPoint>]
let main _ =
    DatabaseSetup.buildIfNotExists connectionString |> Async.RunSynchronously
    BenchmarkRunner.Run<QueryBenchmarks>() |> ignore
    SuccessCode
