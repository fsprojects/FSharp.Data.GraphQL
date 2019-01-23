module FSharp.Data.Benchmarks.Sql.Program

let [<Literal>] private SuccessCode = 0

let connectionString = "Data Source=.;Integrated Security=True"

[<EntryPoint>]
let main argv =
    DatabaseSetup.buildIfNotExists connectionString |> Async.RunSynchronously
    SuccessCode
