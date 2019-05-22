module Program

open BenchmarkDotNet.Running
open FSharp.Data.GraphQL.AsyncValBenchmark
open FSharp.Data.GraphQL.ParsingBenchmark
open FSharp.Data.GraphQL.ExecutionBenchmark
open FSharp.Data.GraphQL.MiddlewaresBenchmark

let defaultSwitch () = 
    BenchmarkSwitcher [| 
        typeof<AsyncValBenchmark>
        typeof<SimpleExecutionBenchmark>
        typeof<ParsingBenchmark>
        typeof<SimpleExecutionWithMiddlewaresBenchmark>
    |]

[<EntryPoint>]
let Main args =
    defaultSwitch().Run args |> ignore
    0
