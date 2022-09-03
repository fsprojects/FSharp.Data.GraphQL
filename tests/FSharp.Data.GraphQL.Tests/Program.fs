namespace FSharp.Data.GraphQL.Tests

open System.Threading
open Xunit
open Xunit.Runners

module Program =
    [<EntryPoint>]
    let main (args : string []) =
        let result = ref 0
        let testAssembly = args.[0]
        use finished = new ManualResetEvent(false)
        use runner = AssemblyRunner.WithoutAppDomain(testAssembly)
        runner.OnExecutionComplete <- (fun _ -> finished.Set() |> ignore)
        runner.Start(null)
        finished.WaitOne() |> ignore
        finished.Dispose()
        !result

[<assembly: CollectionBehavior(DisableTestParallelization = true)>]
do()