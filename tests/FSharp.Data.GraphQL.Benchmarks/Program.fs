module Program

open System
open System.IO
open System.Collections.Concurrent
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open FSharp.Data.GraphQL.AsyncValBenchmark
open FSharp.Data.GraphQL.ParsingBenchmark
open FSharp.Data.GraphQL.ExecutionBenchmark

let defaultSwitch () = BenchmarkSwitcher [| typeof<AsyncValBenchmark>; typeof<SimpleExecutionBenchmark>; typeof<ParsingBenchmark>  |]

[<EntryPoint>]
let Main args =
    let asm = Reflection.Assembly.Load("FSharp.Core")
    printfn "%+A" asm
    defaultSwitch().Run args 
    0