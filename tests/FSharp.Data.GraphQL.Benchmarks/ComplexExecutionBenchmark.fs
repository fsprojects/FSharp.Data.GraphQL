/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
module FSharp.Data.GraphQL.ComplexExecutionBenchmark

#nowarn "40"

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open BenchmarkDotNet.Attributes
open FSharp.Data.GraphQL.Benchmarks

[<Config(typeof<GraphQLBenchConfig>); Jobs.MonoJob; Jobs.CoreJob>]
type ComplexExecutionBenchmark() = class end