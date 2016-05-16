/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

[<AutoOpen>]
module FSharp.Data.GraphQL.BenchmarkProlog

open BenchmarkDotNet.Configs
open BenchmarkDotNet.Columns
open BenchmarkDotNet.Exporters
open BenchmarkDotNet.Diagnostics.Windows

type GraphQLBenchConfig() as this= 
    inherit ManualConfig()
    do
        this.Add(StatisticColumn.Mean, StatisticColumn.Min, StatisticColumn.Max, StatisticColumn.OperationsPerSecond)
        this.Add(MarkdownExporter.GitHub)
        this.Add(MemoryDiagnoser())