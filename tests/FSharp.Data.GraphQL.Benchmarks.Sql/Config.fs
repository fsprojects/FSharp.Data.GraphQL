namespace FSharp.Data.GraphQL.Benchmarks.Sql

open BenchmarkDotNet.Configs
open BenchmarkDotNet.Columns
open BenchmarkDotNet.Exporters

type JobConfig() as this= 
    inherit ManualConfig()
    do
        this.Add(StatisticColumn.Mean, StatisticColumn.Min, StatisticColumn.Max, StatisticColumn.OperationsPerSecond)
        this.Add(MarkdownExporter.GitHub)