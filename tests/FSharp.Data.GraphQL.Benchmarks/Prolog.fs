/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

[<AutoOpen>]
module FSharp.Data.GraphQL.BenchmarkProlog

open BenchmarkDotNet.Configs
open BenchmarkDotNet.Columns
open BenchmarkDotNet.Diagnosers
open BenchmarkDotNet.Exporters

type GraphQLBenchConfig() as this=
    inherit ManualConfig()
    do  this.AddDiagnoser(MemoryDiagnoser.Default)
            .AddColumn(StatisticColumn.Mean)
            .AddColumn(StatisticColumn.Min)
            .AddColumn(StatisticColumn.Max)
            .AddColumn(StatisticColumn.OperationsPerSecond)
            .AddExporter(MarkdownExporter.GitHub)
            .AddExporter(Csv.CsvExporter(Csv.CsvSeparator.Comma)) |> ignore
