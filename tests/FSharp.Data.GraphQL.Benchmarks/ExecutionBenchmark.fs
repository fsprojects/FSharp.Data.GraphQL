/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
module FSharp.Data.GraphQL.ExecutionBenchmark

open System
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution

type Person = 
    { Id : string
      Name : string option
      Friends : string list
      HomePlanet : string option }

let humans = 
    [ { Id = "1000"
        Name = Some "Luke Skywalker"
        Friends = [ "1002"; "1003" ]
        HomePlanet = Some "Tatooine" }
      { Id = "1001"
        Name = Some "Darth Vader"
        Friends = [ "1004" ]
        HomePlanet = Some "Tatooine" }
      { Id = "1002"
        Name = Some "Han Solo"
        Friends = [ "1000"; "1003" ]
        HomePlanet = None }
      { Id = "1003"
        Name = Some "Leia Organa"
        Friends = [ "1000"; "1002" ]
        HomePlanet = Some "Alderaan" }
      { Id = "1004"
        Name = Some "Wilhuff Tarkin"
        Friends = [ "1001" ]
        HomePlanet = None } ]

let getPerson id = humans |> List.tryFind (fun h -> h.Id = id)

let rec Person = 
    Define.Object(name = "Person", isTypeOf = (fun o -> o :? Person), 
                  fieldsFn = fun () -> 
                      [ Define.Field("id", String, resolve = fun _ person -> person.Id)
                        Define.Field("name", Nullable String, resolve = fun _ person -> person.Name)
                        Define.Field("friends", Nullable(ListOf(Nullable Person)), 
                                     resolve = fun _ person -> 
                                         person.Friends
                                         |> List.map getPerson
                                         |> List.toSeq
                                         |> Some)
                        Define.Field("homePlanet", String) ])

let Query = 
    Define.Object
        (name = "Query", 
         fields = [ Define.Field
                        ("hero", Nullable Person, "Retrieves a person by provided id", [ Define.Input("id", String) ], 
                         fun ctx () -> getPerson (ctx.Arg("id"))) ])
let schema = Schema(Query)
let simpleQueryString = """{ 
    hero(id: "1000") { 
        id
    } 
}"""
let simpleAst = parse simpleQueryString
let flatQueryString = """{ 
    hero(id: "1000") { 
        id,
        name, 
        homePlanet
    } 
}"""
let flatAst = parse flatQueryString
let nestedQueryString = """{ 
    hero(id: "1000") { 
        id, 
        name, 
        friends { 
            id, 
            name, 
            friends { 
                id, 
                name, 
                friends { 
                    id, 
                    name 
                } 
            } 
        } 
    } 
}"""
let nestedAst = parse nestedQueryString

open BenchmarkDotNet.Attributes

[<Config(typeof<GraphQLBenchConfig>)>]
type SimpleExecutionBenchmark() = 
    
    [<Setup>]
    member x.Setup() = ()
    
    [<Benchmark>]
    member x.BenchmarkSimpleQueryUnparsed() = schema.AsyncExecute(simpleQueryString) |> Async.RunSynchronously
    
    [<Benchmark>]
    member x.BenchmarkSimpleQueryParsed() = schema.AsyncExecute(simpleAst) |> Async.RunSynchronously
    
    [<Benchmark>]
    member x.BenchmarkFlatQueryUnparsed() = schema.AsyncExecute(flatQueryString) |> Async.RunSynchronously
    
    [<Benchmark>]
    member x.BenchmarkFlatQueryParsed() = schema.AsyncExecute(flatAst) |> Async.RunSynchronously
    
    [<Benchmark>]
    member x.BenchmarkNestedQueryUnparsed() = schema.AsyncExecute(nestedQueryString) |> Async.RunSynchronously
    
    [<Benchmark>]
    member x.BenchmarkNestedQueryParsed() = schema.AsyncExecute(nestedAst) |> Async.RunSynchronously

[<Config(typeof<GraphQLBenchConfig>)>]
type RepeatableExecutionBenchmark() = 
    
    let repeat times action = 
        for i in 0..times do
            action()
            |> Async.RunSynchronously
            |> ignore
    
    let N = 1000
    let repeatN = repeat N
    
    [<Setup>]
    member x.Setup() = ()
    
    [<Benchmark>]
    member x.BenchmarkSimpleQueryUnparsed() = repeatN <| fun () -> schema.AsyncExecute(simpleQueryString)
    
    [<Benchmark>]
    member x.BenchmarkSimpleQueryParsed() = repeatN <| fun () -> schema.AsyncExecute(simpleAst)
    
    [<Benchmark>]
    member x.BenchmarkFlatQueryUnparsed() = repeatN <| fun () -> schema.AsyncExecute(flatQueryString)
    
    [<Benchmark>]
    member x.BenchmarkFlatQueryParsed() = repeatN <| fun () -> schema.AsyncExecute(flatAst)
    
    [<Benchmark>]
    member x.BenchmarkNestedQueryUnparsed() = repeatN <| fun () -> schema.AsyncExecute(nestedQueryString)
    
    [<Benchmark>]
    member x.BenchmarkNestedQueryParsed() = repeatN <| fun () -> schema.AsyncExecute(nestedAst)
