module internal FSharp.Data.GraphQL.Benchmarks.Sql.Helpers

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Execution
open System.Text
open System

let private fail (errs : #seq<Error>) =
    let sb = StringBuilder()
    for err in errs do
        sb.AppendLine(err.ToString()) |> ignore
    failwithf "Query execution has errors:%s%s" Environment.NewLine (sb.ToString())

let private execute (executor : Executor<Root>) (query : string) =
    let root = { RequestId = Guid.NewGuid().ToString() }
    executor.AsyncExecute(query, data = root) |> Async.RunSynchronously

let executeDirect executor query =
    match execute executor query with
    | Direct (data, errors) -> 
        if not (Seq.isEmpty errors) 
        then fail errors 
        else data
    | _ -> failwith "Expected direct result!"

let executeDeferred executor query =
    match execute executor query with
    | Deferred (data, errors, deferred) ->
        if not (Seq.isEmpty errors) 
        then fail errors
        else (data, deferred |> Observable.toSeq |> Array.ofSeq)
    | _ -> failwith "Expected deferred result!"