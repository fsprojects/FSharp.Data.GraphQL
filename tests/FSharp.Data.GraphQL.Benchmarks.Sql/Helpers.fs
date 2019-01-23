module internal FSharp.Data.GraphQL.Benchmarks.Sql.Helpers

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Execution
open System.Text
open System

let private fail (errs : #seq<Error>) =
    let sb = StringBuilder()
    for err in errs do
        sb.AppendLine(err.ToString()) |> ignore
    failwithf "Query execution resulted in errors:%s%s" Environment.NewLine (sb.ToString())

let private execute (executor : Executor<Root>) (query : string) =
    let root = { RequestId = Guid.NewGuid().ToString() }
    executor.AsyncExecute(query, data = root) |> Async.RunSynchronously

let executeDirect executor query =
    match execute executor query with
    | Direct (_, errors) -> if not (Seq.isEmpty errors) then fail errors
    | _ -> failwith "Expected direct result!"

let executeDeferred executor query =
    match execute executor query with
    | Deferred (_, errors, deferred) ->
        if not (Seq.isEmpty errors) then fail errors
        deferred |> Observable.toSeq |> Seq.iter (fun _ -> ())
    | _ -> failwith "Expected deferred result!"