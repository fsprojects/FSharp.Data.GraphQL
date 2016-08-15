#r "../../bin/FSharp.Data.GraphQL.Client/FSharp.Data.GraphQL.Client.dll"

open FSharp.Data.GraphQL
open System.Collections.Generic

#if FABLE
#r "node_modules/fable-core/Fable.Core.dll"
Fable.Core.JsInterop.importAll<unit> "isomorphic-fetch"
#endif

let [<Literal>] serverUrl = "http://localhost:8083"

// The name and arguments of the query will be automatically set by the type provider
let [<Literal>] queryFields = "{ id, description, completed }"

let [<Literal>] taskId = "cfd81e81-18f4-45b9-bd69-74c84fb1eaa2"

type MyClient = GraphQLProvider<serverUrl>

async {
    let! res = MyClient.Mutations.Completed(taskId, true)
    printfn "Success: %b" res
} |> Async.StartImmediate

async {
    let projection = <@@ fun (t: MyClient.Types.Task) -> t.description, t.completed @@>
    let! task = MyClient.Queries.Task(taskId, projection)
    match task with
    | None -> ()
    | Some task ->
        printfn "Task: %s" task.description
        printfn "Status: %b" task.completed
}
|> Async.StartImmediate
