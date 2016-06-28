#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/FSharp.Data.GraphQL.Client.dll"

open FSharp.Data.GraphQL
open System.Collections.Generic

let [<Literal>] serverUrl = "http://localhost:8083"
let [<Literal>] query = "{ hero(id: \"1000\") { id, name, friends { name } } }"

type MyClient = GraphQLProvider<serverUrl>

let hero =
    MyClient.QueryHuman<query>()
    |> Async.RunSynchronously

printfn "My hero is %A" hero.name
printfn "My hero's friends are:"
hero.friends
|> Array.choose (fun x -> x.name)
|> Array.iter (printfn "- %s")


let [<Literal>] query2 = "{ hero(id: \"1000\") { id, name }"
// This code won't compile as the query is not properly formed
// MyClient.QueryHuman<query2>()

