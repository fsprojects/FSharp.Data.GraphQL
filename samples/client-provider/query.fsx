#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/FSharp.Data.GraphQL.Client.dll"

open FSharp.Data.GraphQL
open System.Collections.Generic

let [<Literal>] serverUrl = "http://localhost:8083"
let [<Literal>] query = "{ hero(id: \"1000\") { id, name } }"

type MyClient = GraphQLProvider<serverUrl>

MyClient.QueryHuman<query>()
|> Async.RunSynchronously
|> function hero -> printfn "My hero is %A" hero.name

let [<Literal>] query2 = "{ hero(id: \"1000\") { id, name }"
// This code won't compile as the query is not properly formed
// MyClient.QueryHuman<query2>()

