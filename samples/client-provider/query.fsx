#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/FSharp.Data.GraphQL.Client.dll"

open FSharp.Data.GraphQL

let [<Literal>] serverUrl = "http://localhost:8083"
let [<Literal>] query = "{ hero(id: \"1000\") { name } }"

type MyClient = GraphQLProvider<serverUrl>

MyClient.Query<query>() |> Async.RunSynchronously |> printfn "%A"
