#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/FSharp.Data.GraphQL.Client.dll"

open FSharp.Data.GraphQL
open System.Collections.Generic

let [<Literal>] serverUrl = "http://localhost:8083"
let [<Literal>] query = "{ hero(id: \"1000\") { id, name } }"

let (?) (o: obj) (k: string) = (o :?> IDictionary<string,obj>).Item(k)

type MyClient = GraphQLProvider<serverUrl>

MyClient.Query<query>()
|> Async.RunSynchronously
|> function
    | Choice1Of2 data -> printfn "My hero is %A" (data?hero :?> MyClient.Human).name
    | Choice2Of2 errors -> printfn "Error: %A" errors
