#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/FSharp.Data.GraphQL.Client.dll"

open FSharp.Data.GraphQL
open System.Collections.Generic

let [<Literal>] serverUrl = "http://localhost:8083"

// The name and arguments of the query will be automatically set by the type provider
let [<Literal>] queryFields = "{ id, name, friends { name } }"

type MyClient = GraphQLProvider<serverUrl>

let hero =
    MyClient.QueryHero<queryFields>("1000")
    |> Async.RunSynchronously

let droid =
    MyClient.QueryDroid<queryFields>("2000")
    |> Async.RunSynchronously

// Result is an option type
match hero with
| None -> ()
| Some hero ->
    printfn "My hero is %A" hero.name
    printfn "My hero's friends are:"
    hero.friends
    |> Array.choose (fun x -> x.name)
    |> Array.iter (printfn "- %s")

let [<Literal>] queryFields2 = "{ id, name"
// This code won't compile as the query is not properly formed
// MyClient.QueryHero<queryFields2>("1000")

