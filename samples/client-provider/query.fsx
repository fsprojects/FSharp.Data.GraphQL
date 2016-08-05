#r "../../bin/FSharp.Data.GraphQL.Client/FSharp.Data.GraphQL.Client.dll"

open FSharp.Data.GraphQL
open System.Collections.Generic

#if FABLE
#r "node_modules/fable-core/Fable.Core.dll"
Fable.Core.JsInterop.importAll<unit> "isomorphic-fetch"
#endif

let [<Literal>] serverUrl = "http://localhost:8083"

// The name and arguments of the query will be automatically set by the type provider
let [<Literal>] queryFields = "{ id, name, appearsIn, friends { name } }"
let [<Literal>] queryFieldsWithFragments = "{ ...data, friends { name } } fragment data on Human { id, name, appearsIn }"

type MyClient = GraphQLProvider<serverUrl>

// let droid =
//     MyClient.Queries.Droid<queryFields>("2000")
//     |> Async.RunSynchronously

async {
    let! hero = MyClient.Queries.Hero<queryFieldsWithFragments>("1000")
    match hero with
    | None -> ()
    | Some hero ->
        printfn "My hero is %A" hero.name
        printfn "Appears in %O: %b" MyClient.Types.Episode.Empire
            (hero.appearsIn |> Array.exists ((=) MyClient.Types.Episode.Empire))
        printfn "My hero's friends are:"
        hero.friends
        |> Array.choose (fun x -> x.name)
        |> Array.iter (printfn "- %s")
}
|> Async.StartImmediate

// let freeQuery = "{ hero(id: \"1000\"){ id, name, appearsIn, friends { name } } }"

// let hero2 =
//     MyClient.Query(freeQuery)
//     |> Async.Catch
//     |> Async.RunSynchronously
//     |> function
//     | Choice1Of2 data -> (data :?> IDictionary<string,obj>).["hero"] :?> MyClient.Types.Human |> Some
//     | Choice2Of2 err -> printfn "ERROR: %s" err.Message; None

// printfn "%A" hero2.Value.name

let [<Literal>] queryFields2 = "{ id, name"
// This code won't compile as the query is not properly formed
//MyClient.QueryHero<queryFields2>("1000")

