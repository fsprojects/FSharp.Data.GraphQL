#r "../../bin/FSharp.Data.GraphQL.Client/FSharp.Data.GraphQL.Client.dll"
#r "node_modules/fable-core/Fable.Core.dll"

open FSharp.Data.GraphQL
open System.Collections.Generic

#if FABLE_COMPILER
Fable.Core.JsInterop.importAll<unit> "isomorphic-fetch"
#endif

let [<Literal>] serverUrl = "http://localhost:8083"

type MyClient = GraphQLProvider<serverUrl>

async {
    let! hero = MyClient.Queries.Hero("1000", fun c ->
                Fields(
                    c.name,
                    Selection(c.friends, fun f -> Fields(f.name)),
                    MyClient.Types.Human.On(fun h -> Fields(h.appearsIn))
                ))
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

let freeQuery = "{ hero(id: \"1000\"){ id, name, appearsIn, friends { name } } }"

let (?) (o: obj) (k: string) =
    match o with
    | :? IDictionary<string,obj> as dic -> dic.[k]
    | _ -> null

let hero2 =
    MyClient.Query(freeQuery)
    |> Async.Catch
    |> Async.RunSynchronously
    |> function
    | Choice1Of2 data ->
        printfn "Hero's name: %A" data?hero?name
    | Choice2Of2 err ->
        printfn "ERROR: %s" err.Message
