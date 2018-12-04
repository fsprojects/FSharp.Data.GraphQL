#r "../../bin/FSharp.Data.GraphQL.Client/net47/FSharp.Data.GraphQL.Client.dll"
#load "literals.fsx"

open System
open FSharp.Data.GraphQL
open System.Collections.Generic
open Literals

#if FABLE_COMPILER
#r "node_modules/fable-core/Fable.Core.dll"
Fable.Core.JsInterop.importAll<unit> "isomorphic-fetch"
#endif

// Using the provider like this loads the introspection schema from the server
// It needs a connection to the server to work in design time
//type MyClient = GraphQLProvider<url>

// Using the provider like this makes you load the introspection schema from a string
// The introspection literal is in the literals.fsx file
//type MyClient = GraphQLProvider<url, introspection>

// Using the provider like this makes you load the introspection schema from a file
// The file can be load as a relative path to the path of the project or script
type MyClient = GraphQLProvider<url, "introspection.json">

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
|> Async.RunSynchronously

let freeQuery = "{ hero(id: \"1000\"){ id, name, appearsIn, friends { name } } }"
let (?) (o: obj) (k: string) =
    match o with
    | :? IDictionary<string,obj> as dic -> dic.[k]
    | _ -> null

MyClient.Query(freeQuery)
|> Async.Catch
|> Async.RunSynchronously
|> function
| Choice1Of2 data ->
    printfn "Hero's name: %A" data?hero?name
| Choice2Of2 err ->
    match err with
    | :? AggregateException as agex -> agex.InnerExceptions |> Seq.iter (fun err -> printfn "ERROR: %s" err.Message)
    | _ -> printfn "ERROR: %s" err.Message