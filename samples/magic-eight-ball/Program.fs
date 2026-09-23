module FSharp.Data.GraphQL.Samples.MagicEightBall.Program

open System
open Suave
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Server.Suave
open FSharp.Data.GraphQL.Samples.MagicEightBall.GraphiQL
open FSharp.Data.GraphQL.Samples.MagicEightBall.Schema

[<EntryPoint>]
let main _ =
    let random = Random ()

    let rootFactory (_ctx : HttpContext) : Root = { Random = random }

    let executor = Executor (schema)

    let app = choose [ webPart; GraphQL.graphQL executor rootFactory ]

    startWebServer defaultConfig app

    0
