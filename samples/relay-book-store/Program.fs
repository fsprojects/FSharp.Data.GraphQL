module FSharp.Data.GraphQL.Samples.RelayBookStore.Entry

open System
open Microsoft.Data.Sqlite
open Giraffe
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Samples.RelayBookStore.Schema
open FSharp.Data.GraphQL.Server.AspNetCore
open FSharp.Data.GraphQL.Server.AspNetCore.Giraffe
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging

let errorHandler (ex : Exception) (log : ILogger) =
    log.LogError (EventId (), ex, "An unhandled exception has occurred while executing this request.")
    clearResponse >=> setStatusCode 500

[<EntryPoint>]
let main argv =
    use db = new SqliteConnection ("Data Source=app.db")

    let schema = Schema (queryType)
    let executor = Executor<Root> (schema)

    let rootFactory (ctx : HttpContext) : Root = Root (ctx.GetLogger (), db)

    let builder = WebApplication.CreateBuilder (argv)

    builder.Services.AddGiraffe().AddGraphQL<Root> (executor, rootFactory)
    |> ignore

    let app = builder.Build ()

    if app.Environment.IsDevelopment () then
        app.UseGraphQLAltair "/altair" |> ignore
        app.UseGraphQLGraphiQL "/graphiql" |> ignore
        app.UseGraphQLPlayground "/playground" |> ignore
        app.UseGraphQLVoyager "/voyager" |> ignore
        app.UseRouting () |> ignore
        app.UseEndpoints (fun endpoints -> endpoints.MapBananaCakePop (PathString "/cakePop") |> ignore)
        |> ignore

    app
        .UseGiraffeErrorHandler(errorHandler)
        .UseWebSockets()
        .UseWebSocketsForGraphQL<Root>()
        .UseGiraffe (HttpHandlers.graphQL<Root>)

    app.Run ()

    0
