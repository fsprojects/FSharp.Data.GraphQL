namespace FSharp.Data.GraphQL.Server.AspNetCore.Oxpecker

open System.Runtime.InteropServices
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection

open FsToolkit.ErrorHandling
open Oxpecker

open FSharp.Data.GraphQL.Server.AspNetCore

module HttpEndpointHandlers =

    let internal writeIResult2 (ctx : HttpContext) (taskRes : Task<Result<IResult, IResult>>) : Task = task {
        let! result = taskRes |> TaskResult.defaultWith id
        do! ctx.Write result
    }

    let graphQL<'Root> (ctx : HttpContext) : Task =

        let request = ctx.RequestServices.GetRequiredService<GraphQLRequestHandler<'Root>> ()
        request.HandleAsync () |> writeIResult2 ctx

module HttpEndpoints =

    let graphQL<'Root> (route, configure) : Endpoint =
        SimpleEndpoint (Verbs [ HttpVerb.GET; HttpVerb.POST ], route, HttpEndpointHandlers.graphQL<'Root>, configure)
