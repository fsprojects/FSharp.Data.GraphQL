namespace FSharp.Data.GraphQL.Server.AspNetCore.Giraffe

open System.Runtime.InteropServices
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection

open FsToolkit.ErrorHandling
open Oxpecker

open FSharp.Data.GraphQL.Server.AspNetCore

module HttpEndpoints =

    let internal writeIResult2 (ctx : HttpContext) (taskRes: Task<Result<IResult, IResult>>) : Task = task {
        let! result = taskRes |> TaskResult.defaultWith id
        do! ctx.Write result
    }

    let private handleGraphQL<'Root> (ctx : HttpContext) : Task =

        let request = ctx.RequestServices.GetRequiredService<GraphQLRequest<'Root>>()
        request.HandleAsync () |> writeIResult2 ctx

    let graphQL<'Root> (route, [<Optional>] configure) : Endpoint = SimpleEndpoint(Verbs [HttpVerb.GET; HttpVerb.POST], route, handleGraphQL<'Root>, configure)
