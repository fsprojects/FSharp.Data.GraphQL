namespace FSharp.Data.GraphQL.Server.AspNetCore.Giraffe

open System.Threading.Tasks
open System.Net.Mime
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection

open FsToolkit.ErrorHandling
open Giraffe

open FSharp.Data.GraphQL.Server.AspNetCore

type HttpHandler = HttpFunc -> HttpContext -> HttpFuncResult

module HttpHandlers =

    let rec private moduleType = getModuleType <@ moduleType @>

    let ofTaskIResult ctx (taskRes : Task<IResult>) : HttpFuncResult = task {
        let! res = taskRes
        do! res.ExecuteAsync (ctx)
        return Some ctx
    }

    let ofTaskIResult2 ctx (taskRes : Task<Result<IResult, IResult>>) : HttpFuncResult = taskRes |> TaskResult.defaultWith id |> ofTaskIResult ctx

    let private handleGraphQL<'Root> (next : HttpFunc) (ctx : HttpContext) =

        let request = ctx.RequestServices.GetRequiredService<GraphQLRequestHandler<'Root>> ()
        request.HandleAsync () |> ofTaskIResult2 ctx

    let graphQL<'Root> : HttpHandler = choose [ POST; GET ] >=> handleGraphQL<'Root>

    let private isMultipartRequest (req : HttpRequest) =
        not (System.String.IsNullOrEmpty (req.ContentType))
#if NET6_0
        && req.ContentType.Contains "multipart/form-data"
#else
        && req.ContentType.Contains MediaTypeNames.Multipart.FormData
#endif

    let setRequestType : HttpHandler =
        fun (next) (ctx) ->
            if isMultipartRequest ctx.Request then
                setHttpHeader "Request-Type" "Multipart" next ctx
            else
                setHttpHeader "Request-Type" "Classic" next ctx
