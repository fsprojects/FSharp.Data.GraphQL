namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open System
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection

type Root(ctx : HttpContext) =

    member _.RequestId = ctx.TraceIdentifier
    member _.RequestAborted: System.Threading.CancellationToken = ctx.RequestAborted
    member _.ServiceProvider: IServiceProvider = ctx.RequestServices
    member root.GetRequiredService<'t>() = root.ServiceProvider.GetRequiredService<'t>()
