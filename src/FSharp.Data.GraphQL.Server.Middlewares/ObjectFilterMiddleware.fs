namespace FSharp.Data.GraphQL.Server.Middlewares

open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL

type ObjectFilterMiddleware<'Val, 'Res>() =
    let middleware = fun (ctx : SchemaCompileContext) (next : SchemaCompileContext -> unit) ->
        let objListWithArgs (x : FieldDef<'Val>) = 
            x.WithArgs([ Define.ObjectFilterInput ])
             .WithResolveMiddleware<'Res seq>(fun ctx input next -> printfn "%A" (ctx.Args |> Map.toList); next ctx input)
        ctx.Schema.TypeMap.ListFields<'Val, 'Res>()
        |> Seq.map (fun (o, fields) -> fields |> Seq.map objListWithArgs |> o.WithFields)
        |> Seq.iter (fun o -> ctx.Schema.TypeMap.SetType(o, true))
        next ctx

    interface IExecutorMiddleware with
        member __.ExecuteOperationAsync = None
        member __.PlanOperation = None
        member __.CompileSchema = Some middleware