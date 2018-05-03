namespace FSharp.Data.GraphQL.Server.Middlewares

open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL

type QueryExtensionsMiddleware() =
    let middleware = fun (ctx : SchemaCompileContext) (next : SchemaCompileContext -> unit) ->
        next ctx

    interface IExecutorMiddleware with
        member __.ExecuteOperationAsync = None
        member __.PlanOperation = None
        member __.CompileSchema = Some middleware