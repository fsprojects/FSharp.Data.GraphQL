namespace FSharp.Data.GraphQL.Server.Middlewares

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns
open FSharp.Data.GraphQL.Execution

type QueryExtensionsMiddleware() =
    let middleware = fun (ctx : SchemaCompileContext) (next : SchemaCompileContext -> unit) ->
        // TODO : analyze schema changes
        next ctx