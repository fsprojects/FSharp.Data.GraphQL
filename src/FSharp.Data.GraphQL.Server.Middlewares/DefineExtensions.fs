namespace FSharp.Data.GraphQL.Server.Middlewares

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open MetadataKeys

[<AutoOpen>]
module DefineExtensions =
    type Define with
        static member QueryWeightMiddleware(?threshold : float) : IExecutorMiddleware =
            match threshold with
            | Some t -> upcast QueryWeightMiddleware(t)
            | None -> upcast QueryWeightMiddleware()

        static member ObjectFilterMiddleware<'ObjectType, 'ListType>() : IExecutorMiddleware =
            upcast ObjectFilterMiddleware<'ObjectType, 'ListType>()