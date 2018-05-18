namespace FSharp.Data.GraphQL.Server.Middlewares

open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL

/// Contains extensions for the Define module.
[<AutoOpen>]
module DefineExtensions =
    type Define with
        /// <summary>
        /// Creates a query weight middleware, with the specified query weight threshold.
        /// </summary>
        /// <param name="threshold">
        /// A float value representing the maximum threshold for any query analyzed by the middleware.
        /// </param>
        /// <remarks>
        /// If no threshold value is provided, the middleware will look for it in the ExeuctionContext metadata.
        /// If no threshold value is present on the ExecutionContext metadata, a default value of 0.0 will be used.
        /// </remarks
        static member QueryWeightMiddleware(?threshold : float) : IExecutorMiddleware =
            match threshold with
            | Some t -> upcast QueryWeightMiddleware(t)
            | None -> upcast QueryWeightMiddleware()

        /// <summary>
        /// Creates a object list filter middleware for an object of 'ObjectType, and a list field of 'ListType type.
        /// </summary>
        /// <remarks>
        /// When defined, this middleware analyzes the current schema on the schema compiling phase,
        /// and creates an argument called "filter" for each list field that has the item type defined by the generic type 'ListType.
        /// This argument can be used on the query to specify a filter with operations like "less than", "equals", etc. on the
        /// field of the specified object of 'ObjectType type.
        /// </remarks>
        static member ObjectListFilterMiddleware<'ObjectType, 'ListType>() : IExecutorMiddleware =
            upcast ObjectListFilterMiddleware()