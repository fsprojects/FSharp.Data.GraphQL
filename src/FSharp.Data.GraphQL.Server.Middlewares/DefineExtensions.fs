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
        /// <param name="reportToMetadata">
        /// A boolean flag indicating if the values of the threshold and the weight of the current query should
        /// be reported to the metadata object in the GQLResponse.
        /// </param>
        static member QueryWeightMiddleware(threshold : float, ?reportToMetadata : bool) : IExecutorMiddleware =
            let reportToMetadata = defaultArg reportToMetadata false
            upcast QueryWeightMiddleware(threshold, reportToMetadata)

        /// <summary>
        /// Creates a object list filter middleware for an object of 'ObjectType, and a list field of 'ListType type.
        /// </summary>
        /// <param name="reportToMetadata">
        /// A boolean flag indicating if the values of the filters used by the query
        /// be reported to the metadata object in the GQLResponse.
        /// </param>
        /// <remarks>
        /// When defined, this middleware analyzes the current schema on the schema compiling phase,
        /// and creates an argument called "filter" for each list field that has the item type defined by the generic type 'ListType.
        /// This argument can be used on the query to specify a filter with operations like "less than", "equals", etc. on the
        /// field of the specified object of 'ObjectType type.
        /// </remarks>
        static member ObjectListFilterMiddleware<'ObjectType, 'ListType>(?reportToMetadata : bool) : IExecutorMiddleware =
            let reportToMetadata = defaultArg reportToMetadata false
            upcast ObjectListFilterMiddleware<'ObjectType, 'ListType>(reportToMetadata)