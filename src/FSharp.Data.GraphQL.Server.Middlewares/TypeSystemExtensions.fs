namespace FSharp.Data.GraphQL.Server.Middlewares

open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Server.Middlewares.Literals

/// Contains extensions for the type system.
[<AutoOpen>]
module TypeSystemExtensions =
    type FieldDef<'Val> with
        /// <summary>
        /// Creates a new field definition based on the existing one, containing
        /// the existing metadata information, plus a new entry used to calculate the query
        /// weight by the QueryWeightMiddleware.
        /// </summary>
        /// <param name="weight">A float value representing the weight that this field have on the query.</param>
        member this.WithQueryWeight(weight : float) : FieldDef<'Val> =
            this.WithMetadata(this.Metadata.Add(MetadataKeys.QueryWeightMiddleware.QueryWeight, weight))

    type Metadata with
        /// <summary>
        /// Adds metadata information to the current metadata definition, containing
        /// the maximum query weight (threshold) for query execution.
        /// This value is used by the QueryWeightMiddleware to measure if a query weight is 
        /// below a defined threshold.
        /// </summary>
        /// <param name="threshold">A float value, representing the threshold weight.</param>
        member this.WithQueryWeightThreshold(threshold : float) =
            this.Add(MetadataKeys.QueryWeightMiddleware.QueryWeightThreshold, threshold)

    type ResolveFieldContext with
        /// <summary>
        /// Gets the filter argument value for this field, if it does have one.
        /// Field argument is defined by the ObjectFilterMiddleware.
        /// </summary>
        member this.Filter =
            match this.Args.TryFind(ArgumentKeys.ObjectListFilterMiddleware.Filter) with
            | Some (:? ObjectListFilter as f) -> Some f
            | _ -> None