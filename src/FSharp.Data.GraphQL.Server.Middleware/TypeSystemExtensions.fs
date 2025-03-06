namespace FSharp.Data.GraphQL.Server.Middleware

open System
open System.Collections.Immutable
open FsToolkit.ErrorHandling

open FSharp.Data.GraphQL.Types

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
        member this.WithQueryWeight (weight : float) : FieldDef<'Val> = this.WithMetadata (this.Metadata.Add ("queryWeight", weight))

    type ObjectListFilters = ImmutableDictionary<string, ObjectListFilter>

    type ExecutionContext with

        /// <summary>
        /// Gets the filters applied to the lists.
        /// </summary>
        member this.Filters = this.Metadata.TryFind<ObjectListFilters> "filters"

    type ResolveFieldContext with

        /// <summary>
        /// Gets the filter argument value for this field, if it does have one.
        /// Field argument is defined by the ObjectFilterMiddleware.
        /// </summary>
        member this.Filter =
            match this.Args.TryGetValue "filter" with
            | true, (:? ObjectListFilter as f) -> ValueSome f
            | false, _ -> ValueNone
            | true, _ -> raise (InvalidOperationException "Invalid filter argument type.")
