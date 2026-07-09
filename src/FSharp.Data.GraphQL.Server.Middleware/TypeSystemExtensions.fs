namespace FSharp.Data.GraphQL.Server.Middleware

open System
open System.Collections.Immutable
open System.Linq

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

    open ObjectListFilter.Operators

    type ExecutionInfo with

        member this.ResolveAbstractionFilter (typeMap : TypeMap) =
            match this.Kind with
            | ResolveAbstraction typeFields ->
                match this.ReturnDef with
                | :? UnionDef as union when
                    union.Options
                    |> Seq.map _.Name
                    |> Seq.sort
                    |> _.SequenceEqual(typeFields.Keys |> Seq.sort)
                    ->
                    ValueNone
                | _ ->
                    let getType name = typeMap[name].Type
                    typeFields.Keys
                    |> Seq.map getType
                    |> Seq.toList
                    |> OfTypes
                    |> ValueSome
            | _ -> ValueNone

    type ResolveFieldContext with

        /// <summary>
        /// Gets the filter argument value for this field, if it does have one.
        /// Field argument is defined by the ObjectFilterMiddleware.
        /// </summary>
        member this.Filter =
            match this.Args.TryGetValue "filter" with
            | true, (:? ObjectListFilter as f) ->
                match this.ExecutionInfo.ResolveAbstractionFilter (this.Context.Schema.TypeMap) with
                | ValueSome ofTypes -> ValueSome (ofTypes &&& f)
                | ValueNone -> ValueSome f
            | false, _ -> ValueNone
            | true, _ -> raise (InvalidOperationException "Invalid filter argument type.")

    type ObjectListFilters = ImmutableDictionary<obj list, ObjectListFilter>

    type ExecutionContext with

        /// <summary>
        /// Gets the filters applied to the lists.
        /// </summary>
        member this.Filters = this.Metadata.TryFind<ObjectListFilters> "filters"
