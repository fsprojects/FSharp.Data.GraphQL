namespace FSharp.Data.GraphQL.Server.Middleware

open System
open System.Collections.Immutable
open FsToolkit.ErrorHandling

open FSharp.Data.GraphQL
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

    type ResolveFieldContext with

        /// <summary>
        /// Gets the filter argument value for this field, if it does have one.
        /// Field argument is defined by the ObjectFilterMiddleware.
        /// </summary>
        member this.Filter =
            match this.Args.TryGetValue "filter" with
            | true, (:? ObjectListFilter as f) ->
                match this.ExecutionInfo.Kind with
                | ResolveAbstraction typeFields ->
                    let getType name =
                        match this.Context.Schema.TypeMap.TryFind name with
                        | ValueSome tdef -> tdef.Type
                        | ValueNone -> raise (MalformedGQLQueryException ($"Type '{name}' not found in schema."))
                    match typeFields.Keys |> Seq.map getType |> Seq.toList with
                    | [] -> ValueNone
                    | filters -> ValueSome (f &&& (OfTypes { FieldName = "__typename"; Value = filters }))
                | _ -> ValueSome f
            | false, _ -> ValueNone
            | true, _ -> raise (InvalidOperationException "Invalid filter argument type.")

    type ObjectListFilters = ImmutableDictionary<obj list, ObjectListFilter>

    type ExecutionContext with

        /// <summary>
        /// Gets the filters applied to the lists.
        /// </summary>
        member this.Filters = this.Metadata.TryFind<ObjectListFilters> "filters"
