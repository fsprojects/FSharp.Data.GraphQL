namespace FSharp.Data.GraphQL.Server.Middlewares

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
        member this.WithQueryWeight(weight : float) : FieldDef<'Val> =
            this.WithMetadata(this.Metadata.Add("queryWeight", weight))

    type ResolveFieldContext with
        /// <summary>
        /// Gets the filter argument value for this field, if it does have one.
        /// Field argument is defined by the ObjectFilterMiddleware.
        /// </summary>
        member this.Filter =
            match this.Args.TryFind("filter") with
            | Some (:? ObjectListFilter as f) -> Some f
            | _ -> None

    type Metadata with
        /// <summary>
        /// Creates a new instance of the current Metadata, adding a directive chooser function to it.
        /// Directive chooser will be used by a DirectiveFallbackMiddleware if configured in the Executor.
        /// </summary>
        /// <param name="chooser">The directive chooser to be added in the Metadata object.</param>
        member this.WithDirectiveChooser(chooser : DirectiveChooser) =
            this.Add("directiveChooser", chooser)

        /// <summary>
        /// Creates a new instance of Metadata, adding a directive chooser function to it.
        /// Directive chooser will be used by a DirectiveFallbackMiddleware if configured in the Executor.
        /// </summary>
        /// <param name="chooser">The directive chooser to be added in the Metadata object.</param>
        static member WithDirectiveChooser(chooser : DirectiveChooser) =
            Metadata.Empty.WithDirectiveChooser(chooser)