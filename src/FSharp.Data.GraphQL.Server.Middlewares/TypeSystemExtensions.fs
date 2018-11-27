namespace FSharp.Data.GraphQL.Server.Middlewares

open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Ast

/// A function that checks if a directive should be used in the exection of a query, or changed to a new directive.
type DirectiveChooser = Directive -> Directive option

/// Basic operations on DirectiveChoosers.
module DirectiveChooser =
    let apply (directive : Directive) (chooser : DirectiveChooser) = 
        chooser directive

    let keep : DirectiveChooser = 
        let chooser = fun directive -> Some directive
        chooser

    let fallback : DirectiveChooser = 
        let chooser = fun _ -> None
        chooser

    let acceptWhen (condition : Directive -> bool) : DirectiveChooser = 
        let chooser = fun directive ->
            if condition directive
            then keep directive
            else fallback directive
        chooser

    let fallbackWhen (condition : Directive -> bool) : DirectiveChooser =
        let chooser = fun directive ->
            if condition directive
            then fallback directive
            else keep directive
        chooser

    let fallbackByName name = fallbackWhen (fun d -> d.Name = name)

    let fallbackDefer = fallbackByName "defer"

    let fallbackStream = fallbackByName "stream"

    let fallbackLive = fallbackByName "live"

    let compose (other : DirectiveChooser) (actual : DirectiveChooser) : DirectiveChooser = 
        let chooser = fun directive ->
            match actual directive with
            | Some d -> other d
            | None -> None
        chooser

    let merge (other : DirectiveChooser) (actual : DirectiveChooser) : DirectiveChooser =
        let chooser = fun directive -> 
            match other directive, actual directive with
            | d1, d2 when d1 = d2 -> d1
            | Some d1, Some d2 when d1 <> d2 -> failwith "Can not merge DirectiveChoosers because they don't return the same directive."
            | _ -> None
        chooser

    let fromSeq (choosers : DirectiveChooser seq) : DirectiveChooser = 
        let chooser = fun directive ->
            match Seq.length choosers with
            | 0 -> keep directive
            | _ -> choosers |> Seq.reduce (fun fst snd -> compose fst snd) |> apply directive
        chooser
        
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