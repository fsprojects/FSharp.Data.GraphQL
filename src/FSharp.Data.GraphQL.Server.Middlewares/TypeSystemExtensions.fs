namespace FSharp.Data.GraphQL.Server.Middlewares

open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Ast

/// A function that checks if a directive should be used in the exection of a query, or changed to a new directive.
type DirectiveChooser = Directive -> Directive option

/// Basic operations on DirectiveChoosers.
[<RequireQualifiedAccess>]
module DirectiveChooser =
    /// Apply a chooser to a directive.
    let apply (directive : Directive) (chooser : DirectiveChooser) = 
        chooser directive

    /// Builds a chooser that, given a Directive x, returns Some x.
    let keep : DirectiveChooser = 
        let chooser = fun directive -> Some directive
        chooser

    /// Builds a chooser that, for any Directive, returns None.
    let fallback : DirectiveChooser = 
        let chooser = fun _ -> None
        chooser

    /// Builds a chooser that, when run, runs actual chooser, and if it returns Some directive x, maps
    /// x directive using mapper function to y directive, and return Some y.
    let map (mapper : Directive -> Directive) (actual : DirectiveChooser) : DirectiveChooser =
        let chooser = fun directive ->
            match actual directive with
            | Some d -> mapper d |> keep
            | None -> None
        chooser

    /// Builds a chooser that, given a Directive x, apply the condition filter function to x,
    /// and if it returns true, returns Some x. Otherwise, returns None.
    let keepWhen (condition : Directive -> bool) : DirectiveChooser = 
        let chooser = fun directive ->
            if condition directive
            then keep directive
            else fallback directive
        chooser

    /// Builds a chooser that, given a Directive x, apply the condition filter function to x,
    /// and if it returns true, returns None. Otherwise, returns Some x.
    let fallbackWhen (condition : Directive -> bool) : DirectiveChooser =
        let chooser = fun directive ->
            if condition directive
            then fallback directive
            else keep directive
        chooser

    /// Builds a chooser that, given a Directive x, if x.Name equals given name, returns None.
    /// Otherwise, returns Some x.
    let fallbackByName name = fallbackWhen (fun d -> d.Name = name)

    /// Builds a chooser that, given a Directive x, if x.Name is 'defer', returns None.
    /// Otherwise, returns Some x.
    let fallbackDefer = fallbackByName "defer"

    /// Builds a chooser that, given a Directive x, if x.Name is 'stream', returns None.
    /// Otherwise, returns Some x.
    let fallbackStream = fallbackByName "stream"

    /// Builds a chooser that, given a Directive x, if x.Name is 'live', returns None.
    /// Otherwise, returns Some x.
    let fallbackLive = fallbackByName "live"

    /// Builds a chooser that, when run, runs actual chooser, and if it returns Some directive x,
    /// uses that directive to run other chooser and return its result. If actual chooser returns None,
    /// returns None.
    let compose (other : DirectiveChooser) (actual : DirectiveChooser) : DirectiveChooser = 
        let chooser = fun directive ->
            match actual directive with
            | Some d -> other d
            | None -> None
        chooser

    /// Builds a chooser that, when run, runs actual chooser and other chooser: if any of the choosers return
    /// None, then returns None. Otherwise, compose actual into other, run the composed chooser, and return its result.
    let merge (other : DirectiveChooser) (actual : DirectiveChooser) : DirectiveChooser =
        let chooser = fun directive -> 
            match actual directive, other directive with
            |Some _, Some _ -> compose other actual |> apply directive
            | _ -> None
        chooser

    /// Builds a chooser based on the composal of all choosers in choosers sequence, from first to last.
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