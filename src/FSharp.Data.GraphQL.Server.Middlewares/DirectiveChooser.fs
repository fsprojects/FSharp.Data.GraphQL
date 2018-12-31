namespace FSharp.Data.GraphQL.Server.Middlewares

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
        let chooser = Some
        chooser

    /// Builds a chooser that, for any Directive, returns None.
    let fallback : DirectiveChooser = 
        let chooser = fun _ -> None
        chooser

    /// Builds a chooser that, when run, runs actual chooser, and if it returns Some directive x, maps
    /// x directive using mapper function to y directive, and return Some y. Otherwise, returns None.
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

    /// Reduces a sequence of choosers into a single chooser, by applying reducer function.
    let reduceSeq reducer (choosers : DirectiveChooser seq) =
        choosers |> Seq.reduce reducer

    /// Reduces a sequence of choosers into a single chooser, by applying the compose function to reduce it.
    let composeSeq (choosers : DirectiveChooser seq) : DirectiveChooser = 
        let chooser = fun directive ->
            match Seq.length choosers with
            | 0 -> keep directive
            | _ -> choosers |> reduceSeq compose |> apply directive
        chooser

    /// Reduces a sequence of choosers into a single chooser, by applying the DirectiveChooser.merge to reduce it.
    let mergeSeq (choosers : DirectiveChooser seq) : DirectiveChooser =
        let chooser = fun directive ->
            match Seq.length choosers with
            | 0 -> keep directive
            | _ -> choosers |> reduceSeq merge |> apply directive
        chooser