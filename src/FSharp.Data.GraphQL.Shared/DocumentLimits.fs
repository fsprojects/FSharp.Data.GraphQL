namespace FSharp.Data.GraphQL

/// Default limits on the work that an untrusted document can cause while it is parsed and validated.
[<RequireQualifiedAccess>]
module DocumentLimitsDefaults =

    /// <summary>
    /// The maximum nesting depth of a document.
    /// <para>
    /// The parser counts nested braces, brackets and parentheses outside of strings and comments, before it parses the document.
    /// </para>
    /// <para>
    /// Validation counts nesting once fragment spreads are inlined: every selection set of a field, every inline fragment and
    /// every fragment spread adds one level.
    /// </para>
    /// </summary>
    [<Literal>]
    let MaxNestingDepth = 128

    /// <summary>
    /// The maximum number of selections that the validation of a document may inline.
    /// <para>
    /// Fields, inline fragments and fragment spreads are counted after fragment spreads are inlined, over all the
    /// operations and fragment definitions of the document.
    /// </para>
    /// <para>
    /// The work of validating and planning a document grows with this number, so a higher limit lets a small document
    /// keep a server busy for longer.
    /// </para>
    /// </summary>
    [<Literal>]
    let MaxRecursiveSelections = 25_000

    /// The maximum number of errors that the validation of a document reports before it stops.
    [<Literal>]
    let MaxValidationErrors = 100
