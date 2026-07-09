/// <summary>
/// String filter suffixes:
///   lowercase (e.g. _ends_with, _ew)   → case-insensitive (OrdinalIgnoreCase)
///   Capitalized (e.g. _Ends_With, _EW) → case-sensitive (Ordinal)
/// <para>
/// The <see cref="CI"/> submodule contains lowercase suffixes that map to case-insensitive string comparisons.
/// The <see cref="CS"/> submodule contains capitalized/uppercase suffixes that map to case-sensitive string comparisons.
/// Numeric and comparison operator suffixes are defined at the module level and are case-insensitive by convention.
/// </para>
/// </summary>
[<RequireQualifiedAccess>]
module FSharp.Data.GraphQL.Server.Middleware.FilterSuffixConstants

// Numeric/comparison operators
[<Literal>]
let GreaterThanOrEqualSuffix = "_greater_than_or_equal"
[<Literal>]
let GTESuffix = "_gte"
[<Literal>]
let GreaterThanSuffix = "_greater_than"
[<Literal>]
let GTSuffix = "_gt"
[<Literal>]
let LessThanOrEqualSuffix = "_less_than_or_equal"
[<Literal>]
let LTESuffix = "_lte"
[<Literal>]
let LessThanSuffix = "_less_than"
[<Literal>]
let LTSuffix = "_lt"
[<Literal>]
let InSuffix = "_in"

/// Case-insensitive string operators and all numeric/comparison operators
module CI =
    // String operators (case-insensitive)
    [<Literal>]
    let EndsWithSuffix = "_ends_with"
    [<Literal>]
    let EWSuffix = "_ew"
    [<Literal>]
    let StartsWithSuffix = "_starts_with"
    [<Literal>]
    let SWSuffix = "_sw"
    [<Literal>]
    let ContainsSuffix = "_contains"
    [<Literal>]
    let EqualsSuffix = "_equals"
    [<Literal>]
    let EQSuffix = "_eq"

/// Case-sensitive string operators
module CS =
    [<Literal>]
    let EndsWithSuffix = "_Ends_With"
    [<Literal>]
    let EWSuffix = "_EW"
    [<Literal>]
    let StartsWithSuffix = "_Starts_With"
    [<Literal>]
    let SWSuffix = "_SW"
    [<Literal>]
    let ContainsSuffix = "_Contains"
    [<Literal>]
    let EqualsSuffix = "_Equals"
    [<Literal>]
    let EQSuffix = "_EQ"
