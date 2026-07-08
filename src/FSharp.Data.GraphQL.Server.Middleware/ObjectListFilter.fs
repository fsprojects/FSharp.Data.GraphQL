namespace FSharp.Data.GraphQL.Server.Middleware

open System
open System.Collections
open FSharp.Data.GraphQL

/// A filter definition for a field value.
type FieldFilter<'Val> = { FieldName : string; Value : 'Val }

/// A filter definition for an object list.
/// </summary>
/// <remarks>
/// String-based filters can carry a comparer. When the comparer is not provided by the default
/// string operators, `StartsWith`, `EndsWith`, and string `Contains` preserve the existing
/// case-sensitive `StringComparison.CurrentCulture` behavior.
/// `StringComparer.CurrentCultureIgnoreCase` enables case-insensitive matching.
/// When filters are provided through GraphQL input, lowercase string suffixes are interpreted
/// as case-insensitive and capitalized suffixes are interpreted as case-sensitive.
/// </remarks>
type ObjectListFilter =
    | And of ObjectListFilter * ObjectListFilter
    | Or of ObjectListFilter * ObjectListFilter
    | Not of ObjectListFilter
    | Equals of Filter : FieldFilter<IComparable> * Comparer : IComparer
    | GreaterThan of FieldFilter<IComparable>
    | GreaterThanOrEqual of FieldFilter<IComparable>
    | LessThan of FieldFilter<IComparable>
    | LessThanOrEqual of FieldFilter<IComparable>
    | In of FieldFilter<obj list>
    | StartsWith of Filter : FieldFilter<string> * Comparer : StringComparer
    | EndsWith of Filter : FieldFilter<string> * Comparer : StringComparer
    | Contains of Filter : FieldFilter<IComparable> * Comparer : IComparer
    | OfTypes of Type list
    | FilterField of FieldFilter<ObjectListFilter>

open System.Collections.Generic
open System.Linq.Expressions
open System.Runtime.InteropServices

type private CompareDiscriminatorExpression<'T, 'D> = Expression<Func<'T, 'D, bool>>

/// <summary>
/// Validation error raised when an incoming <see cref="ObjectListFilter"/> cannot be
/// translated to a LINQ expression against the queried entity type.
/// </summary>
type ObjectListFilterValidationException (message : string, [<Optional>] extensions : Dictionary<string, obj> | null) =
    inherit GQLMessageExceptionBase (ErrorKind.Validation, message, extensions)

/// <summary>
/// Function signature for custom value coercion logic. Takes a target CLR type and a JSON
/// primitive value, returns the coerced value or <see langword="ValueNone"/> if coercion is not supported.
/// </summary>
type FilterValueCoercer = Type -> obj -> obj voption

/// <summary>
/// Optional configuration for LINQ translation including discriminator handling.
/// </summary>
type ObjectListFilterLinqOptions<'T, 'D>
    (
        [<Optional>] compareDiscriminator : CompareDiscriminatorExpression<'T, 'D> | null,
        [<Optional>] getDiscriminatorValue : (Type -> 'D) | null,
        [<Optional>] customCoercers : FilterValueCoercer list | null
    ) =

    member _.CompareDiscriminator = compareDiscriminator |> ValueOption.ofObj
    member _.GetDiscriminatorValue = getDiscriminatorValue |> ValueOption.ofObj
    member _.CustomCoercers = customCoercers |> Option.ofObj |> Option.defaultValue []

    static member None = ObjectListFilterLinqOptions<'T, 'D> (null, null, null)

    static member GetCompareDiscriminator (getDiscriminatorValue : Expression<Func<'T, 'D>>) =
        let tParam = Expression.Parameter (typeof<'T>, "x")
        let dParam = Expression.Parameter (typeof<'D>, "d")
        let body = Expression.Equal (Expression.Invoke (getDiscriminatorValue, tParam), dParam)
        Expression.Lambda<Func<'T, 'D, bool>> (body, tParam, dParam)

    new (getDiscriminator : Expression<Func<'T, 'D>>) =
        ObjectListFilterLinqOptions<'T, 'D> (ObjectListFilterLinqOptions.GetCompareDiscriminator getDiscriminator, null, null)
    new (compareDiscriminator : CompareDiscriminatorExpression<'T, 'D>) =
        ObjectListFilterLinqOptions<'T, 'D> (compareDiscriminator, null, null)
    new (getDiscriminatorValue : Type -> 'D) =
        ObjectListFilterLinqOptions<'T, 'D> (null, getDiscriminatorValue, null)
    new (getDiscriminator : Expression<Func<'T, 'D>>, getDiscriminatorValue : Type -> 'D) =
        ObjectListFilterLinqOptions<'T, 'D> (ObjectListFilterLinqOptions.GetCompareDiscriminator getDiscriminator, getDiscriminatorValue, null)
    new (customCoercers : FilterValueCoercer list) =
        ObjectListFilterLinqOptions<'T, 'D> (null, null, customCoercers)
    new (getDiscriminator : Expression<Func<'T, 'D>>, customCoercers : FilterValueCoercer list) =
        ObjectListFilterLinqOptions<'T, 'D> (ObjectListFilterLinqOptions.GetCompareDiscriminator getDiscriminator, null, customCoercers)
    new (compareDiscriminator : CompareDiscriminatorExpression<'T, 'D>, customCoercers : FilterValueCoercer list) =
        ObjectListFilterLinqOptions<'T, 'D> (compareDiscriminator, null, customCoercers)

