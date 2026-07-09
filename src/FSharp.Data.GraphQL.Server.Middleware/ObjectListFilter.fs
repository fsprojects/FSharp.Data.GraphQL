namespace FSharp.Data.GraphQL.Server.Middleware

open System
open System.Collections
open System.Text.Json

/// A filter definition for a field value.
type FieldFilter<'Val> = { FieldName : string; Value : 'Val }

/// <summary>
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

open System.Linq.Expressions
open System.Runtime.InteropServices

type private CompareDiscriminatorExpression<'T, 'D> = Expression<Func<'T, 'D, bool>>

/// <summary>
/// Optional configuration for LINQ translation including discriminator handling.
/// </summary>
/// <example id="item-1"><code lang="fsharp">
/// // discriminator custom condition
/// let result () =
///    queryable.Apply(
///        filter,
///        ObjectListFilterLinqOptions (
///            (fun entity discriminator -> entity.Discriminator.StartsWith discriminator),
///            (function
///            | t when Type.(=)(t, typeof<Cat>) -> "cat+v1"
///            | t when Type.(=)(t, typeof<Dog>) -> "dog+v1")
///        )
///    )
/// </code></example>
/// <example id="item-2"><code lang="fsharp">
/// // discriminator equals
/// let result () =
///     queryable.Apply(
///         filter,
///         ObjectListFilterLinqOptions (
///            (fun entity -> entity.Discriminator),
///            (function
///            | t when Type.(=)(t, typeof<Cat>) -> "cat"
///            | t when Type.(=)(t, typeof<Dog>) -> "dog")
///         )
///     )
/// </code></example>
type ObjectListFilterLinqOptions<'T, 'D>
    (
        [<Optional>] compareDiscriminator : CompareDiscriminatorExpression<'T, 'D> | null,
        [<Optional>] getDiscriminatorValue : (Type -> 'D) | null,
        [<Optional>] jsonOptions : JsonSerializerOptions | null
    ) =

    member _.CompareDiscriminator = compareDiscriminator |> ValueOption.ofObj
    member _.GetDiscriminatorValue = getDiscriminatorValue |> ValueOption.ofObj
    member _.JsonOptions = jsonOptions |> ValueOption.ofObj

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
    new (jsonOptions : JsonSerializerOptions) =
        ObjectListFilterLinqOptions<'T, 'D> (null, null, jsonOptions)
    new (getDiscriminator : Expression<Func<'T, 'D>>, jsonOptions : JsonSerializerOptions) =
        ObjectListFilterLinqOptions<'T, 'D> (ObjectListFilterLinqOptions.GetCompareDiscriminator getDiscriminator, null, jsonOptions)
    new (compareDiscriminator : CompareDiscriminatorExpression<'T, 'D>, jsonOptions : JsonSerializerOptions) =
        ObjectListFilterLinqOptions<'T, 'D> (compareDiscriminator, null, jsonOptions)

