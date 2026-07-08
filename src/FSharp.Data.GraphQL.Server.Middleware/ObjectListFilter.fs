namespace FSharp.Data.GraphQL.Server.Middleware

open System
open System.Collections
open FSharp.Data.GraphQL

/// A filter definition for a field value.
type FieldFilter<'Val> = { FieldName : string; Value : 'Val }

/// <summary>
/// A filter definition for an object list.
/// </summary>
/// <remarks>
/// String-based filters can carry a comparer. When the comparer is not provided by the default
/// case-sensitive operators, the behavior is equivalent to using `StringComparer.Ordinal`.
/// `StringComparer.OrdinalIgnoreCase` enables case-insensitive matching.
/// When filters are provided through GraphQL input, lowercase string suffixes are interpreted
/// as case-insensitive and capitalized suffixes are interpreted as case-sensitive.
/// </remarks>
type ObjectListFilter =
    | And of ObjectListFilter * ObjectListFilter
    | Or of ObjectListFilter * ObjectListFilter
    | Not of ObjectListFilter
    | Equals of Filter : FieldFilter<System.IComparable> * Comparer : IComparer
    | GreaterThan of FieldFilter<System.IComparable>
    | GreaterThanOrEqual of FieldFilter<System.IComparable>
    | LessThan of FieldFilter<System.IComparable>
    | LessThanOrEqual of FieldFilter<System.IComparable>
    | In of FieldFilter<obj list>
    | StartsWith of Filter : FieldFilter<string> * Comparer : StringComparer
    | EndsWith of Filter : FieldFilter<string> * Comparer : StringComparer
    | Contains of Filter : FieldFilter<System.IComparable> * Comparer : IComparer
    | OfTypes of Type list
    | FilterField of FieldFilter<ObjectListFilter>

open System.Linq
open System.Linq.Expressions
open System.Runtime.InteropServices
open System.Reflection
open System.Collections.Generic

type private CompareDiscriminatorExpression<'T, 'D> = Expression<Func<'T, 'D, bool>>

/// <summary>
/// Allows to specify discriminator comparison or discriminator getter
/// and a function that return discriminator value depending on entity type
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
[<Struct>]
type ObjectListFilterLinqOptions<'T, 'D>
    ([<Optional>] compareDiscriminator : CompareDiscriminatorExpression<'T, 'D> | null, [<Optional>] getDiscriminatorValue : (Type -> 'D) | null) =

    member _.CompareDiscriminator = compareDiscriminator |> ValueOption.ofObj
    member _.GetDiscriminatorValue = getDiscriminatorValue |> ValueOption.ofObj

    static member None = ObjectListFilterLinqOptions<'T, 'D> (null, null)

    static member GetCompareDiscriminator (getDiscriminatorValue : Expression<Func<'T, 'D>>) =
        let tParam = Expression.Parameter (typeof<'T>, "x")
        let dParam = Expression.Parameter (typeof<'D>, "d")
        let body = Expression.Equal (Expression.Invoke (getDiscriminatorValue, tParam), dParam)
        Expression.Lambda<Func<'T, 'D, bool>> (body, tParam, dParam)

    new (getDiscriminator : Expression<Func<'T, 'D>>) =
        ObjectListFilterLinqOptions<'T, 'D> (ObjectListFilterLinqOptions.GetCompareDiscriminator getDiscriminator, null)
    new (compareDiscriminator : CompareDiscriminatorExpression<'T, 'D>) = ObjectListFilterLinqOptions<'T, 'D> (compareDiscriminator, null)
    new (getDiscriminatorValue : Type -> 'D) =
        ObjectListFilterLinqOptions<'T, 'D> (compareDiscriminator = null, getDiscriminatorValue = getDiscriminatorValue)
    new (getDiscriminator : Expression<Func<'T, 'D>>, getDiscriminatorValue : Type -> 'D) =
        ObjectListFilterLinqOptions<'T, 'D> (ObjectListFilterLinqOptions.GetCompareDiscriminator getDiscriminator, getDiscriminatorValue)

/// Contains tooling for working with ObjectListFilter.
module ObjectListFilter =
    /// Contains operators for building and comparing ObjectListFilter values.
    module Operators =
        /// Creates a new ObjectListFilter representing an AND operation between two existing ones.
        let ( &&& ) x y = And (x, y)

        /// Creates a new ObjectListFilter representing an OR operation between two existing ones.
        let ( ||| ) x y = Or (x, y)

        /// Creates a new ObjectListFilter representing an EQUALS operation between two comparable values.
        let ( === ) fname value = Equals ({ FieldName = fname; Value = value }, null)

        /// Creates a new ObjectListFilter representing a GREATER THAN operation of a comparable value.
        let ( >>> ) fname value = GreaterThan { FieldName = fname; Value = value }

        /// Creates a new ObjectListFilter representing a GREATER THAN OR EQUAL operation of a comparable value.
        let ( ==> ) fname value = GreaterThanOrEqual { FieldName = fname; Value = value }

        /// Creates a new ObjectListFilter representing a LESS THAN operation of a comparable value.
        let ( <<< ) fname value = LessThan { FieldName = fname; Value = value }

        /// Creates a new ObjectListFilter representing a LESS THAN OR EQUAL operation of a comparable value.
        let ( <== ) fname value = LessThanOrEqual { FieldName = fname; Value = value }

        /// Creates a new ObjectListFilter representing a STARTS WITH operation of a string value.
        let ( =@@ ) fname value = StartsWith ({ FieldName = fname; Value = value }, null)

        /// Creates a new ObjectListFilter representing an ENDS WITH operation of a string value.
        let ( @@= ) fname value = EndsWith ({ FieldName = fname; Value = value }, null)

        /// Creates a new ObjectListFilter representing a CONTAINS operation.
        let ( @=@ ) fname value = Contains ({ FieldName = fname; Value = value }, null)

        /// Creates a new ObjectListFilter representing a IN operation.
        let ( =~= ) fname value = In { FieldName = fname; Value = value }

        /// Creates a new ObjectListFilter representing a field sub comparison.
        let ( --> ) fname filter = FilterField { FieldName = fname; Value = filter }

        /// Creates a new ObjectListFilter representing a NOT operation for the existing one.
        let ( !!! ) filter = Not filter

        /// Creates a new ObjectListFilter representing a case-insensitive EQUALS operation on a string value.
        let ( ===~ ) fname (value : string) = Equals ({ FieldName = fname; Value = value }, StringComparer.OrdinalIgnoreCase)

        /// Creates a new ObjectListFilter representing a case-insensitive STARTS WITH operation on a string value.
        let ( =@@~ ) fname (value : string) = StartsWith ({ FieldName = fname; Value = value }, StringComparer.OrdinalIgnoreCase)

        /// Creates a new ObjectListFilter representing a case-insensitive ENDS WITH operation on a string value.
        let ( @@=~ ) fname (value : string) = EndsWith ({ FieldName = fname; Value = value }, StringComparer.OrdinalIgnoreCase)

        /// Creates a new ObjectListFilter representing a case-insensitive CONTAINS operation on a string value.
        let ( @=@~ ) fname (value : string) = Contains ({ FieldName = fname; Value = value }, StringComparer.OrdinalIgnoreCase)

    let private genericWhereMethod =
        typeof<Queryable>.GetMethods ()
        |> Seq.where (fun m -> m.Name = "Where")
        |> Seq.find (fun m ->
            let parameters = m.GetParameters ()
            parameters.Length = 2
            && parameters[1].ParameterType.GetGenericTypeDefinition () = typedefof<Expression<Func<_, _>>>)

    // Helper to create Where expression
    let whereExpr<'T> (query : IQueryable<'T>) (param : ParameterExpression) predicate =
        let whereMethod = genericWhereMethod.MakeGenericMethod ([| typeof<'T> |])
        Expression.Call (whereMethod, [| query.Expression; Expression.Lambda<Func<'T, bool>> (predicate, param) |])

    let private objectType = typeof<obj>
    let private stringType = typeof<string>
    let private genericIEnumerableType = typedefof<IEnumerable<_>>

    let private stringComparisonType = typeof<StringComparison>
    let private StringStartsWithMethod = stringType.GetMethod ("StartsWith", [| stringType; stringComparisonType |])
    let private StringEndsWithMethod = stringType.GetMethod ("EndsWith", [| stringType; stringComparisonType |])
    let private StringContainsMethod = stringType.GetMethod ("Contains", [| stringType; stringComparisonType |])
    let private StringEqualsMethod = stringType.GetMethod ("Equals", [| stringType; stringComparisonType |])
    let private unwrapOptionMethod =
        FSharp.Data.GraphQL.Helpers.moduleType.GetMethod (nameof Helpers.unwrap)

    let private getCollectionInstanceContainsMethod (memberType : Type) =
        memberType
            .GetMethods(BindingFlags.Instance ||| BindingFlags.Public)
            .FirstOrDefault (fun m -> m.Name = "Contains" && m.GetParameters().Length = 1)
        |> ValueOption.ofObj

    let private getEnumerableContainsMethod (itemType : Type) =
        match
            typeof<Enumerable>
                .GetMethods(BindingFlags.Static ||| BindingFlags.Public)
                .FirstOrDefault (fun m -> m.Name = "Contains" && m.GetParameters().Length = 2)
        with
        | null -> raise (MissingMemberException "Static 'Contains' method with 2 parameters not found on 'Enumerable' class")
        | containsGenericStaticMethod -> containsGenericStaticMethod.MakeGenericMethod ([| itemType |])

    let private getEnumerableCastMethod (itemType : Type) =
        match
            typeof<Enumerable>
                .GetMethods(BindingFlags.Static ||| BindingFlags.Public)
                .FirstOrDefault (fun m -> m.Name = "Cast" && m.GetParameters().Length = 1)
        with
        | null -> raise (MissingMemberException "Static 'Cast' method with 1 parameter not found on 'Enumerable' class")
        | castGenericStaticMethod -> castGenericStaticMethod.MakeGenericMethod ([| itemType |])

    let getField (param : ParameterExpression) fieldName = Expression.PropertyOrField (param, fieldName)

    let hasEqualityOperator (``type`` : Type) =
        ``type``.GetMethods (BindingFlags.Public ||| BindingFlags.Static)
        |> Seq.exists (fun m -> m.Name = " op_Equality")

    let hasInequalityOperator (``type`` : Type) =
        ``type``.GetMethods (BindingFlags.Public ||| BindingFlags.Static)
        |> Seq.exists (fun m -> m.Name = "op_Inequality")

    [<Struct>]
    type SourceExpression private (expression : Expression) =
        new (parameter : ParameterExpression) = SourceExpression (parameter :> Expression)
        new (``member`` : MemberExpression) = SourceExpression (``member`` :> Expression)
        member _.Value = expression
        static member op_Implicit (source : SourceExpression) = source.Value
        static member op_Implicit (parameter : ParameterExpression) = SourceExpression (parameter :> Expression)
        static member op_Implicit (``member`` : MemberExpression) = SourceExpression (``member`` :> Expression)

    let equalsMethod =
        objectType
        |> _.GetMethods(BindingFlags.Instance ||| BindingFlags.Public)
        |> Seq.where (fun m -> m.Name = "Equals")
        |> Seq.head

    let staticEqualsMethod =
        objectType
        |> _.GetMethods(BindingFlags.Static ||| BindingFlags.Public)
        |> Seq.where (fun m -> m.Name = "Equals")
        |> Seq.head

    /// Maps an IComparer to a StringComparison value.
    /// Returns ValueNone for null or Ordinal comparers (use default Expression.Equal path).
    let private comparerToStringComparison (comparer : IComparer) =
        match comparer with
        | null -> ValueNone
        | :? StringComparer as sc ->
            if obj.ReferenceEquals (sc, StringComparer.OrdinalIgnoreCase) then ValueSome StringComparison.OrdinalIgnoreCase
            elif obj.ReferenceEquals (sc, StringComparer.InvariantCultureIgnoreCase) then ValueSome StringComparison.InvariantCultureIgnoreCase
            elif obj.ReferenceEquals (sc, StringComparer.CurrentCultureIgnoreCase) then ValueSome StringComparison.CurrentCultureIgnoreCase
            elif obj.ReferenceEquals (sc, StringComparer.Ordinal) then ValueSome StringComparison.Ordinal
            elif obj.ReferenceEquals (sc, StringComparer.InvariantCulture) then ValueSome StringComparison.InvariantCulture
            elif obj.ReferenceEquals (sc, StringComparer.CurrentCulture) then ValueSome StringComparison.CurrentCulture
            else ValueNone
        | _ -> ValueNone

    let rec buildFilterExpr isEnumerableQuery (param : SourceExpression) buildTypeDiscriminatorCheck filter : Expression =

        let build = buildFilterExpr isEnumerableQuery param buildTypeDiscriminatorCheck

        let (|NoCast|Enumerable|NonEnumerableCast|) value =
            if obj.ReferenceEquals (value, null) then NoCast
            else if isEnumerableQuery then Enumerable
            else NonEnumerableCast (value.GetType ())

        let unsafeConvertTo ``type`` ``member`` = Expression.Convert (Expression.Convert (``member``, objectType), ``type``)

        let normalizeStringMemberExpr (``member`` : MemberExpression) : Expression =
            match ``member``.Type with
            | t when t = stringType -> ``member``
            | _ when not isEnumerableQuery -> unsafeConvertTo stringType ``member``
            | _ when isEnumerableQuery -> Expression.Convert (Expression.Call (unwrapOptionMethod, ``member``), stringType)
            | _ -> Expression.Convert (``member``, stringType)

        match filter with
        | Not (Equals (f, comparer)) ->
            let ``member`` = Expression.PropertyOrField (param, f.FieldName)
            match comparerToStringComparison comparer with
            | ValueSome comparison ->
                let value = Helpers.unwrap (box f.Value) :?> string
                Expression.Not (Expression.Call (normalizeStringMemberExpr ``member``, StringEqualsMethod, Expression.Constant (value, typeof<string>), Expression.Constant comparison)) :> Expression
            | ValueNone ->
                let hasEqualityOperator = hasEqualityOperator ``member``.Type
                match f.Value with
                | NoCast when hasEqualityOperator -> Expression.NotEqual (``member``, Expression.Constant f.Value)
                | NoCast
                | NonEnumerableCast _ ->
                    Expression.NotEqual (Expression.Convert (``member``, objectType), Expression.Convert ((Expression.Constant f.Value), objectType))
                | Enumerable ->
                    let ``const`` = Expression.Constant (Values.normalizeOptional ``member``.Type f.Value)
                    Expression.Not (Expression.Call (``const``, equalsMethod, ``member``))
        | Not f -> f |> build |> Expression.Not :> Expression
        | And (f1, f2) -> Expression.AndAlso (build f1, build f2)
        | Or (f1, f2) -> Expression.OrElse (build f1, build f2)
        | Equals (f, comparer) ->
            let ``member`` = Expression.PropertyOrField (param, f.FieldName)
            match comparerToStringComparison comparer with
            | ValueSome comparison ->
                let value = Helpers.unwrap (box f.Value) :?> string
                Expression.Call (normalizeStringMemberExpr ``member``, StringEqualsMethod, Expression.Constant (value, typeof<string>), Expression.Constant comparison) :> Expression
            | ValueNone ->
                let hasEqualityOperator = hasEqualityOperator ``member``.Type
                match f.Value with
                | NoCast when hasEqualityOperator -> Expression.Equal (``member``, Expression.Constant f.Value)
                | NoCast
                | NonEnumerableCast _ ->
                    Expression.Equal (Expression.Convert (``member``, objectType), Expression.Convert ((Expression.Constant f.Value), objectType))
                | Enumerable ->
                    let ``const`` = Expression.Constant (Values.normalizeOptional ``member``.Type f.Value)
                    Expression.Call (``const``, equalsMethod, ``member``)
        | GreaterThan f ->
            let ``member`` = Expression.PropertyOrField (param, f.FieldName)
            match f.Value with
            | NoCast -> Expression.GreaterThan (``member``, Expression.Constant f.Value)
            | Enumerable -> Expression.GreaterThan (``member``, Expression.Constant (Values.normalizeOptional ``member``.Type f.Value))
            | NonEnumerableCast ``type`` -> Expression.GreaterThan ((unsafeConvertTo ``type`` ``member``), Expression.Constant f.Value)
        | LessThan f ->
            let ``member`` = Expression.PropertyOrField (param, f.FieldName)
            match f.Value with
            | NoCast -> Expression.LessThan (``member``, Expression.Constant f.Value)
            | Enumerable -> Expression.LessThan (``member``, Expression.Constant (Values.normalizeOptional ``member``.Type f.Value))
            | NonEnumerableCast ``type`` -> Expression.LessThan ((unsafeConvertTo ``type`` ``member``), Expression.Constant f.Value)
        | GreaterThanOrEqual f ->
            let ``member`` = Expression.PropertyOrField (param, f.FieldName)
            match f.Value with
            | NoCast -> Expression.GreaterThanOrEqual (``member``, Expression.Constant f.Value)
            | Enumerable -> Expression.GreaterThanOrEqual (``member``, Expression.Constant (Values.normalizeOptional ``member``.Type f.Value))
            | NonEnumerableCast ``type`` -> Expression.GreaterThanOrEqual ((unsafeConvertTo ``type`` ``member``), Expression.Constant f.Value)
        | LessThanOrEqual f ->
            let ``member`` = Expression.PropertyOrField (param, f.FieldName)
            match f.Value with
            | NoCast -> Expression.LessThanOrEqual (``member``, Expression.Constant f.Value)
            | Enumerable -> Expression.LessThanOrEqual (``member``, Expression.Constant (Values.normalizeOptional ``member``.Type f.Value))
            | NonEnumerableCast ``type`` -> Expression.LessThanOrEqual ((unsafeConvertTo ``type`` ``member``), Expression.Constant f.Value)
        | StartsWith (f, comparer) ->
            let ``member`` = Expression.PropertyOrField (param, f.FieldName)
            let comparison = comparerToStringComparison (comparer :> IComparer) |> ValueOption.defaultValue StringComparison.Ordinal
            Expression.Call (normalizeStringMemberExpr ``member``, StringStartsWithMethod, Expression.Constant f.Value, Expression.Constant comparison)
        | EndsWith (f, comparer) ->
            let ``member`` = Expression.PropertyOrField (param, f.FieldName)
            let comparison = comparerToStringComparison (comparer :> IComparer) |> ValueOption.defaultValue StringComparison.Ordinal
            Expression.Call (normalizeStringMemberExpr ``member``, StringEndsWithMethod, Expression.Constant f.Value, Expression.Constant comparison)

        | Contains (f, comparer) ->
            let ``member`` = Expression.PropertyOrField (param, f.FieldName)
            let isEnumerable (memberType : Type) =
                not (Type.(=) (memberType, stringType))
                && typeof<System.Collections.IEnumerable>.IsAssignableFrom (memberType)
                && memberType.GetInterfaces().Any (fun i -> i.FullName.StartsWith "System.Collections.Generic.IEnumerable`1")
            let normalizedValue = Values.normalizeOptional ``member``.Type f.Value
            let callContains memberType =
                let itemType =
                    if ``member``.Type.IsArray then
                        ``member``.Type.GetElementType ()
                    else
                        ``member``.Type.GetGenericArguments()[0]
                let valueType =
                    match normalizedValue with
                    | null -> itemType
                    | value -> value.GetType ()
                let castedMember =
                    if itemType = valueType then
                        ``member`` :> Expression
                    elif isEnumerableQuery then
                        let castMethod = getEnumerableCastMethod valueType
                        Expression.Call (castMethod, ``member``)
                    else
                        let castedEnumerableType = genericIEnumerableType.MakeGenericType ([| valueType |])
                        unsafeConvertTo castedEnumerableType ``member``
                match getCollectionInstanceContainsMethod memberType with
                | ValueNone ->
                    let enumerableContains = getEnumerableContainsMethod valueType
                    Expression.Call (enumerableContains, castedMember, Expression.Constant (normalizedValue))
                | ValueSome instanceContainsMethod -> Expression.Call (castedMember, instanceContainsMethod, Expression.Constant (normalizedValue))
            match ``member``.Member with
            | :? PropertyInfo as prop when prop.PropertyType |> isEnumerable -> callContains prop.PropertyType
            | :? FieldInfo as field when field.FieldType |> isEnumerable -> callContains field.FieldType
            | _ ->
                let unwrappedValue = Helpers.unwrap f.Value
                let comparison = comparerToStringComparison comparer |> ValueOption.defaultValue StringComparison.Ordinal
                Expression.Call (normalizeStringMemberExpr ``member``, StringContainsMethod, Expression.Constant (unwrappedValue :?> string, typeof<string>), Expression.Constant comparison)
        | In f when not (f.Value.IsEmpty) ->
            let ``member`` = Expression.PropertyOrField (param, f.FieldName)
            let enumerableContains = getEnumerableContainsMethod objectType
            Expression.Call (enumerableContains, (Expression.Constant f.Value), Expression.Convert (``member``, objectType))
        | In f -> Expression.Constant (false)
        | OfTypes types ->
            types
            |> Seq.map (fun t -> buildTypeDiscriminatorCheck param t)
            |> Seq.reduce (fun acc expr -> Expression.OrElse (acc, expr))
        | FilterField f ->
            let paramExpr = Expression.PropertyOrField (param, f.FieldName)
            buildFilterExpr isEnumerableQuery (SourceExpression paramExpr) buildTypeDiscriminatorCheck f.Value

    type private CompareDiscriminatorExpressionVisitor<'T, 'D>
        (compareDiscriminator : CompareDiscriminatorExpression<'T, 'D>, param : SourceExpression, value : obj) =
        inherit ExpressionVisitor ()
        override _.VisitParameter (node) =
            if node = compareDiscriminator.Parameters.[0] then
                param.Value
            elif node = compareDiscriminator.Parameters.[1] then
                Expression.Constant (value) :> Expression
            else
                node :> Expression

    let enumerableQueryType = typedefof<EnumerableQuery<_>>

    let apply (options : ObjectListFilterLinqOptions<'T, 'D>) (filter : ObjectListFilter) (query : IQueryable<'T>) =
        let isEnumerableQuery = query.GetType().GetGenericTypeDefinition () = enumerableQueryType
        // Helper for discriminator comparison
        let buildTypeDiscriminatorCheck (param : SourceExpression) (t : Type) =
            match options.CompareDiscriminator, options.GetDiscriminatorValue with
            | ValueNone, ValueNone ->
                Expression.Equal (
                    // Default discriminator property
                    Expression.PropertyOrField (param, "__typename"),
                    // Default discriminator value
                    Expression.Constant (t.FullName)
                )
                :> Expression
            | ValueSome discExpr, ValueNone ->
                // Replace parameters from the original expression with our new ones
                let replacer = CompareDiscriminatorExpressionVisitor (discExpr, param, t.FullName)
                replacer.Visit discExpr.Body
            | ValueNone, ValueSome discValueFn ->
                let discriminatorValue = discValueFn t
                Expression.Equal (
                    // Default discriminator property
                    Expression.PropertyOrField (param, "__typename"),
                    // Provided discriminator value gathered from type
                    Expression.Constant (discriminatorValue)
                )
                :> Expression
            | ValueSome discExpr, ValueSome discValueFn ->
                let discriminatorValue = discValueFn t
                // Replace parameters from the original expression with our new ones
                let replacer = CompareDiscriminatorExpressionVisitor (discExpr, param, discriminatorValue)
                replacer.Visit discExpr.Body
        let queryExpr =
            let param = Expression.Parameter (typeof<'T>, "x")
            let body = buildFilterExpr isEnumerableQuery (SourceExpression param) buildTypeDiscriminatorCheck filter
            whereExpr<'T> query param body
        // Create and execute the final expression
        query.Provider.CreateQuery<'T> (queryExpr)

[<AutoOpen>]
module ObjectListFilterExtensions =

    open ObjectListFilter

    type ObjectListFilter with

        member inline filter.ApplyTo<'T, 'D> (query : IQueryable<'T>, [<Optional>] options : ObjectListFilterLinqOptions<'T, 'D>) =
            apply options filter query

    type IQueryable<'T> with

        member inline query.Apply (filter : ObjectListFilter, [<Optional>] options : ObjectListFilterLinqOptions<'T, 'D>) = apply options filter query
