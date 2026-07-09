namespace FSharp.Data.GraphQL.Server.Middleware

open System
open System.Collections
open System.Collections.Concurrent
open System.Collections.Generic
open System.Linq
open System.Linq.Expressions
open System.Reflection
open System.Runtime.InteropServices
open FSharp.Data.GraphQL

/// Contains tooling for working with ObjectListFilter.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ObjectListFilter =
    /// Contains operators for building and comparing ObjectListFilter values.
    module Operators =
        /// Creates a new ObjectListFilter representing an AND operation between two existing ones.
        let (&&&) x y = And (x, y)

        /// Creates a new ObjectListFilter representing an OR operation between two existing ones.
        let (|||) x y = Or (x, y)

        /// Creates a new ObjectListFilter representing an EQUALS operation between two comparable values.
        let (===) fname value = Equals ({ FieldName = fname; Value = value }, null)

        /// Creates a new ObjectListFilter representing a GREATER THAN operation of a comparable value.
        let (>>>) fname value = GreaterThan { FieldName = fname; Value = value }

        /// Creates a new ObjectListFilter representing a GREATER THAN OR EQUAL operation of a comparable value.
        let (==>) fname value = GreaterThanOrEqual { FieldName = fname; Value = value }

        /// Creates a new ObjectListFilter representing a LESS THAN operation of a comparable value.
        let (<<<) fname value = LessThan { FieldName = fname; Value = value }

        /// Creates a new ObjectListFilter representing a LESS THAN OR EQUAL operation of a comparable value.
        let (<==) fname value = LessThanOrEqual { FieldName = fname; Value = value }

        /// Creates a new ObjectListFilter representing a STARTS WITH operation of a string value.
        let (=@@) fname value = StartsWith ({ FieldName = fname; Value = value }, null)

        /// Creates a new ObjectListFilter representing an ENDS WITH operation of a string value.
        let (@@=) fname value = EndsWith ({ FieldName = fname; Value = value }, null)

        /// Creates a new ObjectListFilter representing a CONTAINS operation.
        let (@=@) fname value = Contains ({ FieldName = fname; Value = value }, null)

        /// Creates a new ObjectListFilter representing a IN operation.
        let (=~=) fname value = In { FieldName = fname; Value = value }

        /// Creates a new ObjectListFilter representing a field sub comparison.
        let (-->) fname filter = FilterField { FieldName = fname; Value = filter }

        /// Creates a new ObjectListFilter representing a NOT operation for the existing one.
        let (!!!) filter = Not filter

        /// Creates a new ObjectListFilter representing a case-insensitive EQUALS operation on a string value.
        let (===~) fname (value : string) =
            Equals ({ FieldName = fname; Value = value }, StringComparer.CurrentCultureIgnoreCase)

        /// Creates a new ObjectListFilter representing a case-insensitive STARTS WITH operation on a string value.
        let (=@@~) fname (value : string) =
            StartsWith ({ FieldName = fname; Value = value }, StringComparer.CurrentCultureIgnoreCase)

        /// Creates a new ObjectListFilter representing a case-insensitive ENDS WITH operation on a string value.
        let (@@=~) fname (value : string) =
            EndsWith ({ FieldName = fname; Value = value }, StringComparer.CurrentCultureIgnoreCase)

        /// Creates a new ObjectListFilter representing a case-insensitive CONTAINS operation on a string value.
        let (@=@~) fname (value : string) =
            Contains ({ FieldName = fname; Value = value }, StringComparer.CurrentCultureIgnoreCase)

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
    let private enumerableType = typeof<Enumerable>
    let private iEnumerableType = typeof<System.Collections.IEnumerable>

    let private stringComparisonType = typeof<StringComparison>
    let private StringStartsWithMethod =
        stringType.GetMethod ("StartsWith", [| stringType; stringComparisonType |])
    let private StringEndsWithMethod =
        stringType.GetMethod ("EndsWith", [| stringType; stringComparisonType |])
    let private StringContainsMethod =
        stringType.GetMethod ("Contains", [| stringType; stringComparisonType |])
    let private StringEqualsMethod = stringType.GetMethod ("Equals", [| stringType; stringComparisonType |])
    let private unwrapOptionMethod =
        FSharp.Data.GraphQL.Helpers.moduleType.GetMethod (nameof Helpers.unwrap)

    /// Cache for MemberInfo (PropertyInfo or FieldInfo) lookups to avoid repeated reflection.
    let private memberInfoCache = System.Collections.Concurrent.ConcurrentDictionary<(Type * string), MemberInfo voption> ()

    /// Checks if a type is the generic IEnumerable<T> interface using structural comparison.
    let private isGenericIEnumerable (t : Type) : bool =
        t.IsGenericType && t.GetGenericTypeDefinition () = genericIEnumerableType

    /// Gets MemberInfo (PropertyInfo or FieldInfo) from cache, performing reflection if not cached.
    /// Mirrors the behavior of Expression.PropertyOrField which checks properties first, then fields.
    let private getCachedMemberInfo (entityType : Type) (stripSuffix : string) : MemberInfo voption =
        let key = (entityType, stripSuffix)
        memberInfoCache.GetOrAdd(
            key,
            Func<(Type * string), MemberInfo voption> (fun _ ->
                // Try property first (matches Expression.PropertyOrField behavior)
                match
                    entityType.GetProperty (
                        stripSuffix,
                        BindingFlags.Public
                        ||| BindingFlags.Instance
                        ||| BindingFlags.IgnoreCase
                    )
                with
                | null ->
                    // Fall back to field if property not found
                    match
                        entityType.GetField (
                            stripSuffix,
                            BindingFlags.Public
                            ||| BindingFlags.Instance
                            ||| BindingFlags.IgnoreCase
                        )
                    with
                    | null -> ValueNone
                    | f -> ValueSome (f :> MemberInfo)
                | p -> ValueSome (p :> MemberInfo)
            )
        )

    let private getCollectionInstanceContainsMethod (memberType : Type) =
        memberType
            .GetMethods(BindingFlags.Instance ||| BindingFlags.Public)
            .FirstOrDefault (fun m -> m.Name = "Contains" && m.GetParameters().Length = 1)
        |> ValueOption.ofObj

    let private getEnumerableContainsMethod (itemType : Type) =
        match
            enumerableType
                .GetMethods(BindingFlags.Static ||| BindingFlags.Public)
                .FirstOrDefault (fun m -> m.Name = "Contains" && m.GetParameters().Length = 2)
        with
        | null -> raise (MissingMemberException "Static 'Contains' method with 2 parameters not found on 'Enumerable' class")
        | containsGenericStaticMethod -> containsGenericStaticMethod.MakeGenericMethod ([| itemType |])

    let private getEnumerableCastMethod (itemType : Type) =
        match
            enumerableType
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
    /// Returns ValueNone for null or unsupported comparers.
    let internal comparerToStringComparison (comparer : IComparer) =
        match comparer with
        | null -> ValueNone
        | :? StringComparer as sc ->
            let mutable isOrdinalIgnoreCase = false

            if StringComparer.IsWellKnownOrdinalComparer (sc, &isOrdinalIgnoreCase) then
                if isOrdinalIgnoreCase then
                    ValueSome StringComparison.OrdinalIgnoreCase
                else
                    ValueSome StringComparison.Ordinal
            else
                let mutable compareInfo = Unchecked.defaultof<Globalization.CompareInfo>
                let mutable compareOptions = Globalization.CompareOptions.None

                if StringComparer.IsWellKnownCultureAwareComparer (sc, &compareInfo, &compareOptions) then
                    let isInvariantCulture = compareInfo.Equals Globalization.CultureInfo.InvariantCulture.CompareInfo
                    let isCurrentCulture = compareInfo.Equals Globalization.CultureInfo.CurrentCulture.CompareInfo

                    match compareOptions with
                    | Globalization.CompareOptions.None when isInvariantCulture -> ValueSome StringComparison.InvariantCulture
                    | Globalization.CompareOptions.IgnoreCase when isInvariantCulture ->
                        ValueSome StringComparison.InvariantCultureIgnoreCase
                    | Globalization.CompareOptions.None when isCurrentCulture -> ValueSome StringComparison.CurrentCulture
                    | Globalization.CompareOptions.IgnoreCase when isCurrentCulture ->
                        ValueSome StringComparison.CurrentCultureIgnoreCase
                    | _ -> ValueNone
                else
                    ValueNone
        | _ -> ValueNone

     /// Gets the type from a MemberInfo (PropertyInfo or FieldInfo).
    let private getMemberType (member' : MemberInfo) : Type =
        match member' with
        | :? PropertyInfo as p -> p.PropertyType
        | :? FieldInfo as f -> f.FieldType
        | _ -> invalidOp $"Unsupported member type: {member'.GetType().Name}"

    /// Resolves the field type within a given entity, stripping suffixes and unwrapping options.
    let private getFieldTypeForEntity (entityType : Type) (fieldName : string) : Type voption =
        let stripSuffix = TypeCoercion.stripOperatorSuffix fieldName
        match getCachedMemberInfo entityType stripSuffix with
        | ValueNone -> ValueNone
        | ValueSome member' -> ValueSome (TypeCoercion.unwrapOption (getMemberType member'))

    /// Returns both the original member type and the unwrapped type.
    /// Useful for detecting if we need to unwrap option expressions at runtime.
    let private getFieldTypeAndOriginal (entityType : Type) (fieldName : string) : (Type * Type) voption =
        let stripSuffix = TypeCoercion.stripOperatorSuffix fieldName
        match getCachedMemberInfo entityType stripSuffix with
        | ValueNone -> ValueNone
        | ValueSome ``member`` ->
            let originalType = getMemberType ``member``
            let unwrappedType = TypeCoercion.unwrapOption originalType
            ValueSome (originalType, unwrappedType)

    /// Detects if a type is enumerable (but not string).
    let private isEnumerableType (``type`` : Type) : bool =
        not (Type.(=) (``type``, stringType))
        && iEnumerableType.IsAssignableFrom (``type``)
        && ``type``.GetInterfaces().Any (fun i -> isGenericIEnumerable i)

    /// <summary>Unwraps the element type from an enumerable type.</summary>
    let private tryGetEnumerableElementType (``type`` : Type) : Type voption = TypeCoercion.tryUnwrapEnumerableElement ``type``

    /// <summary>Gets the closed generic <see cref="System.Linq.Enumerable.Any{T}"/> method for the given element type.</summary>
    let private getEnumerableAnyMethod (elementType : Type) : MethodInfo =
        match
            enumerableType
                .GetMethods(BindingFlags.Static ||| BindingFlags.Public)
                .FirstOrDefault (fun m -> m.Name = "Any" && m.GetParameters().Length = 2)
        with
        | null ->
            let message =
                $"Static 'Any' method with 2 parameters not found on '{enumerableType.FullName}' class. Expected signature: Any<T>(IEnumerable<T>, Func<T,bool>). "
            raise (MissingMemberException message)
        | anyGenericStaticMethod -> anyGenericStaticMethod.MakeGenericMethod ([| elementType |])

    let private normalizeInValue (fieldType : Type) (value : obj) : obj =
        let normalized = Values.normalizeOptional fieldType value
        if obj.ReferenceEquals (normalized, null) then
            null
        elif fieldType.IsGenericType && fieldType.GetGenericTypeDefinition () = typedefof<Nullable<_>> then
            let underlyingType = Nullable.GetUnderlyingType fieldType
            if not (obj.ReferenceEquals (underlyingType, null)) && normalized.GetType () = underlyingType then
                Activator.CreateInstance (fieldType, normalized)
            else
                normalized
        else
            normalized

    let private materializeTypedInArray (fieldType : Type) (values : obj list) : Array =
        let array = Array.CreateInstance (fieldType, values.Length)
        values
        |> List.iteri (fun index value -> array.SetValue (normalizeInValue fieldType value, index))
        array

    let rec buildFilterExpr isEnumerableQuery (param : SourceExpression) buildTypeDiscriminatorCheck filter : Expression =

        let build = buildFilterExpr isEnumerableQuery param buildTypeDiscriminatorCheck

        let (|NoCast|Enumerable|NonEnumerableCast|) value =
            if obj.ReferenceEquals (value, null) then NoCast
            else if isEnumerableQuery then Enumerable
            else NonEnumerableCast (value.GetType ())

        let unsafeConvertTo ``type`` ``member`` = Expression.Convert (Expression.Convert (``member``, objectType), ``type``)

        let normalizeStringMemberExpr (``member`` : Expression) : Expression =
            let memberType = ``member``.Type
            match memberType with
            | t when t = stringType -> ``member``
            | _ when not isEnumerableQuery -> unsafeConvertTo stringType ``member``
            | _ when isEnumerableQuery ->
                // For ParameterExpression (from "_"), we can't call unwrapOptionMethod directly
                if ``member`` :? ParameterExpression then
                    Expression.Convert (``member``, stringType)
                else
                    match ``member`` with
                    | :? MemberExpression as me -> Expression.Convert (Expression.Call (unwrapOptionMethod, me), stringType)
                    | _ -> Expression.Convert (``member``, stringType)
            | _ -> Expression.Convert (``member``, stringType)

        match filter with
        | Not (Equals (f, comparer)) ->
            let ``member`` =
                // Special case: "_" means the element itself, not a field property
                if f.FieldName = "_" then
                    param.Value
                else
                    Expression.PropertyOrField (param, f.FieldName)
            let unwrappedMemberType = TypeCoercion.unwrapOption ``member``.Type
            match comparerToStringComparison comparer with
            | ValueSome comparison when Type.(=) (unwrappedMemberType, stringType) ->
                let value = Helpers.unwrap (box f.Value) :?> string
                Expression.Not (
                    Expression.Call (
                        normalizeStringMemberExpr ``member``,
                        StringEqualsMethod,
                        Expression.Constant (value, stringType),
                        Expression.Constant comparison
                    )
                )
                :> Expression
            | ValueSome _
            | ValueNone ->
                let hasEqualityOperator = hasEqualityOperator ``member``.Type
                match f.Value with
                | NoCast when hasEqualityOperator -> Expression.NotEqual (``member``, Expression.Constant f.Value)
                | NoCast
                | NonEnumerableCast _ ->
                    Expression.NotEqual (Expression.Convert (``member``, objectType), Expression.Convert ((Expression.Constant f.Value), objectType))
                | Enumerable ->
                    let normalized = Values.normalizeOptional ``member``.Type f.Value
                    let ``const`` = Expression.Constant (normalized)
                    let boxedArg = Expression.Convert (``member``, objectType)
                    Expression.Not (Expression.Call (``const``, equalsMethod, boxedArg))
        | Not f -> f |> build |> Expression.Not :> Expression
        | And (f1, f2) -> Expression.AndAlso (build f1, build f2)
        | Or (f1, f2) -> Expression.OrElse (build f1, build f2)
        | Equals (f, comparer) ->
            let ``member`` =
                // Special case: "_" means the element itself, not a field property
                if f.FieldName = "_" then
                    param.Value
                else
                    Expression.PropertyOrField (param, f.FieldName)
            let unwrappedMemberType = TypeCoercion.unwrapOption ``member``.Type
            match comparerToStringComparison comparer with
            | ValueSome comparison when Type.(=) (unwrappedMemberType, stringType) ->
                let value = Helpers.unwrap (box f.Value) :?> string
                Expression.Call (
                    normalizeStringMemberExpr ``member``,
                    StringEqualsMethod,
                    Expression.Constant (value, stringType),
                    Expression.Constant comparison
                )
                :> Expression
            | ValueSome _
            | ValueNone ->
                let hasEqualityOperator = hasEqualityOperator ``member``.Type
                match f.Value with
                | NoCast when hasEqualityOperator -> Expression.Equal (``member``, Expression.Constant f.Value)
                | NoCast
                | NonEnumerableCast _ ->
                    Expression.Equal (Expression.Convert (``member``, objectType), Expression.Convert ((Expression.Constant f.Value), objectType))
                | Enumerable ->
                    let normalized = Values.normalizeOptional ``member``.Type f.Value
                    let ``const`` = Expression.Constant (normalized)
                    let boxedArg = Expression.Convert (``member``, objectType)
                    Expression.Call (``const``, equalsMethod, boxedArg)
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
            let comparison =
                comparerToStringComparison comparer
                |> ValueOption.defaultValue StringComparison.CurrentCulture
            Expression.Call (
                normalizeStringMemberExpr ``member``,
                StringStartsWithMethod,
                Expression.Constant f.Value,
                Expression.Constant comparison
            )
        | EndsWith (f, comparer) ->
            let ``member`` = Expression.PropertyOrField (param, f.FieldName)
            let comparison =
                comparerToStringComparison comparer
                |> ValueOption.defaultValue StringComparison.CurrentCulture
            Expression.Call (normalizeStringMemberExpr ``member``, StringEndsWithMethod, Expression.Constant f.Value, Expression.Constant comparison)

        | Contains (f, comparer) ->
            let ``member`` = Expression.PropertyOrField (param, f.FieldName)
            let isEnumerable (memberType : Type) =
                not (Type.(=) (memberType, stringType))
                && iEnumerableType.IsAssignableFrom (memberType)
                && memberType.GetInterfaces().Any (fun i -> isGenericIEnumerable i)
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
                let comparison =
                    comparerToStringComparison comparer
                    |> ValueOption.defaultValue StringComparison.CurrentCulture
                Expression.Call (
                    normalizeStringMemberExpr ``member``,
                    StringContainsMethod,
                    Expression.Constant (unwrappedValue :?> string, stringType),
                    Expression.Constant comparison
                )
        | In f when not (f.Value.IsEmpty) ->
            let ``member`` =
                // Special case: "_" means the element itself, not a field property
                if f.FieldName = "_" then
                    param.Value
                else
                    Expression.PropertyOrField (param, f.FieldName)
            let fieldType = ``member``.Type
            let typedValues = materializeTypedInArray fieldType f.Value
            let enumerableContains = getEnumerableContainsMethod fieldType
            Expression.Call (enumerableContains, Expression.Constant typedValues, ``member``)
        | In f -> Expression.Constant (false)
        | OfTypes types ->
            types
            |> Seq.map (fun t -> buildTypeDiscriminatorCheck param t)
            |> Seq.reduce (fun acc expr -> Expression.OrElse (acc, expr))
        | FilterField f ->
            let paramType = param.Value.Type
            match getFieldTypeAndOriginal paramType f.FieldName with
            | ValueNone ->
                // Fallback: just recurse (may fail downstream)
                let paramExpr = Expression.PropertyOrField (param, f.FieldName)
                buildFilterExpr isEnumerableQuery (SourceExpression paramExpr) buildTypeDiscriminatorCheck f.Value
            | ValueSome (originalFieldType, unwrappedFieldType) ->
                // Check if the UNWRAPPED type is enumerable
                let isCollection = isEnumerableType unwrappedFieldType

                // Check if this is an option-wrapped collection
                let isOptionWrapped = not (Type.(=) (originalFieldType, unwrappedFieldType))

                if isCollection then
                    let effectiveType = unwrappedFieldType
                    match tryGetEnumerableElementType effectiveType with
                    | ValueNone ->
                        // Should not happen for isEnumerableType, but fallback to direct traversal
                        let paramExpr = Expression.PropertyOrField (param, f.FieldName)
                        buildFilterExpr isEnumerableQuery (SourceExpression paramExpr) buildTypeDiscriminatorCheck f.Value
                    | ValueSome elementType ->
                        // Create lambda parameter for element
                        let elemParam = Expression.Parameter (elementType, "x")
                        // Recursively build inner filter over element type
                        let innerExpr = buildFilterExpr false (SourceExpression elemParam) buildTypeDiscriminatorCheck f.Value
                        // Lambda: x => innerExpr
                        let funcGenericDef = typeof<Func<_, bool>>.GetGenericTypeDefinition ()
                        let lambdaType = funcGenericDef.MakeGenericType ([| elementType; typeof<bool> |])
                        let lambda = Expression.Lambda (lambdaType, innerExpr, elemParam) :> Expression

                        let rawCollExpr = Expression.PropertyOrField (param, f.FieldName)

                        if isOptionWrapped then
                            // Option-wrapped collection: e.g., some_field : option<list<T>>
                            // Strategy: Access the wrapped collection via .Value and pass it to Any<T> with the predicate.
                            // If the option is None, accessing .Value throws NullReferenceException.
                            // We wrap the entire Any call in try-catch to safely return false for None,
                            // effectively treating None collections as "no match".
                            let anyMethod = getEnumerableAnyMethod elementType
                            let valueExpr = Expression.PropertyOrField (rawCollExpr, "Value")
                            let anyCall = Expression.Call (anyMethod, valueExpr, lambda)

                            // Wrap in try-catch: try { Any(opt.Value, pred) } catch (NullReferenceException) { false }
                            let catchBlock = Expression.Catch (typeof<NullReferenceException>, Expression.Constant (false))
                            let tryExpr = Expression.TryCatch (anyCall, catchBlock)
                            tryExpr :> Expression
                        else
                            // Direct collection (not wrapped in option): pass directly to Enumerable.Any
                            let anyMethod = getEnumerableAnyMethod elementType
                            Expression.Call (anyMethod, rawCollExpr, lambda)
                else
                    // Not a collection, treat as scalar
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

        /// <summary>
        /// Applies the filter to a queryable with automatic type coercion of JSON primitives to CLR types. Supports <see cref="Guid"/>,
        /// <see cref="DateTime"/>, <see cref="DateTimeOffset"/>, <see cref="DateOnly"/>, and F# discriminated unions. Pass
        /// <see cref="JsonSerializerOptions"/> via <c>ObjectListFilterLinqOptions</c> constructor for custom serialization.
        /// </summary>
        /// <example>
        /// <code>
        /// // Basic usage - automatic coercion of string to Guid
        /// let filter = "id" === "550e8400-e29b-41d4-a716-446655440000"
        /// let users = filter.ApplyTo query
        ///
        /// // With custom JsonSerializerOptions
        /// let opts = JsonSerializerOptions(PropertyNameCaseInsensitive = true)
        /// let options = ObjectListFilterLinqOptions(opts)
        /// let events = filter.ApplyTo(query, options)
        /// </code>
        /// </example>
        member inline filter.ApplyTo<'T, 'D> (query : IQueryable<'T>, [<Optional>] options : ObjectListFilterLinqOptions<'T, 'D> | null) =
            let options =
                options
                |> ValueOption.ofObj
                |> ValueOption.defaultValue ObjectListFilterLinqOptions<'T, 'D>.None
            let filter = TypeCoercion.coerceFilter options.JsonOptions typeof<'T> filter
            apply options filter query

    type IQueryable<'T> with

        /// <summary>
        /// Applies the filter with automatic type coercion of JSON primitives to CLR types. Supports <see cref="Guid"/>, <see cref="DateTime"/>,
        /// <see cref="DateTimeOffset"/>, <see cref="DateOnly"/>, and F# discriminated unions. Pass <see cref="JsonSerializerOptions"/> via <c>
        /// ObjectListFilterLinqOptions</c> constructor for custom serialization.
        /// </summary>
        /// <example>
        /// <code>
        /// // Basic usage - automatic coercion of string to Guid
        /// let filter = "id" === "550e8400-e29b-41d4-a716-446655440000"
        /// let users = query.Apply filter
        ///
        /// // With custom JsonSerializerOptions
        /// let opts = JsonSerializerOptions(PropertyNameCaseInsensitive = true)
        /// let options = ObjectListFilterLinqOptions(opts)
        /// let events = query.Apply(filter, options)
        /// </code>
        /// </example>
        member inline query.Apply (filter : ObjectListFilter, [<Optional>] options : ObjectListFilterLinqOptions<'T, 'D> | null) =
            filter.ApplyTo (query, options)
