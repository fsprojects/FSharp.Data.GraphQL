namespace FSharp.Data.GraphQL.Server.Middleware

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Reflection
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Extensions
open FSharp.Reflection

[<RequireQualifiedAccess>]
module TypeCoercion =

    /// Case-insensitive instance property lookup. The middleware lowercases field names during
    /// parsing, so we must also ignore casing here.
    let propertyBindFlags =
        BindingFlags.Public
        ||| BindingFlags.Instance
        ||| BindingFlags.IgnoreCase

    /// DU/union member access flags. Internal cases must be resolvable through reflection.
    let unionBindFlags = BindingFlags.Public ||| BindingFlags.NonPublic

    // Cached type references
    let private stringType = typeof<string>

    /// <summary>
    /// If <paramref name="t"/> is <c>voption</c>, <c>option</c>, or <c>Skippable</c>, returns the inner type;
    /// otherwise <see langword="ValueNone"/>.
    /// </summary>
    let tryUnwrapOption (t : Type) : Type voption =
        if t.IsGenericType then
            let fullName = t.GetGenericTypeDefinition().FullName
            if
                fullName.StartsWith ReflectionHelper.ValueOptionTypeName
                || fullName.StartsWith ReflectionHelper.OptionTypeName
                || fullName.StartsWith ReflectionHelper.SkippableTypeName
            then
                ValueSome (t.GetGenericArguments().[0])
            else
                ValueNone
        else
            ValueNone

    let unwrapOption (t : Type) : Type =
        tryUnwrapOption t |> ValueOption.defaultValue t

    /// <summary>
    /// If <paramref name="t"/> is a generic collection, returns the element type. Handles both concrete
    /// collections (where <c>IEnumerable</c> is an implemented interface) and properties typed directly as <c>IEnumerable&lt;T&gt;</c>.
    /// </summary>
    let tryUnwrapEnumerableElement (t : Type) : Type voption =
        let isEnumerableInterface (i : Type) =
            i.IsGenericType
            && Type.(=) (i.GetGenericTypeDefinition (), typedefof<IEnumerable<_>>)
        if Type.(=) (t, stringType) then
            ValueNone
        elif t.IsArray then
            t.GetElementType () |> ValueOption.ofObj
        elif isEnumerableInterface t then
            ValueSome (t.GetGenericArguments()[0])
        else
            t.GetInterfaces ()
            |> Array.vtryFind isEnumerableInterface
            |> ValueOption.map (fun i -> i.GetGenericArguments()[0])

    /// <summary>
    /// Suffixes the middleware's parser preserves on <c>FieldFilter.FieldName</c> for scalar
    /// operators (e.g. <c>meetingId_eq</c>, <c>validFrom_gte</c>). They must be stripped before
    /// resolving the actual CLR property.
    ///
    /// These correspond to the lowercase variants produced after Phase 2 parsing in
    /// <c>SchemaDefinitions.parseFieldCondition</c>. Longer suffixes are listed first to prevent
    /// shorter ones (e.g. <c>_gt</c>) from incorrectly matching longer ones (e.g. <c>_gte</c>).
    /// </summary>
    let operatorSuffixes =
        [|
            // String operators (case-insensitive variants)
            FilterSuffixConstants.CI.StartsWithSuffix
            FilterSuffixConstants.CI.EndsWithSuffix
            FilterSuffixConstants.CI.SWSuffix
            FilterSuffixConstants.CI.EWSuffix
            FilterSuffixConstants.CI.ContainsSuffix
            FilterSuffixConstants.CI.EqualsSuffix
            FilterSuffixConstants.CI.EQSuffix
            // String operators (case-sensitive variants)
            FilterSuffixConstants.CS.StartsWithSuffix
            FilterSuffixConstants.CS.EndsWithSuffix
            FilterSuffixConstants.CS.SWSuffix
            FilterSuffixConstants.CS.EWSuffix
            FilterSuffixConstants.CS.ContainsSuffix
            FilterSuffixConstants.CS.EqualsSuffix
            FilterSuffixConstants.CS.EQSuffix
            // Numeric/comparison operators (from root)
            FilterSuffixConstants.GreaterThanOrEqualSuffix
            FilterSuffixConstants.LessThanOrEqualSuffix
            FilterSuffixConstants.GreaterThanSuffix
            FilterSuffixConstants.LessThanSuffix
            FilterSuffixConstants.GTESuffix
            FilterSuffixConstants.LTESuffix
            FilterSuffixConstants.GTSuffix
            FilterSuffixConstants.LTSuffix
            FilterSuffixConstants.InSuffix
        |]

    let stripOperatorSuffix (fieldName : string) : string =
        operatorSuffixes
        |> Array.vtryFind (fun s -> fieldName.EndsWith (s, StringComparison.OrdinalIgnoreCase))
        |> ValueOption.map (fun s -> fieldName.Substring (0, fieldName.Length - s.Length))
        |> ValueOption.defaultValue fieldName

    // Cached type references for coercion
    let private guidType = typeof<Guid>
    let private dateTimeOffsetType = typeof<DateTimeOffset>
    let private dateTimeType = typeof<DateTime>
    let private dateOnlyType = typeof<DateOnly>
    let private timeOnlyType = typeof<TimeOnly>

    /// <summary>
    /// Built-in type coercers for common CLR types. Each coercer takes a boxed value
    /// and attempts to parse it (typically from a string) into the target type.
    /// </summary>
    let private builtInCoercers : ImmutableDictionary<Type, obj -> obj voption> =
        let tryParseString (tryParse : string -> bool * 'T) (value : obj) : obj voption =
            match value with
            | :? string as s ->
                match tryParse s with
                | true, result -> ValueSome (box result)
                | false, _ -> ValueNone
            | _ -> ValueNone

        seq {
            kvp guidType (tryParseString Guid.TryParse)
            kvp dateTimeOffsetType (tryParseString DateTimeOffset.TryParse)
            kvp dateTimeType (tryParseString DateTime.TryParse)
            kvp dateOnlyType (tryParseString DateOnly.TryParse)
            kvp timeOnlyType (tryParseString TimeOnly.TryParse)
        }
        |> ImmutableDictionary.CreateRange

    /// <summary>
    /// Tries to coerce a JSON-parsed primitive value (typically a <see cref="string"/>) into the
    /// CLR <paramref name="targetType"/>. Supports <see cref="Guid"/>, <see cref="DateTime"/>,
    /// <see cref="DateTimeOffset"/>, <see cref="DateOnly"/>, <see cref="TimeOnly"/>, and single-case F# DUs around any
    /// of the above. Multi-case DUs with fieldless cases are also supported (matches case name
    /// case-insensitively).
    /// </summary>
    // Suppress nullness warnings for the obj / objnull mixture: JSON values can theoretically
    // be null but in our middleware path the caller always boxes a non-null primitive.
    #nowarn "3261"
    let rec tryCoerceValue (customCoercers : FilterValueCoercer list) (targetType : Type) (value : obj | null) : obj voption =
        if isNull value then
            ValueNone
        elif targetType.IsInstanceOfType value then
            // Fast path: already the right type.
            ValueSome value
        else
            // Try custom coercers first
            let customResult =
                customCoercers
                |> List.vtryPick (fun coercer -> coercer targetType value)

            match customResult with
            | ValueSome coerced -> ValueSome coerced
            | ValueNone ->
                // Try built-in coercers from the static dictionary
                match builtInCoercers.TryGetValue targetType with
                | true, coercer -> coercer value
                | false, _ ->
                    // F# DU handling
                    if FSharpType.IsUnion (targetType, unionBindFlags) then
                        let cases = FSharpType.GetUnionCases (targetType, unionBindFlags)
                        if cases.Length = 1 then
                            // Single-case DU wrapping: unwrap one layer and recurse.
                            let case = cases[0]
                            let fields = case.GetFields ()
                            if fields.Length = 1 then
                                let innerType = fields[0].PropertyType
                                match tryCoerceValue customCoercers innerType value with
                                | ValueSome innerVal ->
                                    ValueSome (FSharpValue.MakeUnion (case, [| innerVal |], unionBindFlags))
                                | ValueNone -> ValueNone
                            else
                                ValueNone
                        else
                            // Multi-case DU: if the value is a string matching a fieldless case, create that case.
                            match value with
                            | :? string as str ->
                                cases
                                |> Array.vtryFind (fun c ->
                                    c.GetFields().Length = 0
                                    && String.Equals (c.Name, str, StringComparison.OrdinalIgnoreCase))
                                |> ValueOption.map (fun c -> FSharpValue.MakeUnion (c, [||], unionBindFlags))
                            | _ -> ValueNone
                    else
                        ValueNone

    /// <summary>
    /// Coerces an entire <see cref="ObjectListFilter"/> tree recursively by resolving the
    /// entities's properties and converting filter values into the property's CLR type.
    /// </summary>
    let rec coerceFilter (customCoercers : FilterValueCoercer list) (entityType : Type) (filter : ObjectListFilter) : ObjectListFilter =
        match filter with
        | And (l, r) -> And (coerceFilter customCoercers entityType l, coerceFilter customCoercers entityType r)
        | Or (l, r) -> Or (coerceFilter customCoercers entityType l, coerceFilter customCoercers entityType r)
        | Not f -> Not (coerceFilter customCoercers entityType f)
        | OfTypes _ -> filter
        | Equals (ff, cmp) ->
            match entityType.GetProperty (stripOperatorSuffix ff.FieldName, propertyBindFlags) with
            | null -> filter
            | prop ->
                let unwrapped = unwrapOption prop.PropertyType
                match tryCoerceValue customCoercers unwrapped (box ff.Value) with
                | ValueNone -> filter
                | ValueSome coerced -> Equals ({ ff with Value = coerced :?> IComparable }, cmp)
        | GreaterThan ff
        | GreaterThanOrEqual ff
        | LessThan ff
        | LessThanOrEqual ff as originalFilter ->
            match entityType.GetProperty (stripOperatorSuffix ff.FieldName, propertyBindFlags) with
            | null -> filter
            | prop ->
                let unwrapped = unwrapOption prop.PropertyType
                // For comparison operators, if the property is a single-case DU, coerce to the
                // inner type only — not the DU itself. buildFilterExpr handles the cast via
                // unsafeConvertTo, and the DU's op_GreaterThan etc. take the inner primitive.
                let coercionTarget =
                    match tryUnwrapOption unwrapped with
                    | ValueSome inner -> inner   // already unwrapped above, shouldn't occur
                    | ValueNone ->
                        if FSharpType.IsUnion (unwrapped, unionBindFlags) then
                            let cases = FSharpType.GetUnionCases (unwrapped, unionBindFlags)
                            if cases.Length = 1 then
                                let fields = cases[0].GetFields ()
                                if fields.Length = 1 then fields[0].PropertyType
                                else unwrapped
                            else unwrapped
                        else unwrapped
                match tryCoerceValue customCoercers coercionTarget (box ff.Value) with
                | ValueNone -> filter
                | ValueSome coerced ->
                    let coercedField = { ff with Value = coerced :?> IComparable }
                    match originalFilter with
                    | GreaterThan _ -> GreaterThan coercedField
                    | GreaterThanOrEqual _ -> GreaterThanOrEqual coercedField
                    | LessThan _ -> LessThan coercedField
                    | LessThanOrEqual _ -> LessThanOrEqual coercedField
                    | _ -> filter
        | In ff ->
            match entityType.GetProperty (stripOperatorSuffix ff.FieldName, propertyBindFlags) with
            | null -> filter
            | prop ->
                let unwrapped = unwrapOption prop.PropertyType
                let coercedList =
                    ff.Value
                    |> List.choose (fun item ->
                        match tryCoerceValue customCoercers unwrapped item with
                        | ValueSome coerced -> Some coerced
                        | ValueNone -> None)
                In { ff with Value = coercedList }
        | StartsWith (ff, cmp)
        | EndsWith (ff, cmp) as originalFilter ->
            match entityType.GetProperty (stripOperatorSuffix ff.FieldName, propertyBindFlags) with
            | null -> filter
            | _ ->
                // The pattern argument for string operators is always a plain string —
                // never coerce it into the member's DU type.
                match tryCoerceValue customCoercers stringType (box ff.Value) with
                | ValueNone -> filter
                | ValueSome coerced ->
                    let coercedField = { ff with Value = coerced :?> string }
                    match originalFilter with
                    | StartsWith (_, cmp) -> StartsWith (coercedField, cmp)
                    | EndsWith (_, cmp) -> EndsWith (coercedField, cmp)
                    | _ -> filter
        | Contains (ff, cmp) ->
            match entityType.GetProperty (stripOperatorSuffix ff.FieldName, propertyBindFlags) with
            | null -> filter
            | prop ->
                let unwrapped = unwrapOption prop.PropertyType
                // For enumerable members coerce to the element type; for string-like members
                // keep the pattern value as a plain string (same reasoning as StartsWith/EndsWith).
                let coercionTarget =
                    match tryUnwrapEnumerableElement unwrapped with
                    | ValueSome elementType -> elementType
                    | ValueNone -> stringType
                match tryCoerceValue customCoercers coercionTarget (box ff.Value) with
                | ValueNone -> filter
                | ValueSome coerced -> Contains ({ ff with Value = coerced :?> IComparable }, cmp)
        | FilterField ff ->
            match entityType.GetProperty (ff.FieldName, propertyBindFlags) with
            | null -> filter
            | prop ->
                let unwrapped = unwrapOption prop.PropertyType
                let nestedType =
                    tryUnwrapEnumerableElement unwrapped
                    |> ValueOption.defaultValue unwrapped
                FilterField { FieldName = ff.FieldName; Value = coerceFilter customCoercers nestedType ff.Value }
