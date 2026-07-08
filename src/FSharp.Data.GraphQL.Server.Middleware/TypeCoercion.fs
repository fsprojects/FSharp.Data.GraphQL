namespace FSharp.Data.GraphQL.Server.Middleware

open System
open System.Collections.Generic
open System.Reflection
open System.Text.Json
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Extensions

[<RequireQualifiedAccess>]
module TypeCoercion =

    /// Case-insensitive instance property lookup. The middleware lowercases field names during
    /// parsing, so we must also ignore casing here.
    let propertyBindFlags =
        BindingFlags.Public
        ||| BindingFlags.Instance
        ||| BindingFlags.IgnoreCase

    // Cached type references
    let private stringType = typeof<string>

    /// <summary>
    /// If <paramref name="t"/> is <c>voption</c>, <c>option</c>, or <c>Skippable</c>, returns the inner type; otherwise <see langword="ValueNone"/>.
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
    /// If <paramref name="t"/> is a generic collection, returns the element type. Handles both concrete collections (where <c>IEnumerable</c> is an
    /// implemented interface) and properties typed directly as <c>IEnumerable&lt;T&gt;</c>.
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
    /// Suffixes the middleware's parser preserves on <c>FieldFilter.FieldName</c> for scalar operators (e.g. <c>meetingId_eq</c>, <c>
    /// validFrom_gte</c>). They must be stripped before resolving the actual CLR property.
    /// These correspond to the lowercase variants produced after Phase 2 parsing in <c>SchemaDefinitions.parseFieldCondition</c>. Longer suffixes are
    /// listed first to prevent shorter ones (e.g. <c>_gt</c>) from incorrectly matching longer ones (e.g. <c>_gte</c>).
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

    /// <summary>
    /// Converts a boxed GraphQL scalar primitive to its JSON text representation so that <see cref="JsonSerializer.Deserialize"/> can produce the
    /// correct CLR value. Strings and enum-like values are JSON-quoted (<c>"value"</c>); numbers and booleans are emitted as raw JSON tokens.
    /// </summary>
    let private toJsonString (value : obj) : string voption =
        match value with
        | :? string as s -> ValueSome $"\"{JsonEncodedText.Encode(s)}\""
        | :? bool as b -> ValueSome (if b then "true" else "false")
        | :? int64 as n -> ValueSome (string n)
        | :? int as n -> ValueSome (string n)
        | :? double as n -> ValueSome (sprintf "%g" n)
        | :? float32 as n -> ValueSome (sprintf "%g" n)
        | :? decimal as n -> ValueSome (string n)
        | _ -> ValueNone

    // Suppress nullness warnings for the obj / objnull mixture.
#nowarn "3261"
    /// <summary>
    /// Tries to coerce a value into <paramref name="targetType"/> using STJ deserialization. Primitives are first rendered as a JSON string via
    /// <see cref="toJsonString"/> then parsed with <see cref="JsonDocument.Parse"/> and deserialized. Already-correct values pass through unchanged.
    /// </summary>
    let tryCoerceValue (jsonOptions : JsonSerializerOptions voption) (targetType : Type) (value : objnull) : obj voption =
        if isNull value then
            ValueNone
        elif targetType.IsInstanceOfType value then
            ValueSome value
        else
            match toJsonString value with
            | ValueNone -> ValueNone
            | ValueSome json ->
                try
                    let opts = jsonOptions |> ValueOption.defaultValue JsonSerializerOptions.Default
                    use doc = JsonDocument.Parse json
                    doc.Deserialize (targetType, opts) |> ValueSome
                with _ ->
                    ValueNone

    /// <summary>
    /// Coerces an entire <see cref="ObjectListFilter"/> tree recursively by resolving the entities's properties and converting filter values into the
    /// property's CLR type.
    /// </summary>
    let rec coerceFilter (jsonOptions : JsonSerializerOptions voption) (entityType : Type) (filter : ObjectListFilter) : ObjectListFilter =
        match filter with
        | And (l, r) -> And (coerceFilter jsonOptions entityType l, coerceFilter jsonOptions entityType r)
        | Or (l, r) -> Or (coerceFilter jsonOptions entityType l, coerceFilter jsonOptions entityType r)
        | Not f -> Not (coerceFilter jsonOptions entityType f)
        | OfTypes _ -> filter
        | Equals (ff, cmp) ->
            match entityType.GetProperty (stripOperatorSuffix ff.FieldName, propertyBindFlags) with
            | null -> filter
            | prop ->
                let unwrapped = unwrapOption prop.PropertyType
                match tryCoerceValue jsonOptions unwrapped (box ff.Value) with
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
                match tryCoerceValue jsonOptions unwrapped (box ff.Value) with
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
                let coercedList = ff.Value |> List.vchoose (tryCoerceValue jsonOptions unwrapped)
                In { ff with Value = coercedList }
        | StartsWith (ff, cmp)
        | EndsWith (ff, cmp) as originalFilter ->
            match entityType.GetProperty (stripOperatorSuffix ff.FieldName, propertyBindFlags) with
            | null -> filter
            | _ ->
                match tryCoerceValue jsonOptions stringType (box ff.Value) with
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
                let coercionTarget =
                    match tryUnwrapEnumerableElement unwrapped with
                    | ValueSome elementType -> elementType
                    | ValueNone -> stringType
                match tryCoerceValue jsonOptions coercionTarget (box ff.Value) with
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
                FilterField { FieldName = ff.FieldName; Value = coerceFilter jsonOptions nestedType ff.Value }
