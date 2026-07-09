namespace FSharp.Data.GraphQL.Server.Middleware

open System
open System.Buffers
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
    /// Writes a boxed GraphQL scalar primitive as a JSON token directly into <paramref name="writer"/>. Strings become JSON strings; numbers and
    /// booleans become raw JSON tokens. Returns <c>true</c> if the value was written; <c>false</c> if the type is unsupported.
    /// </summary>
    let private writeJsonValue (value : obj) (writer : Utf8JsonWriter) : bool =
        match value with
        | :? string as s ->
            writer.WriteStringValue s
            true
        | :? bool as b ->
            writer.WriteBooleanValue b
            true
        | :? int64 as n ->
            writer.WriteNumberValue n
            true
        | :? int as n ->
            writer.WriteNumberValue n
            true
        | :? double as n ->
            writer.WriteNumberValue n
            true
        | :? float32 as n ->
            writer.WriteNumberValue n
            true
        | :? decimal as n ->
            writer.WriteNumberValue n
            true
        | _ ->
            false

    // Suppress nullness warnings for the obj / objnull mixture.
#nowarn "3261"
    /// <summary>
    /// Tries to coerce a value into <paramref name="targetType"/> using STJ deserialization. Primitives are written directly as JSON bytes via
    /// <see cref="writeJsonValue"/> into an <see cref="ArrayBufferWriter{T}"/>, then deserialized from <c>ReadOnlySpan&lt;byte&gt;</c>.
    /// Already-correct values pass through unchanged. No intermediate string or <see cref="JsonDocument"/> is allocated.
    /// </summary>
    let tryCoerceValue (jsonOptions : JsonSerializerOptions voption) (targetType : Type) (value : objnull) : obj voption =
        if isNull value then
            ValueNone
        elif targetType.IsInstanceOfType value then
            ValueSome value
        else
            let buffer = ArrayBufferWriter<byte> 64
            use writer = new Utf8JsonWriter (buffer)
            if not (writeJsonValue value writer) then
                ValueNone
            else
                writer.Flush ()
                try
                    let opts = jsonOptions |> ValueOption.defaultValue JsonSerializerOptions.Default
                    JsonSerializer.Deserialize (buffer.WrittenSpan, targetType, opts) |> ValueSome
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
                | ValueSome (:? IComparable as coerced) -> Equals ({ ff with Value = coerced }, cmp)
                | ValueSome _ -> filter
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
                | ValueSome (:? IComparable as coerced) ->
                    let coercedField = { ff with Value = coerced }
                    match originalFilter with
                    | GreaterThan _ -> GreaterThan coercedField
                    | GreaterThanOrEqual _ -> GreaterThanOrEqual coercedField
                    | LessThan _ -> LessThan coercedField
                    | LessThanOrEqual _ -> LessThanOrEqual coercedField
                    | _ -> filter
                | ValueSome _ -> filter
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
                | ValueSome (:? IComparable as coerced) -> Contains ({ ff with Value = coerced }, cmp)
                | ValueSome _ -> filter
        | FilterField ff ->
            match entityType.GetProperty (ff.FieldName, propertyBindFlags) with
            | null -> filter
            | prop ->
                let unwrapped = unwrapOption prop.PropertyType
                let nestedType =
                    tryUnwrapEnumerableElement unwrapped
                    |> ValueOption.defaultValue unwrapped
                FilterField { FieldName = ff.FieldName; Value = coerceFilter jsonOptions nestedType ff.Value }
