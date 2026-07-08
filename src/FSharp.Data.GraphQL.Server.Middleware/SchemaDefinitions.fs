/// Contains customized schema definitions for extensibility features.
[<AutoOpen>]
module FSharp.Data.GraphQL.Server.Middleware.SchemaDefinitions

open System
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Ast
open FsToolkit.ErrorHandling

type private ComparisonOperator =
    | EndsWith of FieldName : string * Comparer : StringComparer
    | StartsWith of FieldName : string * Comparer : StringComparer
    | Contains of FieldName : string * Comparer : StringComparer
    | StringEquals of FieldName : string * Comparer : StringComparer
    | Equals of string
    | GreaterThan of string
    | GreaterThanOrEqual of string
    | LessThan of string
    | LessThanOrEqual of string
    | In of string


let rec private coerceObjectListFilterInput (variables : Variables) inputValue : Result<ObjectListFilter voption, IGQLError list> =

    let parseFieldCondition (s : string) =
        let prefix (suffix : string) (s : string) = s.Substring (0, s.Length - suffix.Length)
        // Phase 1: case-sensitive string ops – match original string against capitalized/uppercase suffixes
        match s with
        | s when s.EndsWith FilterSuffixConstants.CS.EndsWithSuffix && s.Length > FilterSuffixConstants.CS.EndsWithSuffix.Length -> EndsWith (prefix FilterSuffixConstants.CS.EndsWithSuffix s, StringComparer.Ordinal)
        | s when s.EndsWith FilterSuffixConstants.CS.EWSuffix && s.Length > FilterSuffixConstants.CS.EWSuffix.Length -> EndsWith (prefix FilterSuffixConstants.CS.EWSuffix s, StringComparer.Ordinal)
        | s when s.EndsWith FilterSuffixConstants.CS.StartsWithSuffix && s.Length > FilterSuffixConstants.CS.StartsWithSuffix.Length -> StartsWith (prefix FilterSuffixConstants.CS.StartsWithSuffix s, StringComparer.Ordinal)
        | s when s.EndsWith FilterSuffixConstants.CS.SWSuffix && s.Length > FilterSuffixConstants.CS.SWSuffix.Length -> StartsWith (prefix FilterSuffixConstants.CS.SWSuffix s, StringComparer.Ordinal)
        | s when s.EndsWith FilterSuffixConstants.CS.ContainsSuffix && s.Length > FilterSuffixConstants.CS.ContainsSuffix.Length -> Contains (prefix FilterSuffixConstants.CS.ContainsSuffix s, StringComparer.Ordinal)
        | s when s.EndsWith FilterSuffixConstants.CS.EqualsSuffix && s.Length > FilterSuffixConstants.CS.EqualsSuffix.Length -> StringEquals (prefix FilterSuffixConstants.CS.EqualsSuffix s, StringComparer.Ordinal)
        | s when s.EndsWith FilterSuffixConstants.CS.EQSuffix && s.Length > FilterSuffixConstants.CS.EQSuffix.Length -> StringEquals (prefix FilterSuffixConstants.CS.EQSuffix s, StringComparer.Ordinal)
        | _ ->
        // Phase 2: case-insensitive string ops and numeric ops – lower-case before matching
        let s = s.ToLowerInvariant ()
        match s with
        | s when s.EndsWith FilterSuffixConstants.CI.EndsWithSuffix && s.Length > FilterSuffixConstants.CI.EndsWithSuffix.Length -> EndsWith (prefix FilterSuffixConstants.CI.EndsWithSuffix s, StringComparer.OrdinalIgnoreCase)
        | s when s.EndsWith FilterSuffixConstants.CI.EWSuffix && s.Length > FilterSuffixConstants.CI.EWSuffix.Length -> EndsWith (prefix FilterSuffixConstants.CI.EWSuffix s, StringComparer.OrdinalIgnoreCase)
        | s when s.EndsWith FilterSuffixConstants.CI.StartsWithSuffix && s.Length > FilterSuffixConstants.CI.StartsWithSuffix.Length -> StartsWith (prefix FilterSuffixConstants.CI.StartsWithSuffix s, StringComparer.OrdinalIgnoreCase)
        | s when s.EndsWith FilterSuffixConstants.CI.SWSuffix && s.Length > FilterSuffixConstants.CI.SWSuffix.Length -> StartsWith (prefix FilterSuffixConstants.CI.SWSuffix s, StringComparer.OrdinalIgnoreCase)
        | s when s.EndsWith FilterSuffixConstants.CI.ContainsSuffix && s.Length > FilterSuffixConstants.CI.ContainsSuffix.Length -> Contains (prefix FilterSuffixConstants.CI.ContainsSuffix s, StringComparer.OrdinalIgnoreCase)
        | s when s.EndsWith FilterSuffixConstants.CI.EqualsSuffix && s.Length > FilterSuffixConstants.CI.EqualsSuffix.Length -> StringEquals (prefix FilterSuffixConstants.CI.EqualsSuffix s, StringComparer.OrdinalIgnoreCase)
        | s when s.EndsWith FilterSuffixConstants.CI.EQSuffix && s.Length > FilterSuffixConstants.CI.EQSuffix.Length -> StringEquals (prefix FilterSuffixConstants.CI.EQSuffix s, StringComparer.OrdinalIgnoreCase)
        | s when s.EndsWith FilterSuffixConstants.GreaterThanOrEqualSuffix && s.Length > FilterSuffixConstants.GreaterThanOrEqualSuffix.Length -> GreaterThanOrEqual (prefix FilterSuffixConstants.GreaterThanOrEqualSuffix s)
        | s when s.EndsWith FilterSuffixConstants.GTESuffix && s.Length > FilterSuffixConstants.GTESuffix.Length -> GreaterThanOrEqual (prefix FilterSuffixConstants.GTESuffix s)
        | s when s.EndsWith FilterSuffixConstants.GreaterThanSuffix && s.Length > FilterSuffixConstants.GreaterThanSuffix.Length -> GreaterThan (prefix FilterSuffixConstants.GreaterThanSuffix s)
        | s when s.EndsWith FilterSuffixConstants.GTSuffix && s.Length > FilterSuffixConstants.GTSuffix.Length -> GreaterThan (prefix FilterSuffixConstants.GTSuffix s)
        | s when s.EndsWith FilterSuffixConstants.LessThanOrEqualSuffix && s.Length > FilterSuffixConstants.LessThanOrEqualSuffix.Length -> LessThanOrEqual (prefix FilterSuffixConstants.LessThanOrEqualSuffix s)
        | s when s.EndsWith FilterSuffixConstants.LTESuffix && s.Length > FilterSuffixConstants.LTESuffix.Length -> LessThanOrEqual (prefix FilterSuffixConstants.LTESuffix s)
        | s when s.EndsWith FilterSuffixConstants.LessThanSuffix && s.Length > FilterSuffixConstants.LessThanSuffix.Length -> LessThan (prefix FilterSuffixConstants.LessThanSuffix s)
        | s when s.EndsWith FilterSuffixConstants.LTSuffix && s.Length > FilterSuffixConstants.LTSuffix.Length -> LessThan (prefix FilterSuffixConstants.LTSuffix s)
        | s when s.EndsWith FilterSuffixConstants.InSuffix && s.Length > FilterSuffixConstants.InSuffix.Length -> In (prefix FilterSuffixConstants.InSuffix s)
        | s -> Equals s

    let (|EquatableValue|NonEquatableValue|) v =
        match v with
        | IntValue v -> EquatableValue (v :> System.IComparable)
        | FloatValue v -> EquatableValue (v :> System.IComparable)
        | BooleanValue v -> EquatableValue (v :> System.IComparable)
        | StringValue v -> EquatableValue (v :> System.IComparable)
        | EnumValue v -> EquatableValue (v :> System.IComparable)
        | v -> NonEquatableValue v

    let (|ComparableValue|NonComparableValue|) v =
        match v with
        | IntValue v -> ComparableValue (v :> System.IComparable)
        | FloatValue v -> ComparableValue (v :> System.IComparable)
        | BooleanValue v -> ComparableValue (v :> System.IComparable)
        | StringValue v -> ComparableValue (v :> System.IComparable)
        | v -> NonComparableValue v

    let buildAnd x =
        let rec build acc x =
            match x with
            | [] -> acc
            | x :: xs ->
                match acc with
                | ValueNone -> build (ValueSome x) xs
                | ValueSome acc -> build (ValueSome (And (acc, x))) xs
        build ValueNone x

    let buildOr x =
        let rec build acc x =
            match x with
            | [] -> acc
            | x :: xs ->
                match acc with
                | ValueNone -> build (ValueSome x) xs
                | ValueSome acc -> build (ValueSome (Or (acc, x))) xs
        build ValueNone x

    let rec mapFilter (condition : ComparisonOperator) (value : InputValue) =
        let mapFilters fields =
            let coerceResults =
                fields
                |> Seq.map (coerceObjectListFilterInput variables)
                |> Seq.toList
                |> splitSeqErrorsList
            match coerceResults with
            | Error errs -> Error errs
            | Ok coerced -> coerced |> Seq.vchoose id |> Seq.toList |> Ok
        match condition, value with
        | Equals "and", ListValue fields -> fields |> mapFilters |> Result.map buildAnd
        | Equals "or", ListValue fields -> fields |> mapFilters |> Result.map buildOr
        | Equals "not", ObjectValue value ->
            match mapInput value with
            | Error errs -> Error errs
            | Ok ValueNone -> Ok ValueNone
            | Ok (ValueSome filter) -> Ok (ValueSome (Not filter))
        | EndsWith (fname, comparer), StringValue value -> Ok (ValueSome (ObjectListFilter.EndsWith ({ FieldName = fname; Value = value }, comparer)))
        | StartsWith (fname, comparer), StringValue value -> Ok (ValueSome (ObjectListFilter.StartsWith ({ FieldName = fname; Value = value }, comparer)))
        | Contains (fname, comparer), ComparableValue value -> Ok (ValueSome (ObjectListFilter.Contains ({ FieldName = fname; Value = value }, comparer)))
        | StringEquals (fname, comparer), StringValue value -> Ok (ValueSome (ObjectListFilter.Equals ({ FieldName = fname; Value = value }, comparer)))
        | Equals fname, ObjectValue value ->
            match mapInput value with
            | Error errs -> Error errs
            | Ok ValueNone -> Ok ValueNone
            | Ok (ValueSome filter) -> Ok (ValueSome (FilterField { FieldName = fname; Value = filter }))
        | Equals fname, EquatableValue value -> Ok (ValueSome (ObjectListFilter.Equals ({ FieldName = fname; Value = value }, null)))
        | GreaterThan fname, ComparableValue value -> Ok (ValueSome (ObjectListFilter.GreaterThan { FieldName = fname; Value = value }))
        | GreaterThanOrEqual fname, ComparableValue value -> Ok (ValueSome (ObjectListFilter.GreaterThanOrEqual { FieldName = fname; Value = value }))
        | LessThan fname, ComparableValue value -> Ok (ValueSome (ObjectListFilter.LessThan { FieldName = fname; Value = value }))
        | LessThanOrEqual fname, ComparableValue value -> Ok (ValueSome (ObjectListFilter.LessThanOrEqual { FieldName = fname; Value = value }))
        | In fname, ListValue values -> result {
            let! parsedValues =
                values
                |> Seq.map (function
                    | EquatableValue v -> Ok (box v)
                    | NonEquatableValue v ->
                        Error
                            { new IGQLError with
                                member _.Message = $"Cannot coerce '{v.GetType ()}' to 'System.IComparable'"
                            })
                |> Seq.toList
                |> splitSeqErrors
            return ValueSome (ObjectListFilter.In { FieldName = fname; Value = parsedValues |> Array.toList })
          }
        | condition, VariableName variableName ->
            match variables.TryGetValue variableName with
            | true, value -> mapFilter condition (value |> InputValue.OfObject)
            | false, _ -> Errors.Variables.getVariableNotFoundError variableName
        | _ -> Ok ValueNone

    and mapInput value =
        let filterResults =
            value
            |> Seq.map (fun kvp -> mapFilter (parseFieldCondition kvp.Key) kvp.Value)
            |> Seq.toList
            |> splitSeqErrorsList
        match filterResults with
        | Error errs -> Error errs
        | Ok filters -> filters |> Seq.vchoose id |> List.ofSeq |> buildAnd |> Ok

    let rec parse inputValue =
        match inputValue with
        | ObjectValue x -> mapInput x
        | NullValue -> ValueNone |> Ok
        | VariableName variableName ->
            match variables.TryGetValue variableName with
            | true, (:? ObjectListFilter as filter) -> ValueSome filter |> Ok
            | true, value ->
                System.Diagnostics.Debug.Fail "We expect the root value is parsed into ObjectListFilter"
                value |> InputValue.OfObject |> parse
            | false, _ -> Errors.Variables.getVariableNotFoundError variableName
        // TODO: Get union case
        | _ ->
            Error [
                { new IGQLError with
                    member _.Message = $"'ObjectListFilter' must be defined as object but got '{inputValue.GetType ()}'"
                }
            ]
    parse inputValue


/// Defines an object list filter for use as an argument for filter list of object fields.
let ObjectListFilterType : InputCustomDefinition<ObjectListFilter> = {
    Name = "ObjectListFilter"
    Description =
        Some
            "The `ObjectListFilter` input represents field filters for object lists. Lowercase string suffixes such as `_starts_with`/`_sw`, `_ends_with`/`_ew`, `_contains`, and `_equals`/`_eq` are case-insensitive. Capitalized string suffixes such as `_Starts_With`/`_SW`, `_Ends_With`/`_EW`, `_Contains`, and `_Equals`/`_EQ` are case-sensitive. Comparison suffixes such as `_gt`, `_gte`, `_lt`, `_lte`, and `_in` are also supported."
    CoerceInput =
        (fun _ input variables ->
            match input with
            | InlineConstant c ->
                (coerceObjectListFilterInput variables c)
                |> Result.map ValueOption.toObj
            | Variable json ->
                json
                |> InputValue.OfJsonElement
                |> (coerceObjectListFilterInput variables)
                |> Result.map ValueOption.toObj)
}
