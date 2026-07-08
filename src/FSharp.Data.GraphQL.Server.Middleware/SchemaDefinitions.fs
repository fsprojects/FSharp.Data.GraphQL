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

// String filter suffixes:
//   lowercase (e.g. _ends_with, _ew)   → case-insensitive (OrdinalIgnoreCase)
//   Capitalized (e.g. _Ends_With, _EW) → case-sensitive (Ordinal)
[<Literal>]
let private endsWithSuffix = "_ends_with"

[<Literal>]
let private ewSuffix = "_ew"

[<Literal>]
let private EndsWithCSSuffix = "_Ends_With"

[<Literal>]
let private EWCSSuffix = "_EW"

[<Literal>]
let private startsWithSuffix = "_starts_with"

[<Literal>]
let private swSuffix = "_sw"

[<Literal>]
let private StartsWithCSSuffix = "_Starts_With"

[<Literal>]
let private SWCSSuffix = "_SW"

[<Literal>]
let private containsSuffix = "_contains"

[<Literal>]
let private ContainsCSSuffix = "_Contains"

[<Literal>]
let private equalsSuffix = "_equals"

[<Literal>]
let private eqSuffix = "_eq"

[<Literal>]
let private EqualsCSSuffix = "_Equals"

[<Literal>]
let private EQCSSuffix = "_EQ"

[<Literal>]
let private greaterThanOrEqualSuffix = "_greater_than_or_equal"

[<Literal>]
let private gteSuffix = "_gte"

[<Literal>]
let private greaterThanSuffix = "_greater_than"

[<Literal>]
let private gtSuffix = "_gt"

[<Literal>]
let private lessThanOrEqualSuffix = "_less_than_or_equal"

[<Literal>]
let private lteSuffix = "_lte"

[<Literal>]
let private lessThanSuffix = "_less_than"

[<Literal>]
let private ltSuffix = "_lt"

[<Literal>]
let private inSuffix = "_in"

let rec private coerceObjectListFilterInput (variables : Variables) inputValue : Result<ObjectListFilter voption, IGQLError list> =

    let parseFieldCondition (s : string) =
        let prefix (suffix : string) (s : string) = s.Substring (0, s.Length - suffix.Length)
        // Phase 1: case-sensitive string ops – match original string against capitalized/uppercase suffixes
        match s with
        | s when s.EndsWith EndsWithCSSuffix && s.Length > EndsWithCSSuffix.Length -> EndsWith (prefix EndsWithCSSuffix s, StringComparer.Ordinal)
        | s when s.EndsWith EWCSSuffix && s.Length > EWCSSuffix.Length -> EndsWith (prefix EWCSSuffix s, StringComparer.Ordinal)
        | s when s.EndsWith StartsWithCSSuffix && s.Length > StartsWithCSSuffix.Length -> StartsWith (prefix StartsWithCSSuffix s, StringComparer.Ordinal)
        | s when s.EndsWith SWCSSuffix && s.Length > SWCSSuffix.Length -> StartsWith (prefix SWCSSuffix s, StringComparer.Ordinal)
        | s when s.EndsWith ContainsCSSuffix && s.Length > ContainsCSSuffix.Length -> Contains (prefix ContainsCSSuffix s, StringComparer.Ordinal)
        | s when s.EndsWith EqualsCSSuffix && s.Length > EqualsCSSuffix.Length -> StringEquals (prefix EqualsCSSuffix s, StringComparer.Ordinal)
        | s when s.EndsWith EQCSSuffix && s.Length > EQCSSuffix.Length -> StringEquals (prefix EQCSSuffix s, StringComparer.Ordinal)
        | _ ->
        // Phase 2: case-insensitive string ops and numeric ops – lower-case before matching
        let s = s.ToLowerInvariant ()
        match s with
        | s when s.EndsWith endsWithSuffix && s.Length > endsWithSuffix.Length -> EndsWith (prefix endsWithSuffix s, StringComparer.OrdinalIgnoreCase)
        | s when s.EndsWith ewSuffix && s.Length > ewSuffix.Length -> EndsWith (prefix ewSuffix s, StringComparer.OrdinalIgnoreCase)
        | s when s.EndsWith startsWithSuffix && s.Length > startsWithSuffix.Length -> StartsWith (prefix startsWithSuffix s, StringComparer.OrdinalIgnoreCase)
        | s when s.EndsWith swSuffix && s.Length > swSuffix.Length -> StartsWith (prefix swSuffix s, StringComparer.OrdinalIgnoreCase)
        | s when s.EndsWith containsSuffix && s.Length > containsSuffix.Length -> Contains (prefix containsSuffix s, StringComparer.OrdinalIgnoreCase)
        | s when s.EndsWith equalsSuffix && s.Length > equalsSuffix.Length -> StringEquals (prefix equalsSuffix s, StringComparer.OrdinalIgnoreCase)
        | s when s.EndsWith eqSuffix && s.Length > eqSuffix.Length -> StringEquals (prefix eqSuffix s, StringComparer.OrdinalIgnoreCase)
        | s when s.EndsWith greaterThanOrEqualSuffix && s.Length > greaterThanOrEqualSuffix.Length -> GreaterThanOrEqual (prefix greaterThanOrEqualSuffix s)
        | s when s.EndsWith gteSuffix && s.Length > gteSuffix.Length -> GreaterThanOrEqual (prefix gteSuffix s)
        | s when s.EndsWith greaterThanSuffix && s.Length > greaterThanSuffix.Length -> GreaterThan (prefix greaterThanSuffix s)
        | s when s.EndsWith gtSuffix && s.Length > gtSuffix.Length -> GreaterThan (prefix gtSuffix s)
        | s when s.EndsWith lessThanOrEqualSuffix && s.Length > lessThanOrEqualSuffix.Length -> LessThanOrEqual (prefix lessThanOrEqualSuffix s)
        | s when s.EndsWith lteSuffix && s.Length > lteSuffix.Length -> LessThanOrEqual (prefix lteSuffix s)
        | s when s.EndsWith lessThanSuffix && s.Length > lessThanSuffix.Length -> LessThan (prefix lessThanSuffix s)
        | s when s.EndsWith ltSuffix && s.Length > ltSuffix.Length -> LessThan (prefix ltSuffix s)
        | s when s.EndsWith inSuffix && s.Length > inSuffix.Length -> In (prefix inSuffix s)
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
            "The `Filter` scalar type represents a filter on one or more fields of an object in an object list. The filter is represented by a JSON object where the fields are the complemented by specific suffixes to represent a query."
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
