/// Contains customized schema definitions for extensibility features.
[<AutoOpen>]
module FSharp.Data.GraphQL.Server.Middleware.SchemaDefinitions

open System
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Ast
open FsToolkit.ErrorHandling

type private ComparisonOperator =
    | EndsWith of string
    | StartsWith of string
    | Contains of string
    | Equals of string
    | GreaterThan of string
    | GreaterThanOrEqual of string
    | LessThan of string
    | LessThanOrEqual of string
    | In of string

let rec private coerceObjectListFilterInput (variables : Variables) inputValue : Result<ObjectListFilter voption, IGQLError list> =

    let parseFieldCondition (s : string) =
        let s = s.ToLowerInvariant ()
        let prefix (suffix : string) (s : string) = s.Substring (0, s.Length - suffix.Length)
        match s with
        | s when s.EndsWith ("_ends_with") && s.Length > "_ends_with".Length -> EndsWith (prefix "_ends_with" s)
        | s when s.EndsWith ("_ew") && s.Length > "_ew".Length -> EndsWith (prefix "_ew" s)
        | s when s.EndsWith ("_starts_with") && s.Length > "_starts_with".Length -> StartsWith (prefix "_starts_with" s)
        | s when s.EndsWith ("_sw") && s.Length > "_sw".Length -> StartsWith (prefix "_sw" s)
        | s when s.EndsWith ("_contains") && s.Length > "_contains".Length -> Contains (prefix "_contains" s)
        | s when s.EndsWith ("_greater_than") && s.Length > "_greater_than".Length -> GreaterThan (prefix "_greater_than" s)
        | s when s.EndsWith ("_gt") && s.Length > "_gt".Length -> GreaterThan (prefix "_gt" s)
        | s when s.EndsWith ("_greater_than_or_equal") && s.Length > "_greater_than_or_equal".Length -> GreaterThanOrEqual (prefix "_greater_than_or_equal" s)
        | s when s.EndsWith ("_gte") && s.Length > "_gte".Length -> GreaterThanOrEqual (prefix "_gte" s)
        | s when s.EndsWith ("_less_than") && s.Length > "_less_than".Length -> LessThan (prefix "_less_than" s)
        | s when s.EndsWith ("_lt") && s.Length > "_lt".Length -> LessThan (prefix "_lt" s)
        | s when s.EndsWith ("_less_than_or_equal") && s.Length > "_less_than_or_equal".Length -> LessThanOrEqual (prefix "_less_than_or_equal" s)
        | s when s.EndsWith ("_lte") && s.Length > "_lte".Length -> LessThanOrEqual (prefix "_lte" s)
        | s when s.EndsWith ("_in") && s.Length > "_in".Length -> In (prefix "_in" s)
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
        | EndsWith fname, StringValue value -> Ok (ValueSome (ObjectListFilter.EndsWith { FieldName = fname; Value = value }))
        | StartsWith fname, StringValue value -> Ok (ValueSome (ObjectListFilter.StartsWith { FieldName = fname; Value = value }))
        | Contains fname, ComparableValue value -> Ok (ValueSome (ObjectListFilter.Contains { FieldName = fname; Value = value }))
        | Equals fname, ObjectValue value ->
            match mapInput value with
            | Error errs -> Error errs
            | Ok ValueNone -> Ok ValueNone
            | Ok (ValueSome filter) -> Ok (ValueSome (FilterField { FieldName = fname; Value = filter }))
        | Equals fname, EquatableValue value -> Ok (ValueSome (ObjectListFilter.Equals { FieldName = fname; Value = value }))
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
