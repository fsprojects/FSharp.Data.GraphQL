namespace FSharp.Data.GraphQL.Server.Middleware

open System
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Errors


/// Contains customized schema definitions for extensibility features.
[<AutoOpen>]
module SchemaDefinitions =

    let internal removeNoFilter = Seq.where (fun filter -> filter <> NoFilter)

    let rec private coerceObjectListFilterInput x : Result<ObjectListFilter, IGQLError list> =

        let (|EndsWith|StartsWith|GreaterThan|LessThan|Contains|Equals|) (s : string) =
            let s = s.ToLowerInvariant()
            let prefix (suffix : string) (s : string) = s.Substring(0, s.Length - suffix.Length)
            match s with
            | s when s.EndsWith("_ends_with") && s.Length > "_ends_with".Length -> EndsWith (prefix "_ends_with" s)
            | s when s.EndsWith("_ew") && s.Length > "_ew".Length -> EndsWith (prefix "_ew" s)
            | s when s.EndsWith("_starts_with") && s.Length > "_starts_with".Length -> StartsWith (prefix "_starts_with" s)
            | s when s.EndsWith("_sw") && s.Length > "_sw".Length -> StartsWith (prefix "_sw" s)
            | s when s.EndsWith("_greater_than") && s.Length > "_greater_than".Length -> GreaterThan (prefix "_greater_than" s)
            | s when s.EndsWith("_gt") && s.Length > "_gt".Length -> GreaterThan (prefix "_gt" s)
            | s when s.EndsWith("_less_than") && s.Length > "_less_than".Length -> LessThan (prefix "_less_than" s)
            | s when s.EndsWith("_lt") && s.Length > "_lt".Length -> LessThan (prefix "_lt" s)
            | s when s.EndsWith("_contains") && s.Length > "_contains".Length -> Contains (prefix "_contains" s)
            | s -> Equals s

        let (|EquatableValue|Other|) v =
            match v with
            | IntValue v -> EquatableValue (v :> System.IComparable)
            | FloatValue v -> EquatableValue (v :> System.IComparable)
            | BooleanValue v -> EquatableValue (v :> System.IComparable)
            | StringValue v -> EquatableValue (v :> System.IComparable)
            | EnumValue v -> EquatableValue (v :> System.IComparable)
            | v -> Other v

        let (|ComparableValue|Other|) v =
            match v with
            | IntValue v -> ComparableValue (v :> System.IComparable)
            | FloatValue v -> ComparableValue (v :> System.IComparable)
            | BooleanValue v -> ComparableValue (v :> System.IComparable)
            | StringValue v -> ComparableValue (v :> System.IComparable)
            | v -> Other v

        let buildAnd x =
            let rec build acc x =
                match x with
                | [] -> acc
                | x :: xs ->
                    match acc with
                    | NoFilter -> build (x) xs
                    | acc -> build ((And (acc, x))) xs
            build NoFilter x

        let buildOr x =
            let rec build acc x =
                match x with
                | [] -> acc
                | x :: xs ->
                    match acc with
                    | NoFilter -> build (x) xs
                    | acc -> build ((Or (acc, x))) xs
            build NoFilter x

        let rec mapFilter (name : string, value : InputValue) =
            let mapFilters fields =
                let coerceResults =
                    fields |> Seq.map coerceObjectListFilterInput |> Seq.toList |> splitSeqErrorsList
                match coerceResults with
                | Error errs -> Error errs
                | Ok coerced -> coerced |> removeNoFilter |> Seq.toList |> Ok
            match name, value with
            | Equals "and", ListValue fields -> fields |> mapFilters |> Result.map buildAnd
            | Equals "or", ListValue fields -> fields |> mapFilters |> Result.map buildOr
            | Equals "not", ObjectValue value ->
                match mapInput value with
                | Error errs -> Error errs
                | Ok NoFilter -> Ok NoFilter
                | Ok filter -> Ok (Not filter)
            | EndsWith fname, StringValue value -> Ok (EndsWith { FieldName = fname; Value = value })
            | StartsWith fname, StringValue value -> Ok (StartsWith { FieldName = fname; Value = value })
            | Contains fname, StringValue value -> Ok (Contains { FieldName = fname; Value = value })
            | Equals fname, ObjectValue value ->
                match mapInput value with
                | Error errs -> Error errs
                | Ok NoFilter -> Ok NoFilter
                | Ok filter -> Ok (FilterField { FieldName = fname; Value = filter })
            | Equals fname, EquatableValue value -> Ok (Equals { FieldName = fname; Value = value })
            | GreaterThan fname, ComparableValue value -> Ok (GreaterThan { FieldName = fname; Value = value })
            | LessThan fname, ComparableValue value -> Ok (LessThan { FieldName = fname; Value = value })
            | _ -> Ok NoFilter

        and mapInput value =
            let filterResults =
                value |> Map.toSeq |> Seq.map mapFilter |> Seq.toList |> splitSeqErrorsList
            match filterResults with
            | Error errs -> Error errs
            | Ok filters ->
                filters |> removeNoFilter |> List.ofSeq |> buildAnd |> Ok
        match x with
        | ObjectValue x -> mapInput x
        | NullValue -> NoFilter |> Ok
        // TODO: Get union case
        | _ -> Error [{ new IGQLError with member _.Message = $"'ObjectListFilter' must be defined as object but got '{x.GetType ()}'" }]

    let private coerceObjectListFilterValue (x : obj) : ObjectListFilter option =
        match x with
        | :? ObjectListFilter as x -> Some x
        | _ -> None
    //let private coerceObjectListFilterValue (x : obj) =
    //    match x with
    //    | :? ObjectListFilter as x -> Ok x
    //    | _ -> Error [{ new IGQLError with member _.Message = $"Cannot coerce ObjectListFilter output. '%s{x.GetType().FullName}' is not 'ObjectListFilter'" }]

    /// Defines an object list filter for use as an argument for filter list of object fields.
    let ObjectListFilter : ScalarDefinition<ObjectListFilter> =
        { Name = "ObjectListFilter"
          Description =
              Some
                  "The `Filter` scalar type represents a filter on one or more fields of an object in an object list. The filter is represented by a JSON object where the fields are the complemented by specific suffixes to represent a query."
          CoerceInput =
              (function
               | InlineConstant c -> coerceObjectListFilterInput c
               | Variable _ -> raise <| NotSupportedException "List filter cannot be a variable") // TODO: Investigate
          CoerceOutput = coerceObjectListFilterValue }
