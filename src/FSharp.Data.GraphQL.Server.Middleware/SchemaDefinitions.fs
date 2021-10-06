namespace FSharp.Data.GraphQL.Server.Middleware

open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Decoding

/// Contains customized schema definitions for extensibility features.
[<AutoOpen>]
module SchemaDefinitions =
    let rec private coerceObjectListFilterInput x =
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
                    | Some acc -> build (Some (And (acc, x))) xs
                    | None -> build (Some x) xs
            build None x
        let buildOr x =
            let rec build acc x =
                match x with
                | [] -> acc
                | x :: xs ->
                    match acc with
                    | Some acc -> build (Some (Or (acc, x))) xs
                    | None -> build (Some x) xs
            build None x
        let rec mapFilter (name : string, value : Value) =
            let mapFilters fields = fields |> List.map coerceObjectListFilterInput |> List.choose id
            match name, value with
            | Equals "and", ListValue fields -> fields |> mapFilters |> buildAnd
            | Equals "or", ListValue fields -> fields |> mapFilters |> buildOr
            | Equals "not", ObjectValue value ->
                match mapInput value with
                | Some filter -> Some (Not filter)
                | None -> None
            | EndsWith fname, StringValue value -> Some (EndsWith { FieldName = fname; Value = value })
            | StartsWith fname, StringValue value -> Some (StartsWith { FieldName = fname; Value = value })
            | Contains fname, StringValue value -> Some (Contains { FieldName = fname; Value = value })
            | Equals fname, ObjectValue value ->
                match mapInput value with
                | Some filter -> Some (FilterField { FieldName = fname; Value = filter })
                | None -> None
            | Equals fname, EquatableValue value -> Some (Equals { FieldName = fname; Value = value })
            | GreaterThan fname, ComparableValue value -> Some (GreaterThan { FieldName = fname; Value = value })
            | LessThan fname, ComparableValue value -> Some (LessThan { FieldName = fname; Value = value })
            | _ -> None
        and mapInput value =
            let filters = value |> Map.toSeq |> Seq.map mapFilter
            if filters |> Seq.contains None then None
            else filters |> Seq.choose id |> List.ofSeq |> buildAnd
        match x with
        | ObjectValue x ->
            mapInput x
        | _ -> None

    let private coerceObjectListFilterValue (x : obj) : ObjectListFilter option =
        match x with
        | :? ObjectListFilter as x -> Some x
        | _ -> None

    /// Defines an object list filter for use as an argument for filter list of object fields.
    let ObjectListFilter : ScalarDefinition<ObjectListFilter> =
        { Name = "ObjectListFilter"
          Description =
              Some
                  "The `Filter` scalar type represents a filter on one or more fields of an object in an object list. The filter is represented by a JSON object where the fields are the complemented by specific suffixes to represent a query."
        //   CoerceInput = coerceObjectListFilterInput
          Decoder =
            (fun (value : Value) ->
                match coerceObjectListFilterInput value with
                | Some olf -> Ok olf
                | None ->
                    let message = sprintf "Failed to decode %A as an ObjectListFilter" value
                    Error (DecodeError.reason message))
          CoerceValue = coerceObjectListFilterValue }
