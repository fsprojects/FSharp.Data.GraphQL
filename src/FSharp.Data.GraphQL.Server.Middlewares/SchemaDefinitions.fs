namespace FSharp.Data.GraphQL.Server.Middlewares

open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Server.Middlewares.ObjectListFilter.Helpers

[<AutoOpen>]
module SchemaDefinitions =
    let rec private coerceObjectListFilterInput (validFields : Map<string, OutputDef list>) x = 
        let (|EndsWith|StartsWith|GreaterThan|LessThan|Contains|Equals|) (s : string) =
            let s = s.ToLowerInvariant()
            match s with
            | s when s.EndsWith("_ends_with") && s.Length > "_ends_with".Length -> EndsWith (s.Substring(0, "_ends_with".Length))
            | s when s.EndsWith("_starts_with") && s.Length > "_starts_with".Length -> StartsWith (s.Substring(0, "_starts_with".Length))
            | s when s.EndsWith("_gt") && s.Length > "_gt".Length -> GreaterThan (s.Substring(0, "_gt".Length))
            | s when s.EndsWith("_greater_than") && s.Length > "_greater_than".Length -> GreaterThan (s.Substring(0, "_greater_than".Length))
            | s when s.EndsWith("_lt") && s.Length > "_lt".Length -> LessThan (s.Substring(0, "_lt".Length))
            | s when s.EndsWith("_less_than") && s.Length > "_less_than".Length -> LessThan (s.Substring(0, "_less_than".Length))
            | s when s.EndsWith("_contains") && s.Length > "_contains".Length -> Contains (s.Substring(0, "_contains".Length))
            | s -> Equals s
        let (|EquatableValue|Other|) v =
            match v with
            | IntValue v -> EquatableValue (v :> System.IComparable)
            | FloatValue v -> EquatableValue (v :> System.IComparable)
            | BooleanValue v -> EquatableValue (v :> System.IComparable)
            | StringValue v -> EquatableValue (v :> System.IComparable)
            | EnumValue v -> EquatableValue (v :> System.IComparable)
            | v -> Other v // TODO: Should ListValue, ObjectValue and Variable be treated as equatable values?
        let (|ComparableValue|Other|) v =
            match v with
            | IntValue v -> ComparableValue (v :> System.IComparable)
            | FloatValue v -> ComparableValue (v :> System.IComparable)
            | BooleanValue v -> ComparableValue (v :> System.IComparable)
            | StringValue v -> ComparableValue (v :> System.IComparable)
            | v -> Other v // TODO: Should EnumValue, ListValue, ObjectValue and Variable be treated as comparable values?
        let rec mapFilter validFields (name : string, value : Value) =
            let buildAnd x =
                let rec build acc x =
                    match x with
                    | [] -> acc
                    | x :: xs -> match acc with Some acc -> build (Some (acc &&& x)) xs | None -> build (Some x) xs
                build None x
            let buildOr x =
                let rec build acc x =
                    match x with
                    | [] -> acc
                    | x :: xs -> match acc with Some acc -> build (Some (acc ||| x)) xs | None -> build (Some x) xs
                build None x
            let mapFilters fields = fields |> List.map (coerceObjectListFilterInput validFields) |> List.choose id
            match name, value with
            | Equals "and", ListValue fields -> fields |> mapFilters |> buildAnd
            | Equals "or", ListValue fields -> fields |> mapFilters |> buildOr
            | Equals "not", ObjectValue value -> match mapInput validFields value with Some filter -> Some !!!filter | None -> None
            | EndsWith fname, StringValue value -> if validFields.ContainsKey fname then Some (fname @@= value) else None
            | StartsWith fname, StringValue value -> if validFields.ContainsKey fname then Some (fname =@@ value) else None
            | Contains fname, StringValue value -> if validFields.ContainsKey fname then Some (fname @=@ value) else None
            | Equals fname, ObjectValue value -> 
                match validFields.TryFind fname with
                | Some fields -> 
                    fields 
                    |> List.map (fun field ->
                        match mapInput (getFields field) value with 
                        | Some filter -> Some (fname --> filter) 
                        | None -> None)
                    |> List.choose id
                    |> List.tryHead
                | _ -> None
            | Equals fname, EquatableValue value -> if validFields.ContainsKey fname then Some (fname === value) else None
            | GreaterThan fname, ComparableValue value -> if validFields.ContainsKey fname then Some (fname ==> value) else None
            | LessThan fname, ComparableValue value -> if validFields.ContainsKey fname then Some (fname <== value) else None
            | _ -> None
        and mapInput validFields value =
            match value |> Map.toList with
            | [ field ] -> mapFilter validFields field
            | _ -> None
        match x with
        | ObjectValue x -> mapInput validFields x
        | _ -> None
    
    let private coerceObjectListFilterValue (x : obj) : ObjectListFilter option =
        match x with
        | :? ObjectListFilter as x -> Some x
        | _ -> None

    let ObjectListFilter (validFields : Map<string, OutputDef list>) : ScalarDefinition<ObjectListFilter> =
        { Name = "ObjectListFilter"
          Description = 
              Some 
                  "The `Filter` scalar type represents a filter on one or more fields of an object in an object list. The filter is represented by a JSON object where the fields are the complemented by specific suffixes to represent a query."
          CoerceInput = coerceObjectListFilterInput validFields
          CoerceValue = coerceObjectListFilterValue }