/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Client

open System
open Microsoft.FSharp.Reflection
open System.Reflection
open System.Collections
open System.Collections.Generic
open System.Globalization
open FSharp.Data.GraphQL

module Serialization =
    let private isoDateFormat = "yyyy-MM-dd" 
    let private isoDateTimeFormat = "O"
    let isoDateTimeFormats = [|isoDateTimeFormat; isoDateFormat|]

    let private isOption (t : Type) = 
        t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<_ option>

    let private isMap (t : Type) =
       t = typeof<Map<string, obj>>

    let private (|Option|_|) t =
        if isOption t then Some (Option (t.GetGenericArguments().[0]))
        else None

    let private (|Array|_|) (t : Type) =
        if t.IsArray then Some (Array (t.GetElementType()))
        else None

    let private (|List|_|) (t : Type) =
        if t.IsGenericType
        then
            let gtype = t.GetGenericTypeDefinition()
            if gtype = typedefof<_ list>
            then Some (List (t.GetGenericArguments().[0]))
            else None
        else None

    let private (|Seq|_|) (t : Type) =
        if t.IsGenericType
        then
            let gtype = t.GetGenericTypeDefinition()
            if gtype = typedefof<seq<_>>
            then Some (Seq (t.GetGenericArguments().[0]))
            else None
        else None

    let private (|Enum|_|) (t : Type) =
        match t with
        | (Option t | t) when t.IsEnum -> Some (Enum t)
        | _ -> None

    let private makeOption t (value : obj) =
        let otype = typedefof<_ option>
        let cases = FSharpType.GetUnionCases(otype.MakeGenericType([|t|]))
        match value with
        | null -> FSharpValue.MakeUnion(cases.[0], [||])
        | _ -> FSharpValue.MakeUnion(cases.[1], [|value|])

    let private downcastNone<'T> t =
        match t with
        | Option t -> downcast (makeOption t null)
        | _ -> failwithf "Error parsing JSON value: %O is not an option value." t

    let private downcastType (t : Type) x = 
        match t with
        | Option t -> downcast (makeOption t (Convert.ChangeType(x, t)))
        | _ -> downcast (Convert.ChangeType(x, t))

    let private isType (expected : Type) (t : Type) =
        match t with
        | Option t -> t = expected
        | _ -> t = expected

    let private isNumericType (t : Type) =
        [| typeof<decimal>
           typeof<double>
           typeof<single>
           typeof<uint64>
           typeof<int64>
           typeof<uint32>
           typeof<int>
           typeof<uint16>
           typeof<int16>
           typeof<byte>
           typeof<sbyte> |]
        |> Array.exists (fun expected -> isType expected t)

    let private isStringType = isType typeof<string>
    let private isDateTimeType = isType typeof<DateTime>
    let private isDateTimeOffsetType = isType typeof<DateTimeOffset>
    let private isGuidType = isType typeof<Guid>
    let private isBooleanType = isType typeof<bool>

    let private downcastNumber (t : Type) n =
        match t with
        | t when isNumericType t -> downcastType t n
        | _ -> failwithf "Error parsing JSON value: %O is not a numeric type." t

    let private downcastString (t : Type) (s : string) =
        match t with
        | t when isStringType t -> downcastType t s
        | t when isDateTimeType t ->
            match DateTime.TryParseExact(s, isoDateTimeFormats, CultureInfo.InvariantCulture, DateTimeStyles.None) with
            | (true, d) -> downcastType t d
            | _ -> failwithf "Error parsing JSON value: %O is a date type, but parsing of value \"%s\" failed." t s
        | t when isDateTimeOffsetType t ->
            match DateTimeOffset.TryParseExact(s, isoDateTimeFormats, CultureInfo.InvariantCulture, DateTimeStyles.None) with
            | (true, d) -> downcastType t d
            | _ -> failwithf "Error parsing JSON value: %O is a date time offset type, but parsing of value \"%s\" failed." t s
        | t when isGuidType t ->
            match Guid.TryParse(s) with
            | (true, g) -> downcastType t g
            | _ -> failwithf "Error parsing JSON value: %O is a Guid type, but parsing of value \"%s\" failed." t s
        | Enum et ->
            try Enum.Parse(et, s) |> downcastType t
            with _ -> failwithf "Error parsing JSON value: %O is a Enum type, but parsing of value \"%s\" failed." t s
        | _ -> failwithf "Error parsing JSON value: %O is not a string type." t

    let private downcastBoolean (t : Type) b =
        match t with
        | t when isBooleanType t -> downcastType t b
        | _ -> failwithf "Error parsing JSON value: %O is not a boolean type." t

    let rec private getArrayValue (t : Type) (converter : Type -> JsonValue -> obj) (items : JsonValue []) =
        let castArray itemType (items : obj []) : obj =
            let arr = Array.CreateInstance(itemType, items.Length)
            items |> Array.iteri (fun ix i -> arr.SetValue(i, ix))
            upcast arr
        let castList itemType (items : obj list) =
            let tlist = typedefof<_ list>.MakeGenericType([|itemType|])
            let empty = 
                let uc = 
                    Reflection.FSharpType.GetUnionCases(tlist) 
                    |> Seq.filter (fun uc -> uc.Name = "Empty") 
                    |> Seq.exactlyOne
                Reflection.FSharpValue.MakeUnion(uc, [||])
            let rec helper items =
                match items with
                | [] -> empty
                | [x] -> Activator.CreateInstance(tlist, [|x; empty|])
                | x :: xs -> Activator.CreateInstance(tlist, [|x; helper xs|])
            helper items
        Tracer.runAndMeasureExecutionTime "Converted Array JsonValue to CLR array" (fun _ ->
            match t with
            | Option t -> getArrayValue t converter items |> makeOption t
            | Array itype | Seq itype -> items |> Array.map (converter itype) |> castArray itype
            | List itype -> items |> Array.map (converter itype) |> Array.toList |> castList itype
            | _ -> failwithf "Error parsing JSON value: %O is not an array type." t)

    let rec private convert t parsed : obj =
        Tracer.runAndMeasureExecutionTime (sprintf "Converted JsonValue to %O type." t) (fun _ ->
            match parsed with
            | JsonValue.Null -> downcastNone t
            | JsonValue.String s -> downcastString t s
            | JsonValue.Number n -> downcastNumber t n
            | JsonValue.Float n -> downcastNumber t n
            | JsonValue.Record jprops ->
                let jprops = 
                    jprops 
                    |> Array.map (fun (n, v) -> n.ToLowerInvariant(), v) 
                    |> Map.ofSeq
                let tprops t =
                    FSharpType.GetRecordFields(t, true)
                    |> Array.map (fun p -> p.Name.ToLowerInvariant(), p.PropertyType)
                let vals t = 
                    tprops t
                    |> Array.map (fun (n, t) ->
                        match Map.tryFind n jprops with
                        | Some p -> n, convert t p
                        | None -> n, makeOption t null)
                let rcrd =
                    let t = match t with Option t -> t | _ -> t
                    let vals = vals t
                    if isMap t
                    then Map.ofArray vals |> box
                    else FSharpValue.MakeRecord(t, Array.map snd vals, true)
                downcastType t rcrd
            | JsonValue.Array items -> items |> getArrayValue t convert
            | JsonValue.Boolean b -> downcastBoolean t b)

    let deserializeRecord<'T> (json : string) : 'T =
        let t = typeof<'T>
        Tracer.runAndMeasureExecutionTime (sprintf "Deserialized JSON string to record type %s." (t.ToString())) (fun _ ->
        downcast (JsonValue.Parse(json) |> convert t))

    let deserializeMap values =
        let rec helper (values : (string * JsonValue) []) =
            values
            |> Array.map (fun (name, value) ->
                match value with
                | JsonValue.Record fields -> name, (fields |> helper |> Map.ofArray |> box)
                | JsonValue.Null -> name, null
                | JsonValue.String s -> name, box s
                | JsonValue.Number n -> name, box (int n)
                | JsonValue.Float f -> name, box f
                | JsonValue.Array items -> name, (items |> Array.map (fun item -> null, item) |> helper |> Array.map snd |> box)
                | JsonValue.Boolean b -> name, box b)
        Tracer.runAndMeasureExecutionTime "Deserialized JSON Record into FSharp Map" (fun _ ->
            helper values |> Map.ofArray)
        
    let (|EnumerableValue|_|) (x : obj) =
        match x with
        | :? IEnumerable as x -> Some (EnumerableValue (Seq.cast<obj> x |> Array.ofSeq))
        | _ -> None

    let (|OptionValue|_|) (x : obj) =
        let xtype = x.GetType()
        if isOption xtype
        then
            match FSharpValue.GetUnionFields(x, xtype) with
            | (_, [|value|]) -> Some (OptionValue Some value)
            | _ -> Some (OptionValue None)
        else None

    let (|EnumValue|_|) (x : obj) =
        let xtype = x.GetType()
        if xtype.IsEnum
        then Some (x.ToString())
        else None

    let rec toJsonValue (x : obj) : JsonValue =
        let t = x.GetType()
        Tracer.runAndMeasureExecutionTime (sprintf "Converted object type %s to JsonValue" (t.ToString())) (fun _ ->
            match x with
            | null -> JsonValue.Null
            | OptionValue None -> JsonValue.Null
            | :? byte as x -> JsonValue.Number (decimal x)
            | :? sbyte as x -> JsonValue.Number (decimal x)
            | :? uint16 as x -> JsonValue.Number (decimal x)
            | :? int16 as x -> JsonValue.Number (decimal x)
            | :? int as x -> JsonValue.Number (decimal x)
            | :? uint32 as x -> JsonValue.Number (decimal x)
            | :? int64 as x -> JsonValue.Number (decimal x)
            | :? uint64 as x -> JsonValue.Number (decimal x)
            | :? single as x -> JsonValue.Float (float x)
            | :? double as x -> JsonValue.Float x
            | :? decimal as x -> JsonValue.Float (float x)
            | :? string as x -> JsonValue.String x
            | :? Guid as x -> JsonValue.String (x.ToString())
            | :? DateTime as x when x.Date = x -> JsonValue.String (x.ToString(isoDateFormat))
            | :? DateTime as x -> JsonValue.String (x.ToString(isoDateTimeFormat))
            | :? DateTimeOffset as x -> JsonValue.String (x.ToString(isoDateTimeFormat))
            | :? bool as x -> JsonValue.Boolean x
            | :? IDictionary<string, obj> as items ->
                items
                |> Seq.map (fun (KeyValue (k, v)) -> k, toJsonValue v)
                |> Seq.toArray
                |> JsonValue.Record
            | EnumerableValue items -> 
                items
                |> Array.map toJsonValue
                |> JsonValue.Array
            | OptionValue (Some x) -> toJsonValue x
            | EnumValue x -> JsonValue.String x
            | _ ->
                let xtype = t
                let xprops = xtype.GetProperties(BindingFlags.Public ||| BindingFlags.Instance)
                let items = xprops |> Array.map (fun p -> (p.Name.FirstCharLower(), p.GetValue(x) |> toJsonValue))
                JsonValue.Record items)

    let serializeRecord (x : obj) =
        Tracer.runAndMeasureExecutionTime (sprintf "Serialized object type %s to a JSON string" (x.GetType().ToString())) (fun _ ->
            (toJsonValue x).ToString())

    let deserializeSchema (json : string) =
        Tracer.runAndMeasureExecutionTime "Deserialized schema" (fun _ ->
            let result = deserializeRecord<GraphQLReply<IntrospectionResult>> json
            match result.Errors with
            | None -> result.Data.__schema
            | Some errors -> String.concat "\n" errors |> failwithf "%s")