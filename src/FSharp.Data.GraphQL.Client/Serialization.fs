/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Client.Serialization

open System
open FSharp.Data
open FSharp.Data.JsonExtensions
open Microsoft.FSharp.Reflection
open System.Reflection
open System.Collections
open System.Collections.Generic

let private isOption (t : Type) = 
    t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<_ option>

let private (|Option|_|) t =
    if isOption t then Some (Option (t.GetGenericArguments().[0]))
    else None

let private (|Array|_|) (t : Type) =
    if t.IsArray then Some (Array (t.GetElementType()))
    else None

let private (|List|_|) (t : Type) =
    if t.IsGenericTypeDefinition
    then
        let gtype = t.GetGenericTypeDefinition()
        if gtype = typedefof<_ list>
        then Some (List (gtype.GetGenericArguments().[0]))
        else None
    else None

let private (|Seq|_|) (t : Type) =
    if t.IsGenericTypeDefinition
    then
        let gtype = t.GetGenericTypeDefinition()
        if gtype = typedefof<seq<_>>
        then Some (Seq (gtype.GetGenericArguments().[0]))
        else None
    else None

let private makeOption t values =
    let otype = typedefof<_ option>
    let cases = FSharpType.GetUnionCases(otype.MakeGenericType([|t|]))
    FSharpValue.MakeUnion(cases.[0], values)

let private downcastNone<'T> t : 'T =
    match t with
    | Option t -> downcast (makeOption t [||])
    | _ -> failwithf "Error parsing JSON value: %O is not an option value." t

let private downcastSome<'T> t value : 'T =
    match t with
    | Option t -> downcast (makeOption t [|value|])
    | _ -> failwithf "Error parsing JSON value: %O is not an option value." t

let private downcastType (t : Type) x : 'T = 
    match t with
    | Option t -> downcast (makeOption t [|x|])
    | _ -> downcast (Convert.ChangeType(x, t))

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
    |> Array.exists (fun nt ->
        match t with
        | Option t -> nt = t
        | _ -> nt = t)

let private isStringType (t : Type) =
    match t with
    | Option t -> t = typeof<string>
    | _ -> t = typeof<string>

let private isBooleanType (t : Type) =
    match t with
    | Option t -> t = typeof<bool>
    | _ -> t = typeof<bool>

let private downcastNumber (t : Type) n =
    match t with
    | t when isNumericType t -> downcastType t n
    | _ -> failwithf "Error parsing JSON value: %O is not a numeric type." t

let private downcastString (t : Type) s =
    match t with
    | t when isStringType t -> downcastType t s
    | _ -> failwithf "Error parsing JSON value: %O is not a string type." t

let private downcastBoolean (t : Type) b =
    match t with
    | t when isBooleanType t -> downcastType t b
    | _ -> failwithf "Error parsing JSON value: %O is not a boolean type." t

let private getPropertyValue (t : Type) (converter : Type -> JsonValue -> 'T) (propName, propValue) =
    let propType = t.GetProperty(propName, BindingFlags.Public ||| BindingFlags.Instance).PropertyType
    converter propType propValue

let private getArrayValue (t : Type) (converter : Type -> JsonValue -> 'TItem) (items : JsonValue []) : 'TArray =
    match t with
    | Array titem -> items |> Array.map (converter titem) |> downcastType t
    | List titem -> items |> Array.map (converter titem) |> List.ofArray |> downcastType t
    | Seq titem -> items |> Array.map (converter titem) |> Seq.ofArray |> downcastType t
    | _ -> failwithf "Error parsing JSON value: %O is not an array type." t

let deserializeRecord<'T> (json : string) : 'T =
    let t = typeof<'T>
    let rec convert t parsed : 'T =
        match parsed with
        | JsonValue.Null -> downcastNone t
        | JsonValue.String s -> downcastString t s
        | JsonValue.Number n -> downcastNumber t n
        | JsonValue.Float n -> downcastNumber t n
        | JsonValue.Record props -> 
            let vals = props |> Array.map (getPropertyValue t convert >> box)
            Activator.CreateInstance(t, vals) |> downcastType t
        | JsonValue.Array items -> items |> getArrayValue t convert
        | JsonValue.Boolean b -> downcastBoolean t b
    JsonValue.Parse(json) |> convert t

let deserializeDict (json : string) : IDictionary<string, obj> =
    let rec convert parsed : obj =
        match parsed with
        | JsonValue.Null -> null
        | JsonValue.String s -> upcast s
        | JsonValue.Number n -> upcast n
        | JsonValue.Float n -> upcast n
        | JsonValue.Record props -> upcast (props |> Array.map (fun (n, v) -> (n, (convert v))) |> dict)
        | JsonValue.Array items -> upcast (items |> Array.map convert)
        | JsonValue.Boolean b -> upcast b
    match JsonValue.Parse(json) with
    | JsonValue.Record props -> props |> Array.map (fun (n, v) -> (n, (convert v))) |> dict
    | _ -> failwith "The input JSON could not be deserialized to a record type."

let serialize (x : obj) =
    let rec helper (x : obj) : JsonValue =
        match x with
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
        | :? DateTime as x when x.Date = x -> JsonValue.String (x.ToString("yyyy-MM-dd"))
        | :? DateTime as x -> JsonValue.String (x.ToString("O"))
        | :? IEnumerable as x -> 
            Seq.cast<obj> x 
            |> Array.ofSeq 
            |> Array.map helper
            |> JsonValue.Array
        | :? bool as x -> JsonValue.Boolean x
        | null -> JsonValue.Null
        | _ ->
            let xtype = x.GetType()
            let xprops = xtype.GetProperties(BindingFlags.Public ||| BindingFlags.Instance)
            let items = xprops |> Array.map (fun p -> (p.Name, p.GetValue(x) |> helper))
            JsonValue.Record items
    let value = helper x
    value.ToString()