/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Client.Serialization

open System
open FSharp.Data
open Microsoft.FSharp.Reflection
open System.Reflection

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

let fromJson<'T> (json : string) : 'T =
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
