module internal FSharp.Data.GraphQL.Server.Middlewares.ListField

open FSharp.Data.GraphQL.Types
open System.Reflection
open Microsoft.FSharp.Reflection
open FSharp.Data.GraphQL.Ast

let private sortInput = Define.Input("sort", ListOf String)
let private lastInput = Define.Input("last", Int)
let private firstInput = Define.Input("first", Int)
let private skipInput = Define.Input("skip", Int)
let private filterInput = Define.Input("filter", Obj)
let inputs = [ sortInput; lastInput; firstInput; skipInput; filterInput ]

let rec private getPropertyValue (name : string) (r : obj) =
    let getUnionValue field value valueType =
        let _, fields = FSharpValue.GetUnionFields(value, valueType)
        try
            fields.[0]
        with
        | _ -> failwith <| sprintf "Error: Union field %s does not have a union value!" field
    match r.GetType() with
    | t when FSharpType.IsUnion t ->
        let unionValue = getUnionValue name r t
        getPropertyValue name unionValue
    | t ->
        let flags =  BindingFlags.IgnoreCase ||| BindingFlags.Instance ||| BindingFlags.Public
        let propValue = t.GetProperty(name, flags).GetValue(r)
        if isNull propValue then failwith <| sprintf "Error: could not read value of field %s!" name
        try
             propValue :?> System.IComparable
        with
        | :? System.InvalidCastException -> failwith <| sprintf "Field %s is not comparable." name


let private getSortFuncs (fields : string seq) =
    fields
    |> Seq.map (fun field ->
        match field with
        | f when f.EndsWith("_asc") && f.Length > 4 ->
            let f = f.Substring(0, f.Length - 4)
            fun (res : 'Res seq) -> res |> Seq.sortBy (fun res -> getPropertyValue f res)
        | f when f.EndsWith("_desc") && f.Length > 5 ->
            let f = f.Substring(0, f.Length - 5)
            fun (res : 'Res seq) -> res |> Seq.sortByDescending (fun res -> getPropertyValue f res)
        | f -> fun (res : 'Res seq) -> res |> Seq.sortBy (fun res -> getPropertyValue f res))

let applySortFunc (fields : string list) (res : 'Res seq) =
    let mutable res = res
    getSortFuncs fields |> Seq.iter (fun f -> res <- f res)
    res

let [<Literal>] private FilterFailMsg = "Error: unexpected filter arguments!"

let private getFilterFuncs (fields : (string * Value) list) : ('Res seq -> 'Res seq) seq =
    let buildFunc f x =
        fun (res : 'Res seq) -> res |> Seq.filter (fun res -> getPropertyValue f res = x)
    fields
    |> Seq.map (fun (f, value) ->
        match value with
        | IntValue x -> buildFunc f x
        | FloatValue x -> buildFunc f x
        | BooleanValue x -> buildFunc f x
        | StringValue x -> buildFunc f x
        | EnumValue x -> buildFunc f x
        | _ -> failwith FilterFailMsg)

let applyFilterFunc (args : Value) (res : 'Res seq) =
    let mutable res = res
    match args with
    | ObjectValue fields -> fields |> Map.toList |> getFilterFuncs |> Seq.iter (fun f -> res <- f res)
    | _ -> failwith FilterFailMsg
    res