namespace FSharp.Data.GraphQL.Server.Middlewares

open FSharp.Data.GraphQL.Types

type FieldFilter<'Val> =
    { FieldName : string
      Value : 'Val }

type ObjectListFilter =
    | And of ObjectListFilter * ObjectListFilter
    | Or of ObjectListFilter * ObjectListFilter
    | Not of ObjectListFilter
    | Equals of FieldFilter<System.IComparable>
    | GreaterThan of FieldFilter<System.IComparable>
    | LessThan of FieldFilter<System.IComparable>
    | StartsWith of FieldFilter<string>
    | EndsWith of FieldFilter<string>
    | Contains of FieldFilter<string>
    | Field of FieldFilter<ObjectListFilter>
    override this.ToString() =
        let print (x : obj) =
            match x with
            | :? string as x -> printfn "\"%s\"" x
            | x -> printfn "%A" x
        match this with
        | And (f1, f2) -> sprintf "(%A) and (%A)" f1 f2
        | Or (f1, f2) -> sprintf "(%A) or (%A)" f1 f2
        | Not f -> sprintf "not (%A)" f
        | Equals f -> sprintf "%s = %A" f.FieldName (print f.Value)
        | GreaterThan f -> sprintf "%s > %A" f.FieldName (print f.Value)
        | LessThan f -> sprintf "%s < %A" f.FieldName (print f.Value)
        | StartsWith f -> sprintf "%s starts with \"%s\"" f.FieldName f.Value
        | EndsWith f -> sprintf "%s ends with \"%s\"" f.FieldName f.Value
        | Contains f -> sprintf "%s contains \"%s\"" f.FieldName f.Value
        | Field f -> sprintf "%s.(%A)" f.FieldName f.Value

module ObjectListFilter =
    module internal Helpers =
        let rec getFields (def : OutputDef) =
            let mapObj (x : ObjectDef) =
                x.Name, (x.Fields |> Map.toSeq |> Seq.map (snd >> (fun x -> x.TypeDef)) |> List.ofSeq)
            match def with 
            | :? NullableDef as x ->
                match x.OfType with
                | :? ObjectDef as x -> mapObj x |> Seq.singleton |> Map.ofSeq
                | :? UnionDef as x -> x.Options |> Seq.map mapObj |> Map.ofSeq
                | _ -> Map.empty
            | :? ObjectDef as x -> mapObj x |> Seq.singleton |> Map.ofSeq
            | :? UnionDef as x -> x.Options |> Seq.map mapObj |> Map.ofSeq
            | _ -> Map.empty

[<AutoOpen>]
module ObjectListFilterOperators =
    let ( &&& ) x y = And (x, y)
    let ( ||| ) x y = Or (x, y)
    let ( === ) fname value = Equals { FieldName = fname; Value = value }
    let ( ==> ) fname value = GreaterThan { FieldName = fname; Value = value }
    let ( <== ) fname value = LessThan { FieldName = fname; Value = value }
    let ( =@@ ) fname value = StartsWith { FieldName = fname; Value = value }
    let ( @@= ) fname value = EndsWith { FieldName = fname; Value = value }
    let ( @=@ ) fname value = Contains { FieldName = fname; Value = value }
    let ( --> ) fname filter = Field { FieldName = fname; Value = filter }
    let ( !!! ) filter = Not filter