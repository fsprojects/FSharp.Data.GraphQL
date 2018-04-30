namespace FSharp.Data.GraphQL.Server.Middlewares

open FSharp.Data.GraphQL.Types
open System.Reflection
open Microsoft.FSharp.Reflection

[<AutoOpen>]
module TypeSystemExtensions =
    type Define with
        static member SortInput = Define.Input("sort", ListOf String)
        static member LastInput = Define.Input("last", Int)
        static member FirstInput = Define.Input("first", Int)
        static member SkipInput = Define.Input("skip", Int)
        static member WhereInput = Define.Input("where", Obj)
        static member ListInputFields = 
            [ Define.SortInput
              Define.LastInput
              Define.FirstInput
              Define.SkipInput
              Define.WhereInput ]
        static member ListField(name : string, typedef : #OutputDef<'Res>, description : string, 
                                resolve : ResolveFieldContext -> 'Val -> 'Res seq,
                                ?compileResolve : bool) : FieldDef<'Val> =
            let getOrderByFuncs (fields : string seq) =
                let getUnionValue value =
                    let _, fields = FSharpValue.GetUnionFields(value, value.GetType())
                    fields.[0]
                let rec getPropertyValue (name : string) (r : obj) =
                    let t = r.GetType()
                    match t with
                    | t when FSharpType.IsUnion t ->
                        let unionValue = getUnionValue r 
                        getPropertyValue name unionValue
                    | t ->
                        let flags =  BindingFlags.IgnoreCase ||| BindingFlags.Instance ||| BindingFlags.Public
                        t.GetProperty(name, flags).GetValue(r) :?> System.IComparable
                fields
                |> Seq.map (fun field ->
                    match field with
                    | f when f.EndsWith("_ASC") && f.Length > 4 ->
                        let f = f.Substring(0, f.Length - 4)
                        fun (res : 'Res seq) -> res |> Seq.sortBy (fun res -> getPropertyValue f res)
                    | f when f.EndsWith("_DESC") && f.Length > 5 ->
                        let f = f.Substring(0, f.Length - 5)
                        fun (res : 'Res seq) -> res |> Seq.sortByDescending (fun res -> getPropertyValue f res)
                    | f -> fun (res : 'Res seq) -> res |> Seq.sortBy (fun res -> getPropertyValue f res))
            let resolve' = 
                fun ctx v ->
                    let mutable res = resolve ctx v
                    ctx.Args
                    |> Map.iter (fun k v ->
                        match k, v with
                        | "skip", (:? int as index) -> res <- res |> Seq.skip index
                        | "first", (:? int as index) -> res <- res |> Seq.take index
                        | "last", (:? int as index) -> res <- res |> Seq.rev |> Seq.take index
                        | "sort", (:? seq<string> as fields) -> getOrderByFuncs fields |> Seq.iter (fun f -> res <- f res)
                        | _ -> ())
                    res
            match defaultArg compileResolve true with
            | true -> Define.Field(name, ListOf typedef, description, Define.ListInputFields, resolve')
            | false -> Define.Field(name, ListOf typedef, description, Define.ListInputFields, resolve)

    type FieldDef<'Val> with
        member this.WithQueryWeight(weight : float) =
            this.Metadata.Add(MetadataKeys.QueryWeightMiddleware.QueryWeight, weight); this

    type Metadata with
        member this.WithQueryWeightThreshold(threshold : float) =
            this.Add(MetadataKeys.QueryWeightMiddleware.QueryWeightThreshold, threshold); this
        
        static member QueryWeightThreshold(threshold : float) =
            Metadata.Empty.WithQueryWeightThreshold(threshold)