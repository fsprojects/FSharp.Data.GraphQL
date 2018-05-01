namespace FSharp.Data.GraphQL.Server.Middlewares

open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Ast

[<AutoOpen>]
module TypeSystemExtensions =
    type Define with
        static member ListField(name : string, typedef : #OutputDef<'Res>, description : string, 
                                resolve : ResolveFieldContext -> 'Val -> 'Res seq,
                                ?compileResolve : bool) : FieldDef<'Val> =
            let resolve' = 
                fun ctx v ->
                    let mutable res = resolve ctx v
                    ctx.Args
                    |> Map.iter (fun k v ->
                        match k, v with
                        | "skip", (:? int as index) -> res <- res |> Seq.skip index
                        | "first", (:? int as index) -> res <- res |> Seq.take index
                        | "last", (:? int as index) -> res <- res |> Seq.rev |> Seq.take index
                        | "sort", (:? list<string> as fields) -> res <- res |> ListField.applySortFunc fields
                        | "filter", (:? Value as args) -> res <- res |> ListField.applyFilterFunc args
                        | _ -> ())
                    res
            match defaultArg compileResolve true with
            | true -> Define.Field(name, ListOf typedef, description, ListField.inputs, resolve')
            | false -> Define.Field(name, ListOf typedef, description, ListField.inputs, resolve)

    type FieldDef<'Val> with
        member this.WithQueryWeight(weight : float) =
            this.Metadata.Add(MetadataKeys.QueryWeightMiddleware.QueryWeight, weight); this

    type Metadata with
        member this.WithQueryWeightThreshold(threshold : float) =
            this.Add(MetadataKeys.QueryWeightMiddleware.QueryWeightThreshold, threshold); this
        
        static member QueryWeightThreshold(threshold : float) =
            Metadata.Empty.WithQueryWeightThreshold(threshold)