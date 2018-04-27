namespace FSharp.Data.GraphQL.Server.Middlewares

open FSharp.Data.GraphQL.Types

[<AutoOpen>]
module TypeSystemExtensions =
    type Define with
        static member OrderByInput = Define.Input("orderBy", ListOf String)
        static member LastItemsInput = Define.Input("last", Int)
        static member FirstItemsInput = Define.Input("first", Int)
        static member SkipItemsInput = Define.Input("skip", Int)
        static member ListInputFields = 
            [ Define.OrderByInput
              Define.LastItemsInput
              Define.FirstItemsInput
              Define.SkipItemsInput ]
        static member ListField(name : string, typedef : OutputDef<'Res>, description : string, resolve : ResolveFieldContext -> 'Val -> 'Res seq) : FieldDef<'Val> =
            let resolve' = 
                fun ctx v ->
                    let mutable res = resolve ctx v
                    ctx.Args
                    |> Map.iter (fun k v ->
                        match k.ToLower() with
                        | "skip" -> res <- res |> Seq.skip (v :?> int)
                        | "first" -> res <- res |> Seq.take (v :?> int)
                        | "last" -> res <- res |> Seq.rev |> Seq.take (v :?> int)
                        | "orderBy" -> () // TODO: make the logic for filtering
                        | _ -> ())
                    res
            Define.Field(name, ListOf typedef, description, Define.ListInputFields, resolve')

    type FieldDef<'Val> with
        member this.WithQueryWeight(weight : float) =
            this.Metadata.Add(MetadataKeys.QueryWeightMiddleware.QueryWeight, weight); this

    type Metadata with
        member this.WithQueryWeightThreshold(threshold : float) =
            this.Add(MetadataKeys.QueryWeightMiddleware.QueryWeightThreshold, threshold); this
        
        static member QueryWeightThreshold(threshold : float) =
            Metadata.Empty.WithQueryWeightThreshold(threshold)