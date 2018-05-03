namespace FSharp.Data.GraphQL.Server.Middlewares

open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Server.Middlewares.ObjectListFilter.Helpers

[<AutoOpen>]
module TypeSystemExtensions =
    type Define with
        static member FilterInputs (typedef : #OutputDef) =
            let typedef = typedef :> OutputDef
            match typedef with
            | :? ObjectDef | :? NullableDef | :? UnionDef -> 
                let fields = getFields typedef
                [ Define.Input("filter", ObjectListFilter fields) ]
            | _ -> []
        static member ListField(name : string, typedef : #OutputDef<'Res>, description : string, 
                                resolve : ResolveFieldContext -> 'Val -> 'Res seq) : FieldDef<'Val> =                          
            Define.Field(name, ListOf typedef, description, Define.FilterInputs(typedef), resolve)

    type FieldDef<'Val> with
        member this.WithQueryWeight(weight : float) =
            this.Metadata.Add(MetadataKeys.QueryWeightMiddleware.QueryWeight, weight); this

    type Metadata with
        member this.WithQueryWeightThreshold(threshold : float) =
            this.Add(MetadataKeys.QueryWeightMiddleware.QueryWeightThreshold, threshold); this
        
        static member QueryWeightThreshold(threshold : float) =
            Metadata.Empty.WithQueryWeightThreshold(threshold)