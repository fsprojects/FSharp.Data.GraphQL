namespace FSharp.Data.GraphQL.Server.Middlewares

open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Server.Middlewares.ObjectListFieldHelpers

[<AutoOpen>]
module TypeSystemExtensions =
    type Define with
        static member ListField(name : string, typedef : #OutputDef<'Res>, description : string, 
                                resolve : ResolveFieldContext -> 'Val -> 'Res seq) : FieldDef<'Val> =                          
            let itemdef = typedef :> OutputDef                            
            let inputArgs = 
                match itemdef with
                | :? ObjectDef ->
                    let fields = getFields itemdef
                    [ Define.Input("filter", ObjectListFilter fields) ]
                | _ -> []
            Define.Field(name, ListOf typedef, description, inputArgs, resolve)

    type FieldDef<'Val> with
        member this.WithQueryWeight(weight : float) =
            this.Metadata.Add(MetadataKeys.QueryWeightMiddleware.QueryWeight, weight); this

    type Metadata with
        member this.WithQueryWeightThreshold(threshold : float) =
            this.Add(MetadataKeys.QueryWeightMiddleware.QueryWeightThreshold, threshold); this
        
        static member QueryWeightThreshold(threshold : float) =
            Metadata.Empty.WithQueryWeightThreshold(threshold)