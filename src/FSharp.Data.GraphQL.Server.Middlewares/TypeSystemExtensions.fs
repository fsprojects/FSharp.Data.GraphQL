namespace FSharp.Data.GraphQL.Server.Middlewares

open FSharp.Data.GraphQL.Types

[<AutoOpen>]
module TypeSystemExtensions =
    type FieldDef<'Val> with
        member this.WithWeight(weight : float) =
            this.Metadata.Add(Constants.MetadataKeys.weight, weight); this
    type ISchema<'Root> with
        member this.WithWeightThreshold(threshold : float) =
            this.Metadata.Add(Constants.MetadataKeys.weightThreshold, threshold); this