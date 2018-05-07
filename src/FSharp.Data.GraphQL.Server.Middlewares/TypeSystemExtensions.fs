namespace FSharp.Data.GraphQL.Server.Middlewares

open System
open FSharp.Data.GraphQL.Types

[<AutoOpen>]
module TypeSystemExtensions =
    type Define with
        static member ListField(name : string, typedef : #OutputDef<'Res>, description : string, 
                                resolve : ResolveFieldContext -> 'Val -> 'Res seq) : FieldDef<'Val> =
            Define.Field(name, ListOf typedef, description, [ Define.Input("filter", ObjectListFilter) ], resolve)

    type ObjectDef with
        member this.WithFields(fields : FieldDef list) =
            let exists fname = fields |> List.exists (fun x -> x.Name = fname)
            { new ObjectDef with
                member __.Name = this.Name
                member __.Description = this.Description
                member __.Fields = 
                    this.Fields
                    |> Map.toSeq
                    |> Seq.filter (fun (n, _) -> not (exists n))
                    |> Seq.append (fields |> Seq.map (fun x -> x.Name, x))
                    |> Map.ofSeq
                member __.Implements = this.Implements
                member __.IsTypeOf = this.IsTypeOf
              interface TypeDef with
                member __.MakeList() = this.MakeList()
                member __.MakeNullable() = this.MakeNullable()
                member __.Type = (this :> TypeDef).Type
              interface NamedDef with
                member __.Name = (this :> NamedDef).Name }

    type FieldDef with
        member this.WithArgs(args : InputFieldDef list) =
            let exists aname = args |> List.exists (fun x -> x.Name = aname)
            { new FieldDef with
                member __.Name = this.Name
                member __.Description = this.Description
                member __.DeprecationReason = this.DeprecationReason
                member __.TypeDef = this.TypeDef
                member __.Args = 
                    this.Args
                    |> Array.filter (fun x -> not (exists x.Name))
                    |> Array.append (args |> Array.ofList)
                member __.Metadata = this.Metadata
                member __.Resolve = this.Resolve
              interface IEquatable<FieldDef> with
                member __.Equals(other) = this.Equals(other) }

    type FieldDef<'Val> with
        member this.WithQueryWeight(weight : float) =
            this.Metadata.Add(MetadataKeys.QueryWeightMiddleware.QueryWeight, weight); this

    type Metadata with
        member this.WithQueryWeightThreshold(threshold : float) =
            this.Add(MetadataKeys.QueryWeightMiddleware.QueryWeightThreshold, threshold); this
        
        static member QueryWeightThreshold(threshold : float) =
            Metadata.Empty.WithQueryWeightThreshold(threshold)