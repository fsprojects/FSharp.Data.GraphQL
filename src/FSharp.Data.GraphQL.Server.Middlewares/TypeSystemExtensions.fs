namespace FSharp.Data.GraphQL.Server.Middlewares

open System
open FSharp.Data.GraphQL.Types
open Microsoft.FSharp.Quotations
open FSharp.Data.GraphQL.Types

[<AutoOpen>]
module TypeSystemExtensions =
    type Define with
        static member ObjectFilterInput =
            Define.Input("filter", ObjectListFilter)

        static member ListField(name : string, typedef : #OutputDef<'Res>, description : string, 
                                resolve : ResolveFieldContext -> 'Val -> 'Res seq) : FieldDef<'Val> =
            Define.Field(name, ListOf typedef, description, [ Define.ObjectFilterInput ], resolve)

    type ObjectDef<'Val> with
        member this.WithFields(fields : FieldDef<'Val> seq) =
            let exists fname = fields |> Seq.exists (fun x -> x.Name = fname)
            let fields = 
                this.Fields
                |> Map.toSeq
                |> Seq.filter (fun (n, _) -> not (exists n))
                |> Seq.append (fields |> Seq.map (fun x -> x.Name, x))
                |> Map.ofSeq
            { new ObjectDef<'Val> with
                member __.Fields = fields
              interface ObjectDef with
                member __.Fields = fields |> Map.map (fun _ f -> upcast f)
                member __.Name = this.Name
                member __.Description = this.Description
                member __.Implements = this.Implements
                member __.IsTypeOf = this.IsTypeOf
              interface TypeDef with
                member __.MakeList() = this.MakeList()
                member __.MakeNullable() = this.MakeNullable()
                member __.Type = (this :> TypeDef).Type
              interface NamedDef with
                member __.Name = (this :> NamedDef).Name }

    type FieldDef<'Val> with
        member this.WithQueryWeight(weight : float) =
            this.Metadata.Add(MetadataKeys.QueryWeightMiddleware.QueryWeight, weight); this

        member this.WithArgs(args : InputFieldDef seq) =
            let exists aname = args |> Seq.exists (fun x -> x.Name = aname)
            { new FieldDef<'Val> with
                member __.Name = this.Name
                member __.Description = this.Description
                member __.DeprecationReason = this.DeprecationReason
                member __.TypeDef = this.TypeDef
                member __.Args = 
                    this.Args
                    |> Array.filter (fun x -> not (exists x.Name))
                    |> Array.append (args |> Array.ofSeq)
                member __.Metadata = this.Metadata
                member __.Resolve = this.Resolve
              interface IEquatable<FieldDef> with
                member __.Equals(other) = this.Equals(other) }

        member this.WithResolveMiddleware<'Res>(middleware : ResolveFieldContext -> 'Val -> (ResolveFieldContext -> 'Val -> 'Res) -> 'Res) =
            { new FieldDef<'Val> with
                member __.Name = this.Name
                member __.Description = this.Description
                member __.DeprecationReason = this.DeprecationReason
                member __.TypeDef = this.TypeDef
                member __.Args = this.Args
                member __.Metadata = this.Metadata
                member __.Resolve =
                    let resolve expr = <@ fun ctx input -> middleware ctx input %%expr @>
                    match this.Resolve with
                    | Sync (input, output, expr) -> Sync (input, output, resolve expr)
                    | Async (input, output, expr) -> Async (input, output, expr)
                    | resolver -> failwith <| sprintf "Resolver '%A' is not supported!" resolver
              interface IEquatable<FieldDef> with
                member __.Equals(other) = this.Equals(other) }

    type Metadata with
        member this.WithQueryWeightThreshold(threshold : float) =
            this.Add(MetadataKeys.QueryWeightMiddleware.QueryWeightThreshold, threshold); this
        
        static member QueryWeightThreshold(threshold : float) =
            Metadata.Empty.WithQueryWeightThreshold(threshold)