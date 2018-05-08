namespace FSharp.Data.GraphQL.Server.Middlewares

open System
open FSharp.Data.GraphQL.Types
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Linq.RuntimeHelpers

type FieldResolveMiddleware<'Val, 'Res> =
    ResolveFieldContext -> 'Val -> (ResolveFieldContext -> 'Val -> 'Res) -> 'Res

type internal CustomFieldsObjectDefinition<'Val>(source : ObjectDef<'Val>, fields: FieldDef<'Val> seq) =
    let exists fname = 
        fields |> Seq.exists (fun x -> x.Name = fname)
    let fields = 
        source.Fields
        |> Map.toSeq
        |> Seq.filter (fun (n, _) -> not (exists n))
        |> Seq.append (fields |> Seq.map (fun x -> x.Name, x))
        |> Map.ofSeq
    interface ObjectDef<'Val> with
        member __.Fields = fields
    interface ObjectDef with
        member __.Fields = fields |> Map.map (fun _ f -> upcast f)
        member __.Name = source.Name
        member __.Description = source.Description
        member __.Implements = source.Implements
        member __.IsTypeOf = source.IsTypeOf
    interface TypeDef with
        member __.MakeList() = source.MakeList() // TODO : This may need to be changed
        member __.MakeNullable() = source.MakeNullable() // TODO : This may need to be changed
        member __.Type = (source :> TypeDef).Type
    interface NamedDef with
        member __.Name = (source :> NamedDef).Name
    override __.Equals y = source.Equals y
    override __.GetHashCode() = source.GetHashCode()
    override __.ToString() = source.ToString()

type internal CustomResolveFieldDefinition<'Val, 'Res>(source : FieldDef<'Val>, middleware : FieldResolveMiddleware<'Val, 'Res>) =
    interface FieldDef<'Val> with
        member __.Name = source.Name
        member __.Description = source.Description
        member __.DeprecationReason = source.DeprecationReason
        member __.TypeDef = source.TypeDef
        member __.Args = source.Args
        member __.Metadata = source.Metadata
        member __.Resolve =
            let changeResolver expr = 
                let expr =
                    match expr with 
                    | WithValue (_, _, e) -> e
                    | _ -> failwith "Unexpected resolver expression."
                let resolver = <@ fun ctx input -> middleware ctx input %%expr @>
                let compiledResolver = LeafExpressionConverter.EvaluateQuotation resolver
                Expr.WithValue(compiledResolver, resolver.Type, resolver)
            match source.Resolve with
            | Sync (input, output, expr) -> Sync (input, output, changeResolver expr)
            | Async (input, output, expr) -> Async (input, output, changeResolver expr)
            | Undefined -> failwith "Field has no resolve function."
            | x -> failwith <| sprintf "Resolver '%A' is not supported." x
    interface IEquatable<FieldDef> with
        member __.Equals(other) = source.Equals(other)
    override __.Equals y = source.Equals y
    override __.GetHashCode() = source.GetHashCode()
    override __.ToString() = source.ToString()

type internal CustomArgsFieldDefinition<'Val>(source : FieldDef<'Val>, args : InputFieldDef seq) =
    let exists aname = args |> Seq.exists (fun x -> x.Name = aname)
    interface FieldDef<'Val> with
        member __.Name = source.Name
        member __.Description = source.Description
        member __.DeprecationReason = source.DeprecationReason
        member __.TypeDef = source.TypeDef
        member __.Args = 
            source.Args
            |> Array.filter (fun x -> not (exists x.Name))
            |> Array.append (args |> Array.ofSeq)
        member __.Metadata = source.Metadata
        member __.Resolve = source.Resolve
    interface IEquatable<FieldDef> with
        member __.Equals(other) = source.Equals(other)
    override __.Equals y = source.Equals y
    override __.GetHashCode() = source.GetHashCode()
    override __.ToString() = source.ToString()

[<AutoOpen>]
module TypeSystemExtensions =
    type Define with
        static member ObjectFilterInput =
            Define.Input("filter", ObjectListFilter)

        static member ListField(name : string, typedef : #OutputDef<'Res>, description : string, 
                                resolve : ResolveFieldContext -> 'Val -> 'Res seq) : FieldDef<'Val> =
            Define.Field(name, ListOf typedef, description, [ Define.ObjectFilterInput ], resolve)

    type ObjectDef<'Val> with
        member this.WithFields(fields : FieldDef<'Val> seq) : ObjectDef<'Val> =
            upcast CustomFieldsObjectDefinition(this, fields)

    type FieldDef<'Val> with
        member this.WithResolveMiddleware<'Res>(middleware : FieldResolveMiddleware<'Val, 'Res>) : FieldDef<'Val> =
            upcast CustomResolveFieldDefinition(this, middleware)
        member this.WithQueryWeight(weight : float) =
            this.Metadata.Add(MetadataKeys.QueryWeightMiddleware.QueryWeight, weight); this

        member this.WithArgs(args : InputFieldDef seq) : FieldDef<'Val> =
            upcast CustomArgsFieldDefinition(this, args)

    type Metadata with
        member this.WithQueryWeightThreshold(threshold : float) =
            this.Add(MetadataKeys.QueryWeightMiddleware.QueryWeightThreshold, threshold); this
        
        static member QueryWeightThreshold(threshold : float) =
            Metadata.Empty.WithQueryWeightThreshold(threshold)