namespace FSharp.Data.GraphQL.Types

open System
open FSharp.Data.GraphQL.Types
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Linq.RuntimeHelpers

/// A function for field resolve that acts as a middleware, running
/// before the actual field resolve function.
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
        member _.Fields = fields
    interface ObjectDef with
        member _.Fields = fields |> Map.map (fun _ f -> upcast f)
        member _.Name = source.Name
        member _.Description = source.Description
        member _.Implements = source.Implements
        member _.IsTypeOf = source.IsTypeOf
    interface TypeDef with
        member this.MakeList() = upcast (ListOf this)
        member this.MakeNullable() = upcast (Nullable this)
        member _.Type = (source :> TypeDef).Type
    interface NamedDef with
        member _.Name = (source :> NamedDef).Name
    override _.Equals y = source.Equals y
    override _.GetHashCode() = source.GetHashCode()
    override _.ToString() = source.ToString()

type internal CustomOutputFieldDefinition<'Val, 'Res>(source : FieldDef<'Val>, typeDef: OutputDef) =
    interface FieldDef<'Val> with
        member __.Name = source.Name
        member __.Description = source.Description
        member __.DeprecationReason = source.DeprecationReason
        member __.TypeDef = typeDef
        member __.Args = source.Args
        member __.Metadata = source.Metadata
        member __.Resolve = source.Resolve
        
    interface IEquatable<FieldDef> with
        member __.Equals(other) = source.Equals(other)
        
    override __.Equals y = source.Equals y
    override __.GetHashCode() = source.GetHashCode()
    override __.ToString() = source.ToString()

type internal CustomResolveFieldDefinition<'Val, 'Res>(source : FieldDef<'Val>, middleware : FieldResolveMiddleware<'Val, 'Res>) =
    interface FieldDef<'Val> with
        member _.Name = source.Name
        member _.Description = source.Description
        member _.DeprecationReason = source.DeprecationReason
        member _.TypeDef = source.TypeDef
        member _.Args = source.Args
        member _.Metadata = source.Metadata
        member _.Resolve =
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
        member _.Equals(other) = source.Equals(other)
    override _.Equals y = source.Equals y
    override _.GetHashCode() = source.GetHashCode()
    override _.ToString() = source.ToString()

type internal CustomArgsFieldDefinition<'Val>(source : FieldDef<'Val>, args : InputFieldDef seq) =
    let exists aname = args |> Seq.exists (fun x -> x.Name = aname)
    let args =
        source.Args
        |> Seq.filter (fun x -> not (exists x.Name))
        |> Seq.append (args |> Array.ofSeq)
        |> Array.ofSeq
    interface FieldDef<'Val> with
        member _.Name = source.Name
        member _.Description = source.Description
        member _.DeprecationReason = source.DeprecationReason
        member _.TypeDef = source.TypeDef
        member _.Args = args
        member _.Metadata = source.Metadata
        member _.Resolve = source.Resolve

    interface IEquatable<FieldDef> with
        member _.Equals(other) = source.Equals(other)
    override _.Equals y = source.Equals y
    override _.GetHashCode() = source.GetHashCode()
    override _.ToString() = source.ToString()

type internal CustomMetadataFieldDefinition<'Val>(source : FieldDef<'Val>, metadata : Metadata) =
    interface FieldDef<'Val> with
        member _.Name = source.Name
        member _.Description = source.Description
        member _.DeprecationReason = source.DeprecationReason
        member _.TypeDef = source.TypeDef
        member _.Args = source.Args
        member _.Metadata = metadata
        member _.Resolve = source.Resolve

    interface IEquatable<FieldDef> with
        member _.Equals(other) = source.Equals(other)
    override _.Equals y = source.Equals y
    override _.GetHashCode() = source.GetHashCode()
    override _.ToString() = source.ToString()

/// Contains extensions for the type system.
[<AutoOpen>]
module TypeSystemExtensions =
    type ObjectDef<'Val> with
        /// <summary>
        /// Creates a new object definition based on the existing one, containing
        /// the fields of the existing one, plus customized fields.
        /// </summary>
        /// <remarks>
        /// If the object definition has customized fields, and any existing field
        /// has the name of a custom field definition, it is discarded and
        /// replaced by the supplied one.
        /// </remarks>
        /// <param name="fields">Additional field definitions for the derived object definition.</param>
        member this.WithFields(fields : FieldDef<'Val> seq) : ObjectDef<'Val> =
            upcast CustomFieldsObjectDefinition(this, fields)

    type FieldDef<'Val> with
        /// <summary>
        /// Creates a new field definition based on the existing one, containing
        /// a field resolve middleware, that is run before the actual resolve function.
        /// </summary>
        /// <param name="middleware">The middleware function for the field resolve.</param>
        member this.WithResolveMiddleware<'Res>(middleware : FieldResolveMiddleware<'Val, 'Res>) : FieldDef<'Val> =
            upcast CustomResolveFieldDefinition(this, middleware)

        /// <summary>
        /// Creates a new field definition based on the existing one, containing
        /// the metadata supplied. Metadata from old field is replaced by the provided one.
        /// </summary>
        member this.WithMetadata(metadata : Metadata) : FieldDef<'Val> =
            upcast CustomMetadataFieldDefinition(this, metadata)

        /// <summary>
        /// Creates a new field definition based on the existing one, containing
        /// the arguments of the existing one, plus customized arguments.
        /// </summary>
        /// <remarks>
        /// If the field definition has customized arguments, and any existing argument
        /// has the name of a customized argument, it is discarded and
        /// replaced by the supplied one.
        /// </remarks>
        /// <param name="args">Additional argument definitions for the derived field definition.</param>
        member this.WithArgs(args : InputFieldDef seq) : FieldDef<'Val> =
            upcast CustomArgsFieldDefinition(this, args)
