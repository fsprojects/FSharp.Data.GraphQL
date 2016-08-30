/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
namespace FSharp.Data.GraphQL.Types

open System
open System.Collections.Concurrent
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Extensions
open Microsoft.FSharp.Quotations

[<Flags>]
type DirectiveLocation = 
    | QUERY = 1
    | MUTATION = 2
    | SUBSCRIPTION = 4
    | FIELD = 8
    | FRAGMENT_DEFINITION = 16
    | FRAGMENT_SPREAD = 32
    | INLINE_FRAGMENT = 64

module Introspection = 
    type TypeKind = 
        | SCALAR = 1
        | OBJECT  = 2
        | INTERFACE = 3
        | UNION = 4
        | ENUM = 5
        | INPUT_OBJECT = 6
        | LIST = 7
        | NON_NULL = 8
    
    type IntrospectionDirective = 
        { Name : string
          Description : string option
          Locations : DirectiveLocation []
          Args : IntrospectionInputVal [] }
    
    and IntrospectionType = 
        { Kind : TypeKind
          Name : string
          Description : string option
          Fields : IntrospectionField [] option
          Interfaces : IntrospectionTypeRef [] option
          PossibleTypes : IntrospectionTypeRef [] option
          EnumValues : IntrospectionEnumVal [] option
          InputFields : IntrospectionInputVal [] option
          OfType : IntrospectionTypeRef option }
        
        static member Scalar(name : string, description : string option) = 
            { Kind = TypeKind.SCALAR
              Name = name
              Description = description
              Fields = None
              Interfaces = None
              PossibleTypes = None
              EnumValues = None
              InputFields = None
              OfType = None }
        
        static member Object(name : string, description : string option, fields : IntrospectionField [], 
                             interfaces : IntrospectionTypeRef []) = 
            { Kind = TypeKind.OBJECT
              Name = name
              Description = description
              Fields = Some fields
              Interfaces = Some interfaces
              PossibleTypes = None
              EnumValues = None
              InputFields = None
              OfType = None }
        
        static member InputObject(name : string, description : string option, inputFields : IntrospectionInputVal []) = 
            { Kind = TypeKind.INPUT_OBJECT
              Name = name
              Description = description
              Fields = None
              Interfaces = None
              PossibleTypes = None
              EnumValues = None
              InputFields = Some inputFields
              OfType = None }
        
        static member Union(name : string, description : string option, possibleTypes : IntrospectionTypeRef []) = 
            { Kind = TypeKind.UNION
              Name = name
              Description = description
              Fields = None
              Interfaces = None
              PossibleTypes = Some possibleTypes
              EnumValues = None
              InputFields = None
              OfType = None }
        
        static member Enum(name : string, description : string option, enumValues : IntrospectionEnumVal []) = 
            { Kind = TypeKind.ENUM
              Name = name
              Description = description
              Fields = None
              Interfaces = None
              PossibleTypes = None
              EnumValues = Some enumValues
              InputFields = None
              OfType = None }
        
        static member Interface(name : string, description : string option, fields : IntrospectionField [], 
                                possibleTypes : IntrospectionTypeRef []) = 
            { Kind = TypeKind.INTERFACE
              Name = name
              Description = description
              Fields = Some fields
              Interfaces = None
              PossibleTypes = Some possibleTypes
              EnumValues = None
              InputFields = None
              OfType = None }
    
    and IntrospectionTypeRef = 
        { Kind : TypeKind
          Name : string option
          Description : string option
          OfType : IntrospectionTypeRef option }
        
        static member List(inner : IntrospectionTypeRef) = 
            { Kind = TypeKind.LIST
              Name = None
              Description = None
              OfType = Some inner }
        
        static member NonNull(inner : IntrospectionTypeRef) = 
            { Kind = TypeKind.NON_NULL
              Name = None
              Description = None
              OfType = Some inner }
        
        static member Named(inner : IntrospectionType) = 
            { Kind = inner.Kind
              Name = Some inner.Name
              Description = inner.Description
              OfType = None }
    
    and IntrospectionInputVal = 
        { Name : string
          Description : string option
          Type : IntrospectionTypeRef
          DefaultValue : string option }
    
    and IntrospectionEnumVal = 
        { Name : string
          Description : string option
          IsDeprecated : bool
          DeprecationReason : string option }
    
    and IntrospectionField = 
        { Name : string
          Description : string option
          Args : IntrospectionInputVal []
          Type : IntrospectionTypeRef
          IsDeprecated : bool
          DeprecationReason : string option }
    
    and IntrospectionSchema = 
        { QueryType : IntrospectionTypeRef
          MutationType : IntrospectionTypeRef option
          SubscriptionType : IntrospectionTypeRef option
          Types : IntrospectionType []
          Directives : IntrospectionDirective [] }

type ISchema = 
    interface
        inherit seq<NamedDef>
        abstract TypeMap : Map<string, NamedDef>
        abstract Query : ObjectDef
        abstract Mutation : ObjectDef option
        abstract Directives : DirectiveDef []
        abstract TryFindType : string -> NamedDef option
        abstract GetPossibleTypes : AbstractDef -> ObjectDef []
        abstract IsPossibleType : AbstractDef -> ObjectDef -> bool
        abstract Introspected : Introspection.IntrospectionSchema
    end

// 3.1 Types
and TypeDef = 
    interface
        abstract Type : Type
        abstract MakeList : unit -> ListOfDef
        abstract MakeNullable : unit -> NullableDef
    end

and TypeDef<'Val> = 
    interface
        inherit TypeDef
    end

and InputDef = 
    interface
        inherit TypeDef
    end

and InputDef<'Val> = 
    interface
        inherit InputDef
        inherit TypeDef<'Val>
    end

and OutputDef = 
    interface
        inherit TypeDef
    end

and OutputDef<'Val> = 
    interface
        inherit OutputDef
        inherit TypeDef<'Val>
    end

and LeafDef = 
    interface
        inherit TypeDef
    end

and CompositeDef = 
    interface
        inherit TypeDef
    end

and AbstractDef = 
    interface
        inherit TypeDef
        // only abstract types are Interface and Union, which are both composite defs too
        inherit CompositeDef
    end

and NamedDef = 
    interface
        inherit TypeDef
        abstract Name : string
    end

and PlanningContext = 
    { Schema : ISchema
      RootDef : ObjectDef
      Document : Document }

and Includer = Map<string, obj> -> bool

and ExecutionInfo = 
    { /// Field identifier, which may be either field name or alias. For top level execution plan it will be None.
      Identifier : string
      /// Field definition of corresponding type found in current schema.
      Definition : FieldDef
      /// AST node of the parsed query document.
      Ast : Field
      /// A type of execution plan.
      Kind : ExecutionInfoKind
      // logic describing if correlated field should be included in result set
      Include : Includer
      /// Composite definition being the parent of the current field, execution plan refers to.
      ParentDef : OutputDef
      /// Type definition marking returned type
      ReturnDef : OutputDef 
      /// Flag determining if flag allows to have nullable output
      IsNullable : bool }
    override this.ToString () =
        let pad indent (sb: Text.StringBuilder) = for i = 0 to indent do sb.Append '\t' |> ignore
        let nameAs info =
            match info.Ast.Alias with
            | Some alias -> 
                info.Ast.Name + " as " + alias + " of " + info.ReturnDef.ToString() + (if info.IsNullable then "" else "!")
            | None -> info.Ast.Name + " of " + info.ReturnDef.ToString() + (if info.IsNullable then "" else "!")
        let rec str indent sb info =
            match info.Kind with
            | ResolveValue -> 
                pad indent sb
                sb.Append("ResolveValue: ").AppendLine(nameAs info) |> ignore
            | SelectFields fields ->
                pad indent sb
                sb.Append("SelectFields: ").AppendLine(nameAs info) |> ignore
                fields |> List.iter (str (indent+1) sb)
            | ResolveCollection inner ->
                pad indent sb
                sb.Append("ResolveCollection: ").AppendLine(nameAs info) |> ignore
                str (indent+1) sb inner
            | ResolveAbstraction types ->
                pad indent sb
                sb.Append("ResolveAbstraction: ").AppendLine(nameAs info) |> ignore
                types
                |> Map.iter (fun tname fields ->
                    pad (indent+1) sb
                    sb.Append("Case: ").AppendLine(tname) |> ignore
                    fields |> List.iter (str (indent+2) sb))
            
        let sb = Text.StringBuilder ()
        str 0 sb this
        sb.ToString()

/// plan of reduction being a result of application of a query AST on existing schema
and ExecutionInfoKind = 
    // reducer for scalar or enum
    | ResolveValue
    // reducer for selection set applied upon output object
    | SelectFields of fields : ExecutionInfo list
    // reducer for each of the collection elements
    | ResolveCollection of elementPlan : ExecutionInfo
    // reducer for union and interface types to be resolved into ReduceSelection at runtime
    | ResolveAbstraction of typeFields : Map<string, ExecutionInfo list>

and Resolve = 
    | Undefined
    // expr is untyped version of Expr<ResolveFieldContext->'Input->'Output>
    | Sync of input:Type * output:Type * expr:Expr 
    // expr is untyped version of Expr<ResolveFieldContext->'Input->Async<'Output>>
    | Async of input:Type * output:Type * expr:Expr 
    member x.Expr =
        match x with
        | Sync(_,_,e) -> e
        | Async(_,_,e) -> e

and ExecutionStrategy = 
    | Serial
    | Parallel

and ExecutionPlan = 
    { Operation : OperationDefinition
      RootDef : ObjectDef
      Strategy : ExecutionStrategy
      Fields : ExecutionInfo list }
    member x.Item with get(id) = x.Fields |> List.find (fun f -> f.Identifier = id)

and ExecutionContext = 
    { Schema : ISchema
      RootValue : obj
      ExecutionPlan : ExecutionPlan
      Variables : Map<string, obj>
      Errors : ConcurrentBag<exn> }

and ResolveFieldContext = 
    { ExecutionPlan : ExecutionInfo
      Context : ExecutionContext
      ReturnType : TypeDef
      ParentType : ObjectDef
      Schema : ISchema
      Args : Map<string, obj>
      Variables : Map<string, obj> }
    member x.AddError(error : exn) = x.Context.Errors.Add error
    
    member x.TryArg(name : string) : 't option = 
        match Map.tryFind name x.Args with
        | Some o -> Some(o :?> 't)
        | None -> None
    
    member x.Arg(name : string) : 't = downcast Map.find name x.Args

and ExecuteField = ResolveFieldContext -> obj -> AsyncVal<obj>

and FieldDef = 
    interface
        abstract Name : string
        abstract Description : string option
        abstract DeprecationReason : string option
        abstract TypeDef : OutputDef
        abstract Args : InputFieldDef []
        abstract Resolve : Resolve
        abstract Execute : ExecuteField with get, set
        inherit IEquatable<FieldDef>
    end

and FieldDef<'Val> = 
    interface
        inherit FieldDef
    end

and ScalarDef = 
    interface
        abstract Name : string
        abstract Description : string option
        abstract CoerceInput : Value -> obj option
        abstract CoerceOutput : obj -> Value option
        abstract CoerceValue : obj -> obj option
        inherit TypeDef
        inherit NamedDef
        inherit InputDef
        inherit OutputDef
        inherit LeafDef
    end

/// 3.1.1.1 Build-in Scalars
and [<CustomEquality; NoComparison>] ScalarDefinition<'Val> = 
    { Name : string
      Description : string option
      CoerceInput : Value -> 'Val option
      CoerceOutput : 'Val -> Value option
      CoerceValue : obj -> 'Val option }
    
    interface TypeDef with
        member __.Type = typeof<'Val>
        
        member x.MakeNullable() = 
            let nullable : NullableDefinition<_> = { OfType = x }
            upcast nullable
        
        member x.MakeList() = 
            let list: ListOfDefinition<_,_> = { OfType = x }
            upcast list
    
    interface InputDef
    interface OutputDef
    
    interface ScalarDef with
        member x.Name = x.Name
        member x.Description = x.Description
        member x.CoerceInput input = x.CoerceInput input |> Option.map box
        member x.CoerceOutput output = x.CoerceOutput(output :?> 'Val)
        member x.CoerceValue value = (x.CoerceValue value) |> Option.map box
    
    interface InputDef<'Val>
    interface OutputDef<'Val>
    interface LeafDef
    
    interface NamedDef with
        member x.Name = x.Name
    
    override x.Equals y = 
        match y with
        | :? ScalarDefinition<'Val> as s -> x.Name = s.Name
        | _ -> false
    
    override x.GetHashCode() = x.Name.GetHashCode()
    override x.ToString() = x.Name

and EnumVal = 
    interface
        abstract Name : string
        abstract Description : string option
        abstract Value : obj
        abstract DeprecationReason : string option
    end

and EnumValue<'Val> = 
    { Name : string
      Value : 'Val
      Description : string option
      DeprecationReason : string option }
    
    interface EnumVal with
        member x.Name = x.Name
        member x.Description = x.Description
        member x.DeprecationReason = x.DeprecationReason
        member x.Value = upcast x.Value
    
    override x.ToString() = x.Name

and EnumDef = 
    interface
        abstract Name : string
        abstract Description : string option
        abstract Options : EnumVal []
        inherit TypeDef
        inherit InputDef
        inherit OutputDef
        inherit LeafDef
        inherit NamedDef
    end

and EnumDef<'Val> = 
    interface
        abstract Options : EnumValue<'Val> []
        inherit EnumDef
        inherit TypeDef<'Val>
        inherit InputDef<'Val>
        inherit OutputDef<'Val>
    end

and EnumDefinition<'Val> = 
    { Name : string
      Description : string option
      Options : EnumValue<'Val> [] }
    interface InputDef
    interface OutputDef
    
    interface TypeDef with
        member __.Type = typeof<'Val>
        
        member x.MakeNullable() = 
            let nullable : NullableDefinition<_> = { OfType = x }
            upcast nullable
        
        member x.MakeList() = 
            let list: ListOfDefinition<_,_> = { OfType = x }
            upcast list
    
    interface EnumDef<'Val> with
        member x.Options = x.Options
    
    interface EnumDef with
        member x.Name = x.Name
        member x.Description = x.Description
        member x.Options = 
            x.Options
            |> Seq.ofArray
            |> Seq.cast<EnumVal>
            |> Seq.toArray
    
    interface NamedDef with
        member x.Name = x.Name
    
    override x.ToString() = sprintf "enum %s {\n    %s\n}" x.Name (String.Join("\n    ", x.Options))

/// 3.1.2 Objects
and ObjectDef = 
    interface
        abstract Name : string
        abstract Description : string option
        abstract Fields : Map<string, FieldDef>
        abstract Implements : InterfaceDef []
        abstract IsTypeOf : (obj -> bool) option
        inherit TypeDef
        inherit NamedDef
        inherit OutputDef
        inherit CompositeDef
    end

and ObjectDef<'Val> = 
    interface
        abstract Fields : Map<string, FieldDef<'Val>>
        inherit ObjectDef
        inherit TypeDef<'Val>
        inherit OutputDef<'Val>
    end

and [<CustomEquality; NoComparison>] ObjectDefinition<'Val> = 
    { Name : string
      Description : string option
      FieldsFn : Lazy<Map<string, FieldDef<'Val>>>
      Implements : InterfaceDef []
      IsTypeOf : (obj -> bool) option }
    
    interface TypeDef with
        member __.Type = typeof<'Val>
        
        member x.MakeNullable() = 
            let nullable : NullableDefinition<_> = { OfType = x }
            upcast nullable
        
        member x.MakeList() = 
            let list: ListOfDefinition<_,_> = { OfType = x }
            upcast list
    
    interface OutputDef
    
    interface ObjectDef with
        member x.Name = x.Name
        member x.Description = x.Description
        member x.Fields = x.FieldsFn.Force() |> Map.map (fun k v -> upcast v)
        member x.Implements = x.Implements
        member x.IsTypeOf = x.IsTypeOf
    
    interface ObjectDef<'Val> with
        member x.Fields = x.FieldsFn.Force()
    
    interface NamedDef with
        member x.Name = x.Name
    
    override x.Equals y = 
        match y with
        | :? ObjectDefinition<'Val> as f -> f.Name = x.Name
        | _ -> false
    
    override x.GetHashCode() = 
        let mutable hash = x.Name.GetHashCode()
        hash
    
    override x.ToString() = x.Name

and [<CustomEquality; NoComparison>] FieldDefinition<'Val, 'Res> = 
    { Name : string
      Description : string option
      TypeDef : OutputDef<'Res>
      Resolve : Resolve
      Args : InputFieldDef []
      DeprecationReason : string option
      mutable Execute : ExecuteField }
    
    interface FieldDef with
        member x.Name = x.Name
        member x.Description = x.Description
        member x.DeprecationReason = x.DeprecationReason
        member x.TypeDef = x.TypeDef :> OutputDef
        member x.Args = x.Args
        member x.Resolve = x.Resolve
        
        member x.Execute 
            with get () = x.Execute
            and set v = x.Execute <- v
    
    interface FieldDef<'Val>
    
    interface IEquatable<FieldDef> with
        member x.Equals f = x.Name = f.Name && x.TypeDef :> OutputDef = f.TypeDef && x.Args = f.Args
    
    override x.Equals y = 
        match y with
        | :? FieldDef as f -> (x :> IEquatable<FieldDef>).Equals(f)
        | _ -> false
    
    override x.GetHashCode() = 
        let mutable hash = x.Name.GetHashCode()
        hash <- (hash * 397) ^^^ (x.TypeDef.GetHashCode())
        hash <- (hash * 397) ^^^ (x.Args.GetHashCode())
        hash
    
    override x.ToString() = 
        if not (Array.isEmpty x.Args) 
        then x.Name + "(" + String.Join(", ", x.Args) + "): " + x.TypeDef.ToString()
        else x.Name + ": " + x.TypeDef.ToString()

/// 3.1.3 Interfaces
and InterfaceDef = 
    interface
        abstract Name : string
        abstract Description : string option
        abstract Fields : FieldDef []
        abstract ResolveType : (obj -> ObjectDef) option
        inherit TypeDef
        inherit OutputDef
        inherit CompositeDef
        inherit AbstractDef
        inherit NamedDef
    end

and InterfaceDef<'Val> = 
    interface
        abstract Fields : FieldDef<'Val> []
        inherit TypeDef<'Val>
        inherit OutputDef<'Val>
        inherit InterfaceDef
    end

and [<CustomEquality; NoComparison>] InterfaceDefinition<'Val> = 
    { Name : string
      Description : string option
      FieldsFn : unit -> FieldDef<'Val> []
      ResolveType : (obj -> ObjectDef) option }
    
    interface TypeDef with
        member __.Type = typeof<'Val>
        
        member x.MakeNullable() = 
            let nullable : NullableDefinition<_> = { OfType = x }
            upcast nullable
        
        member x.MakeList() = 
            let list: ListOfDefinition<_,_> = { OfType = x }
            upcast list
    
    interface OutputDef
    
    interface InterfaceDef with
        member x.Name = x.Name
        member x.Description = x.Description
        member x.Fields = x.FieldsFn() |> Array.map (fun fdef -> upcast fdef)
        member x.ResolveType = x.ResolveType
    
    interface InterfaceDef<'Val> with
        member x.Fields = x.FieldsFn()
    
    interface NamedDef with
        member x.Name = x.Name
    
    override x.Equals y = 
        match y with
        | :? InterfaceDefinition<'Val> as f -> x.Name = f.Name
        | _ -> false
    
    override x.GetHashCode() = 
        let mutable hash = x.Name.GetHashCode()
        hash
    
    override x.ToString() = 
        let sb = System.Text.StringBuilder("interface ").Append(x.Name).Append(" {")
        // x.Fields |> List.iter (fun f -> sb.Append("\n    ").Append(f.ToString()) |> ignore)
        sb.Append("\n}").ToString()

and UnionDef = 
    interface
        abstract Name : string
        abstract Description : string option
        abstract Options : ObjectDef []
        abstract ResolveType : (obj -> ObjectDef) option
        abstract ResolveValue : obj -> obj
        inherit TypeDef
        inherit OutputDef
        inherit CompositeDef
        inherit AbstractDef
        inherit NamedDef
    end

and UnionDef<'In> = 
    interface
        abstract ResolveType : ('In -> ObjectDef) option
        abstract ResolveValue : 'In -> obj
        inherit UnionDef
        inherit TypeDef<'In>
        inherit OutputDef<'In>
    end

/// 3.1.4 Unions
and [<CustomEquality; NoComparison>] UnionDefinition<'In, 'Out> = 
    { Name : string
      Description : string option
      Options : ObjectDef []
      ResolveType : ('In -> ObjectDef) option
      ResolveValue : 'In -> 'Out }
    
    interface TypeDef with
        member __.Type = typeof<'Out>
        
        member x.MakeNullable() = 
            let nullable : NullableDefinition<_> = { OfType = x }
            upcast nullable
        
        member x.MakeList() = 
            let list: ListOfDefinition<_,_> = { OfType = x }
            upcast list
    
    interface OutputDef
    
    interface UnionDef with
        member x.Name = x.Name
        member x.Description = x.Description
        member x.Options = x.Options
        member x.ResolveType = x.ResolveType |> Option.map (fun fn -> (fun value -> fn (value :?> 'In)))
        member x.ResolveValue value = upcast x.ResolveValue(value :?> 'In)
    
    interface UnionDef<'In> with
        member x.ResolveType = x.ResolveType
        member x.ResolveValue value = upcast x.ResolveValue value
    
    interface NamedDef with
        member x.Name = x.Name
    
    override x.Equals y = 
        match y with
        | :? UnionDefinition<'In, 'Out> as f -> x.Name = f.Name
        | _ -> false
    
    override x.GetHashCode() = 
        let mutable hash = x.Name.GetHashCode()
        hash <- (hash * 397) ^^^ (x.Options.GetHashCode())
        hash
    
    override x.ToString() = "union " + x.Name + " = " + String.Join(" | ", x.Options |> Array.map (fun o -> o.Name))

and ListOfDef = 
    interface
        abstract OfType : TypeDef
        inherit InputDef
        inherit OutputDef
    end

and ListOfDef<'Val, 'Seq when 'Seq :> 'Val seq> = 
    interface
        abstract OfType : TypeDef<'Val>
        inherit TypeDef<'Seq>
        inherit InputDef<'Seq>
        inherit OutputDef<'Seq>
    end

and ListOfDefinition<'Val, 'Seq when 'Seq :> 'Val seq> = 
    { OfType : TypeDef<'Val> }
    interface InputDef
    
    interface TypeDef with
        member __.Type = typeof<'Seq>
        member x.MakeNullable() = 
            let nullable : NullableDefinition<_> = { OfType = x }
            upcast nullable
        
        member x.MakeList() = 
            let list: ListOfDefinition<_, _> = { OfType = x }
            upcast list
    
    interface OutputDef
    
    interface ListOfDef with
        member x.OfType = upcast x.OfType
    
    interface ListOfDef<'Val, 'Seq> with
        member x.OfType = x.OfType
    
    override x.ToString() = 
        match x.OfType with
        | :? NamedDef as named -> "[" + named.Name + "]"
        | other -> "[" + other.ToString() + "]"

and NullableDef = 
    interface
        abstract OfType : TypeDef
        inherit InputDef
        inherit OutputDef
    end

and NullableDef<'Val> = 
    interface
        abstract OfType : TypeDef<'Val>
        inherit InputDef<'Val option>
        inherit OutputDef<'Val option>
    end

and NullableDefinition<'Val> = 
    { OfType : TypeDef<'Val> }
    interface InputDef
    
    interface TypeDef with
        member __.Type = typeof<'Val option>
        member x.MakeNullable() = upcast x
        member x.MakeList() = 
            let list: ListOfDefinition<_,_> = { OfType = x }
            upcast list
    
    interface OutputDef
    
    interface NullableDef with
        member x.OfType = upcast x.OfType
    
    interface NullableDef<'Val> with
        member x.OfType = x.OfType
    
    override x.ToString() = 
        match x.OfType with
        | :? NamedDef as named -> named.Name.Substring(0, named.Name.Length - 1) // remobe bang on sufix
        | other -> other.ToString()

and InputObjectDef = 
    interface
        abstract Name : string
        abstract Description : string option
        abstract Fields : InputFieldDef []
        inherit NamedDef
        inherit InputDef
    end

/// 3.1.6 Input Objects
and InputObjectDefinition<'Val> = 
    { Name : string
      Description : string option
      FieldsFn : unit -> InputFieldDef [] }
    interface InputDef
    
    interface InputObjectDef with
        member x.Name = x.Name
        member x.Description = x.Description
        member x.Fields = x.FieldsFn()
    
    interface TypeDef<'Val>
    interface InputDef<'Val>
    
    interface NamedDef with
        member x.Name = x.Name
    
    interface TypeDef with
        member __.Type = typeof<'Val>

        member x.MakeNullable() = 
            let nullable : NullableDefinition<_> = { OfType = x }
            upcast nullable
        
        member x.MakeList() = 
            let list: ListOfDefinition<_,_> = { OfType = x }
            upcast list

and ExecuteInput = Map<string, obj> -> Value -> obj

and InputFieldDef = 
    interface
        abstract Name : string
        abstract Description : string option
        abstract TypeDef : InputDef
        abstract DefaultValue : obj option
        abstract ExecuteInput : ExecuteInput with get, set
        inherit IEquatable<InputFieldDef>
    end

/// 3.1.2.1 Object Field Arguments
and [<CustomEquality; NoComparison>] InputFieldDefinition<'In> = 
    { Name : string
      Description : string option
      TypeDef : InputDef<'In>
      DefaultValue : 'In option
      mutable ExecuteInput : ExecuteInput }
    
    interface InputFieldDef with
        member x.Name = x.Name
        member x.Description = x.Description
        member x.TypeDef = upcast x.TypeDef
        member x.DefaultValue = x.DefaultValue |> Option.map (fun x -> upcast x)
        
        member x.ExecuteInput 
            with get () = x.ExecuteInput
            and set v = x.ExecuteInput <- v
    
    interface IEquatable<InputFieldDef> with
        member x.Equals f = x.Name = f.Name && x.TypeDef :> InputDef = f.TypeDef
    
    override x.Equals y = 
        match y with
        | :? InputFieldDef as f -> (x :> IEquatable<InputFieldDef>).Equals(f)
        | _ -> false
    
    override x.GetHashCode() = 
        let mutable hash = x.Name.GetHashCode()
        hash <- (hash * 397) ^^^ (x.TypeDef.GetHashCode())
        hash
    
    override x.ToString() = x.Name + ": " + x.TypeDef.ToString()

/// 5.7 Variables
and Variable = 
    { Name : string
      Schema : TypeDef
      DefaultValue : obj }
    override x.ToString() = 
        "$" + x.Name + ": " + x.Schema.ToString() + (if x.DefaultValue <> null then " = " + x.DefaultValue.ToString()
                                                     else "")

and DirectiveDef = 
    { Name : string
      Description : string option
      Locations : DirectiveLocation
      Args : InputFieldDef [] }

[<AutoOpen>]
module SchemaDefinitions = 
    open System.Globalization
    open System.Reflection
    
    let coerceIntValue (x : obj) : int option = 
        match x with
        | null -> None
        | :? int as i -> Some i
        | :? int64 as l -> Some(int l)
        | :? double as d -> Some(int d)
        | :? string as s -> 
            match Int32.TryParse(s) with
            | true, i -> Some i
            | false, _ -> None
        | :? bool as b -> 
            Some(if b then 1
                 else 0)
        | other -> 
            try 
                Some(System.Convert.ToInt32 other)
            with _ -> None
    
    let coerceFloatValue (x : obj) : double option = 
        match x with
        | null -> None
        | :? int as i -> Some(double i)
        | :? int64 as l -> Some(double l)
        | :? double as d -> Some d
        | :? string as s -> 
            match Double.TryParse(s) with
            | true, i -> Some i
            | false, _ -> None
        | :? bool as b -> 
            Some(if b then 1.
                 else 0.)
        | other -> 
            try 
                Some(System.Convert.ToDouble other)
            with _ -> None
    
    let coerceBoolValue (x : obj) : bool option = 
        match x with
        | null -> None
        | :? int as i -> Some(i <> 0)
        | :? int64 as l -> Some(l <> 0L)
        | :? double as d -> Some(d <> 0.)
        | :? string as s -> 
            match Boolean.TryParse(s) with
            | true, i -> Some i
            | false, _ -> None
        | :? bool as b -> Some b
        | other -> 
            try 
                Some(System.Convert.ToBoolean other)
            with _ -> None
    
    let coerceUriValue (x : obj) : Uri option = 
        match x with
        | null -> None
        | :? Uri as u -> Some u
        | :? string as s -> 
            match Uri.TryCreate(s, UriKind.RelativeOrAbsolute) with
            | true, uri -> Some uri
            | false, _ -> None
        | other -> None
    
    let coerceDateValue (x : obj) : DateTime option = 
        match x with
        | null -> None
        | :? DateTime as d -> Some d
        | :? string as s -> 
            match DateTime.TryParse(s) with
            | true, date -> Some date
            | false, _ -> None
        | other -> None
    
    let private coerceIntOuput (x : obj) = 
        match x with
        | :? int as y -> Some(IntValue y)
        | _ -> None
    
    let private coerceFloatOuput (x : obj) = 
        match x with
        | :? float as y -> Some(FloatValue y)
        | _ -> None
    
    let private coerceBoolOuput (x : obj) = 
        match x with
        | :? bool as y -> Some(BooleanValue y)
        | _ -> None
    
    let private coerceStringOuput (x : obj) = 
        match x with
        | :? string as y -> Some(StringValue y)
        | _ -> None
    
    let private coerceUriOutput (x : obj) = 
        match x with
        | :? Uri as uri -> Some(StringValue(uri.ToString()))
        | _ -> None
    
    let private coerceDateOutput (x : obj) = 
        match x with
        | :? DateTime as date -> Some(StringValue(date.ToString("O")))
        | _ -> None
    
    /// Check if provided obj value is an Option and extract its wrapped value as object if possible
    let (|Option|_|) (x : obj) = 
        if x = null then None
        else 
            let t = x.GetType().GetTypeInfo()
            if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>> then 
                t.GetDeclaredProperty("Value").GetValue(x) |> Some
            else None
    
    let coerceStringValue (x : obj) : string option = 
        match x with
        | null -> None
        | :? string as s -> Some s
        | :? bool as b -> 
            Some(if b then "true"
                 else "false")
        | Option o -> Some(o.ToString())
        | _ -> Some(x.ToString())
        
    let coerceIDValue (x : obj) : 't option = 
        match x with
        | null -> None
        | :? string as s -> Some (downcast Convert.ChangeType(s, typeof<'t>))
        | Option o -> Some(downcast Convert.ChangeType(o, typeof<'t>))
        | _ -> Some(downcast Convert.ChangeType(x, typeof<'t>))
    
    let private coerceIntInput = 
        function 
        | IntValue i -> Some i
        | FloatValue f -> Some(int f)
        | StringValue s -> 
            match Int32.TryParse(s, NumberStyles.Float, CultureInfo.InvariantCulture) with
            | true, i -> Some i
            | false, _ -> None
        | BooleanValue b -> 
            Some(if b then 1
                 else 0)
        | _ -> None
    
    let private coerceFloatInput = 
        function 
        | IntValue i -> Some(double i)
        | FloatValue f -> Some f
        | StringValue s -> 
            match Double.TryParse(s, NumberStyles.Float, CultureInfo.InvariantCulture) with
            | true, i -> Some i
            | false, _ -> None
        | BooleanValue b -> 
            Some(if b then 1.
                 else 0.)
        | _ -> None
    
    let coerceStringInput = 
        function 
        | IntValue i -> Some(i.ToString(CultureInfo.InvariantCulture))
        | FloatValue f -> Some(f.ToString(CultureInfo.InvariantCulture))
        | StringValue s -> Some s
        | BooleanValue b -> 
            Some(if b then "true"
                 else "false")
        | _ -> None
    
    let coerceBoolInput = 
        function 
        | IntValue i -> 
            Some(if i = 0 then false
                 else true)
        | FloatValue f -> 
            Some(if f = 0. then false
                 else true)
        | StringValue s -> 
            match Boolean.TryParse(s) with
            | true, i -> Some i
            | false, _ -> None
        | BooleanValue b -> Some b
        | _ -> None
    
    let private coerceIdInput input : 't option= 
        match input with
        | IntValue i -> Some(downcast Convert.ChangeType(i, typeof<'t>)) 
        | StringValue s -> Some(downcast Convert.ChangeType(s, typeof<'t>))
        | _ -> None
    
    let private coerceUriInput = 
        function 
        | StringValue s -> 
            match Uri.TryCreate(s, UriKind.RelativeOrAbsolute) with
            | true, uri -> Some uri
            | false, _ -> None
        | _ -> None
    
    let private coerceDateInput = 
        function 
        | StringValue s -> 
            match DateTime.TryParse(s) with
            | true, date -> Some date
            | false, _ -> None
        | _ -> None
    
    let Nullable(innerDef : #TypeDef<'Val>) : NullableDefinition<'Val> = { OfType = innerDef }
    let ListOf(innerDef : #TypeDef<'Val>) : ListOfDefinition<'Val, 'Seq> = { OfType = innerDef }
    
    let (|Scalar|_|) (tdef : TypeDef) = 
        match tdef with
        | :? ScalarDef as x -> Some x
        | _ -> None
    
    let (|Object|_|) (tdef : TypeDef) = 
        match tdef with
        | :? ObjectDef as x -> Some x
        | _ -> None
    
    let (|Interface|_|) (tdef : TypeDef) = 
        match tdef with
        | :? InterfaceDef as x -> Some x
        | _ -> None
    
    let (|Union|_|) (tdef : TypeDef) = 
        match tdef with
        | :? UnionDef as x -> Some x
        | _ -> None
    
    let (|Enum|_|) (tdef : TypeDef) = 
        match tdef with
        | :? EnumDef as x -> Some x
        | _ -> None
    
    let (|InputObject|_|) (tdef : TypeDef) = 
        match tdef with
        | :? InputObjectDef as x -> Some x
        | _ -> None
    
    let (|List|_|) (tdef : TypeDef) = 
        match tdef with
        | :? ListOfDef as x -> Some x.OfType
        | _ -> None
    
    let (|Nullable|_|) (tdef : TypeDef) = 
        match tdef with
        | :? NullableDef as x -> Some x.OfType
        | _ -> None
    
    let (|NonNull|_|) (tdef : TypeDef) = 
        match tdef with
        | :? NullableDef -> None
        | other -> Some other
    
    let (|Input|_|) (tdef : TypeDef) = 
        match tdef with
        | :? InputDef as i -> Some i
        | _ -> None
    
    let (|Output|_|) (tdef : TypeDef) = 
        match tdef with
        | :? OutputDef as o -> Some o
        | _ -> None
    
    let (|Leaf|_|) (tdef : TypeDef) = 
        match tdef with
        | :? LeafDef as ldef -> Some ldef
        | _ -> None
    
    let (|Composite|_|) (tdef : TypeDef) = 
        match tdef with
        | :? ObjectDef | :? InterfaceDef | :? UnionDef -> Some tdef
        | _ -> None
    
    let (|Abstract|_|) (tdef : TypeDef) = 
        match tdef with
        | :? InterfaceDef | :? UnionDef -> Some(tdef :?> AbstractDef)
        | _ -> None
    
    let rec private named (tdef : TypeDef) = 
        match tdef with
        | :? NamedDef as n -> Some n
        | Nullable inner -> named inner
        | List inner -> named inner
        | _ -> None
    
    let rec (|Named|_|) (tdef : TypeDef) = named tdef
    
    let private ignoreInputResolve (_ : unit) (input : 'T) = ()
    
    let variableOrElse other variables = 
        function 
        | Variable variableName -> Map.tryFind variableName variables |> Option.toObj
        | value -> other value
    
    /// GraphQL type of int
    let Int : ScalarDefinition<int> = 
        { Name = "Int"
          Description = 
              Some 
                  "The `Int` scalar type represents non-fractional signed whole numeric values. Int can represent values between -(2^31) and 2^31 - 1."
          CoerceInput = coerceIntInput
          CoerceValue = coerceIntValue
          CoerceOutput = coerceIntOuput }
    
    /// GraphQL type of boolean
    let Boolean : ScalarDefinition<bool> = 
        { Name = "Boolean"
          Description = Some "The `Boolean` scalar type represents `true` or `false`."
          CoerceInput = coerceBoolInput
          CoerceValue = coerceBoolValue
          CoerceOutput = coerceBoolOuput }
    
    /// GraphQL type of float
    let Float : ScalarDefinition<double> = 
        { Name = "Float"
          Description = 
              Some 
                  "The `Float` scalar type represents signed double-precision fractional values as specified by [IEEE 754](http://en.wikipedia.org/wiki/IEEE_floating_point)."
          CoerceInput = coerceFloatInput
          CoerceValue = coerceFloatValue
          CoerceOutput = coerceFloatOuput }
    
    /// GraphQL type of string
    let String : ScalarDefinition<string> = 
        { Name = "String"
          Description = 
              Some 
                  "The `String` scalar type represents textual data, represented as UTF-8 character sequences. The String type is most often used by GraphQL to represent free-form human-readable text."
          CoerceInput = coerceStringInput
          CoerceValue = coerceStringValue
          CoerceOutput = coerceStringOuput }
    
    /// GraphQL type for custom identifier
    let ID<'Val> : ScalarDefinition<'Val> = 
        { Name = "ID"
          Description = 
              Some 
                  "The `ID` scalar type represents a unique identifier, often used to refetch an object or as key for a cache. The ID type appears in a JSON response as a String; however, it is not intended to be human-readable. When expected as an input type, any string (such as `\"4\"`) or integer (such as `4`) input value will be accepted as an ID."
          CoerceInput = coerceIdInput
          CoerceValue = coerceIDValue
          CoerceOutput = coerceStringOuput }
    
    /// GraphQL type for System.Uri
    let Uri : ScalarDefinition<Uri> = 
        { Name = "URI"
          Description = 
              Some 
                  "The `URI` scalar type represents a string resource identifier compatible with URI standard. The URI type appears in a JSON response as a String."
          CoerceInput = coerceUriInput
          CoerceValue = coerceUriValue
          CoerceOutput = coerceUriOutput }
    
    /// GraphQL type for System.DateTime
    let Date : ScalarDefinition<DateTime> = 
        { Name = "Date"
          Description = 
              Some 
                  "The `Date` scalar type represents a Date value with Time component. The Date type appears in a JSON response as a String representation compatible with ISO-8601 format."
          CoerceInput = coerceDateInput
          CoerceValue = coerceDateValue
          CoerceOutput = coerceDateOutput }
    
    let IncludeDirective : DirectiveDef = 
        { Name = "include"
          Description = 
              Some "Directs the executor to include this field or fragment only when the `if` argument is true."
          Locations = 
              DirectiveLocation.FIELD ||| DirectiveLocation.FRAGMENT_SPREAD ||| DirectiveLocation.INLINE_FRAGMENT
          Args = 
              [| { InputFieldDefinition.Name = "if"
                   Description = Some "Included when true."
                   TypeDef = Boolean
                   DefaultValue = None
                   ExecuteInput = 
                       variableOrElse (coerceBoolInput
                                       >> Option.map box
                                       >> Option.toObj) } |] }
    
    let SkipDirective : DirectiveDef = 
        { Name = "skip"
          Description = Some "Directs the executor to skip this field or fragment when the `if` argument is true."
          Locations = 
              DirectiveLocation.FIELD ||| DirectiveLocation.FRAGMENT_SPREAD ||| DirectiveLocation.INLINE_FRAGMENT
          Args = 
              [| { InputFieldDefinition.Name = "if"
                   Description = Some "Skipped when true."
                   TypeDef = Boolean
                   DefaultValue = None
                   ExecuteInput = 
                       variableOrElse (coerceBoolInput
                                       >> Option.map box
                                       >> Option.toObj) } |] }
    
    let matchParameters (methodInfo : MethodInfo) (ctx : ResolveFieldContext) = 
        methodInfo.GetParameters() |> Array.map (fun param -> ctx.Arg<obj>(param.Name))
    let inline strip (fn : 'In -> 'Out) : obj -> obj = fun i -> upcast fn (i :?> 'In)
    
    let boxMeth =
        let operatorsType = Type.GetType("Microsoft.FSharp.Core.Operators, FSharp.Core", true).GetTypeInfo()
        operatorsType.GetDeclaredMethod("Box").GetGenericMethodDefinition()
        
    let unboxMeth =
        let operatorsType = Type.GetType("Microsoft.FSharp.Core.Operators, FSharp.Core", true).GetTypeInfo()
        operatorsType.GetDeclaredMethod("Unbox").GetGenericMethodDefinition()

    let boxifyExpr (inType: Type) (outType: Type) (expr : Expr) : Expr<ResolveFieldContext -> obj -> obj> = 
        let ctxVar = Var("ctx", typeof<ResolveFieldContext>)
        let valVar = Var("value", typeof<obj>)
        (*
            This is an equivalent of:
                
                <@ fun (ctx: ResolveFieldContext) (value: obj) : obj ->
                       let f: (ResolveFieldContext->'InType->'OutType) = %expr
                       in upcast f ctx (downcast value)  @>
        *)
        Expr.Lambda(ctxVar,
            Expr.Lambda(valVar, 
                Expr.Call(boxMeth.MakeGenericMethod(outType), 
                    [ Expr.Application(
                        Expr.Application(expr, Expr.Var(ctxVar)), 
                        Expr.Call(unboxMeth.MakeGenericMethod(inType), [ Expr.Var(valVar) ])) ])))
        |> Expr.Cast
    
    let inline private asyncBox (a : Async<'Val>) : Async<obj> = async { let! res = a
                                                                         return box res }

    let asyncBoxMeth =
        let operatorsType = Type.GetType("FSharp.Data.GraphQL.Types.SchemaDefinitions, FSharp.Data.GraphQL.Shared", true).GetTypeInfo()
        operatorsType.GetDeclaredMethod("asyncBox").GetGenericMethodDefinition()

    let boxifyExprAsync (inType: Type) (outType: Type) (expr : Expr) : Expr<ResolveFieldContext -> obj -> Async<obj>> = 
        let ctxVar = Var("ctx", typeof<ResolveFieldContext>)
        let valVar = Var("value", typeof<obj>)
        (*
            This is an equivalent of:
                
                <@ fun (ctx: ResolveFieldContext) (value: obj) : obj ->
                       let f: (ResolveFieldContext->'InType->'OutType) = %expr
                       in async {
                           let! res = f ctx (downcast value)
                           return upcast res } @>
        *)
        Expr.Lambda(ctxVar,
            Expr.Lambda(valVar, 
                Expr.Call(asyncBoxMeth.MakeGenericMethod(outType), 
                    [ Expr.Application(
                        Expr.Application(expr, Expr.Var(ctxVar)), 
                        Expr.Call(unboxMeth.MakeGenericMethod(inType), [ Expr.Var(valVar) ])) ])))
        |> Expr.Cast

    let private genMethodResolve<'Val, 'Res> (typeInfo: TypeInfo) (methodInfo: MethodInfo) = 
        let argInfo = typeof<ResolveFieldContext>.GetTypeInfo().GetDeclaredMethod("Arg")
        let valueVar = Var("value", typeof<'Val>)
        let ctxVar = Var("ctx", typeof<ResolveFieldContext>)
        let argExpr (arg : ParameterInfo) = 
            Expr.Call(Expr.Var(ctxVar), argInfo.MakeGenericMethod(arg.ParameterType), [ Expr.Value(arg.Name) ])        
        let args = 
            methodInfo.GetParameters()
            |> Array.map argExpr
            |> Array.toList        
        let expr = 
            Expr.Lambda
                (ctxVar, Expr<'Val -> 'Res>.Lambda(valueVar, Expr.Call(Expr.Var(valueVar), methodInfo, args)))
        Sync(typeof<'Val>, typeof<'Res>, expr)

    let private genPropertyResolve<'Val, 'Res> (typeInfo: TypeInfo) property = 
        let valueVar = Var("value", typeof<'Val>)
        let ctxVar = Var("ctx", typeof<ResolveFieldContext>)
        let expr = 
            Expr.Lambda
                (ctxVar, 
                 Expr<'Val -> 'Res>.Lambda(valueVar, Expr.PropertyGet(Expr.Var(valueVar), property)))
        Sync(typeof<'Val>, typeof<'Res>, expr)
            
    let defaultResolve<'Val, 'Res> (fieldName : string) : Resolve = 
        let typeInfo = typeof<'Val>.GetTypeInfo()
        let property = typeInfo.GetDeclaredProperty(fieldName, ignoreCase = true)
        match property with
        | null -> 
            let methodInfo = typeInfo.GetDeclaredMethod(fieldName, ignoreCase = true)
            genMethodResolve<'Val, 'Res> typeInfo methodInfo            
        | p -> genPropertyResolve<'Val, 'Res> typeInfo p
    
    type Define private () = 
        
        static member Scalar(name : string, coerceInput : Value -> 'T option, coerceOutput : 'T -> Value option, 
                             coerceValue : obj -> 'T option, ?description : string) : ScalarDefinition<'T> = 
            { Name = name
              Description = description
              CoerceInput = coerceInput
              CoerceOutput = coerceOutput
              CoerceValue = coerceValue }
        
        /// GraphQL type for user defined enums
        static member Enum(name : string, options : EnumValue<'Val> list, ?description : string) : EnumDefinition<'Val> = 
            { Name = name
              Description = description
              Options = options |> List.toArray }
        
        /// Single enum option to be used as argument in <see cref="Schema.Enum"/>
        static member EnumValue(name : string, value : 'Val, ?description : string, ?deprecationReason : string) : EnumValue<'Val> = 
            { Name = name
              Description = description
              Value = value
              DeprecationReason = deprecationReason }
        
        /// GraphQL custom object type
        static member Object(name : string, fieldsFn : unit -> FieldDef<'Val> list, ?description : string, 
                             ?interfaces : InterfaceDef list, ?isTypeOf : obj -> bool) : ObjectDef<'Val> = 
            upcast { ObjectDefinition.Name = name
                     Description = description
                     FieldsFn = 
                         lazy (fieldsFn()
                               |> List.map (fun f -> f.Name, f)
                               |> Map.ofList)
                     Implements = defaultArg (Option.map List.toArray interfaces) [||]
                     IsTypeOf = isTypeOf }
        
        /// GraphQL custom object type
        static member Object(name : string, fields : FieldDef<'Val> list, ?description : string, 
                             ?interfaces : InterfaceDef list, ?isTypeOf : obj -> bool) : ObjectDef<'Val> = 
            upcast { ObjectDefinition.Name = name
                     Description = description
                     FieldsFn = 
                         lazy (fields
                               |> List.map (fun f -> f.Name, f)
                               |> Map.ofList)
                     Implements = defaultArg (Option.map List.toArray interfaces) [||]
                     IsTypeOf = isTypeOf }
        
        /// GraphQL custom input object type
        static member InputObject(name : string, fieldsFn : unit -> InputFieldDef list, ?description : string) : InputObjectDefinition<'Out> = 
            { Name = name
              FieldsFn = fun () -> fieldsFn() |> List.toArray
              Description = description }
        
        /// GraphQL custom input object type
        static member InputObject(name : string, fields : InputFieldDef list, ?description : string) : InputObjectDefinition<'Out> = 
            { Name = name
              Description = description
              FieldsFn = fun () -> fields |> List.toArray }
        
        /// Single field defined inside either object types or interfaces
        static member AutoField(name : string, typedef : #OutputDef<'Res>, ?description: string, ?args: InputFieldDef list, ?deprecationReason: string) : FieldDef<'Val> = 
            upcast { FieldDefinition.Name = name
                     Description = description
                     TypeDef = typedef
                     Resolve = defaultResolve<'Val, 'Res> name
                     Args = defaultArg args [] |> Array.ofList
                     DeprecationReason = deprecationReason
                     Execute = Unchecked.defaultof<ExecuteField> }
        
        /// Single field defined inside either object types or interfaces
        static member Field(name : string, typedef : #OutputDef<'Res>) : FieldDef<'Val> = 
            upcast { FieldDefinition.Name = name
                     Description = None
                     TypeDef = typedef
                     Resolve = Undefined
                     Args = [||]
                     DeprecationReason = None
                     Execute = Unchecked.defaultof<ExecuteField> }
        
        /// Single field defined inside either object types or interfaces
        static member Field(name : string, typedef : #OutputDef<'Res>, 
                            [<ReflectedDefinition>] resolve : Expr<ResolveFieldContext -> 'Val -> 'Res>) : FieldDef<'Val> = 
            upcast { FieldDefinition.Name = name
                     Description = None
                     TypeDef = typedef
                     Resolve = Sync(typeof<'Val>, typeof<'Res>, resolve)
                     Args = [||]
                     DeprecationReason = None
                     Execute = Unchecked.defaultof<ExecuteField> }
        
        /// Single field defined inside either object types or interfaces
        static member Field(name : string, typedef : #OutputDef<'Res>, description : string, 
                            [<ReflectedDefinition>] resolve : Expr<ResolveFieldContext -> 'Val -> 'Res>) : FieldDef<'Val> = 
            upcast { FieldDefinition.Name = name
                     Description = Some description
                     TypeDef = typedef
                     Resolve = Sync(typeof<'Val>, typeof<'Res>, resolve)
                     Args = [||]
                     DeprecationReason = None
                     Execute = Unchecked.defaultof<ExecuteField> }
        
        /// Single field defined inside either object types or interfaces
        static member Field(name : string, typedef : #OutputDef<'Res>, description : string, args : InputFieldDef list, 
                            [<ReflectedDefinition>] resolve : Expr<ResolveFieldContext -> 'Val -> 'Res>) : FieldDef<'Val> = 
            upcast { FieldDefinition.Name = name
                     Description = Some description
                     TypeDef = typedef
                     Resolve = Sync(typeof<'Val>, typeof<'Res>, resolve)
                     Args = args |> List.toArray
                     DeprecationReason = None
                     Execute = Unchecked.defaultof<ExecuteField> }
        
        /// Single field defined inside either object types or interfaces
        static member Field(name : string, typedef : #OutputDef<'Res>, description : string, args : InputFieldDef list, 
                            [<ReflectedDefinition>] resolve : Expr<ResolveFieldContext -> 'Val -> 'Res>, 
                            deprecationReason : string) : FieldDef<'Val> = 
            upcast { FieldDefinition.Name = name
                     Description = Some description
                     TypeDef = typedef
                     Resolve = Sync(typeof<'Val>, typeof<'Res>, resolve)
                     Args = args |> List.toArray
                     DeprecationReason = Some deprecationReason
                     Execute = Unchecked.defaultof<ExecuteField> }
        
        /// Single field defined inside either object types or interfaces
        static member AsyncField(name : string, typedef : #OutputDef<'Res>, 
                                 [<ReflectedDefinition>] resolve : Expr<ResolveFieldContext -> 'Val -> Async<'Res>>) : FieldDef<'Val> = 
            upcast { FieldDefinition.Name = name
                     Description = None
                     TypeDef = typedef
                     Resolve = Async(typeof<'Val>, typeof<'Res>, resolve)
                     Args = [||]
                     DeprecationReason = None
                     Execute = Unchecked.defaultof<ExecuteField> }
        
        /// Single field defined inside either object types or interfaces
        static member AsyncField(name : string, typedef : #OutputDef<'Res>, description : string, 
                                 [<ReflectedDefinition>] resolve : Expr<ResolveFieldContext -> 'Val -> Async<'Res>>) : FieldDef<'Val> = 
            upcast { FieldDefinition.Name = name
                     Description = Some description
                     TypeDef = typedef
                     Resolve = Async(typeof<'Val>, typeof<'Res>, resolve)
                     Args = [||]
                     DeprecationReason = None
                     Execute = Unchecked.defaultof<ExecuteField> }
        
        /// Single field defined inside either object types or interfaces
        static member AsyncField(name : string, typedef : #OutputDef<'Res>, description : string, 
                                 args : InputFieldDef list, 
                                 [<ReflectedDefinition>] resolve : Expr<ResolveFieldContext -> 'Val -> Async<'Res>>) : FieldDef<'Val> = 
            upcast { FieldDefinition.Name = name
                     Description = Some description
                     TypeDef = typedef
                     Resolve = Async(typeof<'Val>, typeof<'Res>, resolve)
                     Args = args |> List.toArray
                     DeprecationReason = None
                     Execute = Unchecked.defaultof<ExecuteField> }
        
        /// Single field defined inside either object types or interfaces
        static member AsyncField(name : string, typedef : #OutputDef<'Res>, description : string, 
                                 args : InputFieldDef list, 
                                 [<ReflectedDefinition>] resolve : Expr<ResolveFieldContext -> 'Val -> Async<'Res>>, 
                                 deprecationReason : string) : FieldDef<'Val> = 
            upcast { FieldDefinition.Name = name
                     Description = Some description
                     TypeDef = typedef
                     Resolve = Async(typeof<'Val>, typeof<'Res>, resolve)
                     Args = args |> List.toArray
                     DeprecationReason = Some deprecationReason
                     Execute = Unchecked.defaultof<ExecuteField> }
        
        static member Input(name : string, schema : #InputDef<'In>, ?defaultValue : 'In, ?description : string) : InputFieldDef = 
            upcast { InputFieldDefinition.Name = name
                     Description = description
                     TypeDef = schema
                     DefaultValue = defaultValue
                     ExecuteInput = Unchecked.defaultof<ExecuteInput> }
        
        /// GraphQL custom interface type. It's needs to be implemented object types and should not be used alone.
        static member Interface(name : string, fieldsFn : unit -> FieldDef<'Val> list, ?description : string, 
                                ?resolveType : obj -> ObjectDef) : InterfaceDef<'Val> = 
            upcast { InterfaceDefinition.Name = name
                     Description = description
                     FieldsFn = fun () -> fieldsFn() |> List.toArray
                     ResolveType = resolveType }
        
        /// GraphQL custom interface type. It's needs to be implemented object types and should not be used alone.
        static member Interface(name : string, fields : FieldDef<'Val> list, ?description : string, 
                                ?resolveType : obj -> ObjectDef) : InterfaceDef<'Val> = 
            upcast { InterfaceDefinition.Name = name
                     Description = description
                     FieldsFn = fun () -> fields |> List.toArray
                     ResolveType = resolveType }
        
        /// GraphQL custom union type, materialized as one of the types defined. It can be used as interface/object type field.
        static member Union(name : string, options : ObjectDef list, resolveValue : 'In -> 'Out, 
                            ?resolveType : 'In -> ObjectDef, ?description : string) : UnionDef<'In> = 
            upcast { UnionDefinition.Name = name
                     Description = description
                     Options = options |> List.toArray
                     ResolveType = resolveType
                     ResolveValue = resolveValue }
