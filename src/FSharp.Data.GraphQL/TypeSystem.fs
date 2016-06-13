/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
namespace FSharp.Data.GraphQL.Types

open System
open FSharp.Data.GraphQL.Ast

[<Flags; Serializable>]
type DirectiveLocation = 
    | QUERY = 1
    | MUTATION = 2
    | SUBSCRIPTION = 4
    | FIELD = 8
    | FRAGMENT_DEFINITION = 16
    | FRAGMENT_SPREAD = 32
    | INLINE_FRAGMENT = 64

module Introspection = 
    [<Serializable>]
    type TypeKind = 
        | SCALAR = 1
        | OBJECT  = 2
        | INTERFACE = 3
        | UNION = 4
        | ENUM = 5
        | INPUT_OBJECT = 6
        | LIST = 7
        | NON_NULL = 8
    
    [<Serializable>]
    type IntrospectionDirective = 
        { Name : string
          Description : string option
          Locations : DirectiveLocation []
          Args : IntrospectionInputVal [] }
    
    and [<Serializable>] IntrospectionType = 
        { Kind : TypeKind
          Name : string
          Description : string option
          Fields : IntrospectionField [] option
          Interfaces : IntrospectionTypeRef [] option
          PossibleTypes : IntrospectionTypeRef [] option
          EnumValues : IntrospectionEnumVal [] option
          InputFields : IntrospectionInputVal [] option
          OfType : IntrospectionTypeRef option }
        static member Scalar(name: string, description: string option) = 
            { Kind = TypeKind.SCALAR
              Name = name
              Description = description
              Fields = None
              Interfaces = None
              PossibleTypes = None
              EnumValues = None
              InputFields = None
              OfType = None }
        static member Object(name: string, description: string option, fields: IntrospectionField [], interfaces: IntrospectionTypeRef []) = 
            { Kind = TypeKind.OBJECT
              Name = name
              Description = description
              Fields = Some fields
              Interfaces = Some interfaces
              PossibleTypes = None
              EnumValues = None
              InputFields = None
              OfType = None }
        static member InputObject(name: string, description: string option, inputFields: IntrospectionInputVal []) = 
            { Kind = TypeKind.INPUT_OBJECT
              Name = name
              Description = description
              Fields = None
              Interfaces = None
              PossibleTypes = None
              EnumValues = None
              InputFields = Some inputFields
              OfType = None }
        static member Union(name: string, description: string option, possibleTypes: IntrospectionTypeRef []) = 
            { Kind = TypeKind.UNION
              Name = name
              Description = description
              Fields = None
              Interfaces = None
              PossibleTypes = Some possibleTypes
              EnumValues = None
              InputFields = None
              OfType = None }
        static member Enum(name: string, description: string option, enumValues: IntrospectionEnumVal []) = 
            { Kind = TypeKind.ENUM
              Name = name
              Description = description
              Fields = None
              Interfaces = None
              PossibleTypes = None
              EnumValues = Some enumValues
              InputFields = None
              OfType = None }
        static member Interface(name: string, description: string option, fields: IntrospectionField [], possibleTypes: IntrospectionTypeRef []) = 
            { Kind = TypeKind.INTERFACE
              Name = name
              Description = description
              Fields = Some fields
              Interfaces = None
              PossibleTypes = Some possibleTypes
              EnumValues = None
              InputFields = None
              OfType = None }
    
    and [<Serializable>] IntrospectionTypeRef = 
        { Kind : TypeKind
          Name : string option
          Description : string option
          OfType : IntrospectionTypeRef option }
        static member List(inner: IntrospectionTypeRef) = { Kind = TypeKind.LIST; Name = None; Description = None; OfType = Some inner }
        static member NonNull(inner: IntrospectionTypeRef) = { Kind = TypeKind.NON_NULL; Name = None; Description = None; OfType = Some inner }
        static member Named(inner: IntrospectionType) = { Kind = inner.Kind; Name = Some inner.Name; Description = inner.Description; OfType = None }
    
    and [<Serializable>] IntrospectionInputVal = 
        { Name : string
          Description : string option
          Type : IntrospectionTypeRef
          DefaultValue : string option }
    
    and [<Serializable>] IntrospectionEnumVal = 
        { Name : string
          Description : string option
          IsDeprecated : bool
          DeprecationReason : string option }
    
    and [<Serializable>] IntrospectionField = 
        { Name : string
          Description : string option
          Args : IntrospectionInputVal []
          Type : IntrospectionTypeRef
          IsDeprecated : bool
          DeprecationReason : string option }
    
    and [<Serializable>] IntrospectionSchema = 
        { QueryType : IntrospectionTypeRef
          MutationType : IntrospectionTypeRef option
          SubscriptionType : IntrospectionTypeRef option
          Types : IntrospectionType []
          Directives : IntrospectionDirective [] }

type GraphQLException(msg) = 
    inherit Exception(msg)

type ISchema = 
    interface
        inherit seq<NamedDef>
        abstract TypeMap : Map<string, NamedDef>
        abstract Query : ObjectDef
        abstract Mutation : ObjectDef option
        abstract Directives : DirectiveDef list
        abstract TryFindType : string -> NamedDef option
        abstract GetPossibleTypes : AbstractDef -> ObjectDef list
        abstract IsPossibleType : AbstractDef -> ObjectDef -> bool
        abstract Introspected : Introspection.IntrospectionSchema
    end

// 3.1 Types
and TypeDef = 
    interface
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
    end

and NamedDef = 
    interface
        inherit TypeDef
        abstract Name : string
    end

and FieldDef = 
    interface
        abstract Name : string
        abstract Description : string option
        abstract DeprecationReason : string option
        abstract Type : OutputDef
        abstract Args : InputFieldDef list
        abstract Resolve : ResolveFieldContext -> obj -> Async<obj>
        inherit IEquatable<FieldDef>
    end

and FieldDef<'Val> = 
    interface
        abstract Resolve : ResolveFieldContext -> 'Val -> Async<obj>
        inherit FieldDef
    end

and ResolveFieldContext = 
    { FieldName : string
      Fields : Field list
      FieldType : FieldDef
      ReturnType : TypeDef
      ParentType : ObjectDef
      Schema : ISchema
      Args : Map<string, obj>
      Operation : OperationDefinition
      Fragments : FragmentDefinition list
      Variables : Map<string, obj> }
    member x.Arg(name : string) : 't option = 
        match Map.tryFind name x.Args with
        | Some o -> Some(o :?> 't)
        | None -> None

and ScalarDef = 
    interface
        abstract Name : string
        abstract Description : string option
        abstract CoerceInput : Value -> obj
        abstract CoerceOutput : obj -> Value option
        abstract CoerceValue : obj -> obj option
        inherit IEquatable<ScalarDef>
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
    
    interface ScalarDef with
        member x.Name = x.Name
        member x.Description = x.Description
        member x.CoerceInput input = upcast (x.CoerceInput input)
        member x.CoerceOutput output = x.CoerceOutput(output :?> 'Val)
        member x.CoerceValue value = (x.CoerceValue value) |> Option.map box
    
    interface InputDef<'Val>
    interface OutputDef<'Val>
    interface LeafDef
    
    interface NamedDef with
        member x.Name = x.Name
    
    interface IEquatable<ScalarDef> with
        member x.Equals s = x.Name = s.Name
    
    override x.Equals y = 
        match y with
        | :? ScalarDef as s -> (x :> IEquatable<ScalarDef>).Equals(s)
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
        abstract Options : EnumVal list
        inherit TypeDef
        inherit InputDef
        inherit OutputDef
        inherit LeafDef
        inherit NamedDef
    end

and EnumDef<'Val> = 
    interface
        abstract Options : EnumValue<'Val> list
        inherit EnumDef
        inherit TypeDef<'Val>
        inherit InputDef<'Val>
        inherit OutputDef<'Val>
    end

and EnumDefinition<'Val> = 
    { Name : string
      Description : string option
      Options : EnumValue<'Val> list }
    
    interface EnumDef<'Val> with
        member x.Options = x.Options
    
    interface EnumDef with
        member x.Name = x.Name
        member x.Description = x.Description
        member x.Options = 
            x.Options
            |> Seq.ofList
            |> Seq.cast<EnumVal>
            |> Seq.toList
    
    interface NamedDef with
        member x.Name = x.Name
    
    override x.ToString() = sprintf "enum %s {\n    %s\n}" x.Name (String.Join("\n    ", x.Options))

/// 3.1.2 Objects
and ObjectDef = 
    interface
        abstract Name : string
        abstract Description : string option
        abstract Fields : FieldDef list
        abstract Implements : InterfaceDef list
        abstract IsTypeOf : (obj -> bool) option
        inherit TypeDef
        inherit NamedDef
        inherit OutputDef
        inherit CompositeDef
        inherit IEquatable<ObjectDef>
    end

and ObjectDef<'Val> = 
    interface
        abstract Fields : FieldDef<'Val> list
        inherit ObjectDef
        inherit TypeDef<'Val>
        inherit OutputDef<'Val>
    end

and [<CustomEquality; NoComparison>] ObjectDefinition<'Val> = 
    { Name : string
      Description : string option
      FieldsFn : unit -> FieldDef<'Val> list
      Implements : InterfaceDef list
      IsTypeOf : (obj -> bool) option }
    
    interface ObjectDef with
        member x.Name = x.Name
        member x.Description = x.Description
        
        member x.Fields = 
            x.FieldsFn()
            |> Seq.ofList
            |> Seq.cast<FieldDef>
            |> List.ofSeq
        
        member x.Implements = x.Implements
        member x.IsTypeOf = x.IsTypeOf
    
    interface ObjectDef<'Val> with
        member x.Fields = x.FieldsFn()
    
    interface NamedDef with
        member x.Name = x.Name
    
    interface IEquatable<ObjectDef> with
        member x.Equals f = x.Name = f.Name
    
    override x.Equals y = 
        match y with
        | :? ObjectDef as f -> (x :> IEquatable<ObjectDef>).Equals(f)
        | _ -> false
    
    override x.GetHashCode() = 
        let mutable hash = x.Name.GetHashCode()
        hash
    
    override x.ToString() = 
        let sb = System.Text.StringBuilder("type ")
        sb.Append(x.Name) |> ignore
        if not (List.isEmpty x.Implements) then 
            sb.Append(" implements ").Append(String.Join(", ", x.Implements |> List.map (fun i -> i.Name))) |> ignore
        sb.Append("{") |> ignore
        //x.Fields |> List.iter (fun f -> sb.Append("\n    ").Append(f.ToString()) |> ignore)
        sb.Append("\n}").ToString()

and [<CustomEquality; NoComparison>] FieldDefinition<'Val, 'Res> = 
    { Name : string
      Description : string option
      Type : OutputDef<'Res>
      Resolve : ResolveFieldContext -> 'Val -> Async<'Res>
      Args : InputFieldDef list
      DeprecationReason : string option }
    
    interface FieldDef with
        member x.Name = x.Name
        member x.Description = x.Description
        member x.DeprecationReason = x.DeprecationReason
        member x.Type = x.Type :> OutputDef
        member x.Args = x.Args
        member x.Resolve (ctx : ResolveFieldContext) (value : obj) : Async<obj> = async { let! result = x.Resolve ctx 
                                                                                                            (value :?> 'Val)
                                                                                          return upcast result }
    
    interface FieldDef<'Val> with
        member x.Resolve (ctx : ResolveFieldContext) (value : 'Val) : Async<obj> = async { let! result = x.Resolve ctx 
                                                                                                             value
                                                                                           return upcast result }
    
    interface IEquatable<FieldDef> with
        member x.Equals f = x.Name = f.Name && x.Type :> OutputDef = f.Type && x.Args = f.Args
    
    override x.Equals y = 
        match y with
        | :? FieldDef as f -> (x :> IEquatable<FieldDef>).Equals(f)
        | _ -> false
    
    override x.GetHashCode() = 
        let mutable hash = x.Name.GetHashCode()
        hash <- (hash * 397) ^^^ (x.Type.GetHashCode())
        hash <- (hash * 397) ^^^ (x.Args.GetHashCode())
        hash
    
    override x.ToString() = 
        let mutable s = x.Name + ": " + x.Type.ToString()
        if not (List.isEmpty x.Args) then s <- "(" + String.Join(", ", x.Args) + ")"
        s

/// 3.1.3 Interfaces
and InterfaceDef = 
    interface
        abstract Name : string
        abstract Description : string option
        abstract Fields : FieldDef list
        abstract ResolveType : (obj -> ObjectDef) option
        inherit TypeDef
        inherit OutputDef
        inherit CompositeDef
        inherit AbstractDef
        inherit NamedDef
        inherit IEquatable<InterfaceDef>
    end

and InterfaceDef<'Val> = 
    interface
        abstract Fields : FieldDef<'Val> list
        inherit TypeDef<'Val>
        inherit OutputDef<'Val>
        inherit InterfaceDef
    end

and [<CustomEquality; NoComparison>] InterfaceDefinition<'Val> = 
    { Name : string
      Description : string option
      FieldsFn : unit -> FieldDef<'Val> list
      ResolveType : (obj -> ObjectDef) option }
    
    interface InterfaceDef with
        member x.Name = x.Name
        member x.Description = x.Description
        member x.Fields = x.FieldsFn() |> List.map (fun fdef -> upcast fdef)
        member x.ResolveType = x.ResolveType
    
    interface InterfaceDef<'Val> with
        member x.Fields = x.FieldsFn()
    
    interface NamedDef with
        member x.Name = x.Name
    
    interface IEquatable<InterfaceDef> with
        member x.Equals f = x.Name = f.Name
    
    override x.Equals y = 
        match y with
        | :? InterfaceDef as f -> (x :> IEquatable<InterfaceDef>).Equals(f)
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
        abstract Options : ObjectDef list
        abstract ResolveType : (obj -> ObjectDef) option
        abstract ResolveValue : obj -> obj
        inherit TypeDef
        inherit OutputDef
        inherit CompositeDef
        inherit AbstractDef
        inherit NamedDef
        inherit IEquatable<UnionDef>
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
      Options : ObjectDef list
      ResolveType : ('In -> ObjectDef) option
      ResolveValue : 'In -> 'Out }
    
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
    
    interface IEquatable<UnionDef> with
        member x.Equals f = x.Name = f.Name && x.Description = f.Description && x.Options = f.Options
    
    override x.Equals y = 
        match y with
        | :? InterfaceDef as f -> (x :> IEquatable<UnionDef>).Equals(f)
        | _ -> false
    
    override x.GetHashCode() = 
        let mutable hash = x.Name.GetHashCode()
        hash <- (hash * 397) ^^^ (x.Options.GetHashCode())
        hash
    
    override x.ToString() = "union " + x.Name + " = " + String.Join(" | ", x.Options |> List.map (fun o -> o.Name))

and ListOfDef = 
    interface
        abstract OfType : TypeDef
        inherit InputDef
        inherit OutputDef
    end

and ListOfDef<'Val> = 
    interface
        abstract OfType : TypeDef<'Val>
        inherit TypeDef<'Val seq>
        inherit InputDef<'Val seq>
        inherit OutputDef<'Val seq>
    end

and ListOfDefinition<'Val> = 
    { OfType : TypeDef<'Val> }
    
    interface ListOfDef with
        member x.OfType = upcast x.OfType
    
    interface ListOfDef<'Val> with
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
        abstract Fields : InputFieldDef list
        inherit NamedDef
        inherit InputDef
    end

/// 3.1.6 Input Objects
and InputObjectDefinition<'Val> = 
    { Name : string
      Description : string option
      FieldsFn : unit -> InputFieldDef list }
    
    interface InputObjectDef with
        member x.Name = x.Name
        member x.Description = x.Description
        member x.Fields = x.FieldsFn()
    
    interface TypeDef<'Val>
    interface InputDef<'Val>
    interface NamedDef with
        member x.Name = x.Name

and InputFieldDef = 
    interface
        abstract Name : string
        abstract Description : string option
        abstract Type : InputDef
        abstract DefaultValue : obj option
        inherit NamedDef
        inherit IEquatable<InputFieldDef>
    end

/// 3.1.2.1 Object Field Arguments
and [<CustomEquality; NoComparison>] InputFieldDefinition<'In> = 
    { Name : string
      Description : string option
      Type : InputDef<'In>
      DefaultValue : 'In option }
    
    interface InputFieldDef with
        member x.Name = x.Name
        member x.Description = x.Description
        member x.Type = upcast x.Type
        member x.DefaultValue = x.DefaultValue |> Option.map (fun x -> upcast x)
    
    interface NamedDef with
        member x.Name = x.Name
    
    interface IEquatable<InputFieldDef> with
        member x.Equals f = x.Name = f.Name && x.Type :> InputDef = f.Type
    
    override x.Equals y = 
        match y with
        | :? InputFieldDef as f -> (x :> IEquatable<InputFieldDef>).Equals(f)
        | _ -> false
    
    override x.GetHashCode() = 
        let mutable hash = x.Name.GetHashCode()
        hash <- (hash * 397) ^^^ (x.Type.GetHashCode())
        hash
    
    override x.ToString() = x.Name

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
      Args : InputFieldDef list }

[<AutoOpen>]
module SchemaDefinitions = 
    open System.Globalization
    open System.Reflection
    
    let internal coerceIntValue (x : obj) : int option = 
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
    
    let internal coerceFloatValue (x : obj) : double option = 
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
    
    let internal coerceBoolValue (x : obj) : bool option = 
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
    
    /// Check if provided obj value is an Option and extract its wrapped value as object if possible
    let (|Option|_|) (x : obj) = 
        if x = null then None
        else 
            let t = x.GetType()
            if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>> then 
                let _, fields = Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(x, t)
                Some(fields.[0])
            else None
    
    let internal coerceStringValue (x : obj) : string option = 
        match x with
        | null -> None
        | :? string as s -> Some s
        | :? bool as b -> 
            Some(if b then "true"
                 else "false")
        | Option o -> Some(o.ToString())
        | _ -> Some(x.ToString())
    
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
    
    let private coerceStringInput = 
        function 
        | IntValue i -> Some(i.ToString(CultureInfo.InvariantCulture))
        | FloatValue f -> Some(f.ToString(CultureInfo.InvariantCulture))
        | StringValue s -> Some s
        | BooleanValue b -> 
            Some(if b then "true"
                 else "false")
        | _ -> None
    
    let private coerceBoolInput = 
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
    
    let private coerceIdInput = 
        function 
        | IntValue i -> Some(i.ToString())
        | StringValue s -> Some s
        | _ -> None
    
    let Nullable(innerDef : #TypeDef<'Val>) : NullableDefinition<'Val> = { OfType = innerDef }
    let ListOf(innerDef : #TypeDef<'Val>) : ListOfDefinition<'Val> = { OfType = innerDef }
    
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
        | :? ScalarDef | :? EnumDef | :? InputObjectDef -> Some tdef
        | _ -> None
    
    let (|Output|_|) (tdef : TypeDef) = 
        match tdef with
        | :? ScalarDef | :? EnumDef | :? ObjectDef | :? InterfaceDef | :? UnionDef -> Some tdef
        | _ -> None
    
    let (|Leaf|_|) (tdef : TypeDef) = 
        match tdef with
        | :? ScalarDef | :? EnumDef -> Some tdef
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
    let ID : ScalarDefinition<string> = 
        { Name = "ID"
          Description = 
              Some 
                  "The `ID` scalar type represents a unique identifier, often used to refetch an object or as key for a cache. The ID type appears in a JSON response as a String; however, it is not intended to be human-readable. When expected as an input type, any string (such as `\"4\"`) or integer (such as `4`) input value will be accepted as an ID."
          CoerceInput = coerceIdInput
          CoerceValue = coerceStringValue
          CoerceOutput = coerceStringOuput }
    
    let IncludeDirective : DirectiveDef = 
        { Name = "include"
          Description = 
              Some "Directs the executor to include this field or fragment only when the `if` argument is true."
          Locations = 
              DirectiveLocation.FIELD ||| DirectiveLocation.FRAGMENT_SPREAD ||| DirectiveLocation.INLINE_FRAGMENT
          Args = 
              [ { Name = "if"
                  Description = Some "Included when true."
                  Type = Boolean
                  DefaultValue = None } ] }
    
    let SkipDirective : DirectiveDef = 
        { Name = "skip"
          Description = Some "Directs the executor to skip this field or fragment when the `if` argument is true."
          Locations = 
              DirectiveLocation.FIELD ||| DirectiveLocation.FRAGMENT_SPREAD ||| DirectiveLocation.INLINE_FRAGMENT
          Args = 
              [ { Name = "if"
                  Description = Some "Skipped when true."
                  Type = Boolean
                  DefaultValue = None } ] }
    
    let rec internal coerceAstValue (variables : Map<string, obj>) (value : Value) : obj = 
        match value with
        | IntValue i -> upcast i
        | StringValue s -> upcast s
        | FloatValue f -> upcast f
        | BooleanValue b -> upcast b
        | EnumValue e -> upcast e
        | ListValue values -> 
            let mapped = values |> List.map (coerceAstValue variables)
            upcast mapped
        | ObjectValue fields -> 
            let mapped = fields |> Map.map (fun k v -> coerceAstValue variables v)
            upcast mapped
        | Variable variable -> variables.[variable]
    
    let internal matchParameters (methodInfo : MethodInfo) (ctx : ResolveFieldContext) = 
        methodInfo.GetParameters() |> Array.map (fun param -> ctx.Arg<obj>(param.Name).Value)
    let inline strip (fn : 'In -> 'Out) : obj -> obj = fun i -> upcast fn (i :?> 'In)
    
    let internal defaultResolve<'Val, 'Res> (fieldName : string) : ResolveFieldContext -> 'Val -> Async<'Res> = 
        (fun ctx value -> 
        async { 
            if Object.Equals(value, null) then return Unchecked.defaultof<'Res>
            else 
                let t = value.GetType()
                let memberInfo = 
                    t.GetMember(fieldName, BindingFlags.IgnoreCase ||| BindingFlags.Public ||| BindingFlags.Instance)
                match memberInfo with
                | [||] -> 
                    return raise 
                               (GraphQLException
                                    (sprintf 
                                         "Default resolve function failed. Couldn't find member '%s' inside definition of type '%s'." 
                                         fieldName t.FullName))
                | found -> 
                    let result = 
                        match found.[0] with
                        | :? PropertyInfo as property -> property.GetValue(value, null)
                        | :? MethodInfo as methodInfo -> 
                            let parameters = matchParameters methodInfo ctx
                            methodInfo.Invoke(value, parameters)
                        | :? FieldInfo as field -> field.GetValue(value)
                    return result :?> 'Res
        })
    
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
              Options = options }
        
        /// Single enum option to be used as argument in <see cref="Schema.Enum"/>
        static member EnumValue(name : string, value : 'Val, ?description : string, ?deprecationReason : string) : EnumValue<'Val> = 
            { Name = name
              Description = description
              Value = value
              DeprecationReason = deprecationReason }
        
        /// GraphQL custom object type
        static member Object(name : string, fieldsFn : unit -> FieldDef<'Val> list, ?description : string, 
                             ?interfaces : InterfaceDef list, ?isTypeOf : obj -> bool) : ObjectDef<'Val> = 
            upcast { Name = name
                     Description = description
                     FieldsFn = fieldsFn
                     Implements = 
                         match interfaces with
                         | None -> []
                         | Some i -> i
                     IsTypeOf = isTypeOf }
        
        /// GraphQL custom object type
        static member Object(name : string, fields : FieldDef<'Val> list, ?description : string, 
                             ?interfaces : InterfaceDef list, ?isTypeOf : obj -> bool) : ObjectDef<'Val> = 
            upcast { Name = name
                     Description = description
                     FieldsFn = fun () -> fields
                     Implements = 
                         match interfaces with
                         | None -> []
                         | Some i -> i
                     IsTypeOf = isTypeOf }
        
        /// GraphQL custom input object type
        static member InputObject(name : string, fieldsFn : unit -> InputFieldDef list, ?description : string) : InputObjectDefinition<'Out> = 
            { Name = name
              FieldsFn = fieldsFn
              Description = description }
        
        /// GraphQL custom input object type
        static member InputObject(name : string, fields : InputFieldDef list, ?description : string) : InputObjectDefinition<'Out> = 
            { Name = name
              Description = description
              FieldsFn = fun () -> fields }
        
        /// Single field defined inside either object types or interfaces
        static member Field(name : string, typedef : #OutputDef<'Res>) : FieldDef<'Val> = 
            upcast { Name = name
                     Description = None
                     Type = typedef
                     Resolve = defaultResolve<'Val, 'Res> name
                     Args = []
                     DeprecationReason = None }
        
        /// Single field defined inside either object types or interfaces
        static member Field(name : string, typedef : #OutputDef<'Res>, resolve : ResolveFieldContext -> 'Val -> 'Res) : FieldDef<'Val> = 
            upcast { Name = name
                     Description = None
                     Type = typedef
                     Resolve = fun ctx value -> async { return resolve ctx value }
                     Args = []
                     DeprecationReason = None }
        
        /// Single field defined inside either object types or interfaces
        static member Field(name : string, typedef : #OutputDef<'Res>, description : string, 
                            resolve : ResolveFieldContext -> 'Val -> 'Res) : FieldDef<'Val> = 
            upcast { Name = name
                     Description = Some description
                     Type = typedef
                     Resolve = fun ctx value -> async { return resolve ctx value }
                     Args = []
                     DeprecationReason = None }
        
        /// Single field defined inside either object types or interfaces
        static member Field(name : string, typedef : #OutputDef<'Res>, description : string, args : InputFieldDef list, 
                            resolve : ResolveFieldContext -> 'Val -> 'Res) : FieldDef<'Val> = 
            upcast { Name = name
                     Description = Some description
                     Type = typedef
                     Resolve = fun ctx value -> async { return resolve ctx value }
                     Args = args
                     DeprecationReason = None }
        
        /// Single field defined inside either object types or interfaces
        static member Field(name : string, typedef : #OutputDef<'Res>, description : string, args : InputFieldDef list, 
                            resolve : ResolveFieldContext -> 'Val -> 'Res, deprecationReason : string) : FieldDef<'Val> = 
            upcast { Name = name
                     Description = Some description
                     Type = typedef
                     Resolve = fun ctx value -> async { return resolve ctx value }
                     Args = args
                     DeprecationReason = Some deprecationReason }
        
        /// Single field defined inside either object types or interfaces
        static member AsyncField(name : string, typedef : #OutputDef<'Res>, 
                                 resolve : ResolveFieldContext -> 'Val -> Async<'Res>) : FieldDef<'Val> = 
            upcast { Name = name
                     Description = None
                     Type = typedef
                     Resolve = resolve
                     Args = []
                     DeprecationReason = None }
        
        /// Single field defined inside either object types or interfaces
        static member AsyncField(name : string, typedef : #OutputDef<'Res>, description : string, 
                                 resolve : ResolveFieldContext -> 'Val -> Async<'Res>) : FieldDef<'Val> = 
            upcast { Name = name
                     Description = Some description
                     Type = typedef
                     Resolve = resolve
                     Args = []
                     DeprecationReason = None }
        
        /// Single field defined inside either object types or interfaces
        static member AsyncField(name : string, typedef : #OutputDef<'Res>, description : string, 
                                 args : InputFieldDef list, resolve : ResolveFieldContext -> 'Val -> Async<'Res>) : FieldDef<'Val> = 
            upcast { Name = name
                     Description = Some description
                     Type = typedef
                     Resolve = resolve
                     Args = args
                     DeprecationReason = None }
        
        /// Single field defined inside either object types or interfaces
        static member AsyncField(name : string, typedef : #OutputDef<'Res>, description : string, 
                                 args : InputFieldDef list, resolve : ResolveFieldContext -> 'Val -> Async<'Res>, 
                                 deprecationReason : string) : FieldDef<'Val> = 
            upcast { Name = name
                     Description = Some description
                     Type = typedef
                     Resolve = resolve
                     Args = args
                     DeprecationReason = Some deprecationReason }
        
        static member Input(name : string, schema : #InputDef<'In>, ?defaultValue : 'In, ?description : string) : InputFieldDef = 
            upcast { Name = name
                     Description = description
                     Type = schema
                     DefaultValue = defaultValue }
        
        /// GraphQL custom interface type. It's needs to be implemented object types and should not be used alone.
        static member Interface(name : string, fieldsFn : unit -> FieldDef<'Val> list, ?description : string, 
                                ?resolveType : obj -> ObjectDef) : InterfaceDef<'Val> = 
            upcast { Name = name
                     Description = description
                     FieldsFn = fieldsFn
                     ResolveType = resolveType }
        
        /// GraphQL custom interface type. It's needs to be implemented object types and should not be used alone.
        static member Interface(name : string, fields : FieldDef<'Val> list, ?description : string, 
                                ?resolveType : obj -> ObjectDef) : InterfaceDef<'Val> = 
            upcast { Name = name
                     Description = description
                     FieldsFn = fun () -> fields
                     ResolveType = resolveType }
        
        /// GraphQL custom union type, materialized as one of the types defined. It can be used as interface/object type field.
        static member Union(name : string, options : ObjectDef list, resolveValue : 'In -> 'Out, 
                            ?resolveType : 'In -> ObjectDef, ?description : string) : UnionDef<'In> = 
            upcast { Name = name
                     Description = description
                     Options = options
                     ResolveType = resolveType
                     ResolveValue = resolveValue }
