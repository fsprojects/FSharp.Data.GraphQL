/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
namespace FSharp.Data.GraphQL.Types

open System
open FSharp.Data.GraphQL.Ast

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
        abstract member Name: string
    end
and FieldDef =
    interface 
        abstract Name : string
        abstract DeprecationReason : string option
        abstract Type : OutputDef
        abstract Args : ArgDef list
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

//NOTE: For references, see https://facebook.github.io/graphql/
and GraphQLError = 
    | GraphQLError of string

and ScalarDef =
    interface
        abstract Name : string
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
        member x.CoerceInput input = upcast (x.CoerceInput input)
        member x.CoerceOutput output = x.CoerceOutput (output :?> 'Val)
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
        member x.DeprecationReason = x.DeprecationReason
        member x.Value = upcast x.Value
    override x.ToString() = x.Name

and EnumDef =
    interface 
        abstract Name : string
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
        member x.Options = x.Options |> Seq.ofList |> Seq.cast<EnumVal> |> Seq.toList

    interface NamedDef with
        member x.Name = x.Name

    override x.ToString() = sprintf "enum %s {\n    %s\n}" x.Name (String.Join("\n    ", x.Options))

/// 3.1.2 Objects
and ObjectDef =
    interface
        abstract Name : string
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
        member x.Fields = x.FieldsFn() |> Seq.ofList |> Seq.cast<FieldDef> |> List.ofSeq
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
      Args : ArgDef list
      DeprecationReason : string option }

    interface FieldDef with
        member x.Name = x.Name
        member x.DeprecationReason = x.DeprecationReason
        member x.Type = x.Type :> OutputDef
        member x.Args = x.Args
        member x.Resolve (ctx: ResolveFieldContext) (value: obj) : Async<obj> = async { 
            let! result = x.Resolve ctx (value :?> 'Val)
            return upcast result }

    interface FieldDef<'Val> with
        member x.Resolve (ctx: ResolveFieldContext) (value: 'Val) : Async<obj> = async { 
            let! result = x.Resolve ctx value
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
and [<CustomEquality; NoComparison>]InterfaceDef = 
    { Name : string
      Description : string option
      FieldsFn : unit -> FieldDef list
      ResolveType: (obj -> ObjectDef) option }
    member x.Fields = x.FieldsFn()
    interface TypeDef
    interface OutputDef
    interface CompositeDef
    interface AbstractDef
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

/// 3.1.4 Unions
and [<CustomEquality; NoComparison>]UnionDef = 
    { Name : string
      Description : string option
      Options : ObjectDef list
      ResolveType : (obj -> ObjectDef) option
      ResolveValue: obj -> obj }
      
    interface TypeDef
    interface OutputDef
    interface CompositeDef
    interface AbstractDef
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
        abstract OfType: TypeDef
        inherit InputDef
        inherit OutputDef
    end
and ListOfDef<'Val> = 
    interface 
        abstract OfType: TypeDef<'Val>
        inherit InputDef<'Val seq>
        inherit OutputDef<'Val seq>
    end

and ListOfDefinition<'Val> = 
    { OfType: TypeDef<'Val> }

    interface ListOfDef with
        member x.OfType = upcast x.OfType
        
    interface ListOfDef<'Val> with
        member x.OfType = x.OfType

    override x.ToString() = 
        match x.OfType with
        | :? NamedDef as named -> "[" + named.Name + "]"
        | other -> "[" + other.ToString() + "]"

and NonNullDef = 
    interface 
        abstract OfType: TypeDef
        inherit InputDef
        inherit OutputDef
    end
    
and NonNullDef<'Val> = 
    interface 
        abstract OfType: TypeDef<'Val>
        inherit InputDef<'Val>
        inherit OutputDef<'Val>
    end

and NonNullDefinition<'Val> = 
    { OfType: TypeDef<'Val> }

    interface NonNullDef with
        member x.OfType = upcast x.OfType
        
    interface NonNullDef<'Val> with
        member x.OfType = x.OfType

    override x.ToString() = 
        match x.OfType with
        | :? NamedDef as named -> named.Name + "!"
        | other -> other.ToString() + "!"

and InputObjectDef =
    interface 
        abstract Name : string
        abstract Fields : FieldDef list
        inherit NamedDef
        inherit InputDef
    end

/// 3.1.6 Input Objects
and InputObjectDefinition<'Val> = 
    { Name : string
      FieldsFn : unit -> FieldDef<'Val> list }
    
    interface InputObjectDef with
        member x.Name = x.Name
        member x.Fields = x.FieldsFn() |> List.map (fun f -> upcast f)

    interface InputDef<'Val>
    interface NamedDef with
        member x.Name = x.Name

/// 3.1.2.1 Object Field Arguments
and ArgDef = 
    { Name : string
      Description : string option
      Type : InputDef
      DefaultValue : obj option }
    override x.ToString() = 
        x.Name + ": " + x.Type.ToString() + (if x.DefaultValue.IsSome then " = " + x.DefaultValue.Value.ToString()
                                             else "")

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
      Args : ArgDef list }

and [<Flags>] DirectiveLocation = 
    | QUERY = 1
    | MUTATION = 2
    | SUBSCRIPTION = 4
    | FIELD = 8
    | FRAGMENT_DEFINITION = 16
    | FRAGMENT_SPREAD = 32
    | INLINE_FRAGMENT = 64

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
    let (|Option|_|) (x: obj) =
        if x = null 
        then None
        else 
            let t = x.GetType()
            if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>> 
            then
                let _,fields = Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(x, t)
                Some (fields.[0])
            else None
    
    let internal coerceStringValue (x : obj) : string option = 
        match x with
        | null -> None
        | :? string as s -> Some s
        | :? bool as b -> Some(if b then "true" else "false")
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

    let NonNull (innerDef: #TypeDef<'Val>): NonNullDefinition<'Val> = { OfType = innerDef }
    let ListOf (innerDef: #TypeDef<'Val>): ListOfDefinition<'Val> = { OfType = innerDef }

    let (|Scalar|_|) (tdef: TypeDef) =
        match tdef with
        | :? ScalarDef as x -> Some x
        | _ -> None
    let (|Object|_|) (tdef: TypeDef) =
        match tdef with
        | :? ObjectDef as x -> Some x
        | _ -> None        
    let (|Interface|_|) (tdef: TypeDef) =
        match tdef with
        | :? InterfaceDef as x -> Some x
        | _ -> None        
    let (|Union|_|) (tdef: TypeDef) =
        match tdef with
        | :? UnionDef as x -> Some x
        | _ -> None        
    let (|Enum|_|) (tdef: TypeDef) =
        match tdef with
        | :? EnumDef as x -> Some x
        | _ -> None        
    let (|InputObject|_|) (tdef: TypeDef) =
        match tdef with
        | :? InputObjectDef as x -> Some x
        | _ -> None        
    let (|List|_|) (tdef: TypeDef) =
        match tdef with
        | :? ListOfDef as x -> Some x.OfType
        | _ -> None        
    let (|NonNull|_|) (tdef: TypeDef) =
        match tdef with
        | :? NonNullDef as x -> Some x.OfType
        | _ -> None
    let (|Input|_|) (tdef: TypeDef) =
        match tdef with
        | :? ScalarDef | :? EnumDef | :? InputObjectDef -> Some tdef
        | _ -> None
    let (|Output|_|) (tdef: TypeDef) =
        match tdef with
        | :? ScalarDef | :? EnumDef | :? ObjectDef | :? InterfaceDef | :? UnionDef -> Some tdef
        | _ -> None
    let (|Leaf|_|) (tdef: TypeDef) =
        match tdef with
        | :? ScalarDef | :? EnumDef -> Some tdef
        | _ -> None
    let (|Composite|_|) (tdef: TypeDef) =
        match tdef with
        | :? ObjectDef | :? InterfaceDef | :? UnionDef -> Some tdef
        | _ -> None
    let (|Abstract|_|) (tdef: TypeDef) =
        match tdef with
        | :? InterfaceDef | :? UnionDef -> Some (tdef :?> AbstractDef)
        | _ -> None

    let rec private named (tdef: TypeDef) =
        match tdef with
        | :? NamedDef as n -> Some n
        | NonNull inner -> named inner
        | List inner -> named inner
        | _ -> None
    let rec (|Named|_|) (tdef: TypeDef) = named tdef
        
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
          Description = Some "The `Float` scalar type represents signed double-precision fractional values as specified by [IEEE 754](http://en.wikipedia.org/wiki/IEEE_floating_point)."
          CoerceInput = coerceFloatInput 
          CoerceValue = coerceFloatValue 
          CoerceOutput = coerceFloatOuput }
    
    /// GraphQL type of string
    let String : ScalarDefinition<string> = 
        { Name = "String"
          Description = Some "The `String` scalar type represents textual data, represented as UTF-8 character sequences. The String type is most often used by GraphQL to represent free-form human-readable text."
          CoerceInput = coerceStringInput
          CoerceValue = coerceStringValue
          CoerceOutput = coerceStringOuput }
    
    /// GraphQL type for custom identifier
    let ID : ScalarDefinition<string> = 
        { Name = "ID"
          Description = Some "The `ID` scalar type represents a unique identifier, often used to refetch an object or as key for a cache. The ID type appears in a JSON response as a String; however, it is not intended to be human-readable. When expected as an input type, any string (such as `\"4\"`) or integer (such as `4`) input value will be accepted as an ID."
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
                  Type = NonNull Boolean
                  DefaultValue = None } ] }
    
    let SkipDirective : DirectiveDef = 
        { Name = "skip"
          Description = Some "Directs the executor to skip this field or fragment when the `if` argument is true."
          Locations = 
              DirectiveLocation.FIELD ||| DirectiveLocation.FRAGMENT_SPREAD ||| DirectiveLocation.INLINE_FRAGMENT
          Args = 
              [ { Name = "if"
                  Description = Some "Skipped when true."
                  Type = NonNull Boolean
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
    
    /// Adds a single field to existing object type, returning new object type in result.
//    let mergeField (objectType : ObjectDef) (field : FieldDef) : ObjectDef = 
//        match objectType.Fields |> Seq.tryFind (fun x -> x.Name = field.Name) with
//        | None -> { objectType with Fields = objectType.Fields @ [ field ] } // we must append to the end
//        | Some x when x = field -> objectType
//        | Some x -> 
//            let msg = 
//                sprintf 
//                    "Cannot merge field %A into object type %s, because it already has field %A sharing the same name, but having a different signature." 
//                    field objectType.Name x
//            raise (GraphQLException msg)
    
    /// Adds list of fields to existing object type, returning new object type in result.
//    let mergeFields (objectType : ObjectDef) (fields : FieldDef list) : ObjectDef = 
//        fields |> List.fold mergeField objectType //TODO: optimize
    
    /// Orders object type to implement collection of interfaces, applying all of their field to it.
    /// Returns new object type implementing all of the fields in result.
//    let implements (objectType : ObjectDef) (interfaces : InterfaceDef list) : ObjectDef = 
//        let o = { objectType with Implements = objectType.Implements @ interfaces }        
//        let modified = 
//            interfaces
//            |> List.map (fun i -> i.Fields)
//            |> List.fold mergeFields o
//        modified
                
    let internal matchParameters (methodInfo: MethodInfo) (ctx: ResolveFieldContext) =
        methodInfo.GetParameters()
        |> Array.map (fun param -> ctx.Arg<obj>(param.Name).Value)

    let inline strip (fn: 'In -> 'Out): (obj -> obj) = fun (i) -> upcast fn(i :?> 'In) 
                
    let internal defaultResolve<'Val, 'Res> (fieldName : string) : ResolveFieldContext -> 'Val -> Async<'Res> = 
        (fun ctx value -> 
        async { 
            if Object.Equals(value, null) then return Unchecked.defaultof<'Res>
            else 
                let t = value.GetType()
                let memberInfo = t.GetMember(fieldName, BindingFlags.IgnoreCase ||| BindingFlags.Public ||| BindingFlags.Instance)
                match memberInfo with
                | [||] -> return raise (GraphQLException (sprintf "Default resolve function failed. Couldn't find member '%s' inside definition of type '%s'." fieldName t.FullName))
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
        
        static member Scalar(name : string, coerceInput : (Value -> 'T option), coerceOutput : ('T -> Value option), 
                             coerceValue : (obj -> 'T option), ?description : string) : ScalarDefinition<'T> = 
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
        static member Object(name : string, fieldsFn : unit -> FieldDef<'Val> list, ?description : string, ?interfaces : InterfaceDef list, ?isTypeOf : obj -> bool) : ObjectDefinition<'Val> = 
            { Name = name
              Description = description
              FieldsFn = fieldsFn
              Implements = match interfaces with None -> [] | Some i -> i
              IsTypeOf = isTypeOf }

        /// GraphQL custom object type
        static member Object(name : string, fields : FieldDef<'Val> list, ?description : string, ?interfaces : InterfaceDef list, ?isTypeOf : obj -> bool) : ObjectDefinition<'Val> = 
            { Name = name
              Description = description
              FieldsFn = fun () -> fields
              Implements = match interfaces with None -> [] | Some i -> i
              IsTypeOf = isTypeOf }
            
        /// GraphQL custom input object type
        static member InputObject(name : string, fields : unit -> FieldDef<'Val> list) : InputObjectDefinition<'Val> = { Name = name; FieldsFn = fields }
        
        /// GraphQL custom input object type
        static member InputObject(name : string, fields : FieldDef<'Val> list) : InputObjectDefinition<'Val> = { Name = name; FieldsFn = fun () -> fields }
        
        /// Single field defined inside either object types or interfaces
        static member Field(name : string, typedef : #OutputDef<'Res>) : FieldDef<'Val> = 
            upcast 
                { Name = name
                  Description = None
                  Type = typedef
                  Resolve = defaultResolve<'Val, 'Res> name
                  Args = []
                  DeprecationReason = None }

        /// Single field defined inside either object types or interfaces
        static member Field(name : string, typedef : #OutputDef<'Res>, resolve : ResolveFieldContext -> 'Val -> 'Res) : FieldDef<'Val> = 
            upcast 
                { Name = name
                  Description = None
                  Type = typedef
                  Resolve = fun ctx value -> async { return resolve ctx value }
                  Args = []
                  DeprecationReason = None }
              
        /// Single field defined inside either object types or interfaces
        static member Field(name : string, typedef : #OutputDef<'Res>, description : string, resolve : ResolveFieldContext -> 'Val -> 'Res) : FieldDef<'Val> = 
            upcast 
                { Name = name
                  Description = Some description
                  Type = typedef
                  Resolve = fun ctx value -> async { return resolve ctx value }
                  Args = []
                  DeprecationReason = None }

        /// Single field defined inside either object types or interfaces
        static member Field(name : string, typedef : #OutputDef<'Res>, description : string, args : ArgDef list, resolve : ResolveFieldContext -> 'Val -> 'Res) : FieldDef<'Val> = 
            upcast
                { Name = name
                  Description = Some description
                  Type = typedef
                  Resolve = fun ctx value -> async { return resolve ctx value }
                  Args = args
                  DeprecationReason = None }

        /// Single field defined inside either object types or interfaces
        static member Field(name : string, typedef : #OutputDef<'Res>, description : string, args : ArgDef list, resolve : ResolveFieldContext -> 'Val -> 'Res, deprecationReason : string) : FieldDef<'Val> = 
            upcast
                { Name = name
                  Description = Some description
                  Type = typedef
                  Resolve = fun ctx value -> async { return resolve ctx value }
                  Args = args
                  DeprecationReason = Some deprecationReason }
        
        /// Single field defined inside either object types or interfaces, with asynchronous resolution function
        static member AsyncField(name : string, typedef : #OutputDef<'Res>, resolve : ResolveFieldContext -> 'Val -> Async<'Res>, ?description : string, ?arguments : ArgDef list, ?deprecationReason : string) : FieldDef<'Val> = 
            upcast 
                { Name = name
                  Description = description
                  Type = typedef
                  Resolve = resolve
                  Args = if arguments.IsNone then [] else arguments.Value
                  DeprecationReason = deprecationReason }
        
//        /// Single field defined inside either object types or interfaces
//        static member Field(name : string, typedef : OutputDef<'Res>, ?description : string, ?args : ArgDef list, ?deprecationReason : string) : FieldDefinition<'Val, 'Res> = 
//            { Name = name
//              Description = description
//              Type = typedef
//              Resolve = defaultResolve<'Val, 'Res> name
//              Args = if args.IsNone then [] else args.Value
//              DeprecationReason = deprecationReason }
        
        static member Arg(name : string, schema : InputDef, ?defaultValue : 'T, ?description : string) : ArgDef = 
            { Name = name
              Description = description
              Type = schema
              DefaultValue = 
                  match defaultValue with
                  | Some value -> Some(upcast value)
                  | None -> None }
        
        /// GraphQL custom interface type. It's needs to be implemented object types and should not be used alone.
        static member Interface(name : string, fields : unit -> FieldDef list, ?description : string, ?resolveType: obj -> ObjectDef) : InterfaceDef = 
            { Name = name
              Description = description
              FieldsFn = fields
              ResolveType = resolveType }
        
        /// GraphQL custom interface type. It's needs to be implemented object types and should not be used alone.
        static member Interface(name : string, fields : FieldDef list, ?description : string, ?resolveType: obj -> ObjectDef) : InterfaceDef = 
            { Name = name
              Description = description
              FieldsFn = fun () -> fields
              ResolveType = resolveType }
        
        /// GraphQL custom union type, materialized as one of the types defined. It can be used as interface/object type field.
        static member Union(name : string, options : ObjectDef list, ?resolveType: 'In -> ObjectDef, ?description : string, ?resolveValue: 'In -> 'Out) : UnionDef = 
            { Name = name
              Description = description
              Options = options
              ResolveType = match resolveType with None -> None | Some fn -> Some (fun i -> fn (i :?> 'In))
              ResolveValue = match resolveValue with None -> id | Some fn -> strip fn }
