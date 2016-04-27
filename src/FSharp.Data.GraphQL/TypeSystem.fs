/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
namespace FSharp.Data.GraphQL.Types

open System
open FSharp.Data.GraphQL.Ast

type GraphQLException(msg) = 
    inherit Exception(msg)

type ISchema =
    interface
        inherit System.Collections.Generic.IEnumerable<TypeDef>
        abstract TypeMap: Map<string, TypeDef>
        abstract Query: TypeDef
        abstract Mutation: TypeDef option
        abstract Directives: DirectiveDef list
        abstract TryFindType: string -> TypeDef option
        abstract GetPossibleTypes: TypeDef -> TypeDef list
        abstract IsPossibleType: TypeDef -> TypeDef -> bool
    end

and ResolveFieldContext = 
    { FieldName : string
      Fields : Field list
      FieldType : FieldDef
      ReturnType : TypeDef
      ParentType : TypeDef
      Schema: ISchema
      Args : Map<string, obj>
      Operation : OperationDefinition
      Fragments : FragmentDefinition list
      Variables : Map<string, obj option> }
    member x.Arg(name : string) : 't option = 
        match Map.tryFind name x.Args with
        | Some o -> Some(o :?> 't)
        | None -> None

//NOTE: For references, see https://facebook.github.io/graphql/
and GraphQLError = 
    | GraphQLError of string

/// 3.1.1.1 Build-in Scalars
and [<CustomEquality; NoComparison>] ScalarType = 
    { Name : string
      Description : string option
      CoerceInput : Value -> obj
      CoerceOutput : obj -> Value option
      CoerceValue : obj -> obj option }
    
    interface IEquatable<ScalarType> with
        member x.Equals s = x.Name = s.Name
    
    override x.Equals y = 
        match y with
        | :? ScalarType as s -> (x :> IEquatable<ScalarType>).Equals(s)
        | _ -> false
    
    override x.GetHashCode() = x.Name.GetHashCode()
    override x.ToString() = x.Name

and EnumValue = 
    { Name : string
      Value : obj
      Description : string option
      DeprecationReason: string option }
    override x.ToString() = x.Name

and EnumType = 
    { Name : string
      Description : string option
      Options : EnumValue list }
    override x.ToString() = sprintf "enum %s {\n    %s\n}" x.Name (String.Join("\n    ", x.Options))

/// 3.1.2 Objects
and ObjectType = 
    { Name : string
      Description : string option
      mutable Fields : FieldDef list
      mutable Implements : TypeDef list }
    
    member x.AddField(field : FieldDef) = 
        match List.tryFind<FieldDef> (fun f -> f.Name = field.Name) x.Fields with
        | Some _ -> 
            let msg = 
                sprintf 
                    "Cannot merge field %A into object type %s, because it already has field %A sharing the same name." 
                    field x.Name x
            raise (GraphQLException msg)
        | None -> x.Fields <- x.Fields @ [ field ]
    
    override x.ToString() = 
        let sb = System.Text.StringBuilder("type ")
        sb.Append(x.Name) |> ignore
        if not (List.isEmpty x.Implements) then 
            sb.Append(" implements ").Append(String.Join(", ", x.Implements)) |> ignore
        sb.Append("{") |> ignore
        x.Fields |> List.iter (fun f -> sb.Append("\n    ").Append(f.ToString()) |> ignore)
        sb.Append("\n}").ToString()

and [<CustomEquality; NoComparison>] FieldDef = 
    { Name : string
      Description : string option
      Type : TypeDef
      Resolve : ResolveFieldContext -> obj -> Async<obj>
      Args : ArgDef list
      DeprecationReason: string option }
    
    interface IEquatable<FieldDef> with
        member x.Equals f = 
            x.Name = f.Name && x.Description = f.Description && x.Type = f.Type && x.Args = f.Args
    
    override x.Equals y = 
        match y with
        | :? FieldDef as f -> (x :> IEquatable<FieldDef>).Equals(f)
        | _ -> false
    
    override x.GetHashCode() = 
        let mutable hash = x.Name.GetHashCode()
        hash <- (hash * 397) ^^^ (match x.Description with
                                  | None -> 0
                                  | Some d -> d.GetHashCode())
        hash <- (hash * 397) ^^^ (x.Type.GetHashCode())
        hash <- (hash * 397) ^^^ (x.Args.GetHashCode())
        hash
    
    override x.ToString() = 
        let mutable s = x.Name + ": " + x.Type.ToString()
        if not (List.isEmpty x.Args) then s <- "(" + String.Join(", ", x.Args) + ")"
        s

/// 3.1.3 Interfaces
and InterfaceType = 
    { Name : string
      Description : string option
      mutable Fields : FieldDef list }
    override x.ToString() = 
        let sb = System.Text.StringBuilder("interface ").Append(x.Name).Append(" {")
        x.Fields |> List.iter (fun f -> sb.Append("\n    ").Append(f.ToString()) |> ignore)
        sb.Append("\n}").ToString()

/// 3.1.4 Unions
and UnionType = 
    { Name : string
      Description : string option
      Options : TypeDef list }
    override x.ToString() = "union " + x.Name + " = " + String.Join(" | ", x.Options)

/// 3.1 Types
and TypeDef = 
    | Scalar of ScalarType
    | Enum of EnumType
    | Object of ObjectType
    | Interface of InterfaceType
    | Union of UnionType
    | ListOf of TypeDef
    | NonNull of TypeDef
    | InputObject of ObjectType
    
    override x.ToString() = 
        match x with
        | Scalar y -> y.ToString()
        | Enum y -> y.ToString()
        | Object y -> y.ToString()
        | Interface y -> y.ToString()
        | Union y -> y.ToString()
        | ListOf y -> "[" + y.ToString() + "]"
        | NonNull y -> y.ToString() + "!"
        | InputObject y -> y.ToString()
    
    member x.Name = 
        match x with
        | Scalar s -> s.Name
        | Enum e -> e.Name
        | Object o -> o.Name
        | Interface i -> i.Name
        | Union u -> u.Name
        | ListOf i -> i.Name
        | NonNull i -> i.Name
        | InputObject o -> o.Name

    member x.Description = 
        match x with
        | Scalar scalardef -> scalardef.Description
        | Enum enumdef -> enumdef.Description
        | Object objdef -> objdef.Description
        | Interface idef -> idef.Description
        | Union udef -> udef.Description
        | ListOf _ -> None
        | NonNull _ -> None
        | InputObject indef -> indef.Description

/// 3.1.6 Input Objects
and InputObject = 
    { Name : string
      Fields : FieldDef list }

/// 3.1.2.1 Object Field Arguments
and ArgDef = 
    { Name : string
      Description : string option
      Type : TypeDef
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

and [<Flags>]DirectiveLocation = 
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
    
    let internal coerceStringValue (x : obj) : string option = 
        match x with
        | null -> None
        | :? bool as b -> 
            Some(if b then "true"
                 else "false")
        | other -> Some(other.ToString())
    
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
    
    /// GraphQL type of int
    let Int : TypeDef = 
        Scalar { Name = "Int"
                 Description = 
                     Some 
                         "The `Int` scalar type represents non-fractional signed whole numeric values. Int can represent values between -(2^31) and 2^31 - 1."
                 CoerceInput = coerceIntInput >> box
                 CoerceValue = coerceIntValue >> Option.map box
                 CoerceOutput = coerceIntOuput }
    
    /// GraphQL type of boolean
    let Boolean : TypeDef = 
        Scalar { Name = "Boolean"
                 Description = Some "The `Boolean` scalar type represents `true` or `false`."
                 CoerceInput = coerceBoolInput >> box
                 CoerceValue = coerceBoolValue >> Option.map box
                 CoerceOutput = coerceBoolOuput }
    
    /// GraphQL type of float
    let Float : TypeDef = 
        Scalar { Name = "Float"
                 Description = 
                     Some 
                         "The `Float` scalar type represents signed double-precision fractional values as specified by [IEEE 754](http://en.wikipedia.org/wiki/IEEE_floating_point)."
                 CoerceInput = coerceFloatInput >> box
                 CoerceValue = coerceFloatValue >> Option.map box
                 CoerceOutput = coerceFloatOuput }
    
    /// GraphQL type of string
    let String : TypeDef = 
        Scalar { Name = "String"
                 Description = 
                     Some 
                         "The `String` scalar type represents textual data, represented as UTF-8 character sequences. The String type is most often used by GraphQL to represent free-form human-readable text."
                 CoerceInput = coerceStringInput >> box
                 CoerceValue = coerceStringValue >> Option.map box
                 CoerceOutput = coerceStringOuput }
    
    /// GraphQL type for custom identifier
    let ID : TypeDef = 
        Scalar { Name = "ID"
                 Description = 
                     Some 
                         "The `ID` scalar type represents a unique identifier, often used to refetch an object or as key for a cache. The ID type appears in a JSON response as a String; however, it is not intended to be human-readable. When expected as an input type, any string (such as `\"4\"`) or integer (such as `4`) input value will be accepted as an ID."
                 CoerceInput = coerceIdInput >> box
                 CoerceValue = coerceStringValue >> Option.map box
                 CoerceOutput = coerceStringOuput }
    
    let IncludeDirective : DirectiveDef = 
        { Name = "include"
          Description = 
              Some "Directs the executor to include this field or fragment only when the `if` argument is true."
          Locations = DirectiveLocation.FIELD &&& DirectiveLocation.FRAGMENT_SPREAD &&& DirectiveLocation.INLINE_FRAGMENT 
          Args = 
              [ { Name = "if"
                  Description = Some "Included when true."
                  Type = NonNull Boolean
                  DefaultValue = None } ] }
                  
    let SkipDirective : DirectiveDef = 
        { Name = "skip"
          Description = 
              Some "Directs the executor to skip this field or fragment when the `if` argument is true."
          Locations = DirectiveLocation.FIELD &&& DirectiveLocation.FRAGMENT_SPREAD &&& DirectiveLocation.INLINE_FRAGMENT
          Args = 
              [ { Name = "if"
                  Description = Some "Skipped when true."
                  Type = NonNull Boolean
                  DefaultValue = None } ] }
    
    let rec internal coerceAstValue (variables : Map<string, obj option>) (value : Value) : obj = 
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
        | Variable variable -> variables.[variable] |> Option.toObj
    
    /// Adds a single field to existing object type, returning new object type in result.
    let mergeField (objectType : ObjectType) (field : FieldDef) : ObjectType = 
        match objectType.Fields |> Seq.tryFind (fun x -> x.Name = field.Name) with
        | None -> { objectType with Fields = objectType.Fields @ [ field ] } // we must append to the end
        | Some x when x = field -> objectType
        | Some x -> 
            let msg = 
                sprintf 
                    "Cannot merge field %A into object type %s, because it already has field %A sharing the same name, but having a different signature." 
                    field objectType.Name x
            raise (GraphQLException msg)
    
    /// Adds list of fields to existing object type, returning new object type in result.
    let mergeFields (objectType : ObjectType) (fields : FieldDef list) : ObjectType = 
        fields |> List.fold mergeField objectType //TODO: optimize
    
    /// Orders object type to implement collection of interfaces, applying all of their field to it.
    /// Returns new object type implementing all of the fields in result.
    let implements (objectType : TypeDef) (interfaces : InterfaceType list) : TypeDef = 
        let o = 
            match objectType with
            | Object x -> { x with Implements = x.Implements @ (interfaces |> List.map (fun x -> Interface x)) }
            | other -> failwith ("Expected GrapQL type to be an object but got " + other.ToString())
        
        let modified = 
            interfaces
            |> List.map (fun i -> i.Fields)
            |> List.fold mergeFields o
        
        Object modified
    
    let internal getProperty o pname =
        let t = o.GetType()
        let property = t.GetProperty(pname, BindingFlags.IgnoreCase|||BindingFlags.Public|||BindingFlags.Instance)
        if property = null 
        then raise (GraphQLException (sprintf "Default resolve function failed. Couldn't find property '%s' inside definition of type '%s'." pname t.FullName))
        else property
        
    let internal defaultResolve<'t> (fieldName : string) : ResolveFieldContext -> obj -> Async<obj> = 
        (fun _ v -> 
            async {
                if v = null
                then return null
                else
                    let property = getProperty v fieldName
                    return property.GetValue(v, null)
            }) 

    type Define private() =
        static member Scalar (name: string, coerceInput: Value -> 'T option, coerceOutput: 'T -> Value option, coerceValue: obj -> 'T option, ?description: string) = 
            {
                Name = name
                Description = description
                CoerceInput = coerceInput >> box
                CoerceOutput = (fun x ->
                    match x with
                    | :? 'T as t -> coerceOutput t
                    | _ -> None)
                CoerceValue = coerceValue >> Option.map box
            }
        
        /// GraphQL type for user defined enums
        static member Enum (name: string, options: EnumValue list, ?description: string): TypeDef = Enum { Name = name; Description = description; Options = options }

        /// Single enum option to be used as argument in <see cref="Schema.Enum"/>
        static member EnumValue (name: string, value: 'Val, ?description: string, ?deprecationReason: string): EnumValue = { Name = name; Description = description; Value = value :> obj; DeprecationReason = deprecationReason }

        /// GraphQL custom object type
        static member ObjectType (name: string, fields: FieldDef list, ?description: string, ?interfaces: InterfaceType list): TypeDef = 
            let o = Object { 
                Name = name
                Description = description
                Fields = fields
                Implements = []
            }
            match interfaces with
            | None -> o
            | Some i -> implements o i
                
        /// Single field defined inside either object types or interfaces
        static member Field (name: string, schema: TypeDef, resolve: ResolveFieldContext -> 'Object  -> 'Value, ?description: string, ?arguments: ArgDef list, ?deprecationReason: string): FieldDef =
            {
                Name = name
                Description = description
                Type = schema
                Resolve = fun ctx v -> async { return upcast resolve ctx (v :?> 'Object) }
                Args = if arguments.IsNone then [] else arguments.Value
                DeprecationReason = deprecationReason
            } 
            
        /// Single field defined inside either object types or interfaces, with asynchronous resolution function
        static member AsyncField (name: string, schema: TypeDef, resolve: ResolveFieldContext -> 'Object  -> Async<'Value>, ?description: string, ?arguments: ArgDef list, ?deprecationReason: string): FieldDef =
            {
                Name = name
                Description = description
                Type = schema
                Resolve = fun ctx v -> 
                    async { 
                        let! value = resolve ctx (v :?> 'Object) 
                        return upcast value }
                Args = if arguments.IsNone then [] else arguments.Value
                DeprecationReason = deprecationReason
            } 
        
        /// Single field defined inside either object types or interfaces
        static member Field<'Object> (name: string, schema: TypeDef, ?description: string, ?arguments: ArgDef list, ?deprecationReason: string): FieldDef =
            {
                Name = name
                Description = description
                Type = schema
                Resolve = defaultResolve<'Object> name
                Args = if arguments.IsNone then [] else arguments.Value
                DeprecationReason = deprecationReason
            }

        static member Argument (name: string, schema: TypeDef, ?defaultValue: 'T, ?description: string): ArgDef = {
            Name = name
            Description = description
            Type = schema
            DefaultValue = 
                match defaultValue with
                | Some value -> Some (upcast value)
                | None -> None
        }

        /// GraphQL custom interface type. It's needs to be implemented object types and should not be used alone.
        static member Interface (name: string, fields: FieldDef list, ?description: string): InterfaceType = {
            Name = name
            Description = description
            Fields = fields 
        }

        /// GraphQL custom union type, materialized as one of the types defined. It can be used as interface/object type field.
        static member Union (name: string, options: TypeDef list, ?description: string): TypeDef = 
            let graphQlOptions = 
                options
                |> List.map (fun x ->
                    match x with
                    | Object o -> o.Name
                    | _ -> failwith "Cannot use types other that object types in Union definitions")
            Union {
                Name = name
                Description = description
                Options = options
            }
