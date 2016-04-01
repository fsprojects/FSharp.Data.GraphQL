/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Types

open System
open FSharp.Data.GraphQL.Ast

//NOTE: For references, see https://facebook.github.io/graphql/

type Resolver<'Value> = unit -> Async<'Value>

/// 3.1.1.1 Build-in Scalars
type [<CustomEquality;NoComparison>] Scalar = 
    {
        Name: string
        Description: string option
        CoerceInput: Value -> obj
    }
    interface IEquatable<Scalar> with
      member x.Equals s = x.Name = s.Name
    override x.Equals y = 
        match y with
        | :? Scalar as s -> (x :> IEquatable<Scalar>).Equals(s)
        | _ -> false
    override x.GetHashCode() = x.Name.GetHashCode()
    override x.ToString() = x.Name

and EnumValue = 
    {
        Name: string
        Value: obj
        Description: string option
    }
    override x.ToString() = x.Name

and EnumType = 
    {
        Name: string
        Description: string option
        Options: EnumValue list
    }
    override x.ToString() = 
        sprintf "enum %s {\n    %s\n}" x.Name (String.Join("\n    ", x.Options))

/// 3.1.2 Objects
and ObjectType = 
    {
        Name: string
        Description: string option
        Fields: Field list
        Implements: Interface list
    }
    override x.ToString() =
        let sb = System.Text.StringBuilder("type ")
        sb.Append(x.Name) |> ignore
        if not (List.isEmpty x.Implements) then 
            sb.Append(" implements ").Append(String.Join(", ", x.Implements |> List.map (fun x -> x.Name))) |> ignore
        sb.Append("{") |> ignore
        x.Fields
        |> List.iter (fun f -> sb.Append("\n    ").Append(f.ToString()) |> ignore)
        sb.Append("\n}").ToString()

and [<CustomEquality;NoComparison>] Field = 
    {
        Name: string
        Description: string option
        Schema: GraphQLType
        Resolver: Resolver<obj> option
        Arguments: Argument list
    }
    interface IEquatable<Field> with
      member x.Equals f = 
            x.Name = f.Name && 
            x.Description = f.Description &&
            x.Schema = f.Schema &&
            x.Arguments = f.Arguments
    override x.Equals y = 
        match y with
        | :? Field as f -> (x :> IEquatable<Field>).Equals(f)
        | _ -> false
    override x.GetHashCode() = 
        let mutable hash = x.Name.GetHashCode()
        hash <- (hash*397) ^^^ (match x.Description with | None -> 0 | Some d -> d.GetHashCode())
        hash <- (hash*397) ^^^ (x.Schema.GetHashCode())
        hash <- (hash*397) ^^^ (x.Arguments.GetHashCode())
        hash
    override x.ToString() = 
        let mutable s = x.Name + ": " + x.Schema.ToString()
        if not (List.isEmpty x.Arguments) then
            s <- "(" + String.Join(", ", x.Arguments) + ")"
        s

/// 3.1.3 Interfaces
and Interface =
    {
        Name: string
        Description: string option
        Fields: Field list
    }
    override x.ToString() = 
        let sb = System.Text.StringBuilder("interface ").Append(x.Name).Append(" {")
        x.Fields
        |> List.iter (fun f -> sb.Append("\n    ").Append(f.ToString()) |> ignore)
        sb.Append("\n}").ToString()        

/// 3.1.4 Unions
and Union = 
    {
        Name: string
        Description: string option
        Options: GraphQLType list
    }
    override x.ToString() =
        "union " + x.Name + " = " + String.Join(" | ", x.Options)

/// 3.1 Types
and GraphQLType =
    | Scalar of Scalar
    | Enum of EnumType
    | Object of ObjectType
    | Interface of Interface
    | Union of Union
    | ListOf of GraphQLType
    | NonNull of GraphQLType
    | InputObject of ObjectType
    override x.ToString() =
        match x with
        | Scalar y      -> y.ToString()
        | Enum y        -> y.ToString()
        | Object y      -> y.ToString()
        | Interface y   -> y.ToString()
        | Union y       -> y.ToString()
        | ListOf y      -> "[" + y.ToString() + "]"
        | NonNull y     -> y.ToString() + "!"
        | InputObject y -> y.ToString()

/// 3.1.6 Input Objects
and InputObject = 
    {
        Name: string
        Fields: Field list
    }

/// 3.1.2.1 Object Field Arguments
and Argument = 
    {
        Name: string
        Value: obj
    }
    override x.ToString() = x.Name + ": " + x.Value.ToString()

/// 5.7 Variables
and Variable = 
    {
        Name: string
        Schema: GraphQLType
        DefaultValue: obj
    }
    override x.ToString() =
        "$" + x.Name + ": " + x.Schema.ToString() + (if x.DefaultValue <> null then " = " + x.DefaultValue.ToString() else "")

/// 3.2 Directives
and Directive =
    /// 3.2.1 @skip
    | Skip of Argument list
    /// 3.2.2 @include
    | Include of Argument list
    override x.ToString() =
        match x with
        | Skip args -> "@skip(" + String.Join(", ", args) + ")"
        | Include args -> "@include(" + String.Join(", ", args) + ")"

and QueryField = 
    {
        Name: string
        Directive: Directive option
    }

and Query = 
    {
        Name: Variable list
        Fields: QueryField list
    }

and Mutation = 
    {
        Name: string
    }

/// 3.3 Starting types
and OperationType =
    | Query of Query
    | Mutation of Mutation

and Selection =
    | Field of string
    | Alias of string * string

/// 5.4 Fragments
and FragmentDefinition = 
    {
        Name: string option
        TypeCondition: string
        Directives: Directive list option
        Selections: Selection list
    }

/// 5.4.2 Fragment Spreads
and FragmentSpread = 
    {
        Name: string
        Directives: Directive list option
    }

type TypeViolationException(msg) = 
    inherit Exception(msg)
    
[<AutoOpen>]
module SchemaDefinitions =

    open System.Globalization

    let private coerceIntResult (x: obj) : int option = 
        match x with
        | :? int as i -> Some i
        | :? int64 as l -> Some (int l)
        | :? double as d -> Some (int d)
        | :? string as s -> 
            match Int32.TryParse(s) with
            | true, i -> Some i
            | false, _ -> None
        | :? bool as b -> Some (if b then 1 else 0)
        | other ->
            try
                Some (System.Convert.ToInt32 other)
            with
            | _ -> None

    let private coerceIntInput = function
        | IntValue i -> Some i
        | FloatValue f -> Some (int f)
        | StringValue s -> 
            match Int32.TryParse(s, NumberStyles.Float, CultureInfo.InvariantCulture) with
            | true, i -> Some i
            | false, _ -> None
        | BooleanValue b -> Some (if b then 1 else 0)
        | _ -> None

    let private coerceFloatInput = function
        | IntValue i -> Some (double i)
        | FloatValue f -> Some f
        | StringValue s -> 
            match Double.TryParse(s, NumberStyles.Float, CultureInfo.InvariantCulture) with
            | true, i -> Some i
            | false, _ -> None
        | BooleanValue b -> Some (if b then 1. else 0.)
        | _ -> None

    let private coerceStringInput = function
        | IntValue i -> Some (i.ToString(CultureInfo.InvariantCulture))
        | FloatValue f -> Some (f.ToString(CultureInfo.InvariantCulture))
        | StringValue s -> Some s
        | BooleanValue b -> Some (if b then "true" else "false")
        | _ -> None

    let private coerceBoolInput = function
        | IntValue i -> Some (if i = 0 then false else true)
        | FloatValue f -> Some (if f = 0. then false else true)
        | StringValue s -> 
            match Boolean.TryParse(s) with
            | true, i -> Some i
            | false, _ -> None
        | BooleanValue b -> Some b
        | _ -> None

    let private coerceIdInput = function
        | IntValue i -> Some (i.ToString())
        | StringValue s -> Some s
        | _ -> None

    /// GraphQL type of int
    let Int: GraphQLType = 
        Scalar {
            Name = "Int"
            Description = Some "The `Int` scalar type represents non-fractional signed whole numeric values. Int can represent values between -(2^31) and 2^31 - 1."
            CoerceInput = coerceIntInput >> box
        }

    /// GraphQL type of boolean
    let Bool: GraphQLType = 
        Scalar {
            Name = "Boolean"
            Description = Some "The `Boolean` scalar type represents `true` or `false`."
            CoerceInput = coerceBoolInput >> box
        }

    /// GraphQL type of float
    let Float: GraphQLType = 
        Scalar {
            Name = "Float"
            Description = Some "The `Float` scalar type represents signed double-precision fractional values as specified by [IEEE 754](http://en.wikipedia.org/wiki/IEEE_floating_point)."
            CoerceInput = coerceFloatInput >> box
        }
    
    /// GraphQL type of string
    let String: GraphQLType = 
        Scalar {
            Name = "String"
            Description = Some "The `String` scalar type represents textual data, represented as UTF-8 character sequences. The String type is most often used by GraphQL to represent free-form human-readable text."
            CoerceInput = coerceStringInput >> box
        }
    
    /// GraphQL type for custom identifier
    let ID: GraphQLType = 
        Scalar {
            Name = "ID"
            Description = Some "The `ID` scalar type represents a unique identifier, often used to refetch an object or as key for a cache. The ID type appears in a JSON response as a String; however, it is not intended to be human-readable. When expected as an input type, any string (such as `\"4\"`) or integer (such as `4`) input value will be accepted as an ID."
            CoerceInput = coerceIdInput >> box
        }

    /// Adds a single field to existing object type, returning new object type in result.
    let inline mergeField (objectType: ObjectType) (field: Field) : ObjectType = 
        match objectType.Fields |> Seq.tryFind (fun x -> x.Name = field.Name) with
        | None ->  { objectType with Fields = objectType.Fields @ [ field ] }     // we must append to the end
        | Some x when x = field -> objectType
        | Some x -> 
            let msg = sprintf "Cannot merge field %A into object type %s, because it already has field %A sharing the same name, but having a different signature." field objectType.Name x
            raise (TypeViolationException msg)

    /// Adds list of fields to existing object type, returning new object type in result.
    let mergeFields (objectType: ObjectType) (fields: Field list) : ObjectType = 
        fields
        |> List.fold mergeField objectType      //TODO: optimize

    /// Orders object type to implement collection of interfaces, applying all of their field to it.
    /// Returns new object type implementing all of the fields in result.
    let implements (objectType: GraphQLType) (interfaces: Interface list) : GraphQLType =
        let o = 
            match objectType with
            | Object x -> { x with Implements = x.Implements @ interfaces }
            | other -> failwith ("Expected GrapQL type to be an object but got " + other.ToString())
        let modified = 
            interfaces
            |> List.map (fun i -> i.Fields)
            |> List.fold mergeFields o
        Object modified