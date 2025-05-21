// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc
namespace FSharp.Data.GraphQL.Ast

open System
open System.Text.Json
//NOTE: For references, see https://facebook.github.io/graphql/

/// 2.2 Query Document
type Document = {
    Definitions : Definition list
} with

    member doc.IsEmpty = doc.Definitions.IsEmpty

and Definition =
    | OperationDefinition of OperationDefinition
    | FragmentDefinition of FragmentDefinition

    member x.Name =
        match x with
        | OperationDefinition op -> op.Name
        | FragmentDefinition frag -> frag.Name
    member x.Directives =
        match x with
        | OperationDefinition op -> op.Directives
        | FragmentDefinition frag -> frag.Directives
    member x.SelectionSet =
        match x with
        | OperationDefinition op -> op.SelectionSet
        | FragmentDefinition frag -> frag.SelectionSet

/// 2.2.1 Operations
and OperationDefinition = {
    OperationType : OperationType
    Name : string voption
    VariableDefinitions : VariableDefinition list
    Directives : Directive list
    SelectionSet : Selection list
} with

    member x.IsShortHandQuery =
        x.OperationType = Query
        && x.Name.IsNone
        && x.VariableDefinitions.IsEmpty
        && x.Directives.IsEmpty

and OperationType =
    | Query
    | Mutation
    | Subscription

/// 2.2.2 Selection Sets
and Selection =
    | Field of Field
    | FragmentSpread of FragmentSpread
    /// 2.2.6.2 Inline Fragments
    | InlineFragment of FragmentDefinition

    member x.Directives =
        match x with
        | Field f -> f.Directives
        | FragmentSpread s -> s.Directives
        | InlineFragment f -> f.Directives

/// 2.2.3 Fields
and Field = {
    /// 2.2.5 Field Alias
    Alias : string voption
    Name : string
    Arguments : Argument list
    Directives : Directive list
    SelectionSet : Selection list
} with

    member x.AliasOrName =
        match x.Alias with
        | ValueSome alias -> alias
        | ValueNone -> x.Name

/// 2.2.4 Arguments
and Argument = { Name : string; Value : InputValue }

/// 2.2.6 Fragments
and FragmentSpread = { Name : string; Directives : Directive list }

and FragmentDefinition = {
    Name : string voption
    /// 2.2.6.1 Type Conditions
    TypeCondition : string voption
    Directives : Directive list
    SelectionSet : Selection list
}

/// 2.9 Input Values
and InputValue =
    /// 2.9.1 Int Value
    | IntValue of int64
    /// 2.9.2 Float Value
    | FloatValue of double
    /// 2.9.3 Boolean Value
    | BooleanValue of bool
    /// 2.9.4 String Value
    | StringValue of string
    /// 2.9.5 Null Value
    | NullValue
    /// 2.9.6 Enum Value
    | EnumValue of string
    /// 2.9.7 List Value
    | ListValue of InputValue list
    /// 2.9.8 Input Object Values
    | ObjectValue of Map<string, InputValue>
    /// 2.10 Variables
    | VariableName of string

    with
        static member OfObject (obj : obj) =
            match obj with
            | null -> NullValue
            | :? int64 as value -> IntValue value
            | :? int32 as value -> IntValue (int64 value)
            | :? int16 as value -> IntValue (int64 value)
            | :? double as value -> FloatValue value
            | :? single as value -> FloatValue (double value)
            | :? bool as value -> BooleanValue value
            | :? string as value -> StringValue value
            | :? uint64 as value -> IntValue (int64 value)
            | :? uint32 as value -> IntValue (int64 value)
            | :? uint16 as value -> IntValue (int64 value)
            | value ->
                let ``type`` = value.GetType()
                if ``type``.IsArray then
                    let array = value :?> System.Array
                    let list = [ for i in 0 .. array.Length - 1 -> InputValue.OfObject (array.GetValue i) ]
                    ListValue list
                else
                    let genericType = ``type``.GetGenericTypeDefinition()
                    if typeof<System.Collections.Generic.IReadOnlyDictionary<string, obj>>.IsAssignableFrom genericType then
                        let dict = value :?> System.Collections.Generic.IReadOnlyDictionary<string, obj>
                        let map =
                            dict
                            |> Seq.map (fun kv -> kv.Key.ToString(), InputValue.OfObject kv.Value)
                            |> Map.ofSeq
                        ObjectValue map
                    else
                        failwith "Cannot convert object to 'InputValue'"

        static member OfJsonElement (element : JsonElement) =
            match element.ValueKind with
            | JsonValueKind.Null -> NullValue
            | JsonValueKind.True -> BooleanValue true
            | JsonValueKind.False -> BooleanValue false
            | JsonValueKind.String -> StringValue (element.GetString ())
            | JsonValueKind.Number -> FloatValue (element.GetDouble ())
            | JsonValueKind.Array ->
                ListValue (
                    element.EnumerateArray ()
                    |> Seq.map InputValue.OfJsonElement
                    |> List.ofSeq
                )
            | JsonValueKind.Object ->
                ObjectValue (
                    element.EnumerateObject ()
                    |> Seq.map (fun p -> p.Name, InputValue.OfJsonElement p.Value)
                    |> Map.ofSeq
                )
            | _ -> raise (NotSupportedException "Unsupported JSON element type")

/// 2.2.8 Variables
and VariableDefinition = { VariableName : string; Type : InputType; DefaultValue : InputValue option }

/// 2.2.9 Input Types
and InputType =
    | NamedType of string
    | ListType of InputType
    | NonNullType of InputType

    override x.ToString () =
        let rec str =
            function
            | NamedType name -> name
            | ListType inner -> "[" + (str inner) + "]"
            | NonNullType inner -> (str inner) + "!"
        str x

/// 2.2.10 Directives
and Directive = {
    Name : string
    Arguments : Argument list
} with

    member x.If = x.Arguments |> List.find (fun arg -> arg.Name = "if")

// Type System Definition

and OperationTypeDefinition = { Type : string; Operation : OperationType }

and SchemaDefintion = { OperationTypes : OperationTypeDefinition }

and ObjectTypeDefinition = { Name : string; Interfaces : string[]; Fields : FieldDefinition[] }

and FieldDefinition = { Name : string; Arguments : InputValueDefinition[]; Type : InputType }

and InputValueDefinition = { Name : string; Type : InputType; DefaultValue : InputValue voption }

and InterfaceTypeDefinition = { Name : string; Fields : FieldDefinition[] }

and UnionTypeDefinition = { Name : string; Types : string[] }

and EnumTypeDefinition = { Name : string; Values : string[] }

and InputObjectTypeDefinition = { Name : string; Fields : InputValueDefinition[] }

and TypeDefinition =
    | ScalarTypeDefinition of string
    | ObjectTypeDefinition of ObjectTypeDefinition
    | InterfaceTypeDefinition of InterfaceTypeDefinition
    | UnionTypeDefinition of UnionTypeDefinition
    | EnumTypeDefinition of EnumTypeDefinition
    | InputObjectTypeDefinition of InputObjectTypeDefinition
