/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
namespace FSharp.Data.GraphQL.Ast

//NOTE: For references, see https://facebook.github.io/graphql/

/// 2.2 Query Document
type Document = {
    Definitions: Definition list
}

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
and OperationDefinition =
    {
        OperationType: OperationType
        Name: string option
        VariableDefinitions: VariableDefinition list
        Directives: Directive list
        SelectionSet: Selection list
    }
    member x.IsShortHandQuery =
        x.OperationType = Query && x.Name.IsNone && x.VariableDefinitions.IsEmpty && x.Directives.IsEmpty

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
and Field = 
    {
        /// 2.2.5 Field Alias
        Alias: string option
        Name: string
        Arguments: Argument list
        Directives: Directive list
        SelectionSet: Selection list
    }
    member x.AliasOrName = 
        match x.Alias with
        | Some alias -> alias
        | None -> x.Name

/// 2.2.4 Arguments
and Argument = {
    Name: string
    Value: Value
}

/// 2.2.6 Fragments
and FragmentSpread = {
    Name: string
    Directives: Directive list
}

and FragmentDefinition = {
    Name: string option
    /// 2.2.6.1 Type Conditions
    TypeCondition: string option
    Directives: Directive list
    SelectionSet: Selection list
}

/// 2.9 Input Values
and Value = 
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
    | ListValue of Value list
    /// 2.9.8 Input Object Values
    | ObjectValue of Map<string, Value>
    /// 2.10 Variables
    | Variable of string

/// 2.2.8 Variables
and VariableDefinition = 
    { VariableName: string
      Type: InputType
      DefaultValue: Value option }

/// 2.2.9 Input Types
and InputType = 
    | NamedType of string
    | ListType of InputType
    | NonNullType of InputType
    override x.ToString() =
        let rec str = function
            | NamedType name -> name
            | ListType inner -> "[" + (str inner) + "]"
            | NonNullType inner -> (str inner) + "!"
        str x

/// 2.2.10 Directives
and Directive = 
    {
        Name: string
        Arguments: Argument list
    }
    member x.If = x.Arguments |> List.find (fun arg -> arg.Name = "if")

// Type System Definition

and OperationTypeDefinition = {
    Type: string
    Operation: OperationType
}

and SchemaDefintion = {
    OperationTypes: OperationTypeDefinition
}

and ObjectTypeDefinition = {
    Name: string
    Interfaces: string []
    Fields: FieldDefinition []
}

and FieldDefinition = {
    Name: string
    Arguments: InputValueDefinition []
    Type: InputType
}

and InputValueDefinition = {
    Name: string
    Type: InputType
    DefaultValue: Value option
}

and InterfaceTypeDefinition = {
    Name: string
    Fields: FieldDefinition []
}

and UnionTypeDefinition = {
    Name: string
    Types: string []
}

and EnumTypeDefinition = {
    Name: string
    Values: string []
}

and InputObjectTypeDefinition = {
    Name: string
    Fields: InputValueDefinition []
}

and TypeDefinition =
    | ScalarTypeDefinition of string
    | ObjectTypeDefinition of ObjectTypeDefinition
    | InterfaceTypeDefinition of InterfaceTypeDefinition
    | UnionTypeDefinition of UnionTypeDefinition
    | EnumTypeDefinition of EnumTypeDefinition
    | InputObjectTypeDefinition of InputObjectTypeDefinition

        