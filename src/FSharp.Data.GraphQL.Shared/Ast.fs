// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc
// Copyright (c) 2019 Henrik Feldt

namespace FSharp.Data.GraphQL.Ast

open System

/// There are three types of operations that GraphQL models.
///
/// Each operation is represented by an optional operation name and a selection set.
///
/// https://spec.graphql.org/October2021/#sec-Language.Operations
/// https://spec.graphql.org/October2021/#OperationType
type OperationType =
    /// A read-only fetch.
    | Query
    /// A write followed by a fetch.
    | Mutation
    /// A long-lived request that fetches data in response to source events.
    | Subscription


/// 2.9 Input Values
and InputValue =
    /// 2.9.1 Int InputValue
    /// https://spec.graphql.org/October2021/#sec-Int-Value
    | IntValue of int64
    /// 2.9.2 Float InputValue
    /// https://spec.graphql.org/October2021/#sec-Float-Value
    | FloatValue of double
    /// 2.9.3 Boolean InputValue
    /// https://spec.graphql.org/October2021/#sec-Boolean-Value
    | BooleanValue of bool
    /// 2.9.4 String InputValue
    /// https://spec.graphql.org/October2021/#sec-String-Value
    | StringValue of string
    /// 2.9.5 Null InputValue
    /// https://spec.graphql.org/October2021/#sec-Null-Value
    | NullValue
    /// 2.9.6 Enum InputValue
    /// not "true", "false" or "null"
    /// https://spec.graphql.org/October2021/#sec-Enum-Value
    | EnumValue of string
    /// 2.9.7 List InputValue
    /// May be empty.
    /// https://spec.graphql.org/October2021/#sec-List-Value
    | ListValue of InputValue list
    /// 2.9.8 Input Object Values
    /// May be empty.
    /// https://spec.graphql.org/October2021/#sec-Input-Object-Values
    /// Contains ObjectField: https://spec.graphql.org/October2021/#ObjectField
    | ObjectValue of Map<string, InputValue>
    /// 2.10 Variables
    /// if not Const
    /// https://spec.graphql.org/October2021/#Variable
    | VariableName of string

/// 2.6 Arguments
///
/// Fields are conceptually functions which return values, and occasionally accept arguments which alter their behavior.
/// These arguments often map directly to function arguments within a GraphQL service’s implementation.
///
/// https://spec.graphql.org/October2021/#sec-Language.Arguments
/// https://spec.graphql.org/October2021/#Arguments
type Argument = { Name : string; Value : InputValue }

/// 2.12 Directives
///
/// Directives provide a way to describe alternate runtime execution and type validation behavior in a GraphQL document.
///
/// In some cases, you need to provide options to alter GraphQL’s execution behavior in ways field arguments will not suffice,
/// such as conditionally including or skipping a field. Directives provide this by describing additional information to the
/// executor.
///
/// Directives have a name along with a list of arguments which may accept values of any input type.
///
/// Directives can be used to describe additional information for types, fields, fragments and operations.
///
/// As future versions of GraphQL adopt new configurable execution capabilities, they may be exposed via directives. GraphQL
/// services and tools may also provide any additional custom directive beyond those described here.
///
/// Directives may be provided in a specific syntactic order which may have semantic interpretation.
///
/// https://spec.graphql.org/October2021/#sec-Language.Directives
/// https://spec.graphql.org/October2021/#Directive
type Directive = {
    /// https://spec.graphql.org/October2021/#Name
    Name : string
    /// https://spec.graphql.org/October2021/#Arguments
    Arguments : Argument list
} with

    member x.If = x.Arguments |> List.find (fun arg -> arg.Name = "if")

/// 2.8 Fragments
///
/// Fragments are the primary unit of composition in GraphQL.
///
/// Fragments allow for the reuse of common repeated selections of fields, reducing duplicated text in the document. Inline
/// Fragments can be used directly within a selection to condition upon a type condition when querying against an interface or
/// union.
///
/// https://spec.graphql.org/October2021/#sec-Language.Fragments
/// https://spec.graphql.org/October2021/#FragmentSpread
type FragmentSpread = {
    Name : string
    /// Maybe empty.
    /// https://spec.graphql.org/October2021/#Directives
    Directives : Directive list
}

type IFragmentDefinition =
    abstract Name : string option
    abstract TypeCondition : string option
    abstract Directives : Directive list
    abstract SelectionSet : Selection list

/// <summary><para>
/// 2.8 Fragments
/// </para>
///
/// <para>Fragments are the primary unit of composition in GraphQL.</para>
///
/// <para>Fragments allow for the reuse of common repeated selections of fields, reducing duplicated text in the document. Inline
/// Fragments can be used directly within a selection to condition upon a type condition when querying against an interface or
/// union.</para>
///
/// <para>Example: <c>fragment userFragment on User</c></para>
///
/// <para>https://spec.graphql.org/October2021/#sec-Language.Fragments</para>
///
/// <para>https://spec.graphql.org/October2021/#FragmentDefinition</para>
/// </summary>
and FragmentDefinition = {
    /// Name, but not the constant "on"
    Name : string

    /// 2.8.1 Type Conditions
    ///
    /// Fragments must specify the type they apply to. In this example, friendFields can be used in the context of querying a
    /// User.
    ///
    /// Fragments cannot be specified on any input value (scalar, enumeration, or input object).
    ///
    /// Selections within fragments only return values when the concrete type of the object it is operating on matches the
    /// type of the fragment.
    ///
    /// Example: `fragment userFragment on User`
    ///
    /// https://spec.graphql.org/October2021/#sec-Type-Conditions
    TypeCondition : string

    /// May be empty
    Directives : Directive list

    /// May not be empty
    SelectionSet : Selection list
} with

    interface IFragmentDefinition with
        member x.Name = Some x.Name
        member x.TypeCondition = Some x.TypeCondition
        member x.Directives = x.Directives
        member x.SelectionSet = x.SelectionSet


/// Fragments can be defined inline within a selection set. This is done to conditionally include fields based on their runtime
/// type. This feature of standard fragment inclusion was demonstrated in the query FragmentTyping example. We could accomplish
/// the same thing using inline fragments.
///
/// https://spec.graphql.org/October2021/#sec-Inline-Fragments
/// https://spec.graphql.org/October2021/#InlineFragment
and InlineFragment = {
    /// 2.8.1 Type Conditions
    ///
    /// Fragments must specify the type they apply to. In this example, friendFields can be used in the context of querying a
    /// User.
    ///
    /// Fragments cannot be specified on any input value (scalar, enumeration, or input object).
    ///
    /// Selections within fragments only return values when the concrete type of the object it is operating on matches the
    /// type of the fragment.
    ///
    /// Example: `fragment userFragment on User`
    ///
    /// https://spec.graphql.org/October2021/#sec-Type-Conditions
    TypeCondition : string option

    /// May be empty
    Directives : Directive list

    /// May not be empty
    SelectionSet : Selection list
} with

    interface IFragmentDefinition with
        member x.Name = None
        member x.TypeCondition = x.TypeCondition
        member x.Directives = x.Directives
        member x.SelectionSet = x.SelectionSet


/// <summary><para>
/// 2.4 Selection Sets
/// </para><para>
/// An operation selects the set of information it needs, and will receive exactly that information and nothing more, avoiding
/// over-fetching and under-fetching data.
/// </para></summary>
/// <remarks><para>
/// <code language="graphql">
/// {
///   id
///   firstName
///   lastName
/// }
/// </code>
/// </para><para>
/// https://spec.graphql.org/October2021/#sec-Selection-Sets
/// </para><para>
/// https://spec.graphql.org/October2021/#Selection
/// </para></remarks>
and Selection =
    /// https://spec.graphql.org/October2021/#Field
    | Field of Field
    /// https://spec.graphql.org/October2021/#FragmentSpread
    | FragmentSpread of FragmentSpread
    /// https://spec.graphql.org/October2021/#InlineFragment
    | InlineFragment of InlineFragment

    member x.Directives =
        match x with
        | Field f -> f.Directives
        | FragmentSpread s -> s.Directives
        | InlineFragment f -> f.Directives

/// 2.5 Fields
/// https://spec.graphql.org/October2021/#sec-Language.Fields
///
/// A selection set is primarily composed of fields. A field describes one discrete piece of information available to request
/// within a selection set.
///
/// Some fields describe complex data or relationships to other data. In order to further explore this data, a field may itself
/// contain a selection set, allowing for deeply nested requests. All GraphQL operations must specify their selections down to
/// fields which return scalar values to ensure an unambiguously shaped response.
and Field = {
    /// 2.7 Field Alias
    ///
    /// By default a field’s response key in the response object will use that field’s name. However, you can define a
    /// different response key by specifying an alias.
    ///
    /// https://spec.graphql.org/October2021/#sec-Field-Alias
    /// https://spec.graphql.org/October2021/#Alias
    Alias : string option

    /// https://spec.graphql.org/October2021/#Name
    Name : string

    /// Arguments are unordered.
    /// https://spec.graphql.org/October2021/#Arguments
    Arguments : Argument list

    /// May be empty.
    /// Arguments are ordered.
    /// https://spec.graphql.org/October2021/#Directives
    Directives : Directive list

    /// May be empty.
    /// https://spec.graphql.org/October2021/#SelectionSet
    SelectionSet : Selection list
} with

    member x.AliasOrName =
        match x.Alias with
        | Some alias -> alias
        | None -> x.Name


/// 2.11 Type References
/// Renamed from "Type" as this is frequently used in .Net.
/// https://spec.graphql.org/October2021/#sec-Type-References
/// https://spec.graphql.org/October2021/#Type
type TypeReference =
    /// https://spec.graphql.org/October2021/#NamedType
    | NamedType of string
    /// https://spec.graphql.org/October2021/#ListType
    | ListType of TypeReference
    /// https://spec.graphql.org/October2021/#NamedType
    /// https://spec.graphql.org/October2021/#NonNullType
    | NonNullNameType of string
    /// https://spec.graphql.org/October2021/#ListType
    /// https://spec.graphql.org/October2021/#NonNullType
    | NonNullListType of TypeReference

    override x.ToString () =
        let rec str =
            function
            | NamedType name -> name
            | ListType inner -> $"[{str inner}]"
            | NonNullNameType name -> $"{name}!"
            | NonNullListType inner -> $"{str (ListType inner)}!"

        str x


/// 2.10 Variables
/// A GraphQL operation can be parameterized with variables, maximizing reuse, and avoiding costly string building in clients at
/// runtime.
///
/// If not defined as constant (for example, in DefaultValue), a Variable can be supplied for an input value.
///
/// Variables must be defined at the top of an operation and are in scope throughout the execution of that operation. Values for
/// those variables are provided to a GraphQL service as part of a request so they may be substituted in during execution.
///
/// Variables can be used within fragments. Variables have global scope with a given operation, so a variable used within a
/// fragment must be declared in any top-level operation that transitively consumes that fragment. If a variable is referenced in
/// a fragment and is included by an operation that does not define that variable, that operation is invalid (see All Variable
/// Uses Defined). (from https://spec.graphql.org/October2021/#sec-Language.Variables.Variable-use-within-Fragments)
///
/// https://spec.graphql.org/October2021/#sec-Language.Variables
/// https://spec.graphql.org/October2021/#VariableDefinition
type VariableDefinition = {
    /// https://spec.graphql.org/October2021/#Variable
    VariableName : string
    /// https://spec.graphql.org/October2021/#Type
    Type : TypeReference
    /// https://spec.graphql.org/October2021/#DefaultValue
    DefaultValue : InputValue option
    /// May be empty.
    /// https://spec.graphql.org/October2021/#Directives
    Directives : Directive list
}

/// 2.3 Operations
///
/// There are three types of operations that GraphQL models:
///
/// - query – a read-only fetch.
/// - mutation – a write followed by a fetch.
/// - subscription – a long-lived request that fetches data in response to source events.
///
/// Each operation is represented by an optional operation name and a selection set.
///
/// https://spec.graphql.org/October2021/#sec-Language.Operations
type OperationDefinition = {
    /// Defaults to `query`; called "query shorthand".
    OperationType : OperationType

    /// https://spec.graphql.org/October2021/#Name
    Name : string option

    /// May be empty
    /// https://spec.graphql.org/October2021/#VariableDefinitions
    VariableDefinitions : VariableDefinition list

    /// May be empty
    Directives : Directive list

    /// May not be empty
    SelectionSet : Selection list
} with

    /// Create a new Operation Definition.
    static member Create (selectionSet, ?op : OperationType) = {
        OperationType = defaultArg op Query
        Name = None
        VariableDefinitions = []
        Directives = []
        SelectionSet = selectionSet
    }

    member x.IsShortHandQuery =
        x.OperationType = Query
        && x.Name.IsNone
        && x.VariableDefinitions.IsEmpty
        && x.Directives.IsEmpty

type InputDefinition = string

/// GraphQL field input definition. Can be used as fields for
/// input objects or as arguments for any ordinary field definition.
type InputFieldDefinition = {
    /// Name of the input field / argument.
    Name : string
    /// Optional input field / argument description.
    Description : string option
    /// GraphQL type definition of the input type.
    TypeDef : InputDefinition
    /// Optional default input value - used when no input was provided.
    DefaultValue : InputValue option
}

/// 3.3.1 Root Operation Types
///
/// A schema defines the initial root operation type for each kind of operation it supports: query, mutation, and subscription;
/// this determines the place in the type system where those operations begin.
///
/// The query root operation type must be provided and must be an Object type.
///
/// The mutation root operation type is optional; if it is not provided, the service does not support mutations. If it is
/// provided, it must be an Object type.
///
/// Similarly, the subscription root operation type is also optional; if it is not provided, the service does not support
/// subscriptions. If it is provided, it must be an Object type.
///
/// The query, mutation, and subscription root types must all be different types if provided.
///
/// The fields on the query root operation type indicate what fields are available at the top level of a GraphQL query operation.
///
/// Default Root Operation Type Names:
///
/// While any type can be the root operation type for a GraphQL operation, the type system definition language can omit the schema
/// definition when the query, mutation, and subscription root types are named "Query", "Mutation", and "Subscription" respectively.
///
/// Likewise, when representing a GraphQL schema using the type system definition language, a schema definition should be omitted
/// if it only uses the default root operation type names.
///
/// This example describes a valid complete GraphQL schema, despite not explicitly including a schema definition.
/// The "Query" type is presumed to be the query root operation type of the schema.
///
/// Also see 3.3 Schema.
/// https://spec.graphql.org/October2021/#RootOperationTypeDefinition
type RootOperationTypeDefinition = {
    /// https://spec.graphql.org/October2021/#OperationType
    Operation : OperationType
    /// https://spec.graphql.org/October2021/#NamedType
    NamedType : string
}

/// 3.3 Schema
///
/// A GraphQL service’s collective type system capabilities are referred to as that service’s “schema”. A schema is defined in
/// terms of the types and directives it supports as well as the root operation types for each kind of operation: query, mutation,
/// and subscription; this determines the place in the type system where those operations begin.
///
/// A GraphQL schema must itself be internally valid. This section describes the rules for this validation process where relevant.
///
/// All types within a GraphQL schema must have unique names. No two provided types may have the same name. No provided type may
/// have a name which conflicts with any built in types (including Scalar and Introspection types).
///
/// All directives within a GraphQL schema must have unique names.
///
/// All types and directives defined within a schema must not have a name which begins with "__" (two underscores), as this is
/// used exclusively by GraphQL’s introspection system.
///
/// Example <c>"My schema" schema @important { ... }</c> or <c>schema { ... }</c>.
///
/// https://spec.graphql.org/October2021/#sec-Schema
/// https://spec.graphql.org/October2021/#SchemaDefinition
type SchemaDefinition = {
    /// https://spec.graphql.org/October2021/#Description
    Description : string option

    /// May be empty.
    /// https://spec.graphql.org/October2021/#Directives
    Directives : Directive list

    /// https://spec.graphql.org/October2021/#RootOperationTypeDefinition
    RootOperationTypes : RootOperationTypeDefinition
}

/// 3.6.1 Field Arguments
///
/// Object fields are conceptually functions which yield values. Occasionally object fields can accept arguments to further
/// specify the return value. Object field arguments are defined as a list of all possible argument names and their expected input
/// types.
///
/// All arguments defined within a field must not have a name which begins with "__" (two underscores), as this is used
/// exclusively by GraphQL’s introspection system.
///
/// https://spec.graphql.org/October2021/#InputValueDefinition
type InputValueDefinition = {
    Description : string option

    Name : string

    /// https://spec.graphql.org/October2021/#Type
    Type : TypeReference

    /// https://spec.graphql.org/October2021/#DefaultValue
    DefaultValue : InputValue option

    /// May be empty
    /// https://spec.graphql.org/October2021/#Directives
    Directives : Directive list
}

/// https://spec.graphql.org/October2021/#FieldDefinition
type FieldDefinition = {
    /// https://spec.graphql.org/October2021/#Description
    Description : string option
    Name : string
    /// May be empty
    /// https://spec.graphql.org/October2021/#ArgumentsDefinition
    Arguments : InputValueDefinition list
    /// https://spec.graphql.org/October2021/#Type
    Type : TypeReference
    /// May be empty
    /// https://spec.graphql.org/October2021/#Directives
    Directives : Directive list
}

// TypeDefinitions:

/// 3.5 Scalars
///
/// Scalar types represent primitive leaf values in a GraphQL type system. GraphQL responses take the form of a hierarchical tree;
/// the leaves of this tree are typically GraphQL Scalar types (but may also be Enum types or null values).
///
/// GraphQL provides a number of built-in scalars which are fully defined in the sections below, however type systems may also add
/// additional custom scalars to introduce additional semantic meaning.
///
/// Built-in Scalars:
///
/// GraphQL specifies a basic set of well-defined Scalar types: Int, Float, String, Boolean, and ID. A GraphQL framework should
/// support all of these types, and a GraphQL service which provides a type by these names must adhere to the behavior described
/// for them in this document. As an example, a service must not include a type called Int and use it to represent 64-bit numbers,
/// internationalization information, or anything other than what is defined in this document.
///
/// When returning the set of types from the __Schema introspection type, all referenced built-in scalars must be included. If a
/// built-in scalar type is not referenced anywhere in a schema (there is no field, argument, or input field of that type) then it
/// must not be included.
///
/// When representing a GraphQL schema using the type system definition language, all built-in scalars must be omitted for
/// brevity.
///
/// https://spec.graphql.org/October2021/#ScalarTypeDefinition
type ScalarTypeDefinition = {
    Description : string option

    Name : string

    /// May be empty.
    Directives : Directive list
}

/// 3.5.6 Scalar Extensions
///
/// Scalar type extensions are used to represent a scalar type which has been extended from some original scalar type. For example,
/// this might be used by a GraphQL tool or service which adds directives to an existing scalar.
///
/// Type Validation:
///
/// Scalar type extensions have the potential to be invalid if incorrectly defined.
///
/// 1. The named type must already be defined and must be a Scalar type.
/// 2. Any non-repeatable directives provided must not already apply to the original Scalar type.
///
/// Example: <c>extend scalar [name] directive...</c>
///
/// https://spec.graphql.org/October2021/#sec-Scalar-Extensions
type ScalarTypeExtension = {
    Name : string

    /// May not be empty.
    /// https://spec.graphql.org/October2021/#Directives
    Directives : Directive list
}

/// 3.6 Objects
///
/// GraphQL operations are hierarchical and composed, describing a tree of information. While Scalar types describe the leaf
/// values of these hierarchical operations, Objects describe the intermediate levels.
///
/// GraphQL Objects represent a list of named fields, each of which yield a value of a specific type. Object values should be
/// serialized as ordered maps, where the selected field names (or aliases) are the keys and the result of evaluating the field
/// is the value, ordered by the order in which they appear in the selection set.
///
/// All fields defined within an Object type must not have a name which begins with "__" (two underscores), as this is used
/// exclusively by GraphQL’s introspection system.
///
/// https://spec.graphql.org/October2021/#sec-Objects
type ObjectTypeDefinition = {
    Description : string option
    // "type"
    Name : string
    // "implements .. & .. & .."
    /// May be empty
    /// https://spec.graphql.org/October2021/#ImplementsInterfaces
    ImplementsInterfaces : string list
    /// May be empty.
    /// https://spec.graphql.org/October2021/#Directives
    Directives : Directive list
    // "{" fields "}"
    Fields : FieldDefinition list
}

/// 3.6.3 Object Extensions
///
/// Object type extensions are used to represent a type which has been extended from some original type. For example, this might
/// be used to represent local data, or by a GraphQL service which is itself an extension of another GraphQL service.
///
/// Object type extensions may choose not to add additional fields, instead only adding interfaces or directives.
///
/// https://spec.graphql.org/October2021/#sec-Object-Extensions
/// https://spec.graphql.org/October2021/#ObjectTypeExtension
type ObjectTypeExtension = {
    /// May be empty.
    Interfaces : string list
    /// May be empty.
    /// https://spec.graphql.org/October2021/#Directives
    Directives : Directive list
    /// May be empty.
    Fields : FieldDefinition list
}

/// 3.7 Interfaces
///
/// GraphQL interfaces represent a list of named fields and their arguments. GraphQL objects and interfaces can then implement
/// these interfaces which requires that the implementing type will define all fields defined by those interfaces.
///
/// Fields on a GraphQL interface have the same rules as fields on a GraphQL object; their type can be Scalar, Object, Enum,
/// Interface, or Union, or any wrapping type whose base type is one of those five.
///
/// For example, an interface NamedEntity may describe a required field and types such as Person or Business may then implement
/// this interface to guarantee this field will always exist.
///
/// Types may also implement multiple interfaces. For example, Business implements both the NamedEntity and ValuedEntity
/// interfaces in the example below.
///
/// https://spec.graphql.org/October2021/#sec-Interfaces
type InterfaceTypeDefinition = {
    Description : string option
    Name : string
    /// May be empty.
    /// https://spec.graphql.org/October2021/#Directives
    Directives : Directive list
    /// May not be empty.
    Fields : FieldDefinition list
}

/// 3.7.1 Interface Extensions
///
/// Interface type extensions are used to represent an interface which has been extended from some original interface. For
/// example, this might be used to represent common local data on many types, or by a GraphQL service which is itself an
/// extension of another GraphQL service.
///
/// Interface type extensions may choose not to add additional fields, instead only adding directives.
///
/// https://spec.graphql.org/October2021/#sec-Interface-Extensions
type InterfaceTypeExtension = {
    Name : string

    /// May be empty if fields is non-empty.
    ///
    /// May not be empty if fields is empty.
    /// https://spec.graphql.org/October2021/#Directives
    Directives : Directive list

    /// May not empty if directives is empty.
    ///
    /// May be empty if directives is not empty.
    Fields : FieldDefinition list
}

/// https://spec.graphql.org/October2021/#UnionMemberTypes
type UnionMemberType = string

/// 3.8 Unions
///
/// GraphQL Unions represent an object that could be one of a list of GraphQL Object types, but provides for no guaranteed
/// fields between those types. They also differ from interfaces in that Object types declare what interfaces they implement,
/// but are not aware of what unions contain them.
///
/// With interfaces and objects, only those fields defined on the type can be queried directly; to query other fields on an
/// interface, typed fragments must be used. This is the same as for unions, but unions do not define any fields, so no fields
/// may be queried on this type without the use of type refining fragments or inline fragments (with the exception of the meta-
/// field __typename).
///
/// https://spec.graphql.org/October2021/#sec-Unions
type UnionTypeDefinition = {
    /// Optional description
    Description : string option

    /// The name of the union
    Name : string

    /// May be empty.
    /// https://spec.graphql.org/October2021/#Directives
    Directives : Directive list

    /// May be empty. (!)
    /// https://spec.graphql.org/October2021/#UnionMemberTypes
    Types : UnionMemberType list
}

/// 3.8.1 Union Extensions
///
/// Union type extensions are used to represent a union type which has been extended from some original union type. For example,
/// this might be used to represent additional local data, or by a GraphQL service which is itself an extension of another
/// GraphQL service.
///
/// https://spec.graphql.org/October2021/#sec-Unions
type UnionTypeExtension = {
    Name : string

    /// May be empty.
    /// https://spec.graphql.org/October2021/#Directives
    Directives : Directive list

    /// May not be empty.
    /// https://spec.graphql.org/October2021/#UnionMemberTypes
    Types : UnionMemberType list
}

/// 3.9 Enums
///
/// GraphQL Enum types, like Scalar types, also represent leaf values in a GraphQL type system. However Enum types describe the
/// set of possible values.
///
/// Enums are not references for a numeric value, but are unique values in their own right. They may serialize as a string: the
/// name of the represented value.
///
/// https://spec.graphql.org/October2021/#sec-Enums
/// https://spec.graphql.org/October2021/#EnumValueDefinition
type EnumValueDefinition = {
    Description : string option

    Name : string

    /// May be empty.
    /// https://spec.graphql.org/October2021/#Directives
    Directives : Directive list
}

/// 3.9 Enums
///
/// GraphQL Enum types, like Scalar types, also represent leaf values in a GraphQL type system. However Enum types describe the
/// set of possible values.
///
/// Enums are not references for a numeric value, but are unique values in their own right. They may serialize as a string: the
/// name of the represented value.
///
/// https://spec.graphql.org/October2021/#sec-Enums
/// https://spec.graphql.org/October2021/#EnumTypeDefinition
type EnumTypeDefinition = {
    Description : string option
    Name : string

    Directives : Directive list
    Values : EnumValueDefinition list
}

/// 3.9.1 Enum Extensions
///
/// Enum type extensions are used to represent an enum type which has been extended from some original enum type. For example,
/// this might be used to represent additional local data, or by a GraphQL service which is itself an extension of another
/// GraphQL service.
///
/// https://spec.graphql.org/October2021/#sec-Enum-Extensions
type EnumTypeExtension = {
    Name : string

    /// May be empty if fields is non-empty.
    ///
    /// May not be empty if fields is empty.
    /// https://spec.graphql.org/October2021/#Directives
    Directives : Directive list

    /// May not empty if directives is empty.
    ///
    /// May be empty if directives is not empty.
    Values : EnumValueDefinition list
}


/// 3.10 Input Objects
///
/// Fields may accept arguments to configure their behavior. These inputs are often scalars or enums, but they sometimes need to
/// represent more complex values.
///
/// A GraphQL Input Object defines a set of input fields; the input fields are either scalars, enums, or other input objects.
/// This allows arguments to accept arbitrarily complex structs.
///
/// Circular References:
///
/// Input Objects are allowed to reference other Input Objects as field types. A circular reference occurs when an Input Object
/// references itself either directly or through referenced Input Objects.
///
/// Circular references are generally allowed, however they may not be defined as an unbroken chain of Non-Null singular fields.
/// Such Input Objects are invalid because there is no way to provide a legal value for them.
///
/// Note: there's a long section on how input values are coerced in the spec.
///
/// https://spec.graphql.org/October2021/#sec-Input-Objects
type InputObjectTypeDefinition = {
    Description : string option

    // "input"

    Name : string

    /// May be empty if fields is non-empty.
    ///
    /// May not be empty if fields is empty.
    /// https://spec.graphql.org/October2021/#Directives
    Directives : Directive list

    /// "{" Fields "}"
    ///
    /// May not empty if directives is empty.
    ///
    /// May be empty if directives is not empty.
    ///
    /// https://spec.graphql.org/October2021/#InputFieldsDefinition
    Fields : InputValueDefinition list
}

/// 3.10.1 Input Object Extensions
///
/// Input object type extensions are used to represent an input object type which has been extended from some original input
/// object type. For example, this might be used by a GraphQL service which is itself an extension of another GraphQL service.
///
/// https://spec.graphql.org/October2021/#sec-Input-Object-Extensions
type InputObjectTypeExtension = { Name : string; Directives : Directive list; Fields : InputValueDefinition[] }

type TypeDefinition =
    /// https://spec.graphql.org/October2021/#ScalarTypeExtension
    | ScalarTypeDefinition of ScalarTypeDefinition
    | ObjectTypeDefinition of ObjectTypeDefinition
    | InterfaceTypeDefinition of InterfaceTypeDefinition
    | UnionTypeDefinition of UnionTypeDefinition
    | EnumTypeDefinition of EnumTypeDefinition
    | InputObjectTypeDefinition of InputObjectTypeDefinition

    member x.Directives =
        match x with
        | ScalarTypeDefinition std -> std.Directives
        | ObjectTypeDefinition otd -> otd.Directives
        | InterfaceTypeDefinition itd -> itd.Directives
        | UnionTypeDefinition utd -> utd.Directives
        | EnumTypeDefinition etd -> etd.Directives
        | InputObjectTypeDefinition iotd -> iotd.Directives

/// https://spec.graphql.org/October2021/#ExecutableDirectiveLocation
[<Flags>]
type ExecutableDirectiveLocation =
    | QUERY = 1
    | MUTATION = 2
    | SUBSCRIPTION = 4
    | FIELD = 8
    | FRAGMENT_DEFINITION = 16
    | FRAGMENT_SPREAD = 32
    | INLINE_FRAGMENT = 64


/// https://spec.graphql.org/October2021/#TypeSystemDirectiveLocation
[<Flags>]
type TypeSystemDirectiveLocation =
    | SCHEMA = 1
    | SCALAR = 2
    | OBJECT = 4
    | FIELD_DEFINITION = 8
    | ARGUMENT_DEFINITION = 16
    | INTERFACE = 32
    | UNION = 64
    | ENUM = 128
    | ENUM_VALUE = 256
    | INPUT_OBJECT = 512
    | INPUT_FIELD_DEFINITION = 1024


type DirectiveLocation =
    /// https://spec.graphql.org/October2021/#ExecutableDirectiveLocation
    | ExecutableDirectiveLocation of ExecutableDirectiveLocation
    /// https://spec.graphql.org/October2021/#TypeSystemDirectiveLocation
    | TypeSystemDirectiveLocation of TypeSystemDirectiveLocation

/// 3.13 Directives
///
/// DirectiveDefinition:
///  Description opt  directive @ `Name` `ArgumentsDefinition` opt  on `DirectiveLocations`
///
/// https://spec.graphql.org/October2021/#sec-Type-System.Directives
/// https://spec.graphql.org/October2021/#DirectiveDefinition
type DirectiveDefinition = {
    /// Optional directive description.
    Description : string option

    // "directive" "@"

    /// Directive's name - it's NOT '@' prefixed.
    Name : string

    /// https://spec.graphql.org/October2021/#ArgumentsDefinition
    Arguments : InputValueDefinition list

    // [ "repeatable" ] "on"

    /// Directive location - describes, which part's of the query AST are valid places to include current directive to.
    /// https://spec.graphql.org/October2021/#DirectiveLocation
    Locations : DirectiveLocation
}


/// 3.0 Type System
///
/// The GraphQL Type system describes the capabilities of a GraphQL service and is used to determine if a requested operation is
/// valid, to guarantee the type of response results, and describes the input types of variables to determine if values provided
/// at request time are valid.
///
/// The GraphQL language includes an IDL used to describe a GraphQL service’s type system. Tools may use this definition language
/// to provide utilities such as client code generation or service boot-strapping.
///
/// GraphQL tools or services which only seek to execute GraphQL requests and not construct a new GraphQL schema may choose not to
/// allow TypeSystemDefinition. Tools which only seek to produce schema and not execute requests may choose to only allow
/// TypeSystemDocument and not allow ExecutableDefinition or TypeSystemExtension but should provide a descriptive error if
/// present.
///
/// https://spec.graphql.org/October2021/#sec-Type-System
/// https://spec.graphql.org/October2021/#TypeSystemDefinition
type TypeSystemDefinition =
    /// https://spec.graphql.org/October2021/#SchemaDefinition
    | SchemaDefinition of SchemaDefinition
    /// https://spec.graphql.org/October2021/#TypeDefinition
    | TypeDefinition of TypeDefinition
    /// https://spec.graphql.org/October2021/#DirectiveDefinition
    | DirectiveDefinition of DirectiveDefinition

    member x.Directives =
        match x with
        | SchemaDefinition sd -> sd.Directives
        | TypeDefinition td -> td.Directives
        | DirectiveDefinition _ -> []

/// 3.3.2 Schema Extension
///
/// Schema extensions are used to represent a schema which has been extended from an original schema. For example, this might
/// be used by a GraphQL service which adds additional operation types, or additional directives to an existing schema.
///
/// Note: Schema extensions without additional operation type definitions must not be followed by a { (such as a query shorthand)
/// to avoid parsing ambiguity. The same limitation applies to the type definitions and extensions below.
///
/// https://spec.graphql.org/October2021/#sec-Schema-Extension
/// https://spec.graphql.org/October2021/#SchemaExtension
type SchemaExtension = {
    /// May be empty if OperationTypes is not, else it may not be empty.
    Directives : Directive list
    /// May be empty if the Directives list is not, else it may not be empty.
    OperationTypes : RootOperationTypeDefinition list
}

// Type Extensions:

/// 3.4.3 Type Extensions
///
/// Type extensions are used to represent a GraphQL type which has been extended from some original type. For example, this might
/// be used by a local service to represent additional fields a GraphQL client only accesses locally.
///
/// https://spec.graphql.org/October2021/#sec-Type-Extensions
type TypeExtension =
    /// https://spec.graphql.org/October2021/#sec-Scalar-Extensions
    | ScalarTypeExtension of ScalarTypeExtension
    /// https://spec.graphql.org/October2021/#sec-Object-Extensions
    | ObjectTypeExtension of ObjectTypeExtension
    /// https://spec.graphql.org/October2021/#InterfaceTypeExtension
    | InterfaceTypeExtension of InterfaceTypeExtension
    /// https://spec.graphql.org/October2021/#sec-Union-Extensions
    | UnionTypeExtension of UnionTypeExtension
    /// https://spec.graphql.org/October2021/#sec-Enum-Extensions
    | EnumTypeExtension of EnumTypeExtension
    /// https://spec.graphql.org/October2021/#sec-Input-Object-Extensions
    | InputObjectTypeExtension of InputObjectTypeExtension

    member x.Directives =
        match x with
        | ScalarTypeExtension ste -> ste.Directives
        | ObjectTypeExtension ote -> ote.Directives
        | InterfaceTypeExtension ite -> ite.Directives
        | UnionTypeExtension ute -> ute.Directives
        | EnumTypeExtension ete -> ete.Directives
        | InputObjectTypeExtension iote -> iote.Directives

/// 3.1 Type System Extensions
///
/// Type system extensions are used to represent a GraphQL type system which has been extended from some original type system. For
/// example, this might be used by a local service to represent data a GraphQL client only accesses locally, or by a GraphQL
/// service which is itself an extension of another GraphQL service.
///
/// Tools which only seek to produce and extend schema and not execute requests may choose to only allow
/// TypeSystemExtensionDocument and not allow ExecutableDefinition but should provide a descriptive error if present.
///
/// https://spec.graphql.org/October2021/#sec-Type-System-Extensions
/// https://spec.graphql.org/October2021/#TypeSystemExtension
type TypeSystemExtension =
    /// https://spec.graphql.org/October2021/#SchemaExtension
    | SchemaExtension of SchemaExtension
    /// https://spec.graphql.org/October2021/#TypeExtension
    | TypeExtension of TypeExtension

    member x.Directives =
        match x with
        | SchemaExtension se -> se.Directives
        | TypeExtension te -> te.Directives


/// https://spec.graphql.org/October2021/#TypeSystemDocument
type TypeSystemDocument = {
    /// https://spec.graphql.org/October2021/#TypeSystemDefinition
    Definitions : TypeSystemDefinition list
}


/// Prefer to use either <c>TypeSystemDocument</c> or <c>ExecutableDocument</c>.
/// https://spec.graphql.org/October2021/#Definition
type Definition =
    /// https://spec.graphql.org/October2021/#OperationDefinition
    | OperationDefinition of OperationDefinition
    /// https://spec.graphql.org/October2021/#FragmentDefinition
    | FragmentDefinition of FragmentDefinition
    /// https://spec.graphql.org/October2021/#TypeSystemDefinition
    | TypeSystemDefinition of TypeSystemDefinition
    /// https://spec.graphql.org/October2021/#TypeSystemExtension
    | TypeSystemExtension of TypeSystemExtension

    member x.Name =
        match x with
        | OperationDefinition op -> op.Name
        | FragmentDefinition frag -> Some frag.Name
        | TypeSystemDefinition _ -> None
        | TypeSystemExtension _ -> None

    member x.Directives =
        match x with
        | OperationDefinition op -> op.Directives
        | FragmentDefinition frag -> frag.Directives
        | TypeSystemDefinition tsd -> tsd.Directives
        | TypeSystemExtension tse -> tse.Directives

    member x.SelectionSet =
        match x with
        | OperationDefinition op -> op.SelectionSet
        | FragmentDefinition frag -> frag.SelectionSet
        | _ -> []


/// https://spec.graphql.org/October2021/#ExecutableDefinition
type ExecutableDefinition =
    /// https://spec.graphql.org/October2021/#OperationDefinition
    | OperationDefinition of OperationDefinition
    /// https://spec.graphql.org/October2021/#FragmentDefinition
    | FragmentDefinition of FragmentDefinition

    member x.Directives =
        match x with
        | OperationDefinition op -> op.Directives
        | FragmentDefinition frag -> frag.Directives

    member x.Name =
        match x with
        | OperationDefinition op -> op.Name
        | FragmentDefinition frag -> Some frag.Name

    member x.SelectionSet =
        match x with
        | OperationDefinition op -> op.SelectionSet
        | FragmentDefinition frag -> frag.SelectionSet


/// https://spec.graphql.org/October2021/#ExecutableDocument
type ExecutableDocument = {
    Definitions : ExecutableDefinition list
} with

    member x.Directives = x.Definitions |> List.collect (fun y -> y.Directives)


/// 2.2 Document
///
/// A GraphQL Document describes a complete file or request string operated on by a GraphQL service or client. A document contains
/// multiple definitions, either executable or representative of a GraphQL type system.
///
/// Documents are only executable by a GraphQL service if they are ExecutableDocument and contain at least one
/// OperationDefinition. A Document which contains TypeSystemDefinitionOrExtension must not be executed; GraphQL execution
/// services which receive a Document containing these should return a descriptive error.
///
/// GraphQL services which only seek to execute GraphQL requests and not construct a new GraphQL schema may choose to only permit
/// ExecutableDocument.
///
/// Documents which do not contain OperationDefinition or do contain TypeSystemDefinitionOrExtension may still be parsed and
/// validated to allow client tools to represent many GraphQL uses which may appear across many individual files.
///
/// If a Document contains only one operation, that operation may be unnamed. If that operation is a query without variables or
/// directives then it may also be represented in the shorthand form, omitting both the query keyword as well as the operation
/// name. Otherwise, if a GraphQL Document contains multiple operations, each operation must be named. When submitting a Document
/// with multiple operations to a GraphQL service, the name of the desired operation to be executed must also be provided.
///
/// https://spec.graphql.org/October2021/#sec-Document-Syntax
/// https://spec.graphql.org/October2021/#Document
type Document = {
    Definitions : ExecutableDefinition list
} with

    member d.Directives = d.Definitions |> List.collect (fun y -> y.Directives)

    member d.IsEmpty = d.Definitions.IsEmpty
