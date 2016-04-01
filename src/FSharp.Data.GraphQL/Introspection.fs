/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
module FSharp.Data.GraphQL.Introspection

open FSharp.Data.GraphQL.Types

type TypeKind = 
    | SCALAR = 1
    | OBJECT  = 2
    | INTERFACE = 3
    | UNION = 4
    | ENUM = 5
    | INPUT_OBJECT = 6
    | LIST = 7
    | NON_NULL = 8

type DirectiveLocation =
    | QUERY = 1
    | MUTATION = 2
    | SUBSCRIPTION = 3
    | FIELD = 4
    | FRAGMENT_DEFINITION = 5
    | FRAGMENT_SPREAD = 6
    | INLINE_FRAGMENT = 7

let __TypeKind = Schema.Enum(
    name = "__TypeKind", 
    description = "An enum describing what kind of type a given __Type is.",
    options = [
        Schema.EnumValue("SCALAR", TypeKind.SCALAR, "Indicates this type is a scalar.")
        Schema.EnumValue("OBJECT", TypeKind.OBJECT, "Indicates this type is an object. `fields` and `interfaces` are valid fields.")
        Schema.EnumValue("INTERFACE", TypeKind.INTERFACE, "Indicates this type is an interface. `fields` and `possibleTypes` are valid fields.")
        Schema.EnumValue("UNION", TypeKind.UNION, "Indicates this type is a union. `possibleTypes` is a valid field.")
        Schema.EnumValue("ENUM", TypeKind.ENUM, "Indicates this type is an enum. `enumValues` is a valid field.")
        Schema.EnumValue("INPUT_OBJECT", TypeKind.INPUT_OBJECT, "Indicates this type is an input object. `inputFields` is a valid field.")
        Schema.EnumValue("LIST", TypeKind.LIST, "Indicates this type is a list. `ofType` is a valid field.")
        Schema.EnumValue("NON_NULL", TypeKind.NON_NULL, "Indicates this type is a non-null. `ofType` is a valid field.")
    ])

let __DirectiveLocation = Schema.Enum(
    name = "__DirectiveLocation",
    description = "A Directive can be adjacent to many parts of the GraphQL language, a __DirectiveLocation describes one such possible adjacencies.",
    options = [
        Schema.EnumValue("QUERY", DirectiveLocation.QUERY, "Location adjacent to a query operation.")
        Schema.EnumValue("MUTATION", DirectiveLocation.MUTATION, "Location adjacent to a mutation operation.")
        Schema.EnumValue("SUBSCRIPTION", DirectiveLocation.SUBSCRIPTION, "Location adjacent to a subscription operation.")
        Schema.EnumValue("FIELD", DirectiveLocation.FIELD, "Location adjacent to a field.")
        Schema.EnumValue("FRAGMENT_DEFINITION", DirectiveLocation.FRAGMENT_DEFINITION, "Location adjacent to a fragment definition.")
        Schema.EnumValue("FRAGMENT_SPREAD", DirectiveLocation.FRAGMENT_SPREAD, "Location adjacent to a fragment spread.")
        Schema.EnumValue("INLINE_FRAGMENT", DirectiveLocation.INLINE_FRAGMENT, "Location adjacent to an inline fragment.")
    ])
    
let mutable __Type = Schema.ObjectType(
    name = "__Type",
    description = """
    The fundamental unit of any GraphQL Schema is the type. There are many kinds of types in GraphQL as represented by the `__TypeKind` enum.

    Depending on the kind of a type, certain fields describe information about that type. Scalar types provide no information beyond a name and description, while Enum types provide their values. Object and Interface types provide the fields they describe. Abstract types, Union and Interface, provide the Object types possible at runtime. List and NonNull types compose other types.
    """,
    fields = [
        Schema.Field("kind", NonNull __TypeKind)
        Schema.Field("name", NonNull String)
        Schema.Field("description", String)
    ])
    
let __InputValue = Schema.ObjectType(
    name = "__InputValue",
    description = "Arguments provided to Fields or Directives and the input fields of an InputObject are represented as Input Values which describe their type and optionally a default value.",
    fields = [
        Schema.Field("name", NonNull String)
        Schema.Field("description", String)
        Schema.Field("type", NonNull __Type)
        Schema.Field("defaultValue", String)
    ])
    
let __Field = Schema.ObjectType(
    name = "__Field",
    description = "Object and Interface types are described by a list of Fields, each of which has a name, potentially a list of arguments, and a return type.",
    fields = [
        Schema.Field("name", NonNull String)
        Schema.Field("description", String)
        Schema.Field("args", NonNull(ListOf (NonNull __InputValue)))
        Schema.Field("type", NonNull __Type)
        Schema.Field("isDeprecated", NonNull Bool)
        Schema.Field("deprecationReason", String)
    ])
    
let __EnumValue = Schema.ObjectType(
    name = "__EnumValue",
    description = "One possible value for a given Enum. Enum values are unique values, not a placeholder for a string or numeric value. However an Enum value is returned in a JSON response as a string.",
    fields = [
        Schema.Field("name", NonNull String)
        Schema.Field("description", String)
        Schema.Field("isDeprecated", NonNull Bool)
        Schema.Field("deprecationReason", String)
    ])
    
match __Type with
    | Object o ->
        __Type <-  mergeFields o [
            Schema.Field("fields", ListOf (NonNull __Field))
            Schema.Field("enumValues", ListOf (NonNull __EnumValue))
            Schema.Field("inputFields", ListOf (NonNull __InputValue))
            Schema.Field("interfaces", ListOf (NonNull __Type))
            Schema.Field("possibleTypes", ListOf (NonNull __Type))
            Schema.Field("ofType", __Type)
        ] |> Object

let __Directive = Schema.ObjectType(
    name = "__Directive",
    description = """
    A Directive provides a way to describe alternate runtime execution and type validation behavior in a GraphQL document.' +
    
    In some cases, you need to provide options to alter GraphQL’s execution behavior in ways field arguments will not suffice, such as conditionally including or skipping a field. Directives provide this by describing additional information to the executor.
    """,
    fields = [
        Schema.Field("name", NonNull String)
        Schema.Field("description", String)
        Schema.Field("locations", NonNull (ListOf (NonNull __DirectiveLocation)))
        Schema.Field("args", NonNull (ListOf (NonNull __InputValue)))
        Schema.Field("onOperation", NonNull Bool)
        Schema.Field("onFragment", NonNull Bool)
        Schema.Field("onField", NonNull Bool)
    ])
    
let __Schema = Schema.ObjectType(
    name = "__Schema",
    description = "A GraphQL Schema defines the capabilities of a GraphQL server. It exposes all available types and directives on the server, as well as the entry points for query, mutation, and subscription operations.",
    fields = [
        Schema.Field("types", NonNull (ListOf (NonNull __Type)), description = "A list of all types supported by this server.")
        Schema.Field("queryType", NonNull __Type, description = "The type that query operations will be rooted at.")
        Schema.Field("mutationType", __Type, description = "If this server supports mutation, the type that mutation operations will be rooted at.")
        Schema.Field("subscriptionType", __Type, description = "If this server support subscription, the type that subscription operations will be rooted at.")
        Schema.Field("directives", NonNull (ListOf (NonNull __Directive)), description = "A list of all directives supported by this server.")
    ])

let SchemaMetaFieldDef = Schema.Field(
    name = "__schema",
    description = "Access the current type schema of this server.",
    schema = NonNull __Schema)
    
let TypeMetaFieldDef = Schema.Field(
    name = "__type",
    description = "Request the type information of a single type.",
    schema = __Type,
    arguments = [
        { Name = "Name"; Value = (NonNull String) }
    ])
    
let TypeNameMetaFieldDef = Schema.Field(
    name = "__typename",
    description = "The name of the current Object type at runtime.",
    schema = NonNull String)