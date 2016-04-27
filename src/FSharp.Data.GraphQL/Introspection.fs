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

let internal flagsToList (e:'enum) =
    System.Enum.GetValues(typeof<'enum>)
    |> Seq.cast<'enum>
    |> Seq.filter (fun v -> int(e) &&& int(v) <> 0)
    
let internal graphQLKind (_: ResolveFieldContext) = function
    | Scalar _ -> TypeKind.SCALAR
    | Enum _ -> TypeKind.ENUM
    | Object _ -> TypeKind.OBJECT
    | Interface _ -> TypeKind.INTERFACE
    | Union _ -> TypeKind.UNION
    | ListOf _ -> TypeKind.LIST
    | NonNull _ -> TypeKind.NON_NULL
    | InputObject _ -> TypeKind.INPUT_OBJECT
    
let __TypeKind = Define.Enum(
    name = "__TypeKind", 
    description = "An enum describing what kind of type a given __Type is.",
    options = [
        Define.EnumValue("SCALAR", TypeKind.SCALAR, "Indicates this type is a scalar.")
        Define.EnumValue("OBJECT", TypeKind.OBJECT, "Indicates this type is an object. `fields` and `interfaces` are valid fields.")
        Define.EnumValue("INTERFACE", TypeKind.INTERFACE, "Indicates this type is an interface. `fields` and `possibleTypes` are valid fields.")
        Define.EnumValue("UNION", TypeKind.UNION, "Indicates this type is a union. `possibleTypes` is a valid field.")
        Define.EnumValue("ENUM", TypeKind.ENUM, "Indicates this type is an enum. `enumValues` is a valid field.")
        Define.EnumValue("INPUT_OBJECT", TypeKind.INPUT_OBJECT, "Indicates this type is an input object. `inputFields` is a valid field.")
        Define.EnumValue("LIST", TypeKind.LIST, "Indicates this type is a list. `ofType` is a valid field.")
        Define.EnumValue("NON_NULL", TypeKind.NON_NULL, "Indicates this type is a non-null. `ofType` is a valid field.")
    ])

let __DirectiveLocation = Define.Enum(
    name = "__DirectiveLocation",
    description = "A Directive can be adjacent to many parts of the GraphQL language, a __DirectiveLocation describes one such possible adjacencies.",
    options = [
        Define.EnumValue("QUERY", DirectiveLocation.QUERY, "Location adjacent to a query operation.")
        Define.EnumValue("MUTATION", DirectiveLocation.MUTATION, "Location adjacent to a mutation operation.")
        Define.EnumValue("SUBSCRIPTION", DirectiveLocation.SUBSCRIPTION, "Location adjacent to a subscription operation.")
        Define.EnumValue("FIELD", DirectiveLocation.FIELD, "Location adjacent to a field.")
        Define.EnumValue("FRAGMENT_DEFINITION", DirectiveLocation.FRAGMENT_DEFINITION, "Location adjacent to a fragment definition.")
        Define.EnumValue("FRAGMENT_SPREAD", DirectiveLocation.FRAGMENT_SPREAD, "Location adjacent to a fragment spread.")
        Define.EnumValue("INLINE_FRAGMENT", DirectiveLocation.INLINE_FRAGMENT, "Location adjacent to an inline fragment.")
    ])
    
let mutable __Type = Define.ObjectType(
    name = "__Type",
    description = """
    The fundamental unit of any GraphQL Schema is the type. There are many kinds of types in GraphQL as represented by the `__TypeKind` enum.

    Depending on the kind of a type, certain fields describe information about that type. Scalar types provide no information beyond a name and description, while Enum types provide their values. Object and Interface types provide the fields they describe. Abstract types, Union and Interface, provide the Object types possible at runtime. List and NonNull types compose other types.
    """,
    fields = [
        Define.Field("kind", NonNull __TypeKind, resolve = graphQLKind)
        Define.Field("name", NonNull String)
        Define.Field("description", String)
    ])
   
open System.Reflection
let __InputValue = Define.ObjectType(
    name = "__InputValue",
    description = "Arguments provided to Fields or Directives and the input fields of an InputObject are represented as Input Values which describe their type and optionally a default value.",
    fields = [
        Define.Field("name", NonNull String)
        Define.Field("description", String)
        Define.Field("type", NonNull __Type)
        Define.Field("defaultValue", String, 
            resolve = fun _ input ->
                let property = input.GetType().GetProperty("defaultValue", BindingFlags.IgnoreCase|||BindingFlags.Public|||BindingFlags.Instance)
                if property = null 
                then null
                else property.GetValue(input, null))
    ])
    
let __Field = Define.ObjectType(
    name = "__Field",
    description = "Object and Interface types are described by a list of Fields, each of which has a name, potentially a list of arguments, and a return type.",
    fields = [
        Define.Field("name", NonNull String)
        Define.Field("description", String)
        Define.Field("args", NonNull(ListOf (NonNull __InputValue)))
        Define.Field("type", NonNull __Type)
        Define.Field("isDeprecated", NonNull Boolean, resolve = fun _ f -> Option.isSome f.DeprecationReason)
        Define.Field("deprecationReason", String)
    ])
    
let __EnumValue = Define.ObjectType(
    name = "__EnumValue",
    description = "One possible value for a given Enum. Enum values are unique values, not a placeholder for a string or numeric value. However an Enum value is returned in a JSON response as a string.",
    fields = [
        Define.Field("name", NonNull String)
        Define.Field("description", String)
        Define.Field("isDeprecated", NonNull Boolean, resolve = fun _ (e: EnumValue) -> Option.isSome e.DeprecationReason)
        Define.Field("deprecationReason", String)
    ])
    
match __Type with
    | Object o ->
        __Type <-  mergeFields o [
            Define.Field("fields", ListOf (NonNull __Field), 
                arguments = [ Define.Argument("includeDeprecated", Boolean, false) ],
                resolve = fun ctx t ->
                    let fields = 
                        match t with
                        | Object odef -> odef.Fields
                        | Interface idef -> idef.Fields
                        | _ -> []
                    if ctx.Arg("includeDeprecated").Value
                    then fields
                    else fields |> List.filter (fun f -> Option.isNone f.DeprecationReason))
            Define.Field("interfaces", ListOf (NonNull __Type), resolve = fun _ t -> match t with Object o -> o.Implements | _ -> [])
            Define.Field("possibleTypes", ListOf (NonNull __Type), resolve = fun ctx t -> ctx.Schema.GetPossibleTypes t)
            Define.Field("enumValues", ListOf (NonNull __EnumValue),
                arguments = [ Define.Argument("includeDeprecated", Boolean, false) ],
                resolve = fun ctx t ->
                    match t with
                    | Enum e ->
                        let values = e.Options
                        if ctx.Arg("includeDeprecated").Value
                        then values
                        else values |> List.filter (fun v -> Option.isNone v.DeprecationReason)
                    | _ -> [])
            Define.Field("inputFields", ListOf (NonNull __InputValue), resolve = fun _ t ->
                match t with
                | Object odef -> odef.Fields
                | InputObject idef -> idef.Fields
                | _ -> [])
            Define.Field("ofType", __Type)
        ] |> Object

let __Directive = Define.ObjectType(
    name = "__Directive",
    description = """
    A Directive provides a way to describe alternate runtime execution and type validation behavior in a GraphQL document.' +
    
    In some cases, you need to provide options to alter GraphQL’s execution behavior in ways field arguments will not suffice, such as conditionally including or skipping a field. Directives provide this by describing additional information to the executor.
    """,
    fields = [
        Define.Field("name", NonNull String)
        Define.Field("description", String)
        Define.Field("locations", NonNull (ListOf (NonNull __DirectiveLocation)), resolve = fun _ (directive: DirectiveDef) -> flagsToList directive.Locations)
        Define.Field("args", NonNull (ListOf (NonNull __InputValue)))
        Define.Field("onOperation", NonNull Boolean, resolve = fun _ (d: DirectiveDef) -> 
            d.Locations.HasFlag(DirectiveLocation.QUERY) || d.Locations.HasFlag(DirectiveLocation.MUTATION) || d.Locations.HasFlag(DirectiveLocation.SUBSCRIPTION))
        Define.Field("onFragment", NonNull Boolean, resolve = fun _ (d: DirectiveDef) -> 
            d.Locations.HasFlag(DirectiveLocation.FRAGMENT_SPREAD) || d.Locations.HasFlag(DirectiveLocation.INLINE_FRAGMENT) || d.Locations.HasFlag(DirectiveLocation.FRAGMENT_DEFINITION))
        Define.Field("onField", NonNull Boolean, resolve = fun _ (d: DirectiveDef) -> d.Locations.HasFlag(DirectiveLocation.FIELD))
    ])
    
let __Schema = Define.ObjectType(
    name = "__Schema",
    description = "A GraphQL Schema defines the capabilities of a GraphQL server. It exposes all available types and directives on the server, as well as the entry points for query, mutation, and subscription operations.",
    fields = [
        Define.Field("types", NonNull (ListOf (NonNull __Type)), description = "A list of all types supported by this server.", resolve = fun _ (schema: ISchema) -> (schema :> System.Collections.Generic.IEnumerable<TypeDef>))
        Define.Field("queryType", NonNull __Type, description = "The type that query operations will be rooted at.", resolve = fun _ (schema: ISchema) -> schema.Query)
        Define.Field("mutationType", __Type, description = "If this server supports mutation, the type that mutation operations will be rooted at.", resolve = fun _ (schema: ISchema) -> schema.Mutation)
        Define.Field("subscriptionType", __Type, description = "If this server support subscription, the type that subscription operations will be rooted at.", resolve = fun _ _ -> null)
        Define.Field("directives", NonNull (ListOf (NonNull __Directive)), description = "A list of all directives supported by this server.", resolve = fun _ (schema: ISchema) -> schema.Directives)
    ])

let SchemaMetaFieldDef = Define.Field(
    name = "__schema",
    description = "Access the current type schema of this server.",
    schema = NonNull __Schema,
    resolve = fun ctx _ -> ctx.Schema)
    
let TypeMetaFieldDef = Define.Field(
    name = "__type",
    description = "Request the type information of a single type.",
    schema = __Type,
    arguments = [
        { Name = "name"; Type = (NonNull String); Description = None; DefaultValue = None }
    ],
    resolve = fun ctx _ -> ctx.Schema.TryFindType(ctx.Arg("name").Value))
    
let TypeNameMetaFieldDef = Define.Field(
    name = "__typename",
    description = "The name of the current Object type at runtime.",
    schema = NonNull String,
    resolve = fun ctx _ -> ctx.ParentType.Name)