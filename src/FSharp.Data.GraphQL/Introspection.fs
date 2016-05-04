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

let internal flagsToList (e:'TEnum) =
    System.Enum.GetValues(typeof<'TEnum>)
    |> Seq.cast<'TEnum>
    |> Seq.filter (fun v -> int(e) &&& int(v) <> 0)
    |> List.ofSeq
    
let internal graphQLKind (_: ResolveFieldContext) = function
    | Scalar _ -> TypeKind.SCALAR
    | Enum _ -> TypeKind.ENUM
    | Object _ -> TypeKind.OBJECT
    | Interface _ -> TypeKind.INTERFACE
    | Union _ -> TypeKind.UNION
    | List _ -> TypeKind.LIST
    | NonNull _ -> TypeKind.NON_NULL
    | InputObject _ -> TypeKind.INPUT_OBJECT

open System.Reflection
let internal getProperty o name =
    o.GetType().GetProperty(name, BindingFlags.IgnoreCase ||| BindingFlags.Public ||| BindingFlags.Instance)
    
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
    
let rec __Type = Define.Object(
    name = "__Type",
    description = """The fundamental unit of any GraphQL Schema is the type. There are many kinds of types in GraphQL as represented by the `__TypeKind` enum. Depending on the kind of a type, certain fields describe information about that type. Scalar types provide no information beyond a name and description, while Enum types provide their values. Object and Interface types provide the fields they describe. Abstract types, Union and Interface, provide the Object types possible at runtime. List and NonNull types compose other types.""",
    fields = fun () -> [
        Define.Field("kind", NonNull __TypeKind, resolve = graphQLKind)
        Define.Field("name", NonNull String, resolve = fun _ t ->
            match t with
            | Named namedType -> namedType.Name :> obj
            | _ -> 
                match getProperty t "name" with
                | null -> null
                | property -> property.GetValue(t, null))
        Define.Field("description", String)
        Define.Field("fields", ListOf (NonNull __Field), 
            args = [ Define.Arg("includeDeprecated", Boolean, false) ],
            resolve = fun ctx t ->
                let fieldsOpt = 
                    match t with
                    | Object odef -> Some odef.Fields
                    | Interface idef -> Some idef.Fields
                    | _ -> None
                match fieldsOpt with
                | None -> null
                | Some fields when ctx.Arg("includeDeprecated").Value -> box fields
                | Some fields -> 
                    fields
                    |> List.filter (fun f -> Option.isNone f.DeprecationReason)
                    |> box)
        Define.Field("interfaces", ListOf (NonNull __Type), resolve = fun _ t -> match t with Object o -> box o.Implements | _ -> null)
        Define.Field("possibleTypes", ListOf (NonNull __Type), resolve = fun ctx t -> match t with Abstract a -> box (ctx.Schema.GetPossibleTypes a) | _ -> null)
        Define.Field("enumValues", ListOf (NonNull __EnumValue),
            args = [ Define.Arg("includeDeprecated", Boolean, false) ],
            resolve = fun ctx t ->
                match t with
                | Enum e when ctx.Arg("includeDeprecated").Value -> box e.Options
                | Enum e -> e.Options |> List.filter (fun v -> Option.isNone v.DeprecationReason) |> box
                | _ -> null)
        Define.Field("inputFields", ListOf (NonNull __InputValue), resolve = fun _ t ->
            match t with
            | InputObject idef -> box idef.FieldsFn
            | _ -> null)
        Define.Field("ofType", __Type, resolve = fun _ typedef ->
            match typedef with
            | NonNull inner | List inner -> box inner
            | _ -> null)
    ])
   
and __InputValue = Define.Object(
    name = "__InputValue",
    description = "Arguments provided to Fields or Directives and the input fields of an InputObject are represented as Input Values which describe their type and optionally a default value.",
    fields = fun () -> [
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
    
and __Field = Define.Object(
    name = "__Field",
    description = "Object and Interface types are described by a list of Fields, each of which has a name, potentially a list of arguments, and a return type.",
    fields = fun () -> [
        Define.Field("name", NonNull String)
        Define.Field("description", String)
        Define.Field("args", NonNull(ListOf (NonNull __InputValue)))
        Define.Field("type", NonNull __Type)
        Define.Field("isDeprecated", NonNull Boolean, resolve = fun _ f -> Option.isSome f.DeprecationReason)
        Define.Field("deprecationReason", String)
    ])
    
and __EnumValue = Define.Object(
    name = "__EnumValue",
    description = "One possible value for a given Enum. Enum values are unique values, not a placeholder for a string or numeric value. However an Enum value is returned in a JSON response as a string.",
    fields = fun () -> [
        Define.Field("name", NonNull String)
        Define.Field("description", String)
        Define.Field("isDeprecated", NonNull Boolean, resolve = fun _ (e: EnumValue) -> Option.isSome e.DeprecationReason)
        Define.Field("deprecationReason", String)
    ])

and __Directive = Define.Object(
    name = "__Directive",
    description = """A Directive provides a way to describe alternate runtime execution and type validation behavior in a GraphQL document. In some cases, you need to provide options to alter GraphQL’s execution behavior in ways field arguments will not suffice, such as conditionally including or skipping a field. Directives provide this by describing additional information to the executor.""",
    fields = fun () -> [
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
    
and __Schema = Define.Object(
    name = "__Schema",
    description = "A GraphQL Schema defines the capabilities of a GraphQL server. It exposes all available types and directives on the server, as well as the entry points for query, mutation, and subscription operations.",
    fields = fun () -> [
        Define.Field("types", NonNull (ListOf (NonNull __Type)), description = "A list of all types supported by this server.", resolve = fun _ (schema: ISchema) -> (schema :> System.Collections.Generic.IEnumerable<NamedDef>))
        Define.Field("queryType", NonNull __Type, description = "The type that query operations will be rooted at.", resolve = fun _ (schema: ISchema) -> schema.Query)
        Define.Field("mutationType", __Type, description = "If this server supports mutation, the type that mutation operations will be rooted at.", resolve = fun _ (schema: ISchema) -> schema.Mutation)
        Define.Field("subscriptionType", __Type, description = "If this server support subscription, the type that subscription operations will be rooted at.", resolve = fun _ _ -> null)
        Define.Field("directives", NonNull (ListOf (NonNull __Directive)), description = "A list of all directives supported by this server.", resolve = fun _ (schema: ISchema) -> schema.Directives)
    ])

let SchemaMetaFieldDef = Define.Field(
    name = "__schema",
    description = "Access the current type schema of this server.",
    typedef = NonNull __Schema,
    resolve = fun ctx _ -> ctx.Schema)
    
let TypeMetaFieldDef = Define.Field(
    name = "__type",
    description = "Request the type information of a single type.",
    typedef = __Type,
    args = [
        { Name = "name"; Type = (NonNull String); Description = None; DefaultValue = None }
    ],
    resolve = fun ctx _ -> ctx.Schema.TryFindType(ctx.Arg("name").Value) |> Option.map box |> Option.toObj)
    
let TypeNameMetaFieldDef = Define.Field(
    name = "__typename",
    description = "The name of the current Object type at runtime.",
    typedef = NonNull String,
    resolve = fun ctx _ -> ctx.ParentType.Name)