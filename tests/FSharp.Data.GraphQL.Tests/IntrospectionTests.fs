/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.IntrospectionTests

open System
open Xunit
open FsCheck
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution

[<Fact>]
let ``Introspection executes an introspection query`` () =
    let root = objdef "QueryRoot" [
        Define.Field("onlyField", String)
    ]
    let schema = Schema(root)
    let (Object raw) = root
    let result = schema.Execute(parse introspectionQuery, raw)
    let expected: Map<string,obj> =
        Map.ofList<string, obj> [
          "__schema", upcast Map.ofList<string, obj> [
            "queryType", upcast Map.ofList<string, obj> [
              "name", upcast "QueryRoot"
            ]
            "mutationType", null
            "subscriptionType", null
            "types", upcast [
              Map.ofList<string, obj> [
                "inputFields", null
                "name", upcast "QueryRoot"
                "description", null
                "interfaces", upcast []
                "enumValues", null
                "fields", upcast []
                "kind", upcast "OBJECT"
                "possibleTypes", null
              ] :> obj
              upcast Map.ofList<string, obj> [
                "inputFields", null
                "name", upcast "__Directive"
                "description", upcast "A Directive provides a way to describe alternate runtime execution and type validation behavior in a GraphQL document.\n\nIn some cases, you need to provide options to alter GraphQL’s execution behavior in ways field arguments will not suffice, such as conditionally including or skipping a field. Directives provide this by describing additional information to the executor."
                "interfaces", upcast []
                "enumValues", null
                "fields", upcast [
                  Map.ofList<string, obj> [
                    "name", upcast "name"
                    "description", null
                    "isDeprecated", upcast false
                    "deprecationReason", null
                    "args", upcast []
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "NON_NULL"
                      "name", null
                      "ofType", upcast Map.ofList<string, obj> [
                        "kind", upcast "SCALAR"
                        "name", upcast "String"
                        "ofType", null]]] :> obj
                  upcast Map.ofList<string, obj> [
                    "name", upcast "description"
                    "description", null
                    "isDeprecated", upcast false
                    "deprecationReason", null
                    "args", upcast []
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "SCALAR"
                      "name", upcast "String"
                      "ofType", null]]
                  upcast Map.ofList<string, obj> [
                    "name", upcast "locations"
                    "description", null
                    "isDeprecated", upcast false
                    "deprecationReason", null
                    "args", upcast []
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "NON_NULL"
                      "name", null
                      "ofType", upcast Map.ofList<string, obj> [
                        "kind", upcast "LIST"
                        "name", null
                        "ofType", upcast Map.ofList<string, obj> [
                          "kind", upcast "NON_NULL"
                          "name", null
                          "ofType", upcast Map.ofList<string, obj> [
                            "kind", upcast "ENUM"
                            "name", upcast "__DirectiveLocation"]]]]]
                  upcast Map.ofList<string, obj> [
                    "name", upcast "args"
                    "description", null
                    "isDeprecated", upcast false
                    "deprecationReason", null
                    "args", upcast []
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "NON_NULL"
                      "name", null
                      "ofType", upcast Map.ofList<string, obj> [
                        "kind", upcast "LIST"
                        "name", null
                        "ofType", upcast Map.ofList<string, obj> [
                          "kind", upcast "NON_NULL"
                          "name", null
                          "ofType", upcast Map.ofList<string, obj> [
                            "kind", upcast "OBJECT"
                            "name", upcast "__InputValue"]]]]]
                  upcast Map.ofList<string, obj> [
                    "name", upcast "onOperation"
                    "description", null
                    "isDeprecated", upcast true
                    "deprecationReason", upcast "Use `locations`."
                    "args", upcast []
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "NON_NULL"
                      "name", null
                      "ofType", upcast Map.ofList<string, obj> [
                        "kind", upcast "SCALAR"
                        "name", upcast "Boolean"
                        "ofType", null]]]
                  upcast Map.ofList<string, obj> [
                    "name", upcast "onFragment"
                    "description", null
                    "isDeprecated", upcast true
                    "deprecationReason", upcast "Use `locations`."
                    "args", upcast []
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "NON_NULL"
                      "name", null
                      "ofType", upcast Map.ofList<string, obj> [
                        "kind", upcast "SCALAR"
                        "name", upcast "Boolean"
                        "ofType", null]]]
                  upcast Map.ofList<string, obj> [
                    "name", upcast "onField"
                    "description", null
                    "isDeprecated", upcast true
                    "deprecationReason", upcast "Use `locations`."
                    "args", upcast []
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "NON_NULL"
                      "name", null
                      "ofType", upcast Map.ofList<string, obj> [
                        "kind", upcast "SCALAR"
                        "name", upcast "Boolean"
                        "ofType", null]]]]
                "kind", upcast "OBJECT"
                "possibleTypes", null]
              upcast Map.ofList<string, obj> [
                "inputFields", null
                "name", upcast "__DirectiveLocation"
                "description", upcast "A Directive can be adjacent to many parts of the GraphQL language, a __DirectiveLocation describes one such possible adjacencies."
                "interfaces", null
                "enumValues", upcast [
                  Map.ofList<string, obj> [
                    "name", upcast "QUERY"
                    "description", upcast "Location adjacent to a query operation."
                    "isDeprecated", upcast false
                    "deprecationReason", null] :> obj
                  upcast Map.ofList<string, obj> [
                    "name", upcast "MUTATION"
                    "description", upcast "Location adjacent to a mutation operation."
                    "isDeprecated", upcast false
                    "deprecationReason", null]
                  upcast Map.ofList<string, obj> [
                    "name", upcast "SUBSCRIPTION"
                    "description", upcast "Location adjacent to a subscription operation."
                    "isDeprecated", upcast false
                    "deprecationReason", null]
                  upcast Map.ofList<string, obj> [
                    "name", upcast "FIELD"
                    "description", upcast "Location adjacent to a field."
                    "isDeprecated", upcast false
                    "deprecationReason", null]
                  upcast Map.ofList<string, obj> [
                    "name", upcast "FRAGMENT_DEFINITION"
                    "description", upcast "Location adjacent to a fragment definition."
                    "isDeprecated", upcast false
                    "deprecationReason", null]
                  upcast Map.ofList<string, obj> [
                    "name", upcast "FRAGMENT_SPREAD"
                    "description", upcast "Location adjacent to a fragment spread."
                    "isDeprecated", upcast false
                    "deprecationReason", null]
                  upcast Map.ofList<string, obj> [
                    "name", upcast "INLINE_FRAGMENT"
                    "description", upcast "Location adjacent to an inline fragment."
                    "isDeprecated", upcast false
                    "deprecationReason", null]]
                "fields", null
                "kind", upcast "ENUM"
                "possibleTypes", null]
              upcast Map.ofList<string, obj> [
                "inputFields", null
                "name", upcast "__EnumValue"
                "description", upcast "One possible value for a given Enum. Enum values are unique values, not a placeholder for a string or numeric value. However an Enum value is returned in a JSON response as a string."
                "interfaces", upcast []
                "enumValues", null
                "fields", upcast [
                  Map.ofList<string, obj> [
                    "name", upcast "name"
                    "description", null
                    "isDeprecated", upcast false
                    "deprecationReason", null
                    "args", upcast []
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "NON_NULL"
                      "name", null
                      "ofType", upcast Map.ofList<string, obj> [
                        "kind", upcast "SCALAR"
                        "name", upcast "String"
                        "ofType", null]]] :> obj
                  upcast Map.ofList<string, obj> [
                    "name", upcast "description"
                    "description", null
                    "isDeprecated", upcast false
                    "deprecationReason", null
                    "args", upcast []
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "SCALAR"
                      "name", upcast "String"
                      "ofType", null]]
                  upcast Map.ofList<string, obj> [
                    "name", upcast "isDeprecated"
                    "description", null
                    "isDeprecated", upcast false
                    "deprecationReason", null
                    "args", upcast []
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "NON_NULL"
                      "name", null
                      "ofType", upcast Map.ofList<string, obj> [
                        "kind", upcast "SCALAR"
                        "name", upcast "Boolean"
                        "ofType", null]]]
                  upcast Map.ofList<string, obj> [
                    "name", upcast "deprecationReason"
                    "description", null
                    "isDeprecated", upcast false
                    "deprecationReason", null
                    "args", upcast []
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "SCALAR"
                      "name", upcast "String"
                      "ofType", null]]
                ]
                "kind", upcast "OBJECT"
                "possibleTypes", null
              ]
              upcast Map.ofList<string, obj> [
                "inputFields", null
                "name", upcast "__Field"
                "description", upcast "Object and Interface types are described by a list of Fields, each of which has a name, potentially a list of arguments, and a return type."
                "interfaces", upcast []
                "enumValues", null
                "fields", upcast [
                  Map.ofList<string, obj> [
                    "name", upcast "name"
                    "description", null
                    "isDeprecated", upcast false
                    "deprecationReason", null
                    "args", upcast []
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "NON_NULL"
                      "name", null
                      "ofType", upcast Map.ofList<string, obj> [
                        "kind", upcast "SCALAR"
                        "name", upcast "String"
                        "ofType", null]]] :> obj
                  upcast Map.ofList<string, obj> [
                    "name", upcast "description"
                    "description", null
                    "isDeprecated", upcast false
                    "deprecationReason", null
                    "args", upcast []
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "SCALAR"
                      "name", upcast "String"
                      "ofType", null]]
                  upcast Map.ofList<string, obj> [
                    "name", upcast "args"
                    "description", null
                    "isDeprecated", upcast false
                    "deprecationReason", null
                    "args", upcast []
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "NON_NULL"
                      "name", null
                      "ofType", upcast Map.ofList<string, obj> [
                        "kind", upcast "LIST"
                        "name", null
                        "ofType", upcast Map.ofList<string, obj> [
                          "kind", upcast "NON_NULL"
                          "name", null
                          "ofType", upcast Map.ofList<string, obj> [
                            "kind", upcast "OBJECT"
                            "name", upcast "__InputValue"]]]]]
                  upcast Map.ofList<string, obj> [
                    "name", upcast "type"
                    "description", null
                    "isDeprecated", upcast false
                    "deprecationReason", null
                    "args", upcast []
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "NON_NULL"
                      "name", null
                      "ofType", upcast Map.ofList<string, obj> [
                        "kind", upcast "OBJECT"
                        "name", upcast "__Type"
                        "ofType", null]]]
                  upcast Map.ofList<string, obj> [
                    "name", upcast "isDeprecated"
                    "description", null
                    "isDeprecated", upcast false
                    "deprecationReason", null
                    "args", upcast []
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "NON_NULL"
                      "name", null
                      "ofType", upcast Map.ofList<string, obj> [
                        "kind", upcast "SCALAR"
                        "name", upcast "Boolean"
                        "ofType", null]]]
                  upcast Map.ofList<string, obj> [
                    "name", upcast "deprecationReason"
                    "description", null
                    "isDeprecated", upcast false
                    "deprecationReason", null
                    "args", upcast []
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "SCALAR"
                      "name", upcast "String"
                      "ofType", null]]
                ]
                "kind", upcast "OBJECT"
                "possibleTypes", null
              ]
              upcast Map.ofList<string, obj> [
                "inputFields", null
                "name", upcast "__InputValue"
                "description", upcast "Arguments provided to Fields or Directives and the input fields of an InputObject are represented as Input Values which describe their type and optionally a default value."
                "interfaces", upcast []
                "enumValues", null
                "fields", upcast [
                  Map.ofList<string, obj> [
                    "name", upcast "name"
                    "description", null
                    "isDeprecated", upcast false
                    "deprecationReason", null
                    "args", upcast []
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "NON_NULL"
                      "name", null
                      "ofType", upcast Map.ofList<string, obj> [
                        "kind", upcast "SCALAR"
                        "name", upcast "String"
                        "ofType", null]]] :> obj
                  upcast Map.ofList<string, obj> [
                    "name", upcast "description"
                    "description", null
                    "isDeprecated", upcast false
                    "deprecationReason", null
                    "args", upcast []
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "SCALAR"
                      "name", upcast "String"
                      "ofType", null]]
                  upcast Map.ofList<string, obj> [
                    "name", upcast "type"
                    "description", null
                    "isDeprecated", upcast false
                    "deprecationReason", null
                    "args", upcast []
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "NON_NULL"
                      "name", null
                      "ofType", upcast Map.ofList<string, obj> [
                        "kind", upcast "OBJECT"
                        "name", upcast "__Type"
                        "ofType", null]]]
                  upcast Map.ofList<string, obj> [
                    "name", upcast "defaultValue"
                    "description", upcast "A GraphQL-formatted string representing the default value for this input value."
                    "isDeprecated", upcast false
                    "deprecationReason", null
                    "args", upcast []
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "SCALAR"
                      "name", upcast "String"
                      "ofType", null]]
                ]
                "kind", upcast "OBJECT"
                "possibleTypes", null
              ]
              upcast Map.ofList<string, obj> [
                "inputFields", null
                "name", upcast "__Schema"
                "description", upcast "A GraphQL Schema defines the capabilities of a GraphQL server. It exposes all available types and directives on the server, as well as the entry points for query, mutation, and subscription operations."
                "interfaces", upcast []
                "enumValues", null
                "fields", upcast [
                  Map.ofList<string, obj> [
                    "name", upcast "types"
                    "description", upcast "A list of all types supported by this server."
                    "isDeprecated", upcast false
                    "deprecationReason", null
                    "args", upcast []
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "NON_NULL"
                      "name", null
                      "ofType", upcast Map.ofList<string, obj> [
                        "kind", upcast "LIST"
                        "name", null
                        "ofType", upcast Map.ofList<string, obj> [
                          "kind", upcast "NON_NULL"
                          "name", null
                          "ofType", upcast Map.ofList<string, obj> [
                            "kind", upcast "OBJECT"
                            "name", upcast "__Type"]]]]] :> obj
                  upcast Map.ofList<string, obj> [
                    "name", upcast "queryType"
                    "description", upcast "The type that query operations will be rooted at."
                    "isDeprecated", upcast false
                    "deprecationReason", null
                    "args", upcast []
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "NON_NULL"
                      "name", null
                      "ofType", upcast Map.ofList<string, obj> [
                        "kind", upcast "OBJECT"
                        "name", upcast "__Type"
                        "ofType", null]]]
                  upcast Map.ofList<string, obj> [
                    "name", upcast "mutationType"
                    "description", upcast "If this server supports mutation, the type that mutation operations will be rooted at."
                    "isDeprecated", upcast false
                    "deprecationReason", null
                    "args", upcast []
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "OBJECT"
                      "name", upcast "__Type"
                      "ofType", null]]
                  upcast Map.ofList<string, obj> [
                    "name", upcast "subscriptionType"
                    "description", upcast "If this server support subscription, the type that subscription operations will be rooted at."
                    "isDeprecated", upcast false
                    "deprecationReason", null
                    "args", upcast []
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "OBJECT"
                      "name", upcast "__Type"
                      "ofType", null]]
                  upcast Map.ofList<string, obj> [
                    "name", upcast "directives"
                    "description", upcast "A list of all directives supported by this server."
                    "isDeprecated", upcast false
                    "deprecationReason", null
                    "args", upcast []
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "NON_NULL"
                      "name", null
                      "ofType", upcast Map.ofList<string, obj> [
                        "kind", upcast "LIST"
                        "name", null
                        "ofType", upcast Map.ofList<string, obj> [
                          "kind", upcast "NON_NULL"
                          "name", null
                          "ofType", upcast Map.ofList<string, obj> [
                            "kind", upcast "OBJECT"
                            "name", upcast "__Directive"]]]]]
                ]
                "kind", upcast "OBJECT"
                "possibleTypes", null
              ]
              upcast Map.ofList<string, obj> [
                "inputFields", null
                "name", upcast "__Type"
                "description", upcast "The fundamental unit of any GraphQL Schema is the type. There are many kinds of types in GraphQL as represented by the `__TypeKind` enum.\n\nDepending on the kind of a type, certain fields describe information about that type. Scalar types provide no information beyond a name and description, while Enum types provide their values. Object and Interface types provide the fields they describe. Abstract types, Union and Interface, provide the Object types possible at runtime. List and NonNull types compose other types."
                "interfaces", upcast []
                "enumValues", null
                "fields", upcast [
                  Map.ofList<string, obj> [
                    "name", upcast "kind"
                    "description", null
                    "isDeprecated", upcast false
                    "deprecationReason", null
                    "args", upcast []
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "NON_NULL"
                      "name", null
                      "ofType", upcast Map.ofList<string, obj> [
                        "kind", upcast "ENUM"
                        "name", upcast "__TypeKind"
                        "ofType", null]]] :> obj
                  upcast Map.ofList<string, obj> [
                    "name", upcast "name"
                    "description", null
                    "isDeprecated", upcast false
                    "deprecationReason", null
                    "args", upcast []
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "SCALAR"
                      "name", upcast "String"
                      "ofType", null]]
                  upcast Map.ofList<string, obj> [
                    "name", upcast "description"
                    "description", null
                    "isDeprecated", upcast false
                    "deprecationReason", null
                    "args", upcast []
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "SCALAR"
                      "name", upcast "String"
                      "ofType", null]]
                  upcast Map.ofList<string, obj> [
                    "name", upcast "fields"
                    "description", null
                    "isDeprecated", upcast false
                    "deprecationReason", null
                    "args", upcast [
                      Map.ofList<string, obj> [
                        "name", upcast "includeDeprecated"
                        "description", null
                        "type", upcast Map.ofList<string, obj> [
                          "kind", upcast "SCALAR"
                          "name", upcast "Boolean"
                          "ofType", null]
                        "defaultValue", upcast "false"]:>obj]
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "LIST"
                      "name", null
                      "ofType", upcast Map.ofList<string, obj> [
                        "kind", upcast "NON_NULL"
                        "name", null
                        "ofType", upcast Map.ofList<string, obj> [
                          "kind", upcast "OBJECT"
                          "name", upcast "__Field"
                          "ofType", null]]]]
                  upcast Map.ofList<string, obj> [
                    "name", upcast "interfaces"
                    "description", null
                    "isDeprecated", upcast false
                    "deprecationReason", null
                    "args", upcast []
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "LIST"
                      "name", null
                      "ofType", upcast Map.ofList<string, obj> [
                        "kind", upcast "NON_NULL"
                        "name", null
                        "ofType", upcast Map.ofList<string, obj> [
                          "kind", upcast "OBJECT"
                          "name", upcast "__Type"
                          "ofType", null]]]]
                  upcast Map.ofList<string, obj> [
                    "name", upcast "possibleTypes"
                    "description", null
                    "isDeprecated", upcast false
                    "deprecationReason", null
                    "args", upcast []
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "LIST"
                      "name", null
                      "ofType", upcast Map.ofList<string, obj> [
                        "kind", upcast "NON_NULL"
                        "name", null
                        "ofType", upcast Map.ofList<string, obj> [
                          "kind", upcast "OBJECT"
                          "name", upcast "__Type"
                          "ofType", null]]]]
                  upcast Map.ofList<string, obj> [
                    "name", upcast "enumValues"
                    "description", null
                    "isDeprecated", upcast false
                    "deprecationReason", null
                    "args", upcast [
                      Map.ofList<string, obj> [
                        "name", upcast "includeDeprecated"
                        "description", null
                        "type", upcast Map.ofList<string, obj> [
                          "kind", upcast "SCALAR"
                          "name", upcast "Boolean"
                          "ofType", null]
                        "defaultValue", upcast "false"]:>obj]
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "LIST"
                      "name", null
                      "ofType", upcast Map.ofList<string, obj> [
                        "kind", upcast "NON_NULL"
                        "name", null
                        "ofType", upcast Map.ofList<string, obj> [
                          "kind", upcast "OBJECT"
                          "name", upcast "__EnumValue"
                          "ofType", null]]]]
                  upcast Map.ofList<string, obj> [
                    "name", upcast "inputFields"
                    "description", null
                    "isDeprecated", upcast false
                    "deprecationReason", null
                    "args", upcast []
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "LIST"
                      "name", null
                      "ofType", upcast Map.ofList<string, obj> [
                        "kind", upcast "NON_NULL"
                        "name", null
                        "ofType", upcast Map.ofList<string, obj> [
                          "kind", upcast "OBJECT"
                          "name", upcast "__InputValue"
                          "ofType", null]]]]
                  upcast Map.ofList<string, obj> [
                    "name", upcast "ofType"
                    "description", null
                    "isDeprecated", upcast false
                    "deprecationReason", null
                    "args", upcast []
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "OBJECT"
                      "name", upcast "__Type"
                      "ofType", null]]
                ]
                "kind", upcast "OBJECT"
                "possibleTypes", null
              ]
              upcast Map.ofList<string, obj> [
                "inputFields", null
                "name", upcast "__TypeKind"
                "description", upcast "An enum describing what kind of type a given `__Type` is."
                "interfaces", null
                "enumValues", upcast [
                  Map.ofList<string, obj> [
                    "name", upcast "SCALAR"
                    "description", upcast "Indicates this type is a scalar."
                    "isDeprecated", upcast false
                    "deprecationReason", null] :> obj
                  upcast Map.ofList<string, obj> [
                    "name", upcast "OBJECT"
                    "description", upcast "Indicates this type is an object. `fields` and `interfaces` are valid fields."
                    "isDeprecated", upcast false
                    "deprecationReason", null]
                  upcast Map.ofList<string, obj> [
                    "name", upcast "INTERFACE"
                    "description", upcast "Indicates this type is an interface. `fields` and `possibleTypes` are valid fields."
                    "isDeprecated", upcast false
                    "deprecationReason", null]
                  upcast Map.ofList<string, obj> [
                    "name", upcast "UNION"
                    "description", upcast "Indicates this type is a union. `possibleTypes` is a valid field."
                    "isDeprecated", upcast false
                    "deprecationReason", null]
                  upcast Map.ofList<string, obj> [
                    "name", upcast "ENUM"
                    "description", upcast "Indicates this type is an enum. `enumValues` is a valid field."
                    "isDeprecated", upcast false
                    "deprecationReason", null]
                  upcast Map.ofList<string, obj> [
                    "name", upcast "INPUT_OBJECT"
                    "description", upcast "Indicates this type is an input object. `inputFields` is a valid field."
                    "isDeprecated", upcast false
                    "deprecationReason", null]
                  upcast Map.ofList<string, obj> [
                    "name", upcast "LIST"
                    "description", upcast "Indicates this type is a list. `ofType` is a valid field."
                    "isDeprecated", upcast false
                    "deprecationReason", null]
                  upcast Map.ofList<string, obj> [
                    "name", upcast "NON_NULL"
                    "description", upcast "Indicates this type is a non-null. `ofType` is a valid field."
                    "isDeprecated", upcast false
                    "deprecationReason", null]
                ]
                "fields", null
                "kind", upcast "ENUM"
                "possibleTypes", null]
              upcast Map.ofList<string, obj> [
                "inputFields", null
                "name", upcast "BigDecimal"
                "description", upcast "The `BigDecimal` scalar type represents signed fractional values with arbitrary precision."
                "interfaces", null
                "enumValues", null
                "fields", null
                "kind", upcast "SCALAR"
                "possibleTypes", null]
              upcast Map.ofList<string, obj> [
                "inputFields", null
                "name", upcast "BigInt"
                "description", upcast "The `BigInt` scalar type represents non-fractional signed whole numeric values. BigInt can represent arbitrary big values."
                "interfaces", null
                "enumValues", null
                "fields", null
                "kind", upcast "SCALAR"
                "possibleTypes", null]
              upcast Map.ofList<string, obj> [
                "inputFields", null
                "name", upcast "Boolean"
                "description", upcast "The `Boolean` scalar type represents `true` or `false`."
                "interfaces", null
                "enumValues", null
                "fields", null
                "kind", upcast "SCALAR"
                "possibleTypes", null]
              upcast Map.ofList<string, obj> [
                "inputFields", null
                "name", upcast "Float"
                "description", upcast "The `Float` scalar type represents signed double-precision fractional values as specified by [IEEE 754](http://en.wikipedia.org/wiki/IEEE_floating_point)."
                "interfaces", null
                "enumValues", null
                "fields", null
                "kind", upcast "SCALAR"
                "possibleTypes", null]
              upcast Map.ofList<string, obj> [
                "inputFields", null
                "name", upcast "ID"
                "description", upcast (
                  "The `ID` scalar type represents a unique identifier, often used to " +
                  "refetch an object or as key for a cache. The ID type appears in a JSON " +
                  "response as a String; however, it is not intended to be human-readable. " +
                  "When expected as an input type, any string (such as `\"4\"`) or integer " +
                  "(such as `4`) input value will be accepted as an ID.")
                "interfaces", null
                "enumValues", null
                "fields", null
                "kind", upcast "SCALAR"
                "possibleTypes", null]
              upcast Map.ofList<string, obj> [
                "inputFields", null
                "name", upcast "Int"
                "description", upcast (
                  "The `Int` scalar type represents non-fractional signed whole numeric values. " +
                  "Int can represent values between -(2^31) and 2^31 - 1.")
                "interfaces", null
                "enumValues", null
                "fields", null
                "kind", upcast "SCALAR"
                "possibleTypes", null]
              upcast Map.ofList<string, obj> [
                "inputFields", null
                "name", upcast "Long"
                "description", upcast (
                  "The `Long` scalar type represents non-fractional signed whole numeric values. " +
                  "Long can represent values between -(2^63) and 2^63 - 1.")
                "interfaces", null
                "enumValues", null
                "fields", null
                "kind", upcast "SCALAR"
                "possibleTypes", null]
              upcast Map.ofList<string, obj> [
                "inputFields", null
                "name", upcast "String"
                "description", upcast (
                  "The `String` scalar type represents textual data, represented as UTF-8 " +
                  "character sequences. The String type is most often used by GraphQL to " +
                  "represent free-form human-readable text.")
                "interfaces", null
                "enumValues", null
                "fields", null
                "kind", upcast "SCALAR"
                "possibleTypes", null]]
            "directives", upcast [
              Map.ofList<string, obj> [
                "name", upcast "include"
                "description", upcast "Directs the executor to include this field or fragment only when the `if` argument is true."
                "locations", upcast ["FIELD" :> obj, "FRAGMENT_SPREAD", "INLINE_FRAGMENT"]
                "args", upcast [
                  Map.ofList<string, obj> [
                    "name", upcast "if"
                    "description", upcast "Included when true."
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "NON_NULL"
                      "name", null
                      "ofType", upcast Map.ofList<string, obj> [
                        "kind", upcast "SCALAR"
                        "name", upcast "Boolean"
                        "ofType", null]]
                    "defaultValue", null]]] :> obj
              upcast Map.ofList<string, obj> [
                "name", upcast "skip"
                "description", upcast "Directs the executor to skip this field or fragment when the `if` argument is true."
                "locations", upcast ["FIELD" :> obj, "FRAGMENT_SPREAD", "INLINE_FRAGMENT"]
                "args", upcast [
                  Map.ofList<string, obj> [
                    "name", upcast "if"
                    "description", upcast "Included when true."
                    "type", upcast Map.ofList<string, obj> [
                      "kind", upcast "NON_NULL"
                      "name", null
                      "ofType", upcast Map.ofList<string, obj> [
                        "kind", upcast "SCALAR"
                        "name", upcast "Boolean"
                        "ofType", null]]
                    "defaultValue", null]]]]]]
    equals expected result.Data.Value