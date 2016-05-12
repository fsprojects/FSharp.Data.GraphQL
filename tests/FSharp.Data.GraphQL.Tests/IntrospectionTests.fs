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
    let root = Define.Object("QueryRoot", [
        Define.Field("onlyField", String)
    ])
    let schema = Schema(root)
    let (Object raw) = root
    let result = sync <| schema.AsyncExecute(parse introspectionQuery, raw)
    noErrors result
    let expected: Map<string,obj> = Map.ofList<string, obj> [
        "__schema", upcast Map.ofList<string, obj> [
            "mutationType", null
            "subscriptionType", null
            "queryType", upcast Map.ofList<string, obj> [
                    "name", upcast "QueryRoot"]
            "types", upcast [
                    box <| Map.ofList<string, obj> [
                         "kind", upcast "OBJECT"
                         "name", upcast "QueryRoot"
                         "inputFields", null
                         "interfaces", upcast []
                         "enumValues", null
                         "possibleTypes", null];
                    upcast Map.ofList<string, obj> [
                         "kind", upcast "OBJECT"
                         "name", upcast "__Schema"
                         "fields", upcast [
                                 box <| Map.ofList<string, obj> [
                                      "name", upcast "types"
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
                                                                      "name", upcast "__Type"]]]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "queryType"
                                      "args", upcast []
                                      "type", upcast Map.ofList<string, obj> [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast Map.ofList<string, obj> [
                                                      "kind", upcast "OBJECT"
                                                      "name", upcast "__Type"
                                                      "ofType", null]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "mutationType"
                                      "args", upcast []
                                      "type", upcast Map.ofList<string, obj> [
                                              "kind", upcast "OBJECT"
                                              "name", upcast "__Type"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "subscriptionType"
                                      "args", upcast []
                                      "type", upcast Map.ofList<string, obj> [
                                              "kind", upcast "OBJECT"
                                              "name", upcast "__Type"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "directives"
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
                                                                      "name", upcast "__Directive"]]]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];]
                         "inputFields", null
                         "interfaces", upcast []
                         "enumValues", null
                         "possibleTypes", null];
                    upcast Map.ofList<string, obj> [
                         "kind", upcast "OBJECT"
                         "name", upcast "__Type"
                         "fields", upcast [
                                 box <| Map.ofList<string, obj> [
                                      "name", upcast "kind"
                                      "args", upcast []
                                      "type", upcast Map.ofList<string, obj> [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast Map.ofList<string, obj> [
                                                      "kind", upcast "ENUM"
                                                      "name", upcast "__TypeKind"
                                                      "ofType", null]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "name"
                                      "args", upcast []
                                      "type", upcast Map.ofList<string, obj> [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "description"
                                      "args", upcast []
                                      "type", upcast Map.ofList<string, obj> [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "fields"
                                      "args", upcast [
                                              box <| Map.ofList<string, obj> [
                                                   "name", upcast "includeDeprecated"
                                                   "type", upcast Map.ofList<string, obj> [
                                                           "kind", upcast "SCALAR"
                                                           "name", upcast "Boolean"
                                                           "ofType", null]
                                                   "defaultValue", upcast "false"];]
                                      "type", upcast Map.ofList<string, obj> [
                                              "kind", upcast "LIST"
                                              "name", null
                                              "ofType", upcast Map.ofList<string, obj> [
                                                      "kind", upcast "NON_NULL"
                                                      "name", null
                                                      "ofType", upcast Map.ofList<string, obj> [
                                                              "kind", upcast "OBJECT"
                                                              "name", upcast "__Field"
                                                              "ofType", null]]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "interfaces"
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
                                                              "ofType", null]]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "possibleTypes"
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
                                                              "ofType", null]]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "enumValues"
                                      "args", upcast [
                                              box <| Map.ofList<string, obj> [
                                                   "name", upcast "includeDeprecated"
                                                   "type", upcast Map.ofList<string, obj> [
                                                           "kind", upcast "SCALAR"
                                                           "name", upcast "Boolean"
                                                           "ofType", null]
                                                   "defaultValue", upcast "false"];]
                                      "type", upcast Map.ofList<string, obj> [
                                              "kind", upcast "LIST"
                                              "name", null
                                              "ofType", upcast Map.ofList<string, obj> [
                                                      "kind", upcast "NON_NULL"
                                                      "name", null
                                                      "ofType", upcast Map.ofList<string, obj> [
                                                              "kind", upcast "OBJECT"
                                                              "name", upcast "__EnumValue"
                                                              "ofType", null]]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "inputFields"
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
                                                              "ofType", null]]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "ofType"
                                      "args", upcast []
                                      "type", upcast Map.ofList<string, obj> [
                                              "kind", upcast "OBJECT"
                                              "name", upcast "__Type"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];]
                         "inputFields", null
                         "interfaces", upcast []
                         "enumValues", null
                         "possibleTypes", null];
                    upcast Map.ofList<string, obj> [
                         "kind", upcast "ENUM"
                         "name", upcast "__TypeKind"
                         "fields", null
                         "inputFields", null
                         "interfaces", null
                         "enumValues", upcast [
                                 box <| Map.ofList<string, obj> [
                                      "name", upcast "SCALAR"
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "OBJECT"
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "INTERFACE"
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "UNION"
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "ENUM"
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "INPUT_OBJECT"
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "LIST"
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "NON_NULL"
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];]
                         "possibleTypes", null];
                    upcast Map.ofList<string, obj> [
                         "kind", upcast "SCALAR"
                         "name", upcast "String"
                         "fields", null
                         "inputFields", null
                         "interfaces", null
                         "enumValues", null
                         "possibleTypes", null];
                    upcast Map.ofList<string, obj> [
                         "kind", upcast "SCALAR"
                         "name", upcast "Boolean"
                         "fields", null
                         "inputFields", null
                         "interfaces", null
                         "enumValues", null
                         "possibleTypes", null];
                    upcast Map.ofList<string, obj> [
                         "kind", upcast "OBJECT"
                         "name", upcast "__Field"
                         "fields", upcast [
                                 box <| Map.ofList<string, obj> [
                                      "name", upcast "name"
                                      "args", upcast []
                                      "type", upcast Map.ofList<string, obj> [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast Map.ofList<string, obj> [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "String"
                                                      "ofType", null]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "description"
                                      "args", upcast []
                                      "type", upcast Map.ofList<string, obj> [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "args"
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
                                                                      "name", upcast "__InputValue"]]]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "type"
                                      "args", upcast []
                                      "type", upcast Map.ofList<string, obj> [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast Map.ofList<string, obj> [
                                                      "kind", upcast "OBJECT"
                                                      "name", upcast "__Type"
                                                      "ofType", null]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "isDeprecated"
                                      "args", upcast []
                                      "type", upcast Map.ofList<string, obj> [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast Map.ofList<string, obj> [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "Boolean"
                                                      "ofType", null]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "deprecationReason"
                                      "args", upcast []
                                      "type", upcast Map.ofList<string, obj> [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];]
                         "inputFields", null
                         "interfaces", upcast []
                         "enumValues", null
                         "possibleTypes", null];
                    upcast Map.ofList<string, obj> [
                         "kind", upcast "OBJECT"
                         "name", upcast "__InputValue"
                         "fields", upcast [
                                 box <| Map.ofList<string, obj> [
                                      "name", upcast "name"
                                      "args", upcast []
                                      "type", upcast Map.ofList<string, obj> [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast Map.ofList<string, obj> [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "String"
                                                      "ofType", null]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "description"
                                      "args", upcast []
                                      "type", upcast Map.ofList<string, obj> [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "type"
                                      "args", upcast []
                                      "type", upcast Map.ofList<string, obj> [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast Map.ofList<string, obj> [
                                                      "kind", upcast "OBJECT"
                                                      "name", upcast "__Type"
                                                      "ofType", null]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "defaultValue"
                                      "args", upcast []
                                      "type", upcast Map.ofList<string, obj> [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];]
                         "inputFields", null
                         "interfaces", upcast []
                         "enumValues", null
                         "possibleTypes", null];
                    upcast Map.ofList<string, obj> [
                         "kind", upcast "OBJECT"
                         "name", upcast "__EnumValue"
                         "fields", upcast [
                                 box <| Map.ofList<string, obj> [
                                      "name", upcast "name"
                                      "args", upcast []
                                      "type", upcast Map.ofList<string, obj> [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast Map.ofList<string, obj> [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "String"
                                                      "ofType", null]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "description"
                                      "args", upcast []
                                      "type", upcast Map.ofList<string, obj> [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "isDeprecated"
                                      "args", upcast []
                                      "type", upcast Map.ofList<string, obj> [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast Map.ofList<string, obj> [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "Boolean"
                                                      "ofType", null]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "deprecationReason"
                                      "args", upcast []
                                      "type", upcast Map.ofList<string, obj> [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];]
                         "inputFields", null
                         "interfaces", upcast []
                         "enumValues", null
                         "possibleTypes", null];
                    upcast Map.ofList<string, obj> [
                         "kind", upcast "OBJECT"
                         "name", upcast "__Directive"
                         "fields", upcast [
                                 box <| Map.ofList<string, obj> [
                                      "name", upcast "name"
                                      "args", upcast []
                                      "type", upcast Map.ofList<string, obj> [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast Map.ofList<string, obj> [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "String"
                                                      "ofType", null]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "description"
                                      "args", upcast []
                                      "type", upcast Map.ofList<string, obj> [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "locations"
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
                                                                      "name", upcast "__DirectiveLocation"]]]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "args"
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
                                                                      "name", upcast "__InputValue"]]]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "onOperation"
                                      "args", upcast []
                                      "type", upcast Map.ofList<string, obj> [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast Map.ofList<string, obj> [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "Boolean"
                                                      "ofType", null]]
                                      "isDeprecated", upcast true
                                      "deprecationReason", upcast "Use `locations`."];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "onFragment"
                                      "args", upcast []
                                      "type", upcast Map.ofList<string, obj> [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast Map.ofList<string, obj> [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "Boolean"
                                                      "ofType", null]]
                                      "isDeprecated", upcast true
                                      "deprecationReason", upcast "Use `locations`."];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "onField"
                                      "args", upcast []
                                      "type", upcast Map.ofList<string, obj> [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast Map.ofList<string, obj> [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "Boolean"
                                                      "ofType", null]]
                                      "isDeprecated", upcast true
                                      "deprecationReason", upcast "Use `locations`."];]
                         "inputFields", null
                         "interfaces", upcast []
                         "enumValues", null
                         "possibleTypes", null];
                    upcast Map.ofList<string, obj> [
                         "kind", upcast "ENUM"
                         "name", upcast "__DirectiveLocation"
                         "fields", null
                         "inputFields", null
                         "interfaces", null
                         "enumValues", upcast [
                                 box <| Map.ofList<string, obj> [
                                      "name", upcast "QUERY"
                                      "isDeprecated", upcast false];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "MUTATION"
                                      "isDeprecated", upcast false];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "SUBSCRIPTION"
                                      "isDeprecated", upcast false];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "FIELD"
                                      "isDeprecated", upcast false];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "FRAGMENT_DEFINITION"
                                      "isDeprecated", upcast false];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "FRAGMENT_SPREAD"
                                      "isDeprecated", upcast false];
                                 upcast Map.ofList<string, obj> [
                                      "name", upcast "INLINE_FRAGMENT"
                                      "isDeprecated", upcast false];]
                         "possibleTypes", null];]
            "directives", upcast [
                    box <| Map.ofList<string, obj> [
                         "name", upcast "include"
                         "locations", upcast [
                                 box <| "FIELD";
                                 upcast "FRAGMENT_SPREAD";
                                 upcast "INLINE_FRAGMENT";]
                         "args", upcast [
                                 box <| Map.ofList<string, obj> [
                                      "defaultValue", null
                                      "name", upcast "if"
                                      "type", upcast Map.ofList<string, obj> [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast Map.ofList<string, obj> [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "Boolean"
                                                      "ofType", null]]];]];
                    upcast Map.ofList<string, obj> [
                         "name", upcast "skip"
                         "locations", upcast [
                                 box <| "FIELD";
                                 upcast "FRAGMENT_SPREAD";
                                 upcast "INLINE_FRAGMENT";]
                         "args", upcast [
                                 box <| Map.ofList<string, obj> [
                                      "defaultValue", null
                                      "name", upcast "if"
                                      "type", upcast Map.ofList<string, obj> [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast Map.ofList<string, obj> [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "Boolean"
                                                      "ofType", null]]];]];]]]

    equals expected result.Data.Value