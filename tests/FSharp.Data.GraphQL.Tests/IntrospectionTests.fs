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
    let result = sync <| schema.AsyncExecute(parse Introspection.introspectionQuery, raw)
    noErrors result
    let expected = NameValueLookup.ofList [
        "__schema", upcast NameValueLookup.ofList [
            "mutationType", null
            "subscriptionType", null
            "queryType", upcast NameValueLookup.ofList [
                    "name", upcast "QueryRoot"]
            "types", upcast [
                    box <| NameValueLookup.ofList [
                         "kind", upcast "OBJECT"
                         "name", upcast "QueryRoot"
                         "inputFields", null
                         "interfaces", upcast []
                         "enumValues", null
                         "possibleTypes", null];
                    upcast NameValueLookup.ofList [
                         "kind", upcast "OBJECT"
                         "name", upcast "__Schema"
                         "fields", upcast [
                                 box <| NameValueLookup.ofList [
                                      "name", upcast "types"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "LIST"
                                                      "name", null
                                                      "ofType", upcast NameValueLookup.ofList [
                                                              "kind", upcast "NON_NULL"
                                                              "name", null
                                                              "ofType", upcast NameValueLookup.ofList [
                                                                      "kind", upcast "OBJECT"
                                                                      "name", upcast "__Type"]]]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "queryType"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "OBJECT"
                                                      "name", upcast "__Type"
                                                      "ofType", null]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "mutationType"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "OBJECT"
                                              "name", upcast "__Type"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "subscriptionType"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "OBJECT"
                                              "name", upcast "__Type"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "directives"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "LIST"
                                                      "name", null
                                                      "ofType", upcast NameValueLookup.ofList [
                                                              "kind", upcast "NON_NULL"
                                                              "name", null
                                                              "ofType", upcast NameValueLookup.ofList [
                                                                      "kind", upcast "OBJECT"
                                                                      "name", upcast "__Directive"]]]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];]
                         "inputFields", null
                         "interfaces", upcast []
                         "enumValues", null
                         "possibleTypes", null];
                    upcast NameValueLookup.ofList [
                         "kind", upcast "OBJECT"
                         "name", upcast "__Type"
                         "fields", upcast [
                                 box <| NameValueLookup.ofList [
                                      "name", upcast "kind"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "ENUM"
                                                      "name", upcast "__TypeKind"
                                                      "ofType", null]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "name"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "description"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "fields"
                                      "args", upcast [
                                              box <| NameValueLookup.ofList [
                                                   "name", upcast "includeDeprecated"
                                                   "type", upcast NameValueLookup.ofList [
                                                           "kind", upcast "SCALAR"
                                                           "name", upcast "Boolean"
                                                           "ofType", null]
                                                   "defaultValue", upcast "false"];]
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "LIST"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "NON_NULL"
                                                      "name", null
                                                      "ofType", upcast NameValueLookup.ofList [
                                                              "kind", upcast "OBJECT"
                                                              "name", upcast "__Field"
                                                              "ofType", null]]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "interfaces"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "LIST"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "NON_NULL"
                                                      "name", null
                                                      "ofType", upcast NameValueLookup.ofList [
                                                              "kind", upcast "OBJECT"
                                                              "name", upcast "__Type"
                                                              "ofType", null]]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "possibleTypes"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "LIST"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "NON_NULL"
                                                      "name", null
                                                      "ofType", upcast NameValueLookup.ofList [
                                                              "kind", upcast "OBJECT"
                                                              "name", upcast "__Type"
                                                              "ofType", null]]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "enumValues"
                                      "args", upcast [
                                              box <| NameValueLookup.ofList [
                                                   "name", upcast "includeDeprecated"
                                                   "type", upcast NameValueLookup.ofList [
                                                           "kind", upcast "SCALAR"
                                                           "name", upcast "Boolean"
                                                           "ofType", null]
                                                   "defaultValue", upcast "false"];]
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "LIST"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "NON_NULL"
                                                      "name", null
                                                      "ofType", upcast NameValueLookup.ofList [
                                                              "kind", upcast "OBJECT"
                                                              "name", upcast "__EnumValue"
                                                              "ofType", null]]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "inputFields"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "LIST"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "NON_NULL"
                                                      "name", null
                                                      "ofType", upcast NameValueLookup.ofList [
                                                              "kind", upcast "OBJECT"
                                                              "name", upcast "__InputValue"
                                                              "ofType", null]]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "ofType"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "OBJECT"
                                              "name", upcast "__Type"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];]
                         "inputFields", null
                         "interfaces", upcast []
                         "enumValues", null
                         "possibleTypes", null];
                    upcast NameValueLookup.ofList [
                         "kind", upcast "ENUM"
                         "name", upcast "__TypeKind"
                         "fields", null
                         "inputFields", null
                         "interfaces", null
                         "enumValues", upcast [
                                 box <| NameValueLookup.ofList [
                                      "name", upcast "SCALAR"
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "OBJECT"
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "INTERFACE"
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "UNION"
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "ENUM"
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "INPUT_OBJECT"
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "LIST"
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "NON_NULL"
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];]
                         "possibleTypes", null];
                    upcast NameValueLookup.ofList [
                         "kind", upcast "SCALAR"
                         "name", upcast "String"
                         "fields", null
                         "inputFields", null
                         "interfaces", null
                         "enumValues", null
                         "possibleTypes", null];
                    upcast NameValueLookup.ofList [
                         "kind", upcast "SCALAR"
                         "name", upcast "Boolean"
                         "fields", null
                         "inputFields", null
                         "interfaces", null
                         "enumValues", null
                         "possibleTypes", null];
                    upcast NameValueLookup.ofList [
                         "kind", upcast "OBJECT"
                         "name", upcast "__Field"
                         "fields", upcast [
                                 box <| NameValueLookup.ofList [
                                      "name", upcast "name"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "String"
                                                      "ofType", null]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "description"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "args"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "LIST"
                                                      "name", null
                                                      "ofType", upcast NameValueLookup.ofList [
                                                              "kind", upcast "NON_NULL"
                                                              "name", null
                                                              "ofType", upcast NameValueLookup.ofList [
                                                                      "kind", upcast "OBJECT"
                                                                      "name", upcast "__InputValue"]]]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "type"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "OBJECT"
                                                      "name", upcast "__Type"
                                                      "ofType", null]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "isDeprecated"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "Boolean"
                                                      "ofType", null]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "deprecationReason"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];]
                         "inputFields", null
                         "interfaces", upcast []
                         "enumValues", null
                         "possibleTypes", null];
                    upcast NameValueLookup.ofList [
                         "kind", upcast "OBJECT"
                         "name", upcast "__InputValue"
                         "fields", upcast [
                                 box <| NameValueLookup.ofList [
                                      "name", upcast "name"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "String"
                                                      "ofType", null]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "description"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "type"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "OBJECT"
                                                      "name", upcast "__Type"
                                                      "ofType", null]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "defaultValue"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];]
                         "inputFields", null
                         "interfaces", upcast []
                         "enumValues", null
                         "possibleTypes", null];
                    upcast NameValueLookup.ofList [
                         "kind", upcast "OBJECT"
                         "name", upcast "__EnumValue"
                         "fields", upcast [
                                 box <| NameValueLookup.ofList [
                                      "name", upcast "name"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "String"
                                                      "ofType", null]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "description"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "isDeprecated"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "Boolean"
                                                      "ofType", null]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "deprecationReason"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];]
                         "inputFields", null
                         "interfaces", upcast []
                         "enumValues", null
                         "possibleTypes", null];
                    upcast NameValueLookup.ofList [
                         "kind", upcast "OBJECT"
                         "name", upcast "__Directive"
                         "fields", upcast [
                                 box <| NameValueLookup.ofList [
                                      "name", upcast "name"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "String"
                                                      "ofType", null]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "description"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "locations"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "LIST"
                                                      "name", null
                                                      "ofType", upcast NameValueLookup.ofList [
                                                              "kind", upcast "NON_NULL"
                                                              "name", null
                                                              "ofType", upcast NameValueLookup.ofList [
                                                                      "kind", upcast "ENUM"
                                                                      "name", upcast "__DirectiveLocation"]]]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "args"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "LIST"
                                                      "name", null
                                                      "ofType", upcast NameValueLookup.ofList [
                                                              "kind", upcast "NON_NULL"
                                                              "name", null
                                                              "ofType", upcast NameValueLookup.ofList [
                                                                      "kind", upcast "OBJECT"
                                                                      "name", upcast "__InputValue"]]]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "onOperation"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "Boolean"
                                                      "ofType", null]]
                                      "isDeprecated", upcast true
                                      "deprecationReason", upcast "Use `locations`."];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "onFragment"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "Boolean"
                                                      "ofType", null]]
                                      "isDeprecated", upcast true
                                      "deprecationReason", upcast "Use `locations`."];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "onField"
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "Boolean"
                                                      "ofType", null]]
                                      "isDeprecated", upcast true
                                      "deprecationReason", upcast "Use `locations`."];]
                         "inputFields", null
                         "interfaces", upcast []
                         "enumValues", null
                         "possibleTypes", null];
                    upcast NameValueLookup.ofList [
                         "kind", upcast "ENUM"
                         "name", upcast "__DirectiveLocation"
                         "fields", null
                         "inputFields", null
                         "interfaces", null
                         "enumValues", upcast [
                                 box <| NameValueLookup.ofList [
                                      "name", upcast "QUERY"
                                      "isDeprecated", upcast false];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "MUTATION"
                                      "isDeprecated", upcast false];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "SUBSCRIPTION"
                                      "isDeprecated", upcast false];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "FIELD"
                                      "isDeprecated", upcast false];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "FRAGMENT_DEFINITION"
                                      "isDeprecated", upcast false];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "FRAGMENT_SPREAD"
                                      "isDeprecated", upcast false];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "INLINE_FRAGMENT"
                                      "isDeprecated", upcast false];]
                         "possibleTypes", null];]
            "directives", upcast [
                    box <| NameValueLookup.ofList [
                         "name", upcast "include"
                         "locations", upcast [
                                 box <| "FIELD";
                                 upcast "FRAGMENT_SPREAD";
                                 upcast "INLINE_FRAGMENT";]
                         "args", upcast [
                                 box <| NameValueLookup.ofList [
                                      "defaultValue", null
                                      "name", upcast "if"
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "Boolean"
                                                      "ofType", null]]];]];
                    upcast NameValueLookup.ofList [
                         "name", upcast "skip"
                         "locations", upcast [
                                 box <| "FIELD";
                                 upcast "FRAGMENT_SPREAD";
                                 upcast "INLINE_FRAGMENT";]
                         "args", upcast [
                                 box <| NameValueLookup.ofList [
                                      "defaultValue", null
                                      "name", upcast "if"
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "Boolean"
                                                      "ofType", null]]];]];]]]

    result.Data.Value|> equals (upcast expected)