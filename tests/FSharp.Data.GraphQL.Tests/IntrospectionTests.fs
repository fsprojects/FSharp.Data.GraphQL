/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.IntrospectionTests

#nowarn "25"

open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Types.Introspection

type IntrospectionResult = {
    __schema: IntrospectionSchema
}

type IntrospectionData = {
    Data:  IntrospectionResult
}

[<Fact>]
let ``Introspection schema should be serializable back and forth using json`` () =
    let root = Define.Object("Query", [ Define.Field("onlyField", String) ])
    let schema = Schema(root)
    let query = """query IntrospectionQuery {
      __schema {
        queryType { 
          kind
          name
          description
          ofType {
            ...FullType
          } 
        }
        mutationType { 
          kind
          name
          description
          ofType {
            ...FullType
          } 
        }
        subscriptionType { 
          kind
          name
          description 
          ofType {
            ...FullType
          } 
        }
        types {
          ...FullType
        }
        directives {
          name
          description
          locations
          args {
            ...InputValue
          }
        }
      }
    }

    fragment FullType on __Type {
      kind
      name
      description
      fields(includeDeprecated: true) {
        name
        description
        args {
          ...InputValue
        }
        type {
          ...TypeRef
        }
        isDeprecated
        deprecationReason
      }
      inputFields {
        ...InputValue
      }
      interfaces {
        ...TypeRef
      }
      enumValues(includeDeprecated: true) {
        name
        description
        isDeprecated
        deprecationReason
      }
      possibleTypes {
        ...TypeRef
      }
    }

    fragment InputValue on __InputValue {
      name
      description
      type { ...TypeRef }
      defaultValue
    }

    fragment TypeRef on __Type {
      kind
      name
      description
      ofType {
        kind
        name
        description
        ofType {
          kind
          name
          description
          ofType {
            kind
            name
            description
          }
        }
      }
    }"""
    let result = Executor(schema).AsyncExecute(query) |> sync
    match result with
    | Direct (data, errors) ->
        empty errors
        let json = toJson data
        let deserialized : IntrospectionData = Helpers.fromJson json
        let expected = (schema :> ISchema).Introspected
        deserialized.Data.__schema |> equals expected
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Core type definitions are considered nullable`` () =
    let root = Define.Object("Query", [ Define.Field("onlyField", String) ])
    let schema = Schema(root)
    let query = """{ __type(name: "String") {
      kind
      name
      ofType {
        kind
        name
        ofType {
          kind
          name
          ofType {
            kind
            name
          }
        }
      }
    } }"""
    let result = sync <| Executor(schema).AsyncExecute(query)
    let expected =
      NameValueLookup.ofList [
        "__type", upcast NameValueLookup.ofList [
            "kind", upcast "SCALAR"
            "name", upcast "String"
            "ofType", null]]
    match result with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

type User = { FirstName: string; LastName: string }
type UserInput = { Name: string }

[<Fact>]
let ``Introspection works with query and mutation sharing same generic param`` =
    let user = 
        Define.Object<User>("User", 
            [ Define.AutoField("firstName", String)
              Define.AutoField("lastName", String) ])
    let userInput = 
        Define.InputObject<UserInput>("UserInput", 
            [ Define.Input("name", String) ])
    let query = 
        Define.Object<User list>("Query", 
            [ Define.Field("users", ListOf user, "Query object", [ Define.Input("input", userInput) ], fun _ u -> u) ])
    let mutation = 
        Define.Object<User list>("Mutation", 
            [ Define.Field("addUser", user, "Adds an user", [ Define.Input("input", userInput) ], fun _ u -> u |> List.head)])
    let schema = Schema(query, mutation)
    Executor(schema).AsyncExecute(Introspection.introspectionQuery) |> sync |> ignore
    
[<Fact>]
let ``Default field type definitions are considered non-null`` () =
    let root = Define.Object("Query", [ Define.Field("onlyField", String) ])
    let schema = Schema(root)
    let query = """{ __type(name: "Query") {
      fields {
        name
        type {
          kind
          name
          ofType {
            kind
            name
            ofType {
              kind
              name
              ofType {
                kind
                name
              }
            }
          }
        }
      }
    } }"""
    let result = sync <| Executor(schema).AsyncExecute(query)
    let expected =
      NameValueLookup.ofList [
        "__type", upcast NameValueLookup.ofList [
            "fields", upcast [
                box <| NameValueLookup.ofList [
                    "name", upcast "onlyField"
                    "type", upcast NameValueLookup.ofList [
                        "kind", upcast "NON_NULL"
                        "name", null
                        "ofType", upcast NameValueLookup.ofList [
                            "kind", upcast "SCALAR"
                            "name", upcast "String"
                            "ofType", null]]]]]]
    match result with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"
    
[<Fact>]
let ``Nullabe field type definitions are considered nullable`` () =
    let root = Define.Object("Query", [ Define.Field("onlyField", Nullable String) ])
    let schema = Schema(root)
    let query = """{ __type(name: "Query") {
      fields {
        name
        type {
          kind
          name
          ofType {
            kind
            name
            ofType {
              kind
              name
              ofType {
                kind
                name
              }
            }
          }
        }
      }
    } }"""
    let result = sync <| Executor(schema).AsyncExecute(query)
    let expected =
      NameValueLookup.ofList [
        "__type", upcast NameValueLookup.ofList [
            "fields", upcast [
                box <| NameValueLookup.ofList [
                    "name", upcast "onlyField"
                    "type", upcast NameValueLookup.ofList [
                        "kind", upcast "SCALAR"
                        "name", upcast "String"
                        "ofType", null]]]]]
    match result with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"
    
[<Fact>]
let ``Default field args type definitions are considered non-null`` () =
    let root = Define.Object("Query", [ Define.Field("onlyField", String, "", [ Define.Input("onlyArg", Int) ], fun _ () -> null) ])
    let schema = Schema(root)
    let query = """{ __type(name: "Query") {
      fields {
        args {
          name
          type {
            kind
            name
            ofType {
              kind
              name
              ofType {
                kind
                name
                ofType {
                  kind
                  name
                }
              }
            }
          }
        }
      }
    } }"""
    let result = sync <| Executor(schema).AsyncExecute(query)
    let expected =
      NameValueLookup.ofList [
        "__type", upcast NameValueLookup.ofList [
            "fields", upcast [
                box <| NameValueLookup.ofList [
                    "args", upcast [
                        box <| NameValueLookup.ofList [
                            "name", upcast "onlyArg"
                            "type", upcast NameValueLookup.ofList [
                                "kind", upcast "NON_NULL"
                                "name", null
                                "ofType", upcast NameValueLookup.ofList [
                                    "kind", upcast "SCALAR"
                                    "name", upcast "Int"
                                    "ofType", null]]]]]]]]
    match result with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"
    
[<Fact>]
let ``Nullable field args type definitions are considered nullable`` () =
    let root = Define.Object("Query", [ Define.Field("onlyField", String, "", [ Define.Input("onlyArg", Nullable Int) ], fun _ () -> null) ])
    let schema = Schema(root)
    let query = """{ __type(name: "Query") {
      fields {
        args {
          name
          type {
            kind
            name
            ofType {
              kind
              name
              ofType {
                kind
                name
                ofType {
                  kind
                  name
                }
              }
            }
          }
        }
      }
    } }"""
    let result = sync <| Executor(schema).AsyncExecute(query)
    let expected =
      NameValueLookup.ofList [
        "__type", upcast NameValueLookup.ofList [
            "fields", upcast [
                box <| NameValueLookup.ofList [
                    "args", upcast [
                        box <| NameValueLookup.ofList [
                            "name", upcast "onlyArg"
                            "type", upcast NameValueLookup.ofList [
                                "kind", upcast "SCALAR"
                                "name", upcast "Int"
                                "ofType", null ]]]]]]]
    match result with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Introspection executes an introspection query`` () =
    let root = Define.Object("QueryRoot", [ Define.Field("onlyField", String) ])
    let schema = Schema(root)
    let (Patterns.Object raw) = root
    let result = sync <| Executor(schema).AsyncExecute(parse Introspection.introspectionQuery, raw)
    let expected =
      NameValueLookup.ofList [
        "__schema", upcast NameValueLookup.ofList [
            "queryType", upcast NameValueLookup.ofList [
                    "name", upcast "QueryRoot"]
            "mutationType", null
            "subscriptionType", null
            "types", upcast [
                    box <| NameValueLookup.ofList [
                            "kind", upcast "SCALAR"
                            "name", upcast "Int"
                            "description", upcast "The `Int` scalar type represents non-fractional signed whole numeric values. Int can represent values between -(2^31) and 2^31 - 1."
                            "fields", null
                            "inputFields", null
                            "interfaces", null
                            "enumValues", null
                            "possibleTypes", null
                    ]
                    box <| NameValueLookup.ofList [
                            "kind", upcast "SCALAR"
                            "name", upcast "String"
                            "description", upcast "The `String` scalar type represents textual data, represented as UTF-8 character sequences. The String type is most often used by GraphQL to represent free-form human-readable text."
                            "fields", null
                            "inputFields", null
                            "interfaces", null
                            "enumValues", null
                            "possibleTypes", null
                    ]
                    box <| NameValueLookup.ofList [
                            "kind", upcast "SCALAR"
                            "name", upcast "Boolean"
                            "description", upcast "The `Boolean` scalar type represents `true` or `false`."
                            "fields", null
                            "inputFields", null
                            "interfaces", null
                            "enumValues", null
                            "possibleTypes", null
                    ]
                    box <| NameValueLookup.ofList [
                            "kind", upcast "SCALAR"
                            "name", upcast "Float"
                            "description", upcast "The `Float` scalar type represents signed double-precision fractional values as specified by [IEEE 754](http://en.wikipedia.org/wiki/IEEE_floating_point)."
                            "fields", null
                            "inputFields", null
                            "interfaces", null
                            "enumValues", null
                            "possibleTypes", null
                    ]
                    box <| NameValueLookup.ofList [
                            "kind", upcast "SCALAR"
                            "name", upcast "ID"
                            "description", upcast "The `ID` scalar type represents a unique identifier, often used to refetch an object or as key for a cache. The ID type appears in a JSON response as a String; however, it is not intended to be human-readable. When expected as an input type, any string (such as `\"4\"`) or integer (such as `4`) input value will be accepted as an ID."
                            "fields", null
                            "inputFields", null
                            "interfaces", null
                            "enumValues", null
                            "possibleTypes", null
                    ]
                    box <| NameValueLookup.ofList [
                            "kind", upcast "SCALAR"
                            "name", upcast "Date"
                            "description", upcast "The `Date` scalar type represents a Date value with Time component. The Date type appears in a JSON response as a String representation compatible with ISO-8601 format."
                            "fields", null
                            "inputFields", null
                            "interfaces", null
                            "enumValues", null
                            "possibleTypes", null
                    ]
                    box <| NameValueLookup.ofList [
                            "kind", upcast "SCALAR"
                            "name", upcast "URI"
                            "description", upcast "The `URI` scalar type represents a string resource identifier compatible with URI standard. The URI type appears in a JSON response as a String."
                            "fields", null
                            "inputFields", null
                            "interfaces", null
                            "enumValues", null
                            "possibleTypes", null
                    ]
                    upcast NameValueLookup.ofList [
                         "kind", upcast "OBJECT"
                         "name", upcast "__Schema"
                         "description", upcast "A GraphQL Schema defines the capabilities of a GraphQL server. It exposes all available types and directives on the server, as well as the entry points for query, mutation, and subscription operations."
                         "fields", upcast [
                                 box <| NameValueLookup.ofList [
                                      "name", upcast "directives"
                                      "description", upcast "A list of all directives supported by this server."
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
                                                                      "name", upcast "__Directive"
                                                              ]
                                                      ]
                                              ]
                                      ]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null
                                 ]
                                 box <| NameValueLookup.ofList [
                                      "name", upcast "mutationType"
                                      "description", upcast "If this server supports mutation, the type that mutation operations will be rooted at."
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "OBJECT"
                                              "name", upcast "__Type"
                                              "ofType", null
                                      ]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null
                                 ]
                                 box <| NameValueLookup.ofList [
                                      "name", upcast "queryType"
                                      "description", upcast "The type that query operations will be rooted at."
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "OBJECT"
                                                      "name", upcast "__Type"
                                                      "ofType", null
                                              ]
                                      ]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null
                                 ]
                                 box <| NameValueLookup.ofList [
                                      "name", upcast "subscriptionType"
                                      "description", upcast "If this server support subscription, the type that subscription operations will be rooted at."
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "OBJECT"
                                              "name", upcast "__Type"
                                              "ofType", null
                                      ]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null
                                 ]
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
    match result with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"
