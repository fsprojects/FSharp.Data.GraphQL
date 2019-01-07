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
let inputFieldQuery = """{
  __type(name: "Query") {
    fields {
      name
      args {
        name
        type {
          kind
          name
        }
        defaultValue
      }
    }
  }
}
"""

[<Fact>]
let ``Input field should be marked as nullable when defaultValue is provided`` () =
    let root = Define.Object("Query", [ 
        Define.Field("onlyField", String, "The only field", [ 
            Define.Input("in", String, defaultValue = "1") 
        ], fun _ _ -> "Only value") 
    ])
    let schema = Schema(root)
    let result = sync <| Executor(schema).AsyncExecute(inputFieldQuery)
    let expected = NameValueLookup.ofList [
        "__type", upcast NameValueLookup.ofList [
            "fields", upcast [ 
                NameValueLookup.ofList [
                    "name", upcast "onlyField"
                    "args", upcast [
                        NameValueLookup.ofList [
                            "name", upcast "in"
                            "type", upcast NameValueLookup.ofList [
                                "kind", upcast "SCALAR"
                                "name", upcast "String"
                            ]
                            "defaultValue", upcast "1"
                        ]
                    ]
                ] 
            ]
        ]
    ]
    match result with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Input field should be marked as non-nullable when defaultValue is not provided`` () =
    let root = Define.Object("Query", [ 
        Define.Field("onlyField", String, "The only field", [ 
            Define.Input("in", String) 
        ], fun _ _ -> "Only value") 
    ])
    let schema = Schema(root)
    let result = sync <| Executor(schema).AsyncExecute(inputFieldQuery)
    let expected = NameValueLookup.ofList [
        "__type", upcast NameValueLookup.ofList [
            "fields", upcast [ 
                NameValueLookup.ofList [
                    "name", upcast "onlyField"
                    "args", upcast [
                        NameValueLookup.ofList [
                            "name", upcast "in"
                            "type", upcast NameValueLookup.ofList [
                                "kind", upcast "NON_NULL"
                                "name", null
                            ]
                            "defaultValue", null
                        ]
                    ]
                ] 
            ]
        ]
    ]
    match result with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Input field should be marked as nullable when its type is nullable`` () =
    let root = Define.Object("Query", [ 
        Define.Field("onlyField", String, "The only field", [ 
            Define.Input("in", Nullable String) 
        ], fun _ _ -> "Only value") 
    ])
    let schema = Schema(root)
    let result = sync <| Executor(schema).AsyncExecute(inputFieldQuery)
    let expected = NameValueLookup.ofList [
        "__type", upcast NameValueLookup.ofList [
            "fields", upcast [ 
                NameValueLookup.ofList [
                    "name", upcast "onlyField"
                    "args", upcast [
                        NameValueLookup.ofList [
                            "name", upcast "in"
                            "type", upcast NameValueLookup.ofList [
                                "kind", upcast "SCALAR"
                                "name", upcast "String"
                            ]
                            "defaultValue", null
                        ]
                    ]
                ] 
            ]
        ]
    ]
    match result with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Input field should be marked as nullable when its type is nullable and have default value provided`` () =
    let root = Define.Object("Query", [ 
        Define.Field("onlyField", String, "The only field", [ 
            Define.Input("in", Nullable String, defaultValue = Some "1") 
        ], fun _ _ -> "Only value") 
    ])
    let schema = Schema(root)
    let result = sync <| Executor(schema).AsyncExecute(inputFieldQuery)
    let expected = NameValueLookup.ofList [
        "__type", upcast NameValueLookup.ofList [
            "fields", upcast [ 
                NameValueLookup.ofList [
                    "name", upcast "onlyField"
                    "args", upcast [
                        NameValueLookup.ofList [
                            "name", upcast "in"
                            "type", upcast NameValueLookup.ofList [
                                "kind", upcast "SCALAR"
                                "name", upcast "String"
                            ]
                            "defaultValue", upcast "1"
                        ]
                    ]
                ] 
            ]
        ]
    ]
    match result with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

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
                                      "description", upcast "A list of all types supported by this server."
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
                                      "deprecationReason", null];]
                         "inputFields", null
                         "interfaces", upcast []
                         "enumValues", null
                         "possibleTypes", null];
                    upcast NameValueLookup.ofList [
                         "kind", upcast "OBJECT"
                         "name", upcast "__Directive"
                         "description", upcast "A Directive provides a way to describe alternate runtime execution and type validation behavior in a GraphQL document. In some cases, you need to provide options to alter GraphQL’s execution behavior in ways field arguments will not suffice, such as conditionally including or skipping a field. Directives provide this by describing additional information to the executor."
                         "fields", upcast [
                                 box <| NameValueLookup.ofList [
                                      "name", upcast "args"
                                      "description", null
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
                                      "name", upcast "description"
                                      "description", null
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "locations"
                                      "description", null
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
                                      "name", upcast "name"
                                      "description", null
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
                                      "name", upcast "onField"
                                      "description", null
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
                                      "name", upcast "onFragment"
                                      "description", null
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
                                      "name", upcast "onOperation"
                                      "description", null
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "Boolean"
                                                      "ofType", null]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];]
                         "inputFields", null
                         "interfaces", upcast []
                         "enumValues", null
                         "possibleTypes", null];
                    upcast NameValueLookup.ofList [
                         "kind", upcast "OBJECT"
                         "name", upcast "__InputValue"
                         "description", upcast "Arguments provided to Fields or Directives and the input fields of an InputObject are represented as Input Values which describe their type and optionally a default value."
                         "fields", upcast [
                                 box <| NameValueLookup.ofList [
                                      "name", upcast "defaultValue"
                                      "description", null
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "description"
                                      "description", null
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "name"
                                      "description", null
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
                                      "name", upcast "type"
                                      "description", null
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "OBJECT"
                                                      "name", upcast "__Type"
                                                      "ofType", null]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];]
                         "inputFields", null
                         "interfaces", upcast []
                         "enumValues", null
                         "possibleTypes", null];
                    upcast NameValueLookup.ofList [
                         "kind", upcast "OBJECT"
                         "name", upcast "__Type"
                         "description", upcast "The fundamental unit of any GraphQL Schema is the type. There are many kinds of types in GraphQL as represented by the `__TypeKind` enum. Depending on the kind of a type, certain fields describe information about that type. Scalar types provide no information beyond a name and description, while Enum types provide their values. Object and Interface types provide the fields they describe. Abstract types, Union and Interface, provide the Object types possible at runtime. List and NonNull types compose other types."
                         "fields", upcast [
                                 box <| NameValueLookup.ofList [
                                      "name", upcast "description"
                                      "description", null
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "enumValues"
                                      "description", null
                                      "args", upcast [
                                              box <| NameValueLookup.ofList [
                                                   "name", upcast "includeDeprecated"
                                                   "description", null
                                                   "type", upcast NameValueLookup.ofList [
                                                     "kind", upcast "SCALAR"
                                                     "name", upcast "Boolean"
                                                     "ofType", null]
                                                   "defaultValue", upcast "False"];]
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
                                      "name", upcast "fields"
                                      "description", null
                                      "args", upcast [
                                              box <| NameValueLookup.ofList [
                                                   "name", upcast "includeDeprecated"
                                                   "description", null
                                                   "type", upcast NameValueLookup.ofList [
                                                     "kind", upcast "SCALAR"
                                                     "name", upcast "Boolean"
                                                     "ofType", null]
                                                   "defaultValue", upcast "False"];]
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
                                      "name", upcast "inputFields"
                                      "description", null
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
                                      "name", upcast "interfaces"
                                      "description", null
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
                                      "name", upcast "kind"
                                      "description", null
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
                                      "description", null
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "ofType"
                                      "description", null
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "OBJECT"
                                              "name", upcast "__Type"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "possibleTypes"
                                      "description", null
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
                                      "deprecationReason", null];]
                         "inputFields", null
                         "interfaces", upcast []
                         "enumValues", null
                         "possibleTypes", null];
                    upcast NameValueLookup.ofList [
                         "kind", upcast "OBJECT"
                         "name", upcast "__EnumValue"
                         "description", upcast "One possible value for a given Enum. Enum values are unique values, not a placeholder for a string or numeric value. However an Enum value is returned in a JSON response as a string."
                         "fields", upcast [
                                 box <| NameValueLookup.ofList [
                                      "name", upcast "deprecationReason"
                                      "description", null
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "description"
                                      "description", null
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "isDeprecated"
                                      "description", null
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
                                      "name", upcast "name"
                                      "description", null
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "String"
                                                      "ofType", null]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];]
                         "inputFields", null
                         "interfaces", upcast []
                         "enumValues", null
                         "possibleTypes", null];
                    upcast NameValueLookup.ofList [
                         "kind", upcast "OBJECT"
                         "name", upcast "__Field"
                         "description", upcast "Object and Interface types are described by a list of Fields, each of which has a name, potentially a list of arguments, and a return type."
                         "fields", upcast [
                                 box <| NameValueLookup.ofList [
                                      "name", upcast "args"
                                      "description", null
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
                                      "name", upcast "deprecationReason"
                                      "description", null
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "description"
                                      "description", null
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "SCALAR"
                                              "name", upcast "String"
                                              "ofType", null]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "isDeprecated"
                                      "description", null
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
                                      "name", upcast "name"
                                      "description", null
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
                                      "name", upcast "type"
                                      "description", null
                                      "args", upcast []
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "OBJECT"
                                                      "name", upcast "__Type"
                                                      "ofType", null]]
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];]
                         "inputFields", null
                         "interfaces", upcast []
                         "enumValues", null
                         "possibleTypes", null];
                    upcast NameValueLookup.ofList [
                         "kind", upcast "ENUM"
                         "name", upcast "__TypeKind"
                         "description", upcast "An enum describing what kind of type a given __Type is."
                         "fields", null
                         "inputFields", null
                         "interfaces", null
                         "enumValues", upcast [
                                 box <| NameValueLookup.ofList [
                                      "name", upcast "SCALAR"
                                      "description", upcast "Indicates this type is a scalar."
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "OBJECT"
                                      "description", upcast "Indicates this type is an object. `fields` and `interfaces` are valid fields."
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "INTERFACE"
                                      "description", upcast "Indicates this type is an interface. `fields` and `possibleTypes` are valid fields."
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "UNION"
                                      "description", upcast "Indicates this type is a union. `possibleTypes` is a valid field."
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "ENUM"
                                      "description", upcast "Indicates this type is an enum. `enumValues` is a valid field."
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "INPUT_OBJECT"
                                      "description", upcast "Indicates this type is an input object. `inputFields` is a valid field."
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "LIST"
                                      "description", upcast "Indicates this type is a list. `ofType` is a valid field."
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "NON_NULL"
                                      "description", upcast "Indicates this type is a non-null. `ofType` is a valid field."
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];]
                         "possibleTypes", null];
                    upcast NameValueLookup.ofList [
                         "kind", upcast "ENUM"
                         "name", upcast "__DirectiveLocation"
                         "description", upcast "A Directive can be adjacent to many parts of the GraphQL language, a __DirectiveLocation describes one such possible adjacencies."
                         "fields", null
                         "inputFields", null
                         "interfaces", null
                         "enumValues", upcast [
                                 box <| NameValueLookup.ofList [
                                      "name", upcast "QUERY"
                                      "description", upcast "Location adjacent to a query operation."
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "MUTATION"
                                      "description", upcast "Location adjacent to a mutation operation."
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "SUBSCRIPTION"
                                      "description", upcast "Location adjacent to a subscription operation."
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "FIELD"
                                      "description", upcast "Location adjacent to a field."
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "FRAGMENT_DEFINITION"
                                      "description", upcast "Location adjacent to a fragment definition."
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "FRAGMENT_SPREAD"
                                      "description", upcast "Location adjacent to a fragment spread."
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "INLINE_FRAGMENT"
                                      "description", upcast "Location adjacent to an inline fragment."
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "SCHEMA"
                                      "description", upcast "Location adjacent to a schema IDL definition."
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "SCALAR"
                                      "description", upcast "Location adjacent to a scalar IDL definition."
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "OBJECT"
                                      "description", upcast "Location adjacent to an object IDL definition."
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "FIELD_DEFINITION"
                                      "description", upcast "Location adjacent to a field IDL definition."
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "ARGUMENT_DEFINITION"
                                      "description", upcast "Location adjacent to a field argument IDL definition."
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "INTERFACE"
                                      "description", upcast "Location adjacent to an interface IDL definition."
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "UNION"
                                      "description", upcast "Location adjacent to an union IDL definition."
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "ENUM"
                                      "description", upcast "Location adjacent to an enum IDL definition."
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "ENUM_VALUE"
                                      "description", upcast "Location adjacent to an enum value definition."
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "INPUT_OBJECT"
                                      "description", upcast "Location adjacent to an input object IDL definition."
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];
                                 upcast NameValueLookup.ofList [
                                      "name", upcast "INPUT_FIELD_DEFINITION"
                                      "description", upcast "Location adjacent to an input object field IDL definition."
                                      "isDeprecated", upcast false
                                      "deprecationReason", null];]
                         "possibleTypes", null];
                    upcast NameValueLookup.ofList [
                        "kind", upcast "OBJECT"
                        "name", upcast "QueryRoot"
                        "description", null
                        "fields", upcast [
                            box <| NameValueLookup.ofList [
                                "name", upcast "onlyField"
                                "description", null
                                "args", upcast []
                                "type", upcast NameValueLookup.ofList [
                                    "kind", upcast "NON_NULL"
                                    "name", null
                                    "ofType", upcast NameValueLookup.ofList [
                                        "kind", upcast "SCALAR"
                                        "name", upcast "String"
                                        "ofType", null]]
                                "isDeprecated", upcast false
                                "deprecationReason", null];]
                        "inputFields", null
                        "interfaces", upcast []
                        "enumValues", null
                        "possibleTypes", null];]
            "directives", upcast [
                    box <| NameValueLookup.ofList [
                         "name", upcast "include"
                         "description", upcast "Directs the executor to include this field or fragment only when the `if` argument is true."
                         "locations", upcast [
                                 box <| "FIELD";
                                 upcast "FRAGMENT_SPREAD";
                                 upcast "INLINE_FRAGMENT";]
                         "args", upcast [
                                 box <| NameValueLookup.ofList [
                                      "name", upcast "if"
                                      "description", upcast "Included when true."
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "Boolean"
                                                      "ofType", null]]
                                      "defaultValue", null];]];
                    upcast NameValueLookup.ofList [
                         "name", upcast "skip"
                         "description", upcast "Directs the executor to skip this field or fragment when the `if` argument is true."
                         "locations", upcast [
                                 box <| "FIELD";
                                 upcast "FRAGMENT_SPREAD";
                                 upcast "INLINE_FRAGMENT";]
                         "args", upcast [
                                 box <| NameValueLookup.ofList [
                                      "name", upcast "if"
                                      "description", upcast "Skipped when true."
                                      "type", upcast NameValueLookup.ofList [
                                              "kind", upcast "NON_NULL"
                                              "name", null
                                              "ofType", upcast NameValueLookup.ofList [
                                                      "kind", upcast "SCALAR"
                                                      "name", upcast "Boolean"
                                                      "ofType", null]]
                                      "defaultValue", null];]];
                    upcast NameValueLookup.ofList [
                        "name", upcast "defer"
                        "description", upcast "Defers the resolution of this field or fragment"
                        "locations", upcast [
                            box <| "FIELD";
                            upcast "FRAGMENT_DEFINITION";
                            upcast "FRAGMENT_SPREAD";
                            upcast "INLINE_FRAGMENT";]
                        "args", upcast []]
                    upcast NameValueLookup.ofList [
                        "name", upcast "stream"
                        "description", upcast "Streams the resolution of this field or fragment"
                        "locations", upcast [
                            box <| "FIELD";
                            upcast "FRAGMENT_DEFINITION";
                            upcast "FRAGMENT_SPREAD";
                            upcast "INLINE_FRAGMENT";]
                        "args", upcast []]
                    upcast NameValueLookup.ofList [
                        "name", upcast "live"
                        "description", upcast "Subscribes for live updates of this field or fragment"
                        "locations", upcast [
                            box <| "FIELD";
                            upcast "FRAGMENT_DEFINITION";
                            upcast "FRAGMENT_SPREAD";
                            upcast "INLINE_FRAGMENT";]
                        "args", upcast []]]]]
    match result with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"
