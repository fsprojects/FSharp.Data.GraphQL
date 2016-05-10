/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.VariablesTests

open System
open Xunit
open FsCheck
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution

let TestComplexScalar = Define.Scalar(
    name = "ComplexScalar",
    coerceInput = (fun (StringValue value) -> if value = "SerializedValue" then Some "DeserializedValue" else None),
    coerceOutput = (fun value -> if value = "DeserializedValue" then Some (StringValue "SerializedValue") else None),
    coerceValue = (fun value -> if value = upcast "DeserializedValue" then Some "SerializedValue" else None))

let TestInputObject = Define.InputObject<Map<string, obj>>(
    name = "TestInputObject",
    fields = [
        Define.Input("a", Nullable String)
        Define.Input("b", Nullable(ListOf (Nullable String)))
        Define.Input("c", String)
        Define.Input("d", Nullable TestComplexScalar)
    ])

let TestNestedInputObject = Define.InputObject<Map<string, obj>>(
    name = "TestNestedInputObject",
    fields = [
        Define.Input("na", TestInputObject)
        Define.Input("nb", String)
    ])

let stringifyArg name (ctx: ResolveFieldContext) () =
    match ctx.Arg name |> Option.toObj with
    | null -> null
    | other -> other.ToString()

let stringifyInput = stringifyArg "input"

let TestType = Define.Object(
    name = "TestType",
    fields = [
        Define.Field("fieldWithObjectInput", String, "", [ Define.Input("input", Nullable TestInputObject) ], stringifyInput)
        Define.Field("fieldWithNullableStringInput", String, "", [ Define.Input("input", Nullable String) ], stringifyInput)
        Define.Field("fieldWithNonNullableStringInput", String, "", [ Define.Input("input", String) ], stringifyInput)
        Define.Field("fieldWithDefaultArgumentValue", String, "", [ Define.Input("input", Nullable String, Some "hello world") ], stringifyInput)
        Define.Field("fieldWithNestedInputObject", String, "", [ Define.Input("input", TestNestedInputObject, Map.ofList ["", upcast "hello world"]) ], stringifyInput)
        Define.Field("list", String, "", [ Define.Input("input", Nullable(ListOf (Nullable String))) ], stringifyInput)
        Define.Field("nnList", String, "", [ Define.Input("input", ListOf (Nullable String)) ], stringifyInput)
        Define.Field("listNN", String, "", [ Define.Input("input", Nullable (ListOf String)) ], stringifyInput)
        Define.Field("nnListNN", String, "", [ Define.Input("input", ListOf String) ], stringifyInput)
    ])

let schema = Schema(TestType)

[<Fact>]
let ``Execute handles objects and nullability using inline structs with complex input`` () =
    let ast = parse """{ fieldWithObjectInput(input: {a: "foo", b: ["bar"], c: "baz"}) }"""
    let actual = sync <| schema.AsyncExecute(ast)
    let expected: Map<string, obj> = Map.ofList [ "fieldWithObjectInput", upcast Map.ofList [
        "a", "foo" :> obj
        "b", upcast ["bar"]
        "c", upcast "baz" ]]
    noErrors actual
    equals expected actual.Data.Value
    
[<Fact>]
let ``Execute handles objects and nullability using inline structs and properly parses single value to list`` () =
    let ast = parse """{ fieldWithObjectInput(input: {a: "foo", b: "bar", c: "baz"}) }"""
    let actual = sync <| schema.AsyncExecute(ast)
    let expected: Map<string, obj> = Map.ofList [ "fieldWithObjectInput", upcast Map.ofList [
        "a", "foo" :> obj
        "b", upcast ["bar"]
        "c", upcast "baz" ]]
    noErrors actual
    equals expected actual.Data.Value
    
[<Fact>]
let ``Execute handles objects and nullability using inline structs and doesn't use incorrect value`` () =
    let ast = parse """{ fieldWithObjectInput(input: ["foo", "bar", "baz"]) }"""
    let actual = sync <| schema.AsyncExecute(ast)
    let expected: Map<string, obj> = Map.ofList [ "fieldWithObjectInput", null ]
    noErrors actual
    equals expected actual.Data.Value
    
[<Fact>]
let ``Execute handles objects and nullability using inline structs and proprely coerces complex scalar types`` () =
    let ast = parse """{ fieldWithObjectInput(input: {a: "foo", d: "SerializedValue"}) }"""
    let actual = sync <| schema.AsyncExecute(ast)
    let expected: Map<string, obj> = Map.ofList [ "fieldWithObjectInput", upcast Map.ofList [
        "a", "foo" :> obj
        "d", upcast "DeserializedValue" ]]
    noErrors actual
    equals expected actual.Data.Value
    
[<Fact>]
let ``Execute handles variables with complex inputs`` () =
    let ast = parse """query q($input: TestInputObject) {
          fieldWithObjectInput(input: $input)
        }"""
    let params: Map<string, obj> = Map.ofList [
        "input", upcast Map.ofList [
            "a", "foo" :> obj
            "b", upcast ["bar"]
            "c", upcast "baz"]]
    let actual = sync <| schema.AsyncExecute(ast, variables = params)
    let expected: Map<string, obj> = Map.ofList [ "fieldWithObjectInput", upcast Map.ofList [
        "a", "foo" :> obj
        "b", upcast ["bar"]
        "c", upcast "baz" ]]
    noErrors actual
    equals expected actual.Data.Value
    
[<Fact>]
let ``Execute handles variables with default value when no value was provided`` () =
    let ast = parse """query q($input: TestInputObject = {a: "foo", b: ["bar"], c: "baz"}) {
            fieldWithObjectInput(input: $input)
          }"""
    let actual = sync <| schema.AsyncExecute(ast)
    let expected: Map<string, obj> = Map.ofList [ "fieldWithObjectInput", upcast Map.ofList [
        "a", "foo" :> obj
        "b", upcast ["bar"]
        "c", upcast "baz" ]]
    noErrors actual
    equals expected actual.Data.Value
    
[<Fact>]
let ``Execute handles variables and properly parses single value to list`` () =
    let ast = parse """query q($input: TestInputObject) {
          fieldWithObjectInput(input: $input)
        }"""
    let params: Map<string, obj> = Map.ofList [
        "input", upcast Map.ofList [
            "a", "foo" :> obj
            "b", upcast "bar"
            "c", upcast "baz"]]
    let actual = sync <| schema.AsyncExecute(ast, variables = params)
    let expected: Map<string, obj> = Map.ofList [ "fieldWithObjectInput", upcast Map.ofList [
        "a", "foo" :> obj
        "b", upcast ["bar"]
        "c", upcast "baz" ]]
    noErrors actual
    equals expected actual.Data.Value
    
[<Fact>]
let ``Execute handles variables with complex scalar input`` () =
    let ast = parse """query q($input: TestInputObject) {
          fieldWithObjectInput(input: $input)
        }"""
    let params: Map<string, obj> = Map.ofList [
        "input", upcast Map.ofList [
            "a", "foo" :> obj
            "d", upcast "SerializedValue" ]]
    let actual = sync <| schema.AsyncExecute(ast, variables = params)
    let expected: Map<string, obj> = Map.ofList [ "fieldWithObjectInput", upcast Map.ofList [
        "a", "foo" :> obj
        "d", upcast "DeserializedValue" ]]
    noErrors actual
    equals expected actual.Data.Value

[<Fact>]
let ``Execute handles variables and errors on null for nested non-nulls`` () =
    let ast = parse """query q($input: TestInputObject) {
          fieldWithObjectInput(input: $input)
        }"""
    let params: Map<string, obj> = Map.ofList [
        "input", upcast Map.ofList [
            "a", "foo" :> obj
            "b", upcast ["bar"]
            "c", null]]
    let actual = sync <| schema.AsyncExecute(ast, variables = params)
    let errMsg = "Variable '$input' got invalid value: in field 'c': Expected String!, but got null"
    hasError errMsg actual.Errors.Value
    
[<Fact>]
let ``Execute handles variables and errors on incorrect type`` () =
    let ast = parse """query q($input: TestInputObject) {
          fieldWithObjectInput(input: $input)
        }"""
    let params: Map<string, obj> = Map.ofList ["input", upcast "foo bar"]
    let actual = sync <| schema.AsyncExecute(ast, variables = params)
    let errMsg = "Variable '$input' got invalid value: expected 'TestInputObject', found not an object"
    hasError errMsg actual.Errors.Value
    
[<Fact>]
let ``Execute handles variables and errors on omission of nested non-nulls`` () =
    let ast = parse """query q($input: TestInputObject) {
          fieldWithObjectInput(input: $input)
        }"""
    let params: Map<string, obj> = Map.ofList [
        "input", upcast Map.ofList [
            "a", "foo" :> obj
            "b", upcast ["bar"]]]
    let actual = sync <| schema.AsyncExecute(ast, variables = params)
    equals 1 actual.Errors.Value.Length
    let errMsg = "Variable '$input' got invalid value: in field 'c': Expected String!, but got null"
    equals (GraphQLError errMsg) (actual.Errors.Value.[0])
    
[<Fact>]
let ``Execute handles variables and errors on deep nested errors and with many errors`` () =
    let ast = parse """query q($input: TestNestedInputObject) {
            fieldWithNestedObjectInput(input: $input)
          }"""
    let params: Map<string, obj> = Map.ofList [
        "input", upcast Map.ofList [ "na", Map.ofList [ "a", "foo" :> obj ] :> obj ]]
    let actual = sync <| schema.AsyncExecute(ast, variables = params)
    equals 1 actual.Errors.Value.Length
    let errMsg = "Variable '$input' got invalid value: in field 'na.c': Expected String!, but got null. in field 'nb': Expected String!, but got null."
    equals (GraphQLError errMsg) (actual.Errors.Value.[0])
    
[<Fact>]
let ``Execute handles variables and errors on addition of unknown input field`` () =
    let ast = parse """query q($input: TestInputObject) {
          fieldWithObjectInput(input: $input)
        }"""
    let params: Map<string, obj> = Map.ofList [
        "input", upcast Map.ofList [
            "a", "foo" :> obj
            "b", upcast "bar"
            "c", upcast "baz"]]
    let actual = sync <| schema.AsyncExecute(ast, variables = params)
    let expected: Map<string, obj> = Map.ofList [ "fieldWithObjectInput", upcast Map.ofList [
        "a", "foo" :> obj
        "b", upcast ["bar"]
        "c", upcast "baz" 
        "extra", upcast "dog"]]
    equals 1 actual.Errors.Value.Length
    let errMsg = "Variable '$input' got invalid value: in field 'extra': unknown field."
    equals (GraphQLError errMsg) (actual.Errors.Value.[0])

[<Fact>]
let ``Execute handles variables and allows nullable inputs to be omitted`` () =
    let ast = parse """{ fieldWithNullableStringInput }"""
    let actual = sync <| schema.AsyncExecute(ast)
    let expected: Map<string, obj> = Map.ofList [ "fieldWithNullableStringInput", null ]
    noErrors actual
    equals expected actual.Data.Value
    
[<Fact>]
let ``Execute handles variables and allows nullable inputs to be omitted in a variable`` () =
    let ast = parse """query SetsNullable($value: String) {
        fieldWithNullableStringInput(input: $value)
      }"""
    let actual = sync <| schema.AsyncExecute(ast)
    let expected: Map<string, obj> = Map.ofList [ "fieldWithNullableStringInput", null ]
    noErrors actual
    equals expected actual.Data.Value
    
[<Fact>]
let ``Execute handles variables and allows nullable inputs to be omitted in an unlisted variable`` () =
    let ast = parse """query SetsNullable {
        fieldWithNullableStringInput(input: $value)
      }"""
    let actual = sync <| schema.AsyncExecute(ast)
    let expected: Map<string, obj> = Map.ofList [ "fieldWithNullableStringInput", null ]
    noErrors actual
    equals expected actual.Data.Value
    
[<Fact>]
let ``Execute handles variables and allows nullable inputs to be set to null in a variable`` () =
    let ast = parse """query SetsNullable($value: String) {
        fieldWithNullableStringInput(input: $value)
      }"""
    let actual = sync <| schema.AsyncExecute(ast, variables = Map.ofList [ "value", null ])
    let expected: Map<string, obj> = Map.ofList [ "fieldWithNullableStringInput", null ]
    noErrors actual
    equals expected actual.Data.Value
    
[<Fact>]
let ``Execute handles variables and allows nullable inputs to be set to a value in a variable`` () =
    let ast = parse """query SetsNullable($value: String) {
        fieldWithNullableStringInput(input: $value)
      }"""
    let actual = sync <| schema.AsyncExecute(ast, variables = Map.ofList [ "value", "a" :> obj ])
    let expected: Map<string, obj> = Map.ofList [ "fieldWithNullableStringInput", upcast "a" ]
    noErrors actual
    equals expected actual.Data.Value
    
[<Fact>]
let ``Execute handles variables and allows nullable inputs to be set to a value directly`` () =
    let ast = parse """{ fieldWithNullableStringInput(input: "a") }"""
    let actual = sync <| schema.AsyncExecute(ast)
    let expected: Map<string, obj> = Map.ofList [ "fieldWithNullableStringInput", upcast "a" ]
    noErrors actual
    equals expected actual.Data.Value

[<Fact>]
let ``Execute handles non-nullable scalars and does not allow non-nullable inputs to be omitted in a variable`` () =
    let ast = parse """query SetsNonNullable($value: String!) {
          fieldWithNonNullableStringInput(input: $value)
        }"""
    let actual = sync <| schema.AsyncExecute(ast, variables = Map.ofList [ "value", null ])
    hasError "Variable '$value' of required type String! was not provided." actual.Errors.Value
    
[<Fact>]
let ``Execute handles non-nullable scalars and allows non-nullable inputs to be set to a value in a variable`` () =
    let ast = parse """query SetsNonNullable($value: String!) {
          fieldWithNonNullableStringInput(input: $value)
        }"""
    let actual = sync <| schema.AsyncExecute(ast, variables = Map.ofList [ "value", "a" :> obj ])
    let expected: Map<string, obj> = Map.ofList [ "fieldWithNonNullableStringInput", upcast "a" ]
    noErrors actual
    equals expected actual.Data.Value
    
[<Fact>]
let ``Execute handles non-nullable scalars and allows non-nullable inputs to be set to a value directly`` () =
    let ast = parse """{ fieldWithNonNullableStringInput(input: "a") }"""
    let actual = sync <| schema.AsyncExecute(ast)
    let expected: Map<string, obj> = Map.ofList [ "fieldWithNonNullableStringInput", upcast "a" ]
    noErrors actual
    equals expected actual.Data.Value
    
[<Fact>]
let ``Execute handles non-nullable scalars and passes along null for non-nullable inputs if explcitly set in the query`` () =
    let ast = parse """{ fieldWithNonNullableStringInput }"""
    let actual = sync <| schema.AsyncExecute(ast)
    let expected: Map<string, obj> = Map.ofList [ "fieldWithNonNullableStringInput", null ]
    noErrors actual
    equals expected actual.Data.Value
    
[<Fact>]
let ``Execute handles list inputs and nullability and allows lists to be null`` () =
    let ast = parse """query q($input: [String]) {
          list(input: $input)
        }"""
    let actual = sync <| schema.AsyncExecute(ast, variables = Map.ofList [ "input", null ])
    let expected: Map<string, obj> = Map.ofList [ "list", null ]
    noErrors actual
    equals expected actual.Data.Value
    
[<Fact>]
let ``Execute handles list inputs and nullability and allows lists to contain values`` () =
    let ast = parse """query q($input: [String]) {
          list(input: $input)
        }"""
    let actual = sync <| schema.AsyncExecute(ast, variables = Map.ofList [ "input", [ "A" ] :> obj ])
    let expected: Map<string, obj> = Map.ofList [ "list", upcast [ "A" ]]
    noErrors actual
    equals expected actual.Data.Value
    
[<Fact>]
let ``Execute handles list inputs and nullability and allows lists to contain null`` () =
    let ast = parse """query q($input: [String]) {
          list(input: $input)
        }"""
    let actual = sync <| schema.AsyncExecute(ast, variables = Map.ofList [ "input", [ "A":> obj, null, "B" :> obj ] :> obj ])
    let expected: Map<string, obj> = Map.ofList [ "list", upcast [ "A":> obj, null, "B" :> obj ]]
    noErrors actual
    equals expected actual.Data.Value
    
[<Fact>]
let ``Execute handles list inputs and nullability and does not allow non-null lists to be null`` () =
    let ast = parse """query q($input: [String]!) {
          nnList(input: $input)
        }"""
    let actual = sync <| schema.AsyncExecute(ast, variables = Map.ofList [ "input", null ])
    hasError "Variable '$input' of required type [String]! was not provided." actual.Errors.Value
    
[<Fact>]
let ``Execute handles list inputs and nullability and allows non-null lists to contain values`` () =
    let ast = parse """query q($input: [String]!) {
          nnList(input: $input)
        }"""
    let actual = sync <| schema.AsyncExecute(ast, variables = Map.ofList [ "input", [ "A" ] :> obj ])
    let expected: Map<string, obj> = Map.ofList [ "nnList", upcast [ "A" ]]
    noErrors actual
    equals expected actual.Data.Value
    
[<Fact>]
let ``Execute handles list inputs and nullability and allows non-null lists to contain null`` () =
    let ast = parse """query q($input: [String]!) {
          nnList(input: $input)
        }"""
    let actual = sync <| schema.AsyncExecute(ast, variables = Map.ofList [ "input", [ "A":> obj, null, "B" :> obj ] :> obj ])
    let expected: Map<string, obj> = Map.ofList [ "nnList", upcast [ "A":> obj, null, "B" :> obj ]]
    noErrors actual
    equals expected actual.Data.Value
    
[<Fact>]
let ``Execute handles list inputs and nullability and allows lists of non-nulls to be null`` () =
    let ast = parse """query q($input: [String!]) {
          listNN(input: $input)
        }"""
    let actual = sync <| schema.AsyncExecute(ast, variables = Map.ofList [ "input", null ])
    let expected: Map<string, obj> = Map.ofList [ "listNN", null ]
    noErrors actual
    equals expected actual.Data.Value
    
[<Fact>]
let ``Execute handles list inputs and nullability and allows lists of non-nulls to contain values`` () =
    let ast = parse """query q($input: [String!]) {
          listNN(input: $input)
        }"""
    let actual = sync <| schema.AsyncExecute(ast, variables = Map.ofList [ "input", [ "A" ] :> obj ])
    let expected: Map<string, obj> = Map.ofList [ "listNN", upcast [ "A" ]]
    noErrors actual
    equals expected actual.Data.Value
    
[<Fact>]
let ``Execute handles list inputs and nullability and does not allow lists of non-nulls to contain null`` () =
    let ast = parse """query q($input: [String!]) {
          listNN(input: $input)
        }"""
    let actual = sync <| schema.AsyncExecute(ast, variables = Map.ofList [ "input", [ "A":> obj, null, "B" :> obj ] :> obj ])
    hasError "Variable '$input' got invalid value. In element #1 expected String!, but got null." actual.Errors.Value
    
[<Fact>]
let ``Execute handles list inputs and nullability and does not allow non-null lists of non-nulls to be null`` () =
    let ast = parse """query q($input: [String!]!) {
          nnListNN(input: $input)
        }"""
    let actual = sync <| schema.AsyncExecute(ast, variables = Map.ofList [ "input", null ])
    hasError "Variable '$input' of required type [String!]! was not provided." actual.Errors.Value
    
[<Fact>]
let ``Execute handles list inputs and nullability and does not allow non-null lists of non-nulls to contain values`` () =
    let ast = parse """query q($input: [String!]!) {
          nnListNN(input: $input)
        }"""
    let actual = sync <| schema.AsyncExecute(ast, variables = Map.ofList [ "input", [ "A" ] :> obj ])
    let expected: Map<string, obj> = Map.ofList [ "nnListNN", upcast [ "A" ]]
    noErrors actual
    equals expected actual.Data.Value
    
[<Fact>]
let ``Execute handles list inputs and nullability and does not allow non-null lists of non-nulls to contain null`` () =
    let ast = parse """query q($input: [String!]!) {
          nnListNN(input: $input)
        }"""
    let actual = sync <| schema.AsyncExecute(ast, variables = Map.ofList [ "input", [ "A":> obj, null, "B" :> obj ] :> obj ])
    hasError "Variable '$input' got invalid value. In element #1 expected String!, but got null." actual.Errors.Value
    
[<Fact>]
let ``Execute handles list inputs and nullability and does not allow invalid types to be used as values`` () =
    let ast = parse """query q($input: TestType!) {
          fieldWithObjectInput(input: $input)
        }"""
    let actual = sync <| schema.AsyncExecute(ast, variables = Map.ofList [ "input", [ "A":> obj, "B" :> obj ] :> obj ])
    hasError "Variable '$input' expected value of type TestType! which cannot be used as an input type" actual.Errors.Value
    
[<Fact>]
let ``Execute handles list inputs and nullability and does not allow unknown types to be used as values`` () =
    let ast = parse """query q($input: UnknownType!) {
          fieldWithObjectInput(input: $input)
        }"""
    let actual = sync <| schema.AsyncExecute(ast, variables = Map.ofList [ "input", "whoknows" :> obj ])
    hasError "Variable '$input' expected value of type UnknownType! which cannot be used as an input type" actual.Errors.Value
    
[<Fact>]
let ``Execute uses argument default value when no argument was provided`` () =
    let ast = parse """{ fieldWithDefaultArgumentValue }"""
    let actual = sync <| schema.AsyncExecute(ast)
    let expected: Map<string, obj> = Map.ofList [ "fieldWithDefaultArgumentValue", upcast "hello world" ]
    noErrors actual
    equals expected actual.Data.Value
    
[<Fact>]
let ``Execute uses argument default value when nullable variable provided`` () =
    let ast = parse """query optionalVariable($optional: String) {
        fieldWithDefaultArgumentValue(input: $optional)
      }"""
    let actual = sync <| schema.AsyncExecute(ast, variables = Map.ofList ["optional", null ])
    let expected: Map<string, obj> = Map.ofList [ "fieldWithDefaultArgumentValue", upcast "hello world" ]
    noErrors actual
    equals expected actual.Data.Value
    
[<Fact>]
let ``Execute uses argument default value when argument provided cannot be parsed`` () =
    let ast = parse """{ fieldWithDefaultArgumentValue(input: WRONG_TYPE) }"""
    let actual = sync <| schema.AsyncExecute(ast)
    let expected: Map<string, obj> = Map.ofList [ "fieldWithDefaultArgumentValue", upcast "hello world" ]
    noErrors actual
    equals expected actual.Data.Value