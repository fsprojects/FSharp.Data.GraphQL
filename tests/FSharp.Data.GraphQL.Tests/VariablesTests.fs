// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.VariablesTests

open System.Collections.Immutable
open System.Text.Json

#nowarn "25"

open System
open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Samples.StarWarsApi

let TestComplexScalar =
  Define.Scalar(
    name = "ComplexScalar",
    coerceInput =
        (fun i ->
         let value =
             match i with
             | Variable e -> e.GetString()
             | InlineConstant (StringValue s) -> s
         if value = "SerializedValue" then Some "DeserializedValue" else None),
    coerceOutput = (fun value -> if value = upcast "DeserializedValue" then Some "SerializedValue" else None))

type TestInput = {
    a: string option
    b: string option seq option
    c: string
    d: string option
    e: string option array option
}

let InputArrayOf(innerDef : #TypeDef<'Val>) : ListOfDef<'Val, 'Val array> =
  ListOf innerDef

let TestInputObject =
  Define.InputObject<TestInput>(
    name = "TestInputObject",
    fields = [
        Define.Input("a", Nullable StringType)
        Define.Input("b", Nullable (ListOf (Nullable StringType)))
        Define.Input("c", StringType)
        Define.Input("d", Nullable TestComplexScalar)
        Define.Input("e", Nullable (InputArrayOf (Nullable StringType)))
    ])

type TestNestedInput = {
    na: TestInput option
    nb: string
}

let TestNestedInputObject =
  Define.InputObject<TestNestedInput>(
    name = "TestNestedInputObject",
    fields = [
        Define.Input("na", Nullable TestInputObject)
        Define.Input("nb", StringType)
    ])

type TestRecusiveInput = {
    ra: TestRecusiveInput option
    rb: string
}

#nowarn "40"
let rec TestRecursiveInputObject =
  Define.InputObject<TestRecusiveInput>(
    name = "TestRecusiveInput",
    fieldsFn =
        fun () -> [
            Define.Input("ra", Nullable TestRecursiveInputObject)
            Define.Input("rb", StringType)]
    )

let stringifyArg name (ctx: ResolveFieldContext) () =
    let arg = ctx.TryArg name |> Option.toObj
    JsonSerializer.Serialize(arg, Json.serializerOptions)

let stringifyInput = stringifyArg "input"

type EnumTestType = Foo | Bar

let EnumTestType =
  Define.Enum
    ("EnumTestType"
      , [ Define.EnumValue("Foo", EnumTestType.Foo)
          Define.EnumValue("Bar", EnumTestType.Bar)
        ]
      , "Test enum"
    )

let TestType =
  Define.Object<unit>(
    name = "TestType",
    fields = [
        Define.Field("fieldWithObjectInput", StringType, "", [ Define.Input("input", Nullable TestInputObject) ], stringifyInput)
        Define.Field("fieldWithNullableStringInput", StringType, "", [ Define.Input("input", Nullable StringType) ], stringifyInput)
        Define.Field("fieldWithNonNullableStringInput", StringType, "", [ Define.Input("input", StringType) ], stringifyInput)
        Define.Field("fieldWithDefaultArgumentValue", StringType, "", [ Define.Input("input", Nullable StringType, Some "hello world") ], stringifyInput)
        Define.Field("fieldWithNestedInputObject", StringType, "", [ Define.Input("input", TestNestedInputObject, { na = None; nb = "hello world"}) ], stringifyInput)
        Define.Field("fieldWithRecursiveInputObject", StringType, "", [ Define.Input("input", TestRecursiveInputObject, { ra = None; rb = "hello world"}) ], stringifyInput)
        Define.Field("fieldWithEnumInput", StringType, "", [ Define.Input("input", EnumTestType) ], stringifyInput)
        Define.Field("fieldWithNullableEnumInput", StringType, "", [ Define.Input("input", Nullable EnumTestType) ], stringifyInput)
        Define.Field("list", StringType, "", [ Define.Input("input", Nullable(ListOf (Nullable StringType))) ], stringifyInput)
        Define.Field("nnList", StringType, "", [ Define.Input("input", ListOf (Nullable StringType)) ], stringifyInput)
        Define.Field("listNN", StringType, "", [ Define.Input("input", Nullable (ListOf StringType)) ], stringifyInput)
        Define.Field("nnListNN", StringType, "", [ Define.Input("input", ListOf StringType) ], stringifyInput)
    ])

let schema = Schema(TestType)

[<Fact>]
let ``Execute handles objects and nullability using inline structs with complex input`` () =
    let ast = parse """{ fieldWithObjectInput(input: {a: "foo", b: ["bar"], c: "baz", e: ["baf"]}) }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast)
    let expected = NameValueLookup.ofList [ "fieldWithObjectInput", upcast """{"a":"foo","b":["bar"],"c":"baz","d":null,"e":["baf"]}""" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact(Skip = "This test does not pass anymore since the literal representation of the argument does not match the input type.")>]
let ``Execute handles objects and nullability using inline structs and properly parses single value to list`` () =
    let ast = parse """{ fieldWithObjectInput(input: {a: "foo", b: "bar", c: "baz"}) }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast)
    let expected = NameValueLookup.ofList [ "fieldWithObjectInput", upcast """{"a":"foo","b":["bar"],"c":"baz","d":null,"e":null}""" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact(Skip = "This test can not pass anymore since validation does not allow inputs of incorrect type.")>]
let ``Execute handles objects and nullability using inline structs and doesn't use incorrect value`` () =
    let ast = parse """{ fieldWithObjectInput(input: ["foo", "bar", "baz"]) }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast)
    let expected = NameValueLookup.ofList [ "fieldWithObjectInput", upcast "null" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles objects and nullability using inline structs and properly coerces complex scalar types`` () =
    let ast = parse """{ fieldWithObjectInput(input: {c: "foo", d: "SerializedValue"}) }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast)
    let expected = NameValueLookup.ofList [ "fieldWithObjectInput", upcast """{"a":null,"b":null,"c":"foo","d":"DeserializedValue","e":null}"""]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

let variablesWithInput inputName input = $"""{{"%s{inputName}":%s{input}}}"""
let paramsWithValueInput input =
    JsonDocument.Parse(variablesWithInput "input" input).RootElement.Deserialize<ImmutableDictionary<string, JsonElement>>(Json.serializerOptions)

let testInputObject = """{"a":"foo","b":["bar"],"c":"baz","d":null,"e":null}"""

[<Fact>]
let ``Execute handles variables with complex inputs`` () =
    let ast = parse """query q($input: TestInputObject) {
          fieldWithObjectInput(input: $input)
        }"""
    let params' = paramsWithValueInput testInputObject
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = params')
    let expected = NameValueLookup.ofList [ "fieldWithObjectInput", upcast testInputObject ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles variables with default value when no value was provided`` () =
    let ast = parse """query q($input: TestInputObject = {a: "foo", b: ["bar"], c: "baz"}) {
            fieldWithObjectInput(input: $input)
          }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast)
    let expected = NameValueLookup.ofList [ "fieldWithObjectInput", upcast testInputObject ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles variables and errors on null for nested non-nulls`` () =
    let ast = parse """query q($input: TestInputObject) {
          fieldWithObjectInput(input: $input)
        }"""
    let testInputObject = """{"a":"foo","b":["bar"],"c":null}"""
    let params' = paramsWithValueInput testInputObject
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = params')
    match actual with
    | RequestError errors ->
        hasError "Variable '$input' of type 'TestInputObject': in field 'c': expected value of type 'String' but got 'None'." errors
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles variables and errors on incorrect type`` () =
    let ast = parse """query q($input: TestInputObject) {
          fieldWithObjectInput(input: $input)
        }"""
    let testInputObject = "\"foo bar\""
    let params' = paramsWithValueInput testInputObject
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = params')
    let errMsg = $"Variable '$input' of type 'TestInputObject': expected to be '%O{JsonValueKind.Object}' but got '%O{JsonValueKind.String}'."
    match actual with
    | RequestError errors ->
        hasError errMsg errors
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles variables and errors on omission of nested non-nulls`` () =
    let ast = parse """query q($input: TestInputObject) {
          fieldWithObjectInput(input: $input)
        }"""
    let testInputObject = """{"a":"foo","b":["bar"]}"""
    let params' = paramsWithValueInput testInputObject
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = params')
    match actual with
    | RequestError errors ->
        List.length errors |> equals 1
        hasError "Variable '$input' of type 'TestInputObject': in field 'c': expected value of type 'String' but got 'None'." errors
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles variables and allows nullable inputs to be omitted`` () =
    let ast = parse """{ fieldWithNullableStringInput }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast)
    let expected = NameValueLookup.ofList [ "fieldWithNullableStringInput", upcast "null" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles variables and allows nullable inputs to be omitted in a variable`` () =
    let ast = parse """query SetsNullable($value: String) {
        fieldWithNullableStringInput(input: $value)
      }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast)
    let expected = NameValueLookup.ofList [ "fieldWithNullableStringInput", upcast "null" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact(Skip = "This test does not pass anymore, since validation requires variables to be defined in the operation.")>]
let ``Execute handles variables and allows nullable inputs to be omitted in an unlisted variable`` () =
    let ast = parse """query SetsNullable {
        fieldWithNullableStringInput(input: $value)
      }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast)
    let expected = NameValueLookup.ofList [ "fieldWithNullableStringInput", upcast "null" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles variables and allows nullable inputs to be set to null in a variable`` () =
    let ast = parse """query SetsNullable($value: String) {
        fieldWithNullableStringInput(input: $value)
      }"""
    let testInputValue = "null"
    let params' = paramsWithValueInput testInputValue
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = params')
    let expected = NameValueLookup.ofList [ "fieldWithNullableStringInput", upcast testInputValue ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles variables and allows nullable inputs to be set to a value in a variable`` () =
    let ast = parse """query SetsNullable($value: String) {
        fieldWithNullableStringInput(input: $value)
      }"""
    let paramsWithValueInput input =
        JsonDocument.Parse(variablesWithInput "value" input).RootElement.Deserialize<ImmutableDictionary<string, JsonElement>>(Json.serializerOptions)
    let testInputValue = "\"a\""
    let params' = paramsWithValueInput testInputValue
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = params')
    let expected = NameValueLookup.ofList [ "fieldWithNullableStringInput", upcast testInputValue ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles variables and allows nullable inputs to be set to a value directly`` () =
    let ast = parse """{ fieldWithNullableStringInput(input: "a") }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast)
    let expected = NameValueLookup.ofList [ "fieldWithNullableStringInput", upcast "\"a\"" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles non-nullable scalars and does not allow non-nullable inputs to be omitted in a variable`` () =
    let ast = parse """query SetsNonNullable($value: String!) {
          fieldWithNonNullableStringInput(input: $value)
        }"""

    let paramsWithValueInput input =
        JsonDocument.Parse(variablesWithInput "value" input).RootElement.Deserialize<ImmutableDictionary<string, JsonElement>>(Json.serializerOptions)
    let testInputValue = "null"
    let params' = paramsWithValueInput testInputValue
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = params')
    match actual with
    | RequestError errors ->
        hasError "Variable '$value': expected value of type 'String' but got 'None'." errors
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles non-nullable scalars and allows non-nullable inputs to be set to a value in a variable`` () =
    let ast = parse """query SetsNonNullable($value: String!) {
          fieldWithNonNullableStringInput(input: $value)
        }"""

    let paramsWithValueInput input =
        JsonDocument.Parse(variablesWithInput "value" input).RootElement.Deserialize<ImmutableDictionary<string, JsonElement>>(Json.serializerOptions)
    let testInputValue = "\"a\""
    let params' = paramsWithValueInput testInputValue
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = params')
    let expected = NameValueLookup.ofList [ "fieldWithNonNullableStringInput", upcast testInputValue ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles non-nullable scalars and allows non-nullable inputs to be set to a value directly`` () =
    let ast = parse """{ fieldWithNonNullableStringInput(input: "a") }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast)
    let expected = NameValueLookup.ofList [ "fieldWithNonNullableStringInput", upcast "\"a\"" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact(Skip = "This test can not pass anymore, since validation does not allow to omit non nullable arguments with no default value.")>]
let ``Execute handles non-nullable scalars and passes along null for non-nullable inputs if explcitly set in the query`` () =
    let ast = parse """{ fieldWithNonNullableStringInput }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast)
    let expected = NameValueLookup.ofList [ "fieldWithNonNullableStringInput", upcast "null" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles nested input objects and nullability using inline structs and properly coerces complex scalar types`` () =
    let ast = parse """{ fieldWithNestedInputObject(input: {na:{c:"c"},nb:"b"})}"""
    let actual = sync <| Executor(schema).AsyncExecute(ast)
    let expected = NameValueLookup.ofList [ "fieldWithNestedInputObject", upcast """{"na":{"a":null,"b":null,"c":"c","d":null,"e":null},"nb":"b"}""" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles recursive input objects and nullability using inline structs and properly coerces complex scalar types`` () =
    let ast = parse """{ fieldWithRecursiveInputObject(input: {ra:{rb:"bb"},rb:"b"})}"""
    let actual = sync <| Executor(schema).AsyncExecute(ast)
    let expected = NameValueLookup.ofList [ "fieldWithRecursiveInputObject", upcast """{"ra":{"ra":null,"rb":"bb"},"rb":"b"}""" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and allows lists to be null`` () =
    let ast = parse """query q($input: [String]) {
          list(input: $input)
        }"""
    let testInputValue = "null"
    let params' = paramsWithValueInput testInputValue
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = params')
    let expected = NameValueLookup.ofList [ "list", upcast testInputValue ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and allows lists to contain values`` () =
    let ast = parse """query q($input: [String]) {
          list(input: $input)
        }"""
    let testInputList = "[\"A\"]"
    let params' = paramsWithValueInput testInputList
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = params')
    let expected = NameValueLookup.ofList [ "list", upcast testInputList ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and allows lists to contain null`` () =
    let ast = parse """query q($input: [String]) {
          list(input: $input)
        }"""
    let testInputList = "[\"A\",null,\"B\"]"
    let params' = paramsWithValueInput testInputList
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = params')
    let expected = NameValueLookup.ofList [ "list", upcast testInputList ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and does not allow non-null lists to be null`` () =
    let ast = parse """query q($input: [String]!) {
          nnList(input: $input)
        }"""
    let testInputList = "null"
    let params' = paramsWithValueInput testInputList
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = params')
    match actual with
    | RequestError errors ->
        hasError "Variable '$input': expected value of type '[String]!', but no value was found." errors
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and allows non-null lists to contain values`` () =
    let ast = parse """query q($input: [String]!) {
          nnList(input: $input)
        }"""
    let testInputList = "[\"A\"]"
    let params' = paramsWithValueInput testInputList
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = params')
    let expected = NameValueLookup.ofList [ "nnList", upcast testInputList ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and allows non-null lists to contain null`` () =
    let ast = parse """query q($input: [String]!) {
          nnList(input: $input)
        }"""
    let testInputList = "[\"A\",null,\"B\"]"
    let params' = paramsWithValueInput testInputList
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = params')
    let expected = NameValueLookup.ofList [ "nnList", upcast testInputList ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and allows lists of non-nulls to be null`` () =
    let ast = parse """query q($input: [String!]) {
          listNN(input: $input)
        }"""
    let testInputList = "null"
    let params' = paramsWithValueInput testInputList
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = params')
    let expected = NameValueLookup.ofList [ "listNN", upcast testInputList ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and allows lists of non-nulls to contain values`` () =
    let ast = parse """query q($input: [String!]) {
          listNN(input: $input)
        }"""
    let testInputList = "[\"A\"]"
    let params' = paramsWithValueInput testInputList
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = params')
    let expected = NameValueLookup.ofList [ "listNN", upcast testInputList ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and does not allow lists of non-nulls to contain null`` () =
    let ast = parse """query q($input: [String!]) {
          listNN(input: $input)
        }"""
    let testInputList = "[\"A\",null,\"B\"]"
    let params' = paramsWithValueInput testInputList
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = params')
    match actual with
    | RequestError errors ->
        hasError "Variable '$input': list element expected value of type 'String' but got 'None'." errors
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and does not allow non-null lists of non-nulls to be null`` () =
    let ast = parse """query q($input: [String!]!) {
          nnListNN(input: $input)
        }"""
    let testInputList = "null"
    let params' = paramsWithValueInput testInputList
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = params')
    match actual with
    | RequestError errors ->
        hasError "Variable '$input': expected value of type '[String!]!', but no value was found." errors
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and does not allow non-null lists of non-nulls to contain values`` () =
    let ast = parse """query q($input: [String!]!) {
          nnListNN(input: $input)
        }"""
    let testInputList = "[\"A\"]"
    let params' = paramsWithValueInput testInputList
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = params')
    let expected = NameValueLookup.ofList [ "nnListNN", upcast testInputList ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and does not allow non-null lists of non-nulls to contain null`` () =
    let ast = parse """query q($input: [String!]!) {
          nnListNN(input: $input)
        }"""
    let testInputList = "[\"A\",null,\"B\"]"
    let params' = paramsWithValueInput testInputList
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = params')
    match actual with
    | RequestError errors ->
        hasError "Variable '$input': list element expected value of type 'String' but got 'None'." errors
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and does not allow invalid types to be used as values`` () =
    let ast = parse """query q($input: TestType!) {
          fieldWithObjectInput(input: $input)
        }"""
    // as that kind of an error inside of a query is guaranteed to fail in every call, we're gonna to fail noisy here
    let e = throws<MalformedQueryException> (fun () ->
        let testInputList = "[\"A\",\"B\"]"
        let params' = paramsWithValueInput testInputList
        Executor(schema).AsyncExecute(ast, variables = params')
        |> sync
        |> ignore)
    e.Message |> equals "GraphQL query defined variable '$input' of type 'TestType!' which is not an input type definition"

[<Fact>]
let ``Execute handles list inputs and nullability and does not allow unknown types to be used as values`` () =
    let ast = parse """query q($input: UnknownType!) {
          fieldWithObjectInput(input: $input)
        }"""
    // as that kind of an error inside of a query is guaranteed to fail in every call, we're gonna to fail noisy here
    let e = throws<MalformedQueryException> (fun () ->
        let testInputValue = "\"whoknows\""
        let params' = paramsWithValueInput testInputValue
        Executor(schema).AsyncExecute(ast, variables = params')
        |> sync
        |> ignore)
    e.Message |> equals "GraphQL query defined variable '$input' of type 'UnknownType!' which is not known in the current schema"

[<Fact>]
let ``Execute uses argument default value when no argument was provided`` () =
    let ast = parse """{ fieldWithDefaultArgumentValue }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast)
    let expected = NameValueLookup.ofList [ "fieldWithDefaultArgumentValue", upcast "\"hello world\"" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

let paramsWithOptionalInput input =
    JsonDocument.Parse(variablesWithInput "optional" input).RootElement.Deserialize<ImmutableDictionary<string, JsonElement>>(Json.serializerOptions)

[<Fact>]
let ``Execute uses argument default value when nullable variable provided`` () =
    let ast = parse """query optionalVariable($optional: String) {
        fieldWithDefaultArgumentValue(input: $optional)
      }"""
    let testInputValue = "\"hello world\""
    let params' = paramsWithOptionalInput testInputValue
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = params')
    let expected = NameValueLookup.ofList [ "fieldWithDefaultArgumentValue", upcast testInputValue ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact(Skip = "This test does not pass anymore since validation requires that literal representations of input types match their types.")>]
let ``Execute uses argument default value when argument provided cannot be parsed`` () =
    let ast = parse """{ fieldWithDefaultArgumentValue(input: WRONG_TYPE) }"""
    let actual = sync <| Executor(schema).AsyncExecute(ast)
    let expected = NameValueLookup.ofList [ "fieldWithDefaultArgumentValue", upcast "\"hello world\"" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

let paramsWithEnumInput input =
    JsonDocument.Parse(variablesWithInput "enumVar" input).RootElement.Deserialize<ImmutableDictionary<string, JsonElement>>(Json.serializerOptions)

[<Fact>]
let ``Execute handles enum input as variable`` () =
    let ast = parse """query fieldWithEnumValue($enumVar: EnumTestType!) {
        fieldWithEnumInput(input: $enumVar)
      }"""
    let testInputValue = "\"Foo\""
    let params' = paramsWithEnumInput testInputValue
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = params')
    let expected = NameValueLookup.ofList [ "fieldWithEnumInput", upcast "{\"kind\":\"Foo\"}" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles nullable null enum input as variable`` () =
    let ast = parse """query fieldWithNullableEnumValue($enumVar: EnumTestType) {
        fieldWithNullableEnumInput(input: $enumVar)
      }"""
    let testInputValue = "null"
    let params' = paramsWithEnumInput testInputValue
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = params')
    let expected = NameValueLookup.ofList [ "fieldWithNullableEnumInput", upcast testInputValue ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles union enum input as variable`` () =
    let ast = parse """query fieldWithEnumValue($enumVar: EnumTestType!) {
        fieldWithEnumInput(input: $enumVar)
      }"""
    let testInputValue = "\"Bar\""
    let params' = paramsWithEnumInput testInputValue
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = params')
    let expected = NameValueLookup.ofList [ "fieldWithEnumInput", upcast "{\"kind\":\"Bar\"}" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles Some union enum input as variable`` () =
    let ast = parse """query fieldWithNullableEnumValue($enumVar: EnumTestType) {
        fieldWithNullableEnumInput(input: $enumVar)
      }"""
    let testInputValue = "\"Bar\""
    let params' = paramsWithEnumInput testInputValue
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = params')
    let expected = NameValueLookup.ofList [ "fieldWithNullableEnumInput", upcast "{\"kind\":\"Bar\"}" ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles None enum input as variable`` () =
    let ast = parse """query fieldWithNullableEnumValue($enumVar: EnumTestType) {
        fieldWithNullableEnumInput(input: $enumVar)
      }"""
    let testInputValue = "null"
    let params' = paramsWithEnumInput testInputValue
    let actual = sync <| Executor(schema).AsyncExecute(ast, variables = params')
    let expected = NameValueLookup.ofList [ "fieldWithNullableEnumInput", upcast testInputValue ]
    match actual with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"
