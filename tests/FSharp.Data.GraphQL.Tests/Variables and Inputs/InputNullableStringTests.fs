// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.InputNullableStringTests

open Xunit
open System
open System.Collections.Immutable
open System.Text.Json

#nowarn "25"

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Samples.StarWarsApi
open ErrorHelpers

let stringifyArg name (ctx : ResolveFieldContext) () =
    let arg = ctx.TryArg name |> Option.toObj
    JsonSerializer.Serialize (arg, Json.serializerOptions)

let stringifyInput = stringifyArg "input"

let TestType =
    Define.Object<unit> (
        name = "TestType",
        fields =
            [ Define.Field (
                  "fieldWithDefaultArgumentValue",
                  StringType,
                  "",
                  [ Define.Input ("input", Nullable StringType, Some "hello world") ],
                  stringifyInput
              )
              Define.Field ("fieldWithNullableStringInput", StringType, "", [ Define.Input ("input", Nullable StringType) ], stringifyInput)
              Define.Field ("fieldWithNonNullableStringInput", StringType, "", [ Define.Input ("input", StringType) ], stringifyInput) ]
    )

let schema = Schema (TestType)


let variablesWithInput inputName input = $"""{{"%s{inputName}":%s{input}}}"""

let paramsWithValueInput input =
    JsonDocument
        .Parse(variablesWithInput "input" input)
        .RootElement.Deserialize<ImmutableDictionary<string, JsonElement>> (Json.serializerOptions)

[<Fact>]
let ``Execute handles variables and allows nullable inputs to be omitted`` () =
    let ast = parse """{ fieldWithNullableStringInput }"""
    let result = sync <| Executor(schema).AsyncExecute (ast)
    let expected = NameValueLookup.ofList [ "fieldWithNullableStringInput", upcast "null" ]
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)

[<Fact>]
let ``Execute handles variables and allows nullable inputs to be omitted in a variable`` () =
    let ast =
        parse
            """query SetsNullable($value: String) {
        fieldWithNullableStringInput(input: $value)
      }"""

    let result = sync <| Executor(schema).AsyncExecute (ast)
    let expected = NameValueLookup.ofList [ "fieldWithNullableStringInput", upcast "null" ]
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)

[<Fact>]
let ``Execute handles variables and allows nullable inputs to be set to null in a variable`` () =
    let ast =
        parse
            """query SetsNullable($value: String) {
        fieldWithNullableStringInput(input: $value)
      }"""

    let testInputValue = "null"
    let params' = paramsWithValueInput testInputValue

    let result = sync <| Executor(schema).AsyncExecute (ast, variables = params')
    let expected = NameValueLookup.ofList [ "fieldWithNullableStringInput", upcast testInputValue ]
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)

[<Fact>]
let ``Execute handles variables and allows nullable inputs to be set to a value in a variable`` () =
    let ast =
        parse
            """query SetsNullable($value: String) {
        fieldWithNullableStringInput(input: $value)
      }"""

    let paramsWithValueInput input =
        JsonDocument
            .Parse(variablesWithInput "value" input)
            .RootElement.Deserialize<ImmutableDictionary<string, JsonElement>> (Json.serializerOptions)

    let testInputValue = "\"a\""
    let params' = paramsWithValueInput testInputValue

    let result = sync <| Executor(schema).AsyncExecute (ast, variables = params')
    let expected = NameValueLookup.ofList [ "fieldWithNullableStringInput", upcast testInputValue ]
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)

[<Fact>]
let ``Execute handles variables and allows nullable inputs to be set to a value directly`` () =
    let ast = parse """{ fieldWithNullableStringInput(input: "a") }"""
    let result = sync <| Executor(schema).AsyncExecute (ast)
    let expected = NameValueLookup.ofList [ "fieldWithNullableStringInput", upcast "\"a\"" ]
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)

[<Fact>]
let ``Execute handles non-nullable scalars and does not allow non-nullable inputs to be omitted in a variable`` () =
    let ast =
        parse
            """query SetsNonNullable($value: String!) {
          fieldWithNonNullableStringInput(input: $value)
        }"""

    let paramsWithValueInput input =
        JsonDocument
            .Parse(variablesWithInput "value" input)
            .RootElement.Deserialize<ImmutableDictionary<string, JsonElement>> (Json.serializerOptions)

    let testInputValue = "null"
    let params' = paramsWithValueInput testInputValue
    let result = sync <| Executor(schema).AsyncExecute (ast, variables = params')
    ensureRequestError result <| fun [ error ] ->
        error |> ensureInputCoercionError (Variable "value") "Non-nullable variable '$value' expected value of type 'String!', but got 'null'." "String!"

[<Fact>]
let ``Execute handles non-nullable scalars and allows non-nullable inputs to be set to a value in a variable`` () =
    let ast =
        parse
            """query SetsNonNullable($value: String!) {
          fieldWithNonNullableStringInput(input: $value)
        }"""

    let paramsWithValueInput input =
        JsonDocument
            .Parse(variablesWithInput "value" input)
            .RootElement.Deserialize<ImmutableDictionary<string, JsonElement>> (Json.serializerOptions)

    let testInputValue = "\"a\""
    let params' = paramsWithValueInput testInputValue

    let result = sync <| Executor(schema).AsyncExecute (ast, variables = params')
    let expected = NameValueLookup.ofList [ "fieldWithNonNullableStringInput", upcast testInputValue ]
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)

[<Fact>]
let ``Execute handles non-nullable scalars and allows non-nullable inputs to be set to a value directly`` () =
    let ast = parse """{ fieldWithNonNullableStringInput(input: "a") }"""
    let result = sync <| Executor(schema).AsyncExecute (ast)
    let expected = NameValueLookup.ofList [ "fieldWithNonNullableStringInput", upcast "\"a\"" ]
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)

[<Fact>]
let ``Execute uses argument default value when no argument was provided`` () =
    let ast = parse """{ fieldWithDefaultArgumentValue }"""
    let result = sync <| Executor(schema).AsyncExecute (ast)
    let expected = NameValueLookup.ofList [ "fieldWithDefaultArgumentValue", upcast "\"hello world\"" ]
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)

let paramsWithOptionalInput input =
    JsonDocument
        .Parse(variablesWithInput "optional" input)
        .RootElement.Deserialize<ImmutableDictionary<string, JsonElement>> (Json.serializerOptions)

[<Fact>]
let ``Execute uses argument default value when nullable variable provided`` () =
    let ast =
        parse
            """query optionalVariable($optional: String) {
        fieldWithDefaultArgumentValue(input: $optional)
      }"""

    let testInputValue = "\"hello world\""
    let params' = paramsWithOptionalInput testInputValue
    let result = sync <| Executor(schema).AsyncExecute (ast, variables = params')
    let expected = NameValueLookup.ofList [ "fieldWithDefaultArgumentValue", upcast testInputValue ]
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)
