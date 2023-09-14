// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.InputEnumTests

open System.Collections.Immutable
open System.Text.Json

#nowarn "25"

open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Samples.StarWarsApi

let stringifyArg name (ctx : ResolveFieldContext) () =
    let arg = ctx.TryArg name |> Option.toObj
    JsonSerializer.Serialize (arg, Json.serializerOptions)

let stringifyInput = stringifyArg "input"

type EnumTestType =
    | Foo
    | Bar

let EnumTestType =
    Define.Enum ("EnumTestType", [ Define.EnumValue ("Foo", EnumTestType.Foo); Define.EnumValue ("Bar", EnumTestType.Bar) ], "Test enum")

let TestType =
    Define.Object<unit> (
        name = "TestType",
        fields =
            [ Define.Field ("fieldWithEnumInput", StringType, "", [ Define.Input ("input", EnumTestType) ], stringifyInput)
              Define.Field ("fieldWithNullableEnumInput", StringType, "", [ Define.Input ("input", Nullable EnumTestType) ], stringifyInput) ]
    )

let schema = Schema (TestType)


let variablesWithInput inputName input = $"""{{"%s{inputName}":%s{input}}}"""

let paramsWithEnumInput input =
    JsonDocument
        .Parse(variablesWithInput "enumVar" input)
        .RootElement.Deserialize<ImmutableDictionary<string, JsonElement>> (Json.serializerOptions)

[<Fact>]
let ``Execute handles enum input as variable`` () =
    let ast =
        parse
            """query fieldWithEnumValue($enumVar: EnumTestType!) {
        fieldWithEnumInput(input: $enumVar)
      }"""

    let testInputValue = "\"Foo\""
    let params' = paramsWithEnumInput testInputValue
    let actual = sync <| Executor(schema).AsyncExecute (ast, variables = params')
    let expected = NameValueLookup.ofList [ "fieldWithEnumInput", upcast "\"Foo\"" ]

    match actual with
    | Direct (data, errors) ->
        empty errors
        data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles nullable null enum input as variable`` () =
    let ast =
        parse
            """query fieldWithNullableEnumValue($enumVar: EnumTestType) {
        fieldWithNullableEnumInput(input: $enumVar)
      }"""

    let testInputValue = "null"
    let params' = paramsWithEnumInput testInputValue
    let actual = sync <| Executor(schema).AsyncExecute (ast, variables = params')
    let expected = NameValueLookup.ofList [ "fieldWithNullableEnumInput", upcast testInputValue ]

    match actual with
    | Direct (data, errors) ->
        empty errors
        data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles union enum input as variable`` () =
    let ast =
        parse
            """query fieldWithEnumValue($enumVar: EnumTestType!) {
        fieldWithEnumInput(input: $enumVar)
      }"""

    let testInputValue = "\"Bar\""
    let params' = paramsWithEnumInput testInputValue
    let actual = sync <| Executor(schema).AsyncExecute (ast, variables = params')
    let expected = NameValueLookup.ofList [ "fieldWithEnumInput", upcast "\"Bar\"" ]

    match actual with
    | Direct (data, errors) ->
        empty errors
        data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles Some union enum input as variable`` () =
    let ast =
        parse
            """query fieldWithNullableEnumValue($enumVar: EnumTestType) {
        fieldWithNullableEnumInput(input: $enumVar)
      }"""

    let testInputValue = "\"Bar\""
    let params' = paramsWithEnumInput testInputValue
    let actual = sync <| Executor(schema).AsyncExecute (ast, variables = params')
    let expected = NameValueLookup.ofList [ "fieldWithNullableEnumInput", upcast "\"Bar\"" ]

    match actual with
    | Direct (data, errors) ->
        empty errors
        data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles None enum input as variable`` () =
    let ast =
        parse
            """query fieldWithNullableEnumValue($enumVar: EnumTestType) {
        fieldWithNullableEnumInput(input: $enumVar)
      }"""

    let testInputValue = "null"
    let params' = paramsWithEnumInput testInputValue
    let actual = sync <| Executor(schema).AsyncExecute (ast, variables = params')
    let expected = NameValueLookup.ofList [ "fieldWithNullableEnumInput", upcast testInputValue ]

    match actual with
    | Direct (data, errors) ->
        empty errors
        data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"
