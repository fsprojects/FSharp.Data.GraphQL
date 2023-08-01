// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.InputListTests

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

let TestType =
    Define.Object<unit> (
        name = "TestType",
        fields =
            [ Define.Field ("list", StringType, "", [ Define.Input ("input", Nullable (ListOf (Nullable StringType))) ], stringifyInput)
              Define.Field ("nnList", StringType, "", [ Define.Input ("input", ListOf (Nullable StringType)) ], stringifyInput)
              Define.Field ("listNN", StringType, "", [ Define.Input ("input", Nullable (ListOf StringType)) ], stringifyInput)
              Define.Field ("nnListNN", StringType, "", [ Define.Input ("input", ListOf StringType) ], stringifyInput) ]
    )

let schema = Schema (TestType)


let variablesWithInput inputName input = $"""{{"%s{inputName}":%s{input}}}"""

let paramsWithValueInput input =
    JsonDocument
        .Parse(variablesWithInput "input" input)
        .RootElement.Deserialize<ImmutableDictionary<string, JsonElement>> (Json.serializerOptions)

[<Fact>]
let ``Execute handles list inputs and nullability and allows lists to be null`` () =
    let ast =
        parse
            """query q($input: [String]) {
          list(input: $input)
        }"""

    let testInputValue = "null"
    let params' = paramsWithValueInput testInputValue
    let actual = sync <| Executor(schema).AsyncExecute (ast, variables = params')
    let expected = NameValueLookup.ofList [ "list", upcast testInputValue ]

    match actual with
    | Direct (data, errors) ->
        empty errors
        data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and allows lists to contain values`` () =
    let ast =
        parse
            """query q($input: [String]) {
          list(input: $input)
        }"""

    let testInputList = "[\"A\"]"
    let params' = paramsWithValueInput testInputList
    let actual = sync <| Executor(schema).AsyncExecute (ast, variables = params')
    let expected = NameValueLookup.ofList [ "list", upcast testInputList ]

    match actual with
    | Direct (data, errors) ->
        empty errors
        data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and allows lists to contain null`` () =
    let ast =
        parse
            """query q($input: [String]) {
          list(input: $input)
        }"""

    let testInputList = "[\"A\",null,\"B\"]"
    let params' = paramsWithValueInput testInputList
    let actual = sync <| Executor(schema).AsyncExecute (ast, variables = params')
    let expected = NameValueLookup.ofList [ "list", upcast testInputList ]

    match actual with
    | Direct (data, errors) ->
        empty errors
        data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and does not allow non-null lists to be null`` () =
    let ast =
        parse
            """query q($input: [String]!) {
          nnList(input: $input)
        }"""

    let testInputList = "null"
    let params' = paramsWithValueInput testInputList
    let actual = sync <| Executor(schema).AsyncExecute (ast, variables = params')

    match actual with
    | RequestError errors -> hasError "Variable '$input': expected value of type '[String]!', but no value was found." errors
    | _ -> fail "Expected RequestError GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and allows non-null lists to contain values`` () =
    let ast =
        parse
            """query q($input: [String]!) {
          nnList(input: $input)
        }"""

    let testInputList = "[\"A\"]"
    let params' = paramsWithValueInput testInputList
    let actual = sync <| Executor(schema).AsyncExecute (ast, variables = params')
    let expected = NameValueLookup.ofList [ "nnList", upcast testInputList ]

    match actual with
    | Direct (data, errors) ->
        empty errors
        data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and allows non-null lists to contain null`` () =
    let ast =
        parse
            """query q($input: [String]!) {
          nnList(input: $input)
        }"""

    let testInputList = "[\"A\",null,\"B\"]"
    let params' = paramsWithValueInput testInputList
    let actual = sync <| Executor(schema).AsyncExecute (ast, variables = params')
    let expected = NameValueLookup.ofList [ "nnList", upcast testInputList ]

    match actual with
    | Direct (data, errors) ->
        empty errors
        data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and allows lists of non-nulls to be null`` () =
    let ast =
        parse
            """query q($input: [String!]) {
          listNN(input: $input)
        }"""

    let testInputList = "null"
    let params' = paramsWithValueInput testInputList
    let actual = sync <| Executor(schema).AsyncExecute (ast, variables = params')
    let expected = NameValueLookup.ofList [ "listNN", upcast testInputList ]

    match actual with
    | Direct (data, errors) ->
        empty errors
        data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and allows lists of non-nulls to contain values`` () =
    let ast =
        parse
            """query q($input: [String!]) {
          listNN(input: $input)
        }"""

    let testInputList = "[\"A\"]"
    let params' = paramsWithValueInput testInputList
    let actual = sync <| Executor(schema).AsyncExecute (ast, variables = params')
    let expected = NameValueLookup.ofList [ "listNN", upcast testInputList ]

    match actual with
    | Direct (data, errors) ->
        empty errors
        data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and does not allow lists of non-nulls to contain null`` () =
    let ast =
        parse
            """query q($input: [String!]) {
          listNN(input: $input)
        }"""

    let testInputList = "[\"A\",null,\"B\"]"
    let params' = paramsWithValueInput testInputList
    let actual = sync <| Executor(schema).AsyncExecute (ast, variables = params')

    match actual with
    | RequestError errors -> hasError "Variable '$input': list element expected value of type 'String!' but got 'null'." errors
    | _ -> fail "Expected RequestError GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and does not allow non-null lists of non-nulls to be null`` () =
    let ast =
        parse
            """query q($input: [String!]!) {
          nnListNN(input: $input)
        }"""

    let testInputList = "null"
    let params' = paramsWithValueInput testInputList
    let actual = sync <| Executor(schema).AsyncExecute (ast, variables = params')

    match actual with
    | RequestError errors -> hasError "Variable '$input': expected value of type '[String!]!', but no value was found." errors
    | _ -> fail "Expected RequestError GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and does not allow non-null lists of non-nulls to contain values`` () =
    let ast =
        parse
            """query q($input: [String!]!) {
          nnListNN(input: $input)
        }"""

    let testInputList = "[\"A\"]"
    let params' = paramsWithValueInput testInputList
    let actual = sync <| Executor(schema).AsyncExecute (ast, variables = params')
    let expected = NameValueLookup.ofList [ "nnListNN", upcast testInputList ]

    match actual with
    | Direct (data, errors) ->
        empty errors
        data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles list inputs and nullability and does not allow non-null lists of non-nulls to contain null`` () =
    let ast =
        parse
            """query q($input: [String!]!) {
          nnListNN(input: $input)
        }"""

    let testInputList = "[\"A\",null,\"B\"]"
    let params' = paramsWithValueInput testInputList
    let actual = sync <| Executor(schema).AsyncExecute (ast, variables = params')

    match actual with
    | RequestError errors -> hasError "Variable '$input': list element expected value of type 'String!' but got 'null'." errors
    | _ -> fail "Expected RequestError GQResponse"
