// The MIT License (MIT)
// Copyright (mand) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.InputComplexTests

open Xunit
open System
open System.Collections.Immutable
open System.Text.Json
open System.Text.Json.Serialization

#nowarn "25"

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Samples.StarWarsApi
open ErrorHelpers

let TestComplexScalar =
    Define.Scalar (
        name = "ComplexScalar",
        coerceInput =
            (fun i ->
                let value =
                    match i with
                    | InputParameterValue.Variable optArr -> optArr.GetString ()
                    | InputParameterValue.InlineConstant (StringValue s) -> s

                if value = "SerializedValue" then Ok "DeserializedValue"
                else Error { new IGQLError with member _.Message = "" }),
        coerceOutput =
            (fun value ->
                if value = upcast "DeserializedValue" then Some "SerializedValue"
                else None)
    )

type TestInput =
    { mand : string
      opt1 : string option
      opt2 : string option
      optSeq : string option seq option
      voptSeq : string option seq voption // string voption seq voption is too hard to implement
      optArr : string option array option
      voptArr : string option array voption } // string voption array voption is too hard to implement

let InputArrayOf (innerDef : #TypeDef<'Val>) : ListOfDef<'Val, 'Val array> = ListOf innerDef

let TestInputObject =
    Define.InputObject<TestInput> (
        name = "TestInputObject",
        fields =
            [ Define.Input ("mand", StringType)
              Define.Input ("opt1", Nullable StringType)
              Define.Input ("opt2", Nullable TestComplexScalar)
              Define.Input ("optSeq", Nullable (ListOf (Nullable StringType)))
              Define.Input ("voptSeq", Nullable (ListOf (Nullable StringType)))
              Define.Input ("optArr", Nullable (InputArrayOf (Nullable StringType)))
              Define.Input ("voptArr", Nullable (InputArrayOf (Nullable StringType))) ]
    )

let TestType =
    Define.Object<unit> (
        name = "TestType",
        fields = [ Define.Field ("fieldWithObjectInput", StringType, "", [ Define.Input ("input", Nullable TestInputObject) ], stringifyInput) ]
    )

let schema = Schema (TestType)

[<Fact>]
let ``Execute handles objects and nullability using inline structs with complex input`` () =
    let ast =
        parse """{ fieldWithObjectInput(input: {mand: "baz", opt1: "foo", optSeq: ["bar"], optArr: ["baf"]}) }"""
    let result = sync <| Executor(schema).AsyncExecute (ast)

    let expected =
        NameValueLookup.ofList
            [ "fieldWithObjectInput",
              upcast """{"mand":"baz","opt1":"foo","opt2":null,"optSeq":["bar"],"voptSeq":null,"optArr":["baf"],"voptArr":null}""" ]

    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)

// See https://spec.graphql.org/October2021/#sec-List
[<Fact(Skip = "Validation needs to be updated to allow")>]
let ``Execute handles objects and nullability using inline structs and properly parses single value to list`` () =
    let ast = parse """{ fieldWithObjectInput(input: {mand:"baz", opt1: "foo", optSeq: "bar"}) }"""
    let result = sync <| Executor(schema).AsyncExecute (ast)
    let expected =
        NameValueLookup.ofList [ "fieldWithObjectInput", upcast """{"mand":"baz", "opt1":"foo", "optSeq":["bar"], "opt2":null, "optArr":null}""" ]
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)

[<Fact>]
let ``Execute handles objects and nullability using inline structs and properly coerces complex scalar types`` () =
    let ast = parse """{ fieldWithObjectInput(input: {mand: "foo", opt2: "SerializedValue"}) }"""
    let result = sync <| Executor(schema).AsyncExecute (ast)
    let expected =
        NameValueLookup.ofList
            [ "fieldWithObjectInput",
              upcast """{"mand":"foo","opt1":null,"opt2":"DeserializedValue","optSeq":null,"voptSeq":null,"optArr":null,"voptArr":null}""" ]

    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)

let variablesWithInput inputName input = $"""{{"%s{inputName}":%s{input}}}"""

let paramsWithValueInput input =
    JsonDocument
        .Parse(variablesWithInput "input" input)
        .RootElement.Deserialize<ImmutableDictionary<string, JsonElement>> (Json.serializerOptions)

let testInputObject =
    """{"mand":"baz","opt1":"foo","opt2":null,"optSeq":["bar"],"voptSeq":["bar"],"optArr":null,"voptArr":null}"""

[<Fact>]
let ``Execute handles variables with complex inputs`` () =
    let ast =
        parse
            """query q($input: TestInputObject) {
          fieldWithObjectInput(input: $input)
        }"""

    let params' = paramsWithValueInput testInputObject
    let result = sync <| Executor(schema).AsyncExecute (ast, variables = params')
    let expected = NameValueLookup.ofList [ "fieldWithObjectInput", upcast testInputObject ]
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)

[<Fact>]
let ``Execute handles variables with default value when no value was provided`` () =
    let ast =
        parse
            """query q($input: TestInputObject = {mand:"baz", opt1: "foo", optSeq: ["bar"], voptSeq:["bar"]}) {
            fieldWithObjectInput(input: $input)
          }"""

    let result = sync <| Executor(schema).AsyncExecute (ast)
    let expected = NameValueLookup.ofList [ "fieldWithObjectInput", upcast testInputObject ]
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)

[<Fact>]
let ``Execute handles variables and errors on null for nested non-nulls`` () =
    let ast =
        parse
            """query q($input: TestInputObject) {
          fieldWithObjectInput(input: $input)
        }"""

    let testInputObject = """{"mand":null, "opt1":"foo", "optSeq":["bar"], "voptSeq":["bar"]}"""
    let params' = paramsWithValueInput testInputObject
    let result = sync <| Executor(schema).AsyncExecute (ast, variables = params')
    ensureRequestError result <| fun [ error ] ->
        let message = "Non-nullable field 'mand' expected value of type 'String!', but got 'null'."
        error |> ensureInputObjectFieldCoercionError (Variable "input") message [] "TestInputObject" "String!"

[<Fact>]
let ``Execute handles variables and errors on incorrect type`` () =
    let ast =
        parse
            """query q($input: TestInputObject) {
          fieldWithObjectInput(input: $input)
        }"""

    let testInputObject = "\"foo bar\""
    let params' = paramsWithValueInput testInputObject
    let result = sync <| Executor(schema).AsyncExecute (ast, variables = params')
    ensureRequestError result <| fun [ error ] ->
        let message = $"Variable '$input' expected to be '%O{JsonValueKind.Object}' but got '%O{JsonValueKind.String}'."
        error |> ensureInputCoercionError (Variable "input") message "TestInputObject"

[<Fact>]
let ``Execute handles variables and errors on omission of nested non-nulls`` () =
    let ast =
        parse
            """query q($input: TestInputObject) {
          fieldWithObjectInput(input: $input)
        }"""

    let testInputObject = """{"opt1":"foo","optSeq":["bar"]}"""
    let params' = paramsWithValueInput testInputObject
    let result = sync <| Executor(schema).AsyncExecute (ast, variables = params')
    ensureRequestError result <| fun [ error ] ->
        let message = "Non-nullable field 'mand' expected value of type 'String!', but got 'null'."
        error |> ensureInputObjectFieldCoercionError (Variable "input") message [] "TestInputObject" "String!"

[<Fact>]
let ``Execute handles list inputs and nullability and does not allow invalid types to be used as values`` () =
    let ast =
        parse
            """query q($input: TestInputObject!) {
          fieldWithObjectInput(input: $input)
        }"""
    // as that kind of an error inside of opt1 query is guaranteed to fail in every call, we're gonna to fail noisy here
    let testInputList = "[\"A\",\"B\"]"
    let params' = paramsWithValueInput testInputList
    let result = sync <| Executor(schema).AsyncExecute (ast, variables = params')
    ensureRequestError result <| fun [ error ] ->
        let message = $"Variable '$input' expected to be '%O{JsonValueKind.Object}' but got '%O{JsonValueKind.Array}'."
        error |> ensureInputCoercionError (Variable "input") message "TestInputObject!"
        match error.Extensions with
        | Include extensions -> equals (box "TestInputObject!") extensions[CustomErrorFields.ObjectType]
        | Skip -> fail $"extensions.{CustomErrorFields.ObjectType} is abset"

[<Fact>]
let ``Execute handles list inputs and nullability and does not allow unknown types to be used as values`` () =
    let ast =
        parse
            """query q($input: UnknownType!) {
          fieldWithObjectInput(input: $input)
        }"""
    // as that kind of an error inside of opt1 query is guaranteed to fail in every call, we're gonna to fail noisy here
    let testInputValue = "\"whoknows\""
    let params' = paramsWithValueInput testInputValue
    let result = sync <| Executor(schema).AsyncExecute (ast, variables = params')
    let expectedError =
        let message = "Variable '$input' in operation 'q' has a type that is not an input type defined by the schema (UnknownType!)."
        GQLProblemDetails.CreateWithKind (message, Validation)
    ensureRequestError result <| fun [ error ] -> error |> equals expectedError
