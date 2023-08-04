// The MIT License (MIT)
// Copyright (mand) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.InputNestedTests

open System.Text.Json

#nowarn "25"

open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Samples.StarWarsApi

let InputArrayOf (innerDef : #TypeDef<'Val>) : ListOfDef<'Val, 'Val array> = ListOf innerDef

let TestInputObject = InputComplexTests.TestInputObject

type TestInput = InputComplexTests.TestInput

type TestNestedInput = { n : string; no : TestInput option; nvo : TestInput voption }

let TestNestedInputObject =
    Define.InputObject<TestNestedInput> (
        name = "TestNestedInputObject",
        fields =
            [ Define.Input ("n", StringType)
              Define.Input ("no", Nullable TestInputObject)
              Define.Input ("nvo", Nullable TestInputObject) ]
    )

#nowarn "40"

type TestRecusiveInput = { r : string; ro : TestRecusiveInput option; rvo : TestRecusiveInput voption }

let rec TestRecursiveInputObject =
    Define.InputObject<TestRecusiveInput> (
        name = "TestRecusiveInput",
        fieldsFn =
            fun () ->
                [ Define.Input ("r", StringType)
                  Define.Input ("ro", Nullable TestRecursiveInputObject)
                  Define.Input ("rvo", Nullable TestRecursiveInputObject) ]
    )

let stringifyArg name (ctx : ResolveFieldContext) () =
    let arg = ctx.TryArg name |> Option.toObj
    JsonSerializer.Serialize (arg, Json.serializerOptions)

let stringifyInput = stringifyArg "input"

let TestType =
    Define.Object<unit> (
        name = "TestType",
        fields =
            [ Define.Field (
                  "fieldWithNestedInputObject",
                  StringType,
                  "",
                  [ Define.Input ("input", TestNestedInputObject, { n = "hello world"; no = None; nvo = ValueNone }) ],
                  stringifyInput
              )
              Define.Field (
                  "fieldWithRecursiveInputObject",
                  StringType,
                  "",
                  [ Define.Input ("input", TestRecursiveInputObject, { r = "hello world"; ro = None; rvo = ValueNone }) ],
                  stringifyInput
              ) ]
    )

let schema = Schema (TestType)

[<Fact>]
let ``Execute handles nested input objects and nullability using inline structs and properly coerces complex scalar types`` () =
    let ast = parse """{ fieldWithNestedInputObject(input: {n:"optSeq", no:{mand:"mand"}, nvo:{mand:"mand"}})}"""
    let actual = sync <| Executor(schema).AsyncExecute (ast)
    let expected =
        NameValueLookup.ofList
            [ "fieldWithNestedInputObject",
                 upcast """{"n":"optSeq","no":{"mand":"mand","opt1":null,"opt2":null,"optSeq":null,"voptSeq":null,"optArr":null,"voptArr":null},"nvo":{"mand":"mand","opt1":null,"opt2":null,"optSeq":null,"voptSeq":null,"optArr":null,"voptArr":null}}""" ]

    match actual with
    | Direct (data, errors) ->
        empty errors
        data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute handles recursive input objects and nullability using inline structs and properly coerces complex scalar types`` () =
    let ast = parse """{ fieldWithRecursiveInputObject(input: {r:"optSeq", ro:{r:"mand"}, rvo:{r:"mand"}})}"""
    let actual = sync <| Executor(schema).AsyncExecute (ast)
    let expected =
        NameValueLookup.ofList [ "fieldWithRecursiveInputObject", upcast """{"r":"optSeq","ro":{"r":"mand","ro":null,"rvo":null},"rvo":{"r":"mand","ro":null,"rvo":null}}""" ]

    match actual with
    | Direct (data, errors) ->
        empty errors
        data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"
