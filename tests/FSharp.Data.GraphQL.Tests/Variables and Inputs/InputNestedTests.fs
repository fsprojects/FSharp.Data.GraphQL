// The MIT License (MIT)
// Copyright (mand) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.InputNestedTests

open Xunit
open System
open System.Text.Json

#nowarn "25"

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Samples.StarWarsApi

let InputArrayOf (innerDef : #TypeDef<'Val>) : ListOfDef<'Val, 'Val array> = ListOf innerDef

let TestInputObject = InputComplexTests.TestInputObject

type TestInput = InputComplexTests.TestInput

type TestNestedInput = {
    n : string
    no : TestInput option
    nvo : TestInput voption
    nl : TestInput list
    nlo : TestInput list option
    nlvo : TestInput list voption
}

let TestNestedInputObject =
    Define.InputObject<TestNestedInput> (
        name = "TestNestedInputObject",
        fields = [
            Define.Input ("n", StringType)
            Define.Input ("no", Nullable TestInputObject)
            Define.Input ("nvo", Nullable TestInputObject)
            Define.Input ("nl", ListOf TestInputObject)
            Define.Input ("nlo", Nullable (ListOf TestInputObject))
            Define.Input ("nlvo", Nullable (ListOf TestInputObject))
        ]
    )

#nowarn "40"

type TestRecusiveInput = { r : string; ro : TestRecusiveInput option; rvo : TestRecusiveInput voption }

let rec TestRecursiveInputObject =
    Define.InputObject<TestRecusiveInput> (
        name = "TestRecusiveInput",
        fieldsFn =
            fun () -> [
                Define.Input ("r", StringType)
                Define.Input ("ro", Nullable TestRecursiveInputObject)
                Define.Input ("rvo", Nullable TestRecursiveInputObject)
            ]
    )

let stringifyArg name (ctx : ResolveFieldContext) () =
    let arg = ctx.TryArg name |> Option.toObj
    JsonSerializer.Serialize (arg, Json.serializerOptions)

let stringifyInput = stringifyArg "input"

let TestType =
    Define.Object<unit> (
        name = "TestType",
        fields = [
            Define.Field (
                "fieldWithNestedInputObject",
                StringType,
                "",
                [
                    Define.Input (
                        "input",
                        TestNestedInputObject,
                        {
                            n = "hello world"
                            no = None
                            nvo = ValueNone
                            nl = []
                            nlo = None
                            nlvo = ValueNone
                        }
                    )
                ],
                stringifyInput
            )
            Define.Field (
                "fieldWithRecursiveInputObject",
                StringType,
                "",
                [
                    Define.Input ("input", TestRecursiveInputObject, { r = "hello world"; ro = None; rvo = ValueNone })
                ],
                stringifyInput
            )
        ]
    )

let schema = Schema (TestType)

[<Fact>]
let ``Execute handles nested input objects and nullability using inline structs and properly coerces complex scalar types`` () =
    let ast =
        parse """{ fieldWithNestedInputObject(input: {n:"optSeq", no:{mand:"mand"}, nvo:{mand:"mand"}, nl: []})}"""
    let result = sync <| Executor(schema).AsyncExecute (ast)
    let expected =
        NameValueLookup.ofList [
            "fieldWithNestedInputObject",
            upcast
                """{"n":"optSeq","no":{"mand":"mand","opt1":null,"opt2":null,"optSeq":null,"voptSeq":null,"optArr":null,"voptArr":null},"nvo":{"mand":"mand","opt1":null,"opt2":null,"optSeq":null,"voptSeq":null,"optArr":null,"voptArr":null},"nl":[],"nlo":null,"nlvo":null}"""
        ]

    ensureDirect result
    <| fun data errors ->
        empty errors
        data |> equals (upcast expected)

[<Fact>]
let ``Execute handles nested input objects and nullability using inline structs and properly coerces complex scalar types with empty lists`` () =
    let ast =
        parse """{ fieldWithNestedInputObject(input: {n:"optSeq", no:{mand:"mand"}, nvo:{mand:"mand"}, nl:[], nlo: [], nlvo: []})}"""
    let result = sync <| Executor(schema).AsyncExecute (ast)
    let expected =
        NameValueLookup.ofList [
            "fieldWithNestedInputObject",
            upcast
                """{"n":"optSeq","no":{"mand":"mand","opt1":null,"opt2":null,"optSeq":null,"voptSeq":null,"optArr":null,"voptArr":null},"nvo":{"mand":"mand","opt1":null,"opt2":null,"optSeq":null,"voptSeq":null,"optArr":null,"voptArr":null},"nl":[],"nlo":[],"nlvo":[]}"""
        ]

    ensureDirect result
    <| fun data errors ->
        empty errors
        data |> equals (upcast expected)

[<Fact>]
let ``Execute handles nested input objects and nullability using inline structs and properly coerces complex scalar types with lists`` () =
    let ast =
        parse """{ fieldWithNestedInputObject(input: {n:"optSeq", nl:[{mand:"mand"}], nlo: [{mand:"mand"}], nlvo: [{mand:"mand"}]})}"""
    let result = sync <| Executor(schema).AsyncExecute (ast)
    let expected =
        NameValueLookup.ofList [
            "fieldWithNestedInputObject",
            upcast
                """{"n":"optSeq","no":null,"nvo":null,"nl":[{"mand":"mand","opt1":null,"opt2":null,"optSeq":null,"voptSeq":null,"optArr":null,"voptArr":null}],"nlo":[{"mand":"mand","opt1":null,"opt2":null,"optSeq":null,"voptSeq":null,"optArr":null,"voptArr":null}],"nlvo":[{"mand":"mand","opt1":null,"opt2":null,"optSeq":null,"voptSeq":null,"optArr":null,"voptArr":null}]}"""
        ]

    ensureDirect result
    <| fun data errors ->
        empty errors
        data |> equals (upcast expected)

[<Fact>]
let ``Execute handles recursive input objects and nullability using inline structs and properly coerces complex scalar types`` () =
    let ast =
        parse """{ fieldWithRecursiveInputObject(input: {r:"optSeq", ro:{r:"mand"}, rvo:{r:"mand"}})}"""
    let result = sync <| Executor(schema).AsyncExecute (ast)
    let expected =
        NameValueLookup.ofList [
            "fieldWithRecursiveInputObject",
            upcast """{"r":"optSeq","ro":{"r":"mand","ro":null,"rvo":null},"rvo":{"r":"mand","ro":null,"rvo":null}}"""
        ]

    ensureDirect result
    <| fun data errors ->
        empty errors
        data |> equals (upcast expected)
