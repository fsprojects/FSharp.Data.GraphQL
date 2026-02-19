// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.LazyEnumerationExceptionTests

open System.Collections.Generic
open Xunit

#nowarn "0025"

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution

type TestItem = { Name: string }

type TestContainer = {
    Title: string
    Tags: string seq
    Count: int
}

let TestItemType =
    Define.Object<TestItem>(
        "TestItem", [
            Define.Field("name", StringType, fun _ x -> x.Name)
        ])

let TestContainerType =
    Define.Object<TestContainer>(
        "TestContainer", [
            Define.Field("title", StringType, fun _ x -> x.Title)
            Define.Field(
                "tags",
                Nullable (ListOf StringType),
                fun _ x -> x.Tags |> Some
            )
            Define.Field("count", IntType, fun _ x -> x.Count)
        ])

[<Fact>]
let ``Execution must return null with field error when nullable list field throws during lazy enumeration`` () =
    let schema =
        Schema(
            Define.Object<unit>(
                "Query", [
                    Define.Field(
                        "tags",
                        Nullable (ListOf StringType),
                        fun _ _ ->
                            seq {
                                yield "first"
                                failwith "Boom during enumeration"
                            }
                            |> Some
                    )
                ]))
    let expectedData =
        NameValueLookup.ofList [
            "tags", null
        ]
    let expectedErrors =
        [
            GQLProblemDetails.CreateWithKind ("Boom during enumeration", Execution, [ box "tags" ])
        ]
    let result = sync <| Executor(schema).AsyncExecute(parse "{ tags }", getMockInputContext, ())
    ensureDirect result <| fun data errors ->
        data |> equals (upcast expectedData)
        errors |> equals expectedErrors

[<Fact>]
let ``Execution must return null with field error when struct nullable list field throws during lazy enumeration`` () =
    let schema =
        Schema(
            Define.Object<unit>(
                "Query", [
                    Define.Field(
                        "tags",
                        StructNullable (ListOf StringType),
                        fun _ _ ->
                            seq {
                                yield "first"
                                failwith "Boom during enumeration"
                            }
                            |> ValueSome
                    )
                ]))
    let expectedData =
        NameValueLookup.ofList [
            "tags", null
        ]
    let expectedErrors =
        [
            GQLProblemDetails.CreateWithKind ("Boom during enumeration", Execution, [ box "tags" ])
        ]
    let result = sync <| Executor(schema).AsyncExecute(parse "{ tags }", getMockInputContext, ())
    ensureDirect result <| fun data errors ->
        data |> equals (upcast expectedData)
        errors |> equals expectedErrors

[<Fact>]
let ``Execution must propagate error when non-nullable list field throws during lazy enumeration`` () =
    let schema =
        Schema(
            Define.Object<unit>(
                "Query", [
                    Define.Field(
                        "tags",
                        ListOf StringType,
                        fun _ _ ->
                            seq {
                                yield "first"
                                failwith "Boom during enumeration"
                            }
                    )
                ]))
    let expectedError = GQLProblemDetails.CreateWithKind ("Boom during enumeration", Execution, [ box "tags" ])
    let result = sync <| Executor(schema).AsyncExecute(parse "{ tags }", getMockInputContext, ())
    ensureRequestError result <| fun [ error ] -> error |> equals expectedError

[<Fact>]
let ``Execution must return null with field error when nullable list of objects throws KeyNotFoundException during lazy enumeration`` () =
    let tagsById = dict [ "a", { Name = "Alpha" } ]
    let schema =
        Schema(
            Define.Object<unit>(
                "Query", [
                    Define.Field(
                        "tags",
                        Nullable (ListOf TestItemType),
                        fun _ _ ->
                            [ "a"; "b" ]
                            |> Seq.map (fun id -> tagsById[id])
                            |> Some
                    )
                ]))
    let result = sync <| Executor(schema).AsyncExecute(parse "{ tags { name } }", getMockInputContext, ())
    ensureDirect result <| fun data errors ->
        let expectedData =
            NameValueLookup.ofList [
                "tags", null
            ]
        data |> equals (upcast expectedData)
        equals 1 errors.Length
        hasError "was not present in the dictionary." errors

[<Fact>]
let ``Execution must return partial result when sibling nullable field throws during lazy enumeration`` () =
    let schema =
        Schema(
            Define.Object<unit>(
                "Query", [
                    Define.Field(
                        "tags",
                        Nullable (ListOf StringType),
                        fun _ _ ->
                            seq {
                                yield "first"
                                failwith "Boom during enumeration"
                            }
                            |> Some
                    )
                    Define.Field(
                        "name",
                        StringType,
                        fun _ _ -> "Hello"
                    )
                ]))
    let expectedData =
        NameValueLookup.ofList [
            "tags", null
            "name", upcast "Hello"
        ]
    let result = sync <| Executor(schema).AsyncExecute(parse "{ tags name }", getMockInputContext, ())
    ensureDirect result <| fun data errors ->
        data |> equals (upcast expectedData)
        equals 1 errors.Length
        hasError "Boom during enumeration" errors

[<Fact>]
let ``Execution must return sibling fields on nested object when nullable list throws during lazy enumeration`` () =
    let container = {
        Title = "Meeting"
        Tags = seq {
            yield "first"
            failwith "Boom during enumeration"
        }
        Count = 42
    }
    let schema =
        Schema(
            Define.Object<unit>(
                "Query", [
                    Define.Field("container", TestContainerType, fun _ _ -> container)
                ]))
    let expectedData =
        NameValueLookup.ofList [
            "container", upcast NameValueLookup.ofList [
                "title", upcast "Meeting"
                "tags", null
                "count", upcast 42
            ]
        ]
    let result = sync <| Executor(schema).AsyncExecute(parse "{ container { title tags count } }", getMockInputContext, ())
    ensureDirect result <| fun data errors ->
        data |> equals (upcast expectedData)
        equals 1 errors.Length
        hasError "Boom during enumeration" errors

[<Fact>]
let ``Execution must return sibling fields on nested struct nullable list that throws during lazy enumeration`` () =
    let TestContainerStructType =
        Define.Object<TestContainer>(
            "TestContainerStruct", [
                Define.Field("title", StringType, fun _ x -> x.Title)
                Define.Field(
                    "tags",
                    StructNullable (ListOf StringType),
                    fun _ x -> x.Tags |> ValueSome
                )
                Define.Field("count", IntType, fun _ x -> x.Count)
            ])
    let container = {
        Title = "Meeting"
        Tags = seq {
            yield "first"
            failwith "Boom during enumeration"
        }
        Count = 42
    }
    let schema =
        Schema(
            Define.Object<unit>(
                "Query", [
                    Define.Field("container", TestContainerStructType, fun _ _ -> container)
                ]))
    let expectedData =
        NameValueLookup.ofList [
            "container", upcast NameValueLookup.ofList [
                "title", upcast "Meeting"
                "tags", null
                "count", upcast 42
            ]
        ]
    let result = sync <| Executor(schema).AsyncExecute(parse "{ container { title tags count } }", getMockInputContext, ())
    ensureDirect result <| fun data errors ->
        data |> equals (upcast expectedData)
        equals 1 errors.Length
        hasError "Boom during enumeration" errors

[<Fact>]
let ``Execution must return sibling objects when one nested nullable list throws during lazy enumeration`` () =
    let goodContainer = {
        Title = "Good"
        Tags = seq { yield "alpha"; yield "beta" }
        Count = 1
    }
    let badContainer = {
        Title = "Bad"
        Tags = seq {
            yield "first"
            failwith "Boom during enumeration"
        }
        Count = 2
    }
    let schema =
        Schema(
            Define.Object<unit>(
                "Query", [
                    Define.Field("good", TestContainerType, fun _ _ -> goodContainer)
                    Define.Field("bad", TestContainerType, fun _ _ -> badContainer)
                ]))
    let expectedData =
        NameValueLookup.ofList [
            "good", upcast NameValueLookup.ofList [
                "title", upcast "Good"
                "tags", upcast [| box "alpha"; box "beta" |]
                "count", upcast 1
            ]
            "bad", upcast NameValueLookup.ofList [
                "title", upcast "Bad"
                "tags", null
                "count", upcast 2
            ]
        ]
    let result = sync <| Executor(schema).AsyncExecute(parse "{ good { title tags count } bad { title tags count } }", getMockInputContext, ())
    ensureDirect result <| fun data errors ->
        data |> equals (upcast expectedData)
        equals 1 errors.Length
        hasError "Boom during enumeration" errors
