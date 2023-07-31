// The MIT License (MIT)

module FSharp.Data.GraphQL.Tests.InputTests

open System

#nowarn "25"

open Xunit
open System.Runtime.InteropServices
open System.Text.Json.Serialization

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Server.Relay

type InputRecord = {
    a: string
    b: string
    c: string
}

let InputRecordType = Define.InputObject<InputRecord>("InputRecord",
    [
        Define.Input("a", StringType)
        Define.Input("b", StringType)
        Define.Input("c", StringType)
    ])

type InputRecordOptional = {
    a: string
    b: string option
    c: string voption
}

let InputRecordOptionalType = Define.InputObject<InputRecordOptional>("InputRecordOptional",
    [
        Define.Input("a", StringType)
        Define.Input("b", Nullable StringType)
        Define.Input("c", Nullable StringType)
    ])

type InputRecordNested = {
    a: InputRecord
    b: InputRecord option
    c: InputRecord voption
}

let InputRecordNestedType = Define.InputObject<InputRecordNested>("InputRecordNested",
    [
        Define.Input("a", InputRecordType)
        Define.Input("b", Nullable InputRecordType)
        Define.Input("c", Nullable InputRecordType)
    ])

type InputObject(
    a: string,
    b: string,
    c: string
) =
    member val A = a
    member val B = b
    member val C = c

let InputObjectType = Define.InputObject<InputObject>("InputObject",
    [
        Define.Input("a", StringType)
        Define.Input("b", StringType)
        Define.Input("c", StringType)
    ])

type InputObjectOptional(
    a: string,
    [<Optional>] b: string voption,
    ?c: string
) =
    member val A = a
    member val B = b
    member val C = c

let InputObjectOptionalType = Define.InputObject<InputObjectOptional>("InputObjectOptional",
    [
        Define.Input("a", StringType)
        Define.Input("b", Nullable StringType)
        Define.Input("c", Nullable StringType)
    ])

let schema =
    let schema =
        Schema(
            query = Define.Object("Query", fun () ->
            [
                Define.Field(
                    "recordInputs",
                    IntType,
                    [
                        Define.Input("record", InputRecordType)
                        Define.Input("recordOptional", InputRecordOptionalType)
                        Define.Input("recordNested", InputRecordNestedType)
                    ],
                    fun _ _ -> 5)
                Define.Field(
                    "objectInputs",
                    IntType,
                    [
                        Define.Input("object", InputObjectType)
                        Define.Input("objectOptional", InputObjectOptionalType)
                    ],
                    fun _ _ -> 5)
            ]))
    Executor<obj>(schema)

[<Fact>]
let ``Execute handles creation of inline input records with all fields`` () =
    let query = """{
      recordInputs(
        record: { a: "a", b: "b", c: "c" },
        recordOptional: { a: "a", b: "b", c: "c" },
        recordNested: { a: { a: "a", b: "b", c: "c" }, b: { a: "a", b: "b", c: "c" }, c: { a: "a", b: "b", c: "c" } }
      )
    }"""
    let result = sync <| schema.AsyncExecute(parse query)
    match result with
    | Direct(data, errors) -> empty errors
    | response -> fail $"Expected a direct GQLResponse but got {Environment.NewLine}{response}"

[<Fact>]
let ``Execute handles creation of inline input records with optional null fields`` () =
    let query = """{
      recordInputs(
        record: { a: "a", b: "b", c: "c" },
        recordOptional: null,
        recordNested: { a: { a: "a", b: "b", c: "c" }, b: null, c: null }
      )
    }"""
    let result = sync <| schema.AsyncExecute(parse query)
    match result with
    | Direct(data, errors) -> empty errors
    | _ -> fail "Expected a direct GQLResponse"

[<Fact>]
let ``Execute handles creation of inline input records with mandatory only fields`` () =
    let query = """{
      recordInputs(
        record: { a: "a", b: "b", c: "c" },
        recordNested: { a: { a: "a", b: "b", c: "c" } }
      )
    }"""
    let result = sync <| schema.AsyncExecute(parse query)
    match result with
    | Direct(data, errors) -> empty errors
    | _ -> fail "Expected a direct GQLResponse"

//[<Fact>]
//let ``Execute handles creation of input records from variables`` () =


//[<Fact>]
//let ``Execute handles creation of inline input objects`` () =

//[<Fact>]
//let ``Execute handles creation of input objects from variables`` () =
