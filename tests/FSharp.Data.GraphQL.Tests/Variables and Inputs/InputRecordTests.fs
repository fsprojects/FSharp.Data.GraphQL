// The MIT License (MIT)

module FSharp.Data.GraphQL.Tests.InputRecordTests

#nowarn "25"

open Xunit
open System
open System.Collections.Immutable
open System.Runtime.InteropServices
open System.Text.Json

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Samples.StarWarsApi

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
        Define.Input("b", Nullable InputRecordOptionalType)
        Define.Input("c", Nullable InputRecordOptionalType)
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
                    StringType,
                    [
                        Define.Input("record", InputRecordType)
                        Define.Input("recordOptional", Nullable InputRecordOptionalType)
                        Define.Input("recordNested", Nullable InputRecordNestedType)
                    ],
                    stringifyInput) // TODO: add all args tringificaiton
                Define.Field(
                    "objectInputs",
                    StringType,
                    [
                        Define.Input("object", InputObjectType)
                        Define.Input("objectOptional", Nullable InputObjectOptionalType)
                    ],
                    stringifyInput) // TODO: add all args tringificaiton
            ]))
    Executor schema

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

let variablesWithAllInputs(record, optRecord) = $"""
    {{
        "record":%s{record},
        "optRecord":%s{optRecord}
    }}
"""

let paramsWithValues variables =
    JsonDocument
        .Parse(variables : string)
        .RootElement.Deserialize<ImmutableDictionary<string, JsonElement>> (Json.serializerOptions)

[<Fact>]
let ``Execute handles creation of input records from variables with all fields`` () =
    let query = """query ($record: InputRecord!, $optRecord: InputRecordOptional){
      recordInputs(
        record: $record,
        recordOptional: $optRecord,
        recordNested: { a: $record, b: $optRecord, c: $optRecord }
      )
    }"""
    let testInputObject = """{"a":"a","b":"b","c":"c"}"""
    let params' = variablesWithAllInputs(testInputObject, testInputObject) |> paramsWithValues
    let actual = sync <| schema.AsyncExecute(parse query, variables = params')
    //let expected = NameValueLookup.ofList [ "recordInputs", upcast testInputObject ]
    match actual with
    | Direct(data, errors) ->
        empty errors
        //data |> equals (upcast expected)
    | response -> fail $"Expected a direct GQLResponse but got {Environment.NewLine}{response}"

[<Fact>]
let ``Execute handles creation of input records from variables with optional null fields`` () =
    let query = """query ($record: InputRecord!, $optRecord: InputRecordOptional){
      recordInputs(
        record: $record,
        recordOptional: $optRecord,
        recordNested: { a: $record, b: $optRecord, c: $optRecord }
      )
    }"""
    let testInputObject = """{"a":"a","b":"b","c":"c"}"""
    let params' = variablesWithAllInputs(testInputObject, "null") |> paramsWithValues
    let result = sync <| schema.AsyncExecute(parse query, variables = params')
    match result with
    | Direct(data, errors) ->
        empty errors
    | _ -> fail "Expected a direct GQLResponse"

[<Fact>]
let ``Execute handles creation of input records from variables with mandatory only fields`` () =
    let query = """query ($record: InputRecord!){
      recordInputs(
        record: $record,
        recordNested: { a: $record }
      )
    }"""
    let testInputObject = """{"a":"a","b":"b","c":"c"}"""
    let params' = variablesWithAllInputs(testInputObject, "null") |> paramsWithValues
    let result = sync <| schema.AsyncExecute(parse query, variables = params')
    match result with
    | Direct(data, errors) ->
        empty errors
    | _ -> fail "Expected a direct GQLResponse"
