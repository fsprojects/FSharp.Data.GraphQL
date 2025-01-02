// The MIT License (MIT)

module FSharp.Data.GraphQL.Tests.InputRecordListTests

#nowarn "25"

open Xunit
open System.Collections.Immutable
open System.Text.Json
open System.Text.Json.Serialization

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Tests.InputRecordTests

let schema verify =
    let schema =
        Schema (
            query =
                Define.Object (
                    "Query",
                    [ Define.Field (
                          "recordInputs",
                          StringType,
                          [ Define.Input ("record", ListOf InputRecordType)
                            Define.Input ("recordOptional",ListOf (Nullable InputRecordOptionalType))
                            Define.Input ("recordNested",ListOf (Nullable InputRecordNestedType)) ],
                            (fun ctx name ->
                                let recordNested = ctx.Arg<InputRecordNested> "recordNested"
                                match verify with
                                | Nothing -> ()
                                | AllInclude -> recordNested.s |> ValueOption.iter _.VerifyAllInclude
                                | AllSkip -> recordNested.s |> ValueOption.iter _.VerifyAllSkip
                                | SkipAndIncludeNull -> recordNested.s |> ValueOption.iter _.VerifySkipAndIncludeNull
                                stringifyInput ctx name
                            )
                      ) // TODO: add all args stringificaiton
                      Define.Field (
                          "objectInputs",
                          StringType,
                          [ Define.Input ("object", InputObjectType)
                            Define.Input ("objectOptional", Nullable InputObjectOptionalType) ],
                          stringifyInput
                      ) ] // TODO: add all args stringificaiton
                )
        )

    Executor schema

[<Fact>]
let ``Execute handles creation of inline empty input records list`` () =
    let query =
        """{
      recordInputs(
        record: [],
        recordOptional: [],
        recordNested: []       
      )
    }"""
    let result = sync <| (schema AllInclude).AsyncExecute(parse query)
    ensureDirect result <| fun data errors -> empty errors

[<Fact>]
let ``Execute handles creation of inline input records list with all fields`` () =
    let query =
        """{
      recordInputs(
        record: [{ a: "a", b: "b", c: "c" }],
        recordOptional: [{ a: "a", b: "b", c: "c" }],
        recordNested: [
          a: { a: "a", b: "b", c: "c" },
          b: { a: "a", b: "b", c: "c" },
          c: { a: "a", b: "b", c: "c" },
          s: { a: "a", b: "b", c: "c" },
          l: [{ a: "a", b: "b", c: "c" }]
        ]
      )
    }"""
    let result = sync <| (schema AllInclude).AsyncExecute(parse query)
    ensureDirect result <| fun data errors -> empty errors

[<Fact>]
let ``Execute handles creation of inline input records list with optional null fields`` () =
    let query =
        """{
      recordInputs(
        record: [{ a: "a", b: "b", c: "c" }],
        recordOptional: [null],
        recordNested: [{ a: { a: "a", b: "b", c: "c" }, b: null, c: null, s: null, l: [] }]
      )
    }"""
    let result = sync <| (schema Nothing).AsyncExecute(parse query)
    ensureDirect result <| fun data errors -> empty errors

[<Fact>]
let ``Execute handles creation of inline input records list with mandatory only fields`` () =
    let query =
        """{
      recordInputs(
        record: [{ a: "a", b: "b", c: "c" }],
        recordNested: [{ a: { a: "a", b: "b", c: "c" }, l: [{ a: "a", b: "b", c: "c" }] }]
      )
    }"""
    let result = sync <| (schema Nothing).AsyncExecute(parse query)
    ensureDirect result <| fun data errors -> empty errors

let variablesWithAllInputs (record, optRecord, skippable) =
    $"""
    [{{
        "record":%s{record},
        "optRecord":%s{optRecord},
        "skippable": %s{skippable},
        "list":[%s{record}]
    }}]
"""

let paramsWithValues variables =
    JsonDocument
        .Parse(variables : string)
        .RootElement.Deserialize<ImmutableDictionary<string, JsonElement>> (serializerOptions)

[<Fact>]
let ``Execute handles creation of input records list from variables with all fields`` () =
    let query =
        """query ($record: InputRecord!, $optRecord: InputRecordOptional, $skippable: InputRecordSkippable, $list: [InputRecord!]!){
      recordInputs(
        record: $record,
        recordOptional: $optRecord,
        recordNested: [{ a: $record, b: $optRecord, c: $optRecord, s: $skippable, l: $list }]
      )
    }"""
    let testInputObject = """{"a":"a","b":"b","c":"c"}"""
    let params' =
        variablesWithAllInputs(testInputObject, testInputObject, testInputObject) |> paramsWithValues
    let result = sync <| (schema AllInclude).AsyncExecute(parse query, variables = params')
    //let expected = NameValueLookup.ofList [ "recordInputs", upcast testInputObject ]
    ensureDirect result <| fun data errors ->
        empty errors
        //data |> equals (upcast expected)

[<Fact>]
let ``Execute handles creation of input records list from variables with optional null fields`` () =
    let query =
        """query ($record: InputRecord!, $optRecord: InputRecordOptional, $skippable: InputRecordSkippable, $list: [InputRecord!]!){
      recordInputs(
        record: $record,
        recordOptional: $optRecord,
        recordNested: [{ a: $record, b: $optRecord, c: $optRecord, s: $skippable, l: $list }]
      )
    }"""
    let testInputObject = """{"a":"a","b":"b","c":"c"}"""
    let testInputSkippable = """{ "a": null, "b": null, "c": null }"""
    let params' = variablesWithAllInputs(testInputObject, "null", testInputSkippable) |> paramsWithValues
    let result = sync <| (schema SkipAndIncludeNull).AsyncExecute(parse query, variables = params')
    ensureDirect result <| fun data errors -> empty errors

[<Fact>]
let ``Execute handles creation of input records from variables with mandatory only fields`` () =
    let query =
        """query ($record: InputRecord!, $list: [InputRecord!]!){
      recordInputs(
        record: $record,
        recordNested: [{ a: $record, l: $list }]
      )
    }"""
    let testInputObject = """{"a":"a","b":"b","c":"c"}"""
    let params' = variablesWithAllInputs(testInputObject, "null", "{}") |> paramsWithValues
    let result = sync <| (schema AllSkip).AsyncExecute(parse query, variables = params')
    ensureDirect result <| fun data errors -> empty errors
