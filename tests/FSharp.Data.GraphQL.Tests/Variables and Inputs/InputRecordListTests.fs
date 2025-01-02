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
                          [ Define.Input ("records", ListOf InputRecordType)
                            Define.Input ("recordsOptional", Nullable (ListOf (Nullable InputRecordOptionalType)))
                            Define.Input ("recordsNested", ListOf (Nullable InputRecordNestedType)) ],
                            (fun ctx name ->
                                let _ = ctx.Arg<InputRecord list> "records"
                                let _ = ctx.TryArg<list<InputRecordOptional option>> "recordsOptional"
                                let recordNested =
                                    ctx.Arg<list<InputRecordNested option>> "recordsNested"
                                    |> List.tryHead
                                    |> Option.flatten
                                match verify, recordNested with
                                | Nothing, _ -> ()
                                | AllInclude, Some recordNested -> recordNested.s |> ValueOption.iter _.VerifyAllInclude
                                | AllSkip, Some recordNested -> recordNested.s |> ValueOption.iter _.VerifyAllSkip
                                | SkipAndIncludeNull, Some recordNested -> recordNested.s |> ValueOption.iter _.VerifySkipAndIncludeNull
                                | _ -> ()
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
        records: [],
        recordsOptional: [],
        recordsNested: []
      )
    }"""
    let result = sync <| (schema AllInclude).AsyncExecute(parse query)
    ensureDirect result <| fun data errors -> empty errors

[<Fact>]
let ``Execute handles creation of inline input records list with all fields`` () =
    let query =
        """{
      recordInputs(
        records: [{ a: "a", b: "b", c: "c" }],
        recordsOptional: [{ a: "a", b: "b", c: "c" }],
        recordsNested: [{
          a: { a: "a", b: "b", c: "c" },
          b: { a: "a", b: "b", c: "c" },
          c: { a: "a", b: "b", c: "c" },
          s: { a: "a", b: "b", c: "c" },
          l: [{ a: "a", b: "b", c: "c" }]
        }]
      )
    }"""
    let result = sync <| (schema AllInclude).AsyncExecute(parse query)
    ensureDirect result <| fun data errors -> empty errors

[<Fact>]
let ``Execute handles creation of inline input records list with optional null fields`` () =
    let query =
        """{
      recordInputs(
        records: [{ a: "a", b: "b", c: "c" }],
        recordsOptional: [null],
        recordsNested: [{ a: { a: "a", b: "b", c: "c" }, b: null, c: null, s: null, l: [] }]
      )
    }"""
    let result = sync <| (schema Nothing).AsyncExecute(parse query)
    ensureDirect result <| fun data errors -> empty errors

[<Fact>]
let ``Execute handles creation of inline input records list with mandatory only fields`` () =
    let query =
        """{
      recordInputs(
        records: [{ a: "a", b: "b", c: "c" }],
        recordsNested: [{ a: { a: "a", b: "b", c: "c" }, l: [{ a: "a", b: "b", c: "c" }] }]
      )
    }"""
    let result = sync <| (schema Nothing).AsyncExecute(parse query)
    ensureDirect result <| fun data errors -> empty errors

let variablesWithAllInputs (record, optRecord, skippable) =
    $"""
    {{
        "records":[%s{record}],
        "optRecords":[%s{optRecord}],
        "nestedRecords":[ {{ "a": {record}, "b": {optRecord}, "c": {optRecord}, "s": {skippable}, "l": [{record}] }}]
    }}
"""

let paramsWithValues variables =
    JsonDocument
        .Parse(variables : string)
        .RootElement.Deserialize<ImmutableDictionary<string, JsonElement>> (serializerOptions)

[<Fact>]
let ``Execute handles creation of input records list from variables with all fields`` () =
    let query =
        """query (
            $records: [InputRecord!]!,
            $optRecords: [InputRecordOptional],
            $nestedRecords: [InputRecordNested]!
      ) {
      recordInputs(
        records: $records,
        recordsOptional: $optRecords,
        recordsNested: $nestedRecords
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
        """query (
            $records: [InputRecord!]!,
            $optRecords: [InputRecordOptional],
            $nestedRecords: [InputRecordNested]!
      ) {
      recordInputs(
        records: $records,
        recordsOptional: $optRecords,
        recordsNested: $nestedRecords
      )
    }"""
    let testInputObject = """{"a":"a","b":"b","c":"c"}"""
    let testInputSkippable = """{ "a": null, "b": null, "c": null }"""
    let params' = variablesWithAllInputs(testInputObject, "null", testInputSkippable) |> paramsWithValues
    let result = sync <| (schema SkipAndIncludeNull).AsyncExecute(parse query, variables = params')
    ensureDirect result <| fun data errors -> empty errors

[<Fact>]
let ``Execute handles creation of input records from variables with mandatory only fields`` () =
    let variablesWithAllInputs (record) =
        $"""
        {{
            "record":%s{record},
            "list":[%s{record}]
        }}
    """

    let query =
        """query ($record: InputRecord!, $list: [InputRecord!]!){
      recordInputs(
        records: [$record],
        recordsNested: [{ a: $record, l: $list }]
      )
    }"""
    let testInputObject = """{"a":"a","b":"b","c":"c"}"""
    let params' = variablesWithAllInputs testInputObject |> paramsWithValues
    let result = sync <| (schema AllSkip).AsyncExecute(parse query, variables = params')
    ensureDirect result <| fun data errors -> empty errors
