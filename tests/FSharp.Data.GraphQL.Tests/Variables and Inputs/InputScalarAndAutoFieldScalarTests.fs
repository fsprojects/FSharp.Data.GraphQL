// The MIT License (MIT)

module FSharp.Data.GraphQL.Tests.InputScalarAndAutoFieldScalarTests

#nowarn "25"

open Xunit
open System

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution

type InputRecord = InputRecordTests.InputRecord

// Nullable cannot be set into non nullable
let ``InputRecord Nullable to Non-nullable Type`` =
    Define.InputObject<InputRecord> (
        "InputRecordNullableToNonNullable",
        [ Define.Input ("a", StringType)
          Define.Input ("b", Nullable StringType)
          Define.Input ("c", Nullable StringType) ]
    )

type InputRecordOptional = InputRecordTests.InputRecordOptional

let ``InputRecord Non-nullable to Nullable Type`` =
    Define.InputObject<InputRecordOptional> (
        "InputRecordNonNullableToNullable",
        [ Define.Input ("a", StringType); Define.Input ("b", StringType); Define.Input ("c", StringType) ]
    )

let ``Record Non-nullable to Nullable Type`` =
    Define.Object<InputRecord> (
        "RecordNonNullableToNullable",
        [ Define.AutoField ("a", StringType)
          Define.AutoField ("b", Nullable StringType)
          Define.AutoField ("c", Nullable StringType) ]
    )

let ``Record Nullable to Nullable Type`` =
    Define.Object<InputRecordOptional> (
        "RecordOptionalNullableToNullable",
        [ Define.AutoField ("a", StringType)
          Define.AutoField ("b", Nullable StringType)
          Define.AutoField ("c", Nullable StringType) ]
    )

type InputObject = InputRecordTests.InputObject

let ``InputObject Nullable to Non-nullable Type`` =
    Define.InputObject<InputObject> (
        "InputObjectNullableToNonNullable",
        [ Define.Input ("a", StringType)
          Define.Input ("b", Nullable StringType)
          Define.Input ("c", Nullable StringType) ]
    )

type InputObjectOptional = InputRecordTests.InputObjectOptional

let ``InputObject Non-nullable to Nullable Type`` =
    Define.InputObject<InputObjectOptional> (
        "InputObjectNonNullableToNullable",
        [ Define.Input ("a", StringType); Define.Input ("b", StringType); Define.Input ("c", StringType) ]
    )

let ``Object Non-nullable to Nullable Type`` =
    Define.Object<InputObject> (
        "ObjectNonNullableToNullable",
        [ Define.Field ("a", StringType)
          Define.Field ("b", Nullable StringType)
          Define.Field ("c", Nullable StringType) ]
    )

let ``Object Nullable to Nullable Type`` =
    Define.Object<InputObjectOptional> (
        "ObjectNullableToNullable",
        [ Define.Field ("a", StringType)
          Define.Field ("b", Nullable StringType)
          Define.Field ("c", Nullable StringType) ]
    )

[<Fact>]
let ``Schema can be created for unmatched input nullable fields on record`` () =
    Schema (
        query =
            DefineRec.Object (
                "Query",
                fun () ->
                    [ Define.Field (
                            "rightRecord",
                            ``Record Nullable to Nullable Type``,
                            [ Define.Input ("record", ``InputRecord Non-nullable to Nullable Type``) ],
                            (fun ctx _ -> ctx.Arg<InputRecordOptional> ("record"))
                        ) ]
            )
    ) |> Executor :> obj

[<Fact>]
let ``Schema cannot be created for unmatched input nullable fields on record`` () =
    Assert.Throws<InvalidInputTypeException> (fun () ->
        Schema (
            query =
                Define.Object (
                    "Query",
                        [ Define.Field (
                                "wrongRecord",
                                StringType,
                                [ Define.Input ("record", ``InputRecord Nullable to Non-nullable Type``) ],
                                stringifyInput
                            ) ]
                )
        ) |> Executor :> obj
    )

[<Fact>]
let ``Schema can be created for matched input nullable fields on class`` () =
    Schema (
        query =
            Define.Object (
                "Query",
                    [ Define.Field (
                            "rightObject",
                            ``Object Nullable to Nullable Type``,
                            [ Define.Input ("object", ``InputObject Non-nullable to Nullable Type``) ],
                            (fun ctx _ -> ctx.Arg<InputObjectOptional> ("object"))
                        ) ]
            )
    ) |> Executor :> obj

[<Fact>]
let ``Schema cannot be created for unmatched input nullable fields on class`` () =
    Assert.Throws<InvalidInputTypeException> (fun () ->
        Schema (
            query =
                Define.Object (
                    "Query",
                        [ Define.Field (
                                "wrongObject",
                                StringType,
                                [ Define.Input ("object", ``InputObject Nullable to Non-nullable Type``) ],
                                stringifyInput
                            ) ]
                )
        ) |> Executor :> obj
    )


let schema =
    lazy
        let schema =
            Schema<unit> (
                query =
                    Define.Object (
                        "Query",
                            [
                              Define.Field (
                                  "record",
                                  ``Record Nullable to Nullable Type``,
                                  [ Define.Input ("record", ``InputRecord Non-nullable to Nullable Type``) ],
                                  (fun ctx _ -> ctx.Arg<InputRecordOptional> ("record"))
                              )
                              Define.Field (
                                  "object",
                                  ``Object Nullable to Nullable Type``,
                                  [ Define.Input ("object", ``InputObject Non-nullable to Nullable Type``) ],
                                  (fun ctx _ -> ctx.Arg<InputObjectOptional> ("object"))
                              ) ]
                    )
            )

        Executor schema

[<Fact>]
let ``Execute handles nullable auto-fields in input and output record fields coercion`` () =
    let query =
        """{
      record(record: { a: "a", b: "b", c: "c" }) {
        a
        b
        c
      }
    }"""

    let result = sync <| schema.Value.AsyncExecute (parse query)
    let expected =
        NameValueLookup.ofList [ "record", upcast NameValueLookup.ofList [ "a", "a" :> obj; "b", "b"; "c", "c"] ]

    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)

[<Fact>]
let ``Execute handles nullable auto-fields in input and output object fields coercion`` () =
    let query =
        """{
      record(record: { a: "a", b: "b", c: "c" }) {
        a
        b
        c
      }
    }"""

    let result = sync <| schema.Value.AsyncExecute (parse query)
    let expected =
        NameValueLookup.ofList [ "record", upcast NameValueLookup.ofList [ "a", "a" :> obj; "b", "b"; "c", "c" ] ]

    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)

