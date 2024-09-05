// The MIT License (MIT)

module FSharp.Data.GraphQL.Tests.InputObjectValidatorTests

#nowarn "25"

open Xunit
open System
open System.Collections.Immutable
open System.Text.Json

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Validation
open FSharp.Data.GraphQL.Validation.ValidationResult
open ErrorHelpers

type InputRecord = { Country : string; ZipCode : string; City : string }

let createSingleError message =
    [{ new IGQLError with member _.Message = message }]

let InputRecordType =
    Define.InputObject<InputRecord> (
        "InputRecord",
        [ Define.Input ("country", StringType); Define.Input ("zipCode", StringType); Define.Input ("city", StringType) ],
        fun inputRecord ->
            match inputRecord.Country with
            | "US" when inputRecord.ZipCode.Length <> 5 -> ValidationError <| createSingleError "ZipCode must be 5 characters for US"
            | "US" -> Success
            | _ -> ValidationError <| createSingleError "Unsupported country"
    )

type InputRecordNested = { HomeAddress : InputRecord; WorkAddress : InputRecord option; MailingAddress : InputRecord voption }

let InputRecordNestedType =
    Define.InputObject<InputRecordNested> (
        "InputRecordNested",
        [ Define.Input ("homeAddress", InputRecordType)
          Define.Input ("workAddress", Nullable InputRecordType)
          Define.Input ("mailingAddress", Nullable InputRecordType) ],
        fun inputRecord ->
            match inputRecord.MailingAddress, inputRecord.WorkAddress with
            | ValueNone, None -> ValidationError <| createSingleError "MailingAddress or WorkAddress must be provided"
            | _ -> Success
            @@
            if inputRecord.MailingAddress.IsSome && inputRecord.HomeAddress = inputRecord.MailingAddress.Value then
                ValidationError <| createSingleError "HomeAddress and MailingAddress must be different"
            else
                Success
    )

type InputObject (country : string, zipCode : string, city : string) =
    member val Country = country
    member val ZipCode = zipCode
    member val City = city

let InputObjectType =
    Define.InputObject<InputObject> (
        "InputObject",
        [ Define.Input ("country", StringType); Define.Input ("zipCode", StringType); Define.Input ("city", StringType) ],
        fun (inputObject: InputObject) ->
            match inputObject.Country with
            | "US" when inputObject.ZipCode.Length <> 5 -> ValidationError <| createSingleError "ZipCode must be 5 characters for US"
            | "US" -> Success
            | _ -> ValidationError <| createSingleError "Unsupported country"
    )

type InputObjectNested (homeAddress : InputObject, workAddress : InputObject voption, ?mailingAddress : InputObject) =
    member val HomeAddress = homeAddress
    member val WorkAddress = workAddress
    member val MailingAddress = mailingAddress

let InputObjectNestedType =
    Define.InputObject<InputObjectNested> (
        "InputObjectOptional",
        [ Define.Input ("homeAddress", InputObjectType)
          Define.Input ("workAddress", Nullable InputObjectType)
          Define.Input ("mailingAddress", Nullable InputObjectType) ],
        fun (inputObject: InputObjectNested) ->
            match inputObject.MailingAddress, inputObject.WorkAddress with
            | None, ValueNone -> ValidationError <| createSingleError "MailingAddress or WorkAddress must be provided"
            | _ -> Success
            @@
            if inputObject.MailingAddress.IsSome && inputObject.HomeAddress = inputObject.MailingAddress.Value then
                ValidationError <| createSingleError "HomeAddress and MailingAddress must be different"
            else
                Success

    )

let schema =
    let schema =
        Schema (
            query =
                DefineRec.Object (
                    "Query",
                    fun () ->
                        [ Define.Field (
                              "recordInputs",
                              StringType,
                              [ Define.Input ("record", InputRecordType)
                                Define.Input ("recordNested", Nullable InputRecordNestedType) ],
                              stringifyInput
                          ) // TODO: add all args stringificaiton
                          Define.Field (
                              "objectInputs",
                              StringType,
                              [ Define.Input ("object", InputObjectType)
                                Define.Input ("objectNested", Nullable InputObjectNestedType) ],
                              stringifyInput
                          ) ] // TODO: add all args stringificaiton
                )
        )

    Executor schema

[<Fact>]
let ``Execute handles validation of valid inline input records with all fields`` () =
    let query =
        """{
      recordInputs(
        record: { country: "US", zipCode: "12345", city: "Miami" },
        recordNested: { homeAddress: { country: "US", zipCode: "12345", city: "Miami" }, workAddress: { country: "US", zipCode: "67890", city: "Miami" } }
      )
    }"""
    let result = sync <| schema.AsyncExecute(parse query)
    ensureDirect result <| fun data errors -> empty errors

[<Fact>]
let ``Execute handles validation of invalid inline input records with all fields`` () =
    let query =
        """{
      recordInputs(
        record: { country: "US", zipCode: "", city: "Miami" },
        recordNested: { homeAddress: { country: "US", zipCode: "12345", city: "Miami" }, workAddress: { country: "US", zipCode: "67890", city: "Miami" }, mailingAddress: { country: "US", zipCode: "12345", city: "Miami" } }
      )
    }"""
    let result = sync <| schema.AsyncExecute(parse query)
    match result with
    | RequestError [ zipCodeError ; addressError ] ->
        zipCodeError |> ensureInputObjectValidationError (Argument "record") "ZipCode must be 5 characters for US" [] "InputRecord!"
        addressError |> ensureInputObjectValidationError (Argument "recordNested") "HomeAddress and MailingAddress must be different" [] "InputRecordNested"
    | response -> fail $"Expected 'RequestError' GQLResponse but got\n{response}"


let variablesWithAllInputs (record, record1, record2, record3) =
    $"""
    {{
        "record":%s{record},
        "recordNested": {{ "homeAddress": %s{record1}, "workAddress": %s{record2}, "mailingAddress": %s{record3} }}
    }}
"""

let paramsWithValues variables =
    JsonDocument
        .Parse(variables : string)
        .RootElement.Deserialize<ImmutableDictionary<string, JsonElement>> (serializerOptions)

[<Fact>]
let ``Execute handles validation of valid input records from variables with all fields`` () =
    let query =
        """query ($record: InputRecord!, $recordNested: InputRecordNested){
      recordInputs(
        record: $record,
        recordNested: $recordNested
      )
    }"""
    let params' =
        variablesWithAllInputs(
            """{ "country": "US", "zipCode": "12345", "city": "Miami" }""",
            """{ "country": "US", "zipCode": "12345", "city": "Miami" }""",
            """{ "country": "US", "zipCode": "67890", "city": "Miami" }""",
            """null"""
        ) |> paramsWithValues
    let result = sync <| schema.AsyncExecute(parse query, variables = params')
    //let expected = NameValueLookup.ofList [ "recordInputs", upcast testInputObject ]
    ensureDirect result <| fun data errors ->
        empty errors
        //data |> equals (upcast expected)

[<Fact>]
let ``Execute handles validation of invalid input records from variables with all fields`` () =
    let query =
        """query ($record: InputRecord!, $recordNested: InputRecordNested){
      recordInputs(
        record: $record,
        recordNested: $recordNested
      )
    }"""
    let params' =
        variablesWithAllInputs(
            """{ "country": "US", "zipCode": "", "city": "Miami" }""",
            """{ "country": "US", "zipCode": "12345", "city": "Miami" }""",
            """{ "country": "US", "zipCode": "67890", "city": "Miami" }""",
            """{ "country": "US", "zipCode": "12345", "city": "Miami" }"""
        ) |> paramsWithValues
    let result = sync <| schema.AsyncExecute(parse query, variables = params')
    //let expected = NameValueLookup.ofList [ "recordInputs", upcast testInputObject ]
    ensureRequestError result <| fun [ zipCodeError ; addressError ] ->
        zipCodeError |> ensureInputObjectValidationError (Variable "record") "ZipCode must be 5 characters for US" [ box "mailingAddress" ] "InputRecord"
        addressError |> ensureInputObjectValidationError (Variable "recordNested") "HomeAddress and MailingAddress must be different" [] "InputRecordNested"

let variablesWithAllNestedInputs (record1, record2, record3) =
    $"""
    {{
        "record1":%s{record1},
        "record2":%s{record2},
        "record3":%s{record3}
    }}
"""

[<Fact>]
let ``Execute handles validation of valid input records from variables with all nested fields`` () =
    let query =
        """query ($record1: InputRecord!, $record2: InputRecord!, $record3: InputRecord!){
      recordInputs(
        record: $record1,
        recordNested: { homeAddress: $record1, workAddress: $record2, mailingAddress: $record3 }
      )
    }"""
    let params' =
        variablesWithAllNestedInputs(
            """{ "country": "US", "zipCode": "12345", "city": "Miami" }""",
            """{ "country": "US", "zipCode": "67890", "city": "Miami" }""",
            """null"""
        ) |> paramsWithValues
    let result = sync <| schema.AsyncExecute(parse query, variables = params')
    //let expected = NameValueLookup.ofList [ "recordInputs", upcast testInputObject ]
    ensureDirect result <| fun data errors ->
        empty errors
        //data |> equals (upcast expected)

[<Fact>]
let ``Execute handles validation of invalid input records from variables with all nested fields`` () =
    let query =
        """query ($record1: InputRecord!, $record2: InputRecord, $record3: InputRecord){
      recordInputs(
        record: $record1,
        recordNested: { homeAddress: $record2, workAddress: $record3, mailingAddress: $record2 }
      )
    }"""
    let params' =
        variablesWithAllNestedInputs(
            """{ "country": "US", "zipCode": "", "city": "Miami" }""",
            """{ "country": "US", "zipCode": "12345", "city": "Miami" }""",
            """{ "country": "US", "zipCode": "67890", "city": "Miami" }"""
        ) |> paramsWithValues
    let result = sync <| schema.AsyncExecute(parse query, variables = params')
    //let expected = NameValueLookup.ofList [ "recordInputs", upcast testInputObject ]
    ensureRequestError result <| fun [ error ] ->
        error |> ensureInputObjectValidationError (Variable "record1") "ZipCode must be 5 characters for US" [ box "mailingAddress" ] "InputRecord"
        // Because all variables are coerced together validation of the inline object that contains variables do not happen
        // as total variables coercion failed
        //hasError "Object 'Query': field 'recordInputs': argument 'recordNested': HomeAddress and MailingAddress must be different" errors
