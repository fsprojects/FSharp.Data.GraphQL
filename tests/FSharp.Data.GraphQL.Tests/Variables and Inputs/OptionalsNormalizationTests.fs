// The MIT License (MIT)

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module FSharp.Data.GraphQL.Tests.OptionalsNormalizationTests

#nowarn "25"

open Xunit
open System
open System.Collections.Immutable
open System.Text.Json

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser

module Phantom =

    type ZipCode = interface end
    type City = interface end
    type State = interface end

    module Address =

        type Line1 = interface end
        type Line2 = interface end

open FSharp.Data.GraphQL.Tests.OptionalsNormalizationTests

type AddressLine1 = ValidString<Phantom.Address.Line1>
type AddressLine2 = ValidString<Phantom.Address.Line2>
type City = ValidString<Phantom.City>
type State = ValidString<Phantom.State>
type ZipCode = ValidString<Phantom.ZipCode>

type AddressRecord = {
    Line1: AddressLine1 voption
    Line2: AddressLine2 voption
    City: City voption
    State: State voption
    ZipCode: ZipCode voption
}

type AddressClass(zipCode, city, state, line1, line2) =
    member _.Line1 : AddressLine1 voption = line1
    member _.Line2 : AddressLine2 voption = line2
    member _.City : City voption = city
    member _.State : State voption = state
    member _.ZipCode : ZipCode voption = zipCode

[<Struct>]
type AddressStruct (
    zipCode : ZipCode voption,
    city : City voption,
    state : State voption,
    line1 : AddressLine1 voption,
    line2 : AddressLine2 voption
) =
    member _.Line1 = line1
    member _.Line2 = line2
    member _.City = city
    member _.State = state
    member _.ZipCode = zipCode


open Validus
open FSharp.Data.GraphQL.Tests.OptionalsNormalizationTests.String
open FSharp.Data.GraphQL.Tests.OptionalsNormalizationTests.Operators

[<RequireQualifiedAccess>]
module State =

    open ValidString
    open Validus.Operators

    let create : Validator<string, State> =
        (Check.String.lessThanLen 100 <+> validateStringCharacters) *|* ValidString

    let createOrWhitespace : Validator<string, State voption> =
        (allowEmpty ?=> (Check.String.lessThanLen 100 <+> validateStringCharacters)) *|* ValueOption.map ValidString

module Address =

    open ValidString
    open Validus.Operators

    let createLine1 : Validator<string, AddressLine1 voption> =
        (allowEmpty ?=> (Check.String.lessThanLen 1000 <+> validateStringCharacters)) *|* ValueOption.map ValidString

    let createLine2 : Validator<string, AddressLine2 voption> =
        (allowEmpty ?=> (Check.String.lessThanLen 1000 <+> validateStringCharacters)) *|* ValueOption.map ValidString

    let createZipCode : Validator<string, ZipCode voption> =
        (allowEmpty ?=> (Check.String.lessThanLen 100 <+> validateStringCharacters)) *|* ValueOption.map ValidString

    let createCity : Validator<string, City voption> =
        (allowEmpty ?=> (Check.String.lessThanLen 100 <+> validateStringCharacters)) *|* ValueOption.map ValidString


    open Scalars

    let Line1Type = Define.ValidStringScalar<AddressLine1>("AddressLine1", createLine1, ValidString.value, "Address line 1")
    let Line2Type = Define.ValidStringScalar<AddressLine2>("AddressLine2", createLine2, ValidString.value, "Address line 2")
    let ZipCodeType = Define.ValidStringScalar<ZipCode>("AddressZipCode", createZipCode, ValidString.value, "Address zip code")
    let CityType = Define.ValidStringScalar<City>("City", createCity, ValidString.value)
    let StateType = Define.ValidStringScalar<State>("State", State.createOrWhitespace, ValidString.value)

let InputAddressRecordType =
    Define.InputObject<AddressRecord>(
        name = "InputAddressRecord",
        fields = [
            Define.Input("line1", Nullable Address.Line1Type)
            Define.Input("line2", Nullable Address.Line2Type)
            Define.Input("zipCode", Nullable Address.ZipCodeType)
            Define.Input("city", Nullable Address.CityType)
            Define.Input("state", Nullable Address.StateType)
        ]
    )

let InputAddressClassType =
    Define.InputObject<AddressClass>(
        name = "InputAddressObject",
        fields = [
            Define.Input("line1", Nullable Address.Line1Type)
            Define.Input("line2", Nullable Address.Line2Type)
            Define.Input("zipCode", Nullable Address.ZipCodeType)
            Define.Input("city", Nullable Address.CityType)
            Define.Input("state", Nullable Address.StateType)
        ]
    )

let InputAddressStructType =
    Define.InputObject<AddressStruct>(
        name = "InputAddressStruct",
        fields = [
            Define.Input("line1", Nullable Address.Line1Type)
            Define.Input("line2", Nullable Address.Line2Type)
            Define.Input("zipCode", Nullable Address.ZipCodeType)
            Define.Input("city", Nullable Address.CityType)
            Define.Input("state", Nullable Address.StateType)
        ]
    )

open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Validation
open FSharp.Data.GraphQL.Validation.ValidationResult
open ErrorHelpers

let createSingleError message =
    [{ new IGQLError with member _.Message = message }]

type InputRecordNested = { HomeAddress : AddressRecord; WorkAddress : AddressRecord option; MailingAddress : AddressRecord voption }

let InputRecordNestedType =
    Define.InputObject<InputRecordNested> (
        "InputRecordNested",
        [ Define.Input ("homeAddress", InputAddressRecordType)
          Define.Input ("workAddress", Nullable InputAddressRecordType)
          Define.Input ("mailingAddress", Nullable InputAddressRecordType) ],
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

let schema =
    let schema =
        Schema (
            query =
                Define.Object (
                    "Query",
                    [ Define.Field (
                          "recordInputs",
                          StringType,
                          [ Define.Input ("record", InputAddressRecordType)
                            Define.Input ("recordOptional", Nullable InputAddressRecordType)
                            Define.Input ("recordNested", Nullable InputRecordNestedType) ],
                          stringifyInput
                      ) // TODO: add all args stringificaiton
                      Define.Field (
                          "objectInputs",
                          StringType,
                          [ Define.Input ("object", InputAddressClassType)
                            Define.Input ("objectOptional", Nullable InputAddressClassType) ],
                          stringifyInput
                      ) // TODO: add all args stringificaiton
                      Define.Field (
                          "structInputs",
                          StringType,
                          [ Define.Input ("struct", InputAddressStructType)
                            Define.Input ("structOptional", Nullable InputAddressStructType) ],
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
        record: { zipCode: "12345", city: "Miami" },
        recordOptional: { zipCode: "12345", city: "Miami" },
        recordNested: { homeAddress: { zipCode: "12345", city: "Miami" }, workAddress: { zipCode: "67890", city: "Miami" } }
      )
      objectInputs(
        object: { zipCode: "12345", city: "Miami" },
        objectOptional: { zipCode: "12345", city: "Miami" }
      )
      structInputs(
        struct: { zipCode: "12345", city: "Miami" },
        structOptional: { zipCode: "12345", city: "Miami" }
      )
    }"""
    let result = sync <| schema.AsyncExecute(parse query)
    ensureDirect result <| fun data errors -> empty errors

[<Fact>]
let ``Execute handles validation of valid inline input records with mandatory-only fields`` () =
    let query =
        """{
      recordInputs(
        record: { zipCode: "12345", city: "Miami" },
        recordNested: { homeAddress: { zipCode: "12345", city: "Miami" }, workAddress: { zipCode: "67890", city: "Miami" } }
      )
      objectInputs(
        object: { zipCode: "12345", city: "Miami" },
      )
      structInputs(
        struct: { zipCode: "12345", city: "Miami" },
      )
    }"""
    let result = sync <| schema.AsyncExecute(parse query)
    ensureDirect result <| fun data errors -> empty errors
