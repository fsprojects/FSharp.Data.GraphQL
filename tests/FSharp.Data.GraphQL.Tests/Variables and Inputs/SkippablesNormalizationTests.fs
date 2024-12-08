// The MIT License (MIT)

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module FSharp.Data.GraphQL.Tests.SkippablesNormalizationTests

#nowarn "25"

open Xunit
open System
open System.Collections.Immutable
open System.Text.Json
open System.Text.Json.Serialization

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser

open FSharp.Data.GraphQL.Tests.OptionalsNormalizationTests

type AddressRecord = {
    Line1: AddressLine1 Skippable
    Line2: AddressLine2 voption Skippable
    City: City Skippable
    State: State Skippable
    ZipCode: ZipCode Skippable
} with
    member r.VerifyAllSkip =
        Assert.True r.Line1.isSkip
        Assert.True r.Line2.isSkip
        Assert.True r.City.isSkip
        Assert.True r.State.isSkip
        Assert.True r.ZipCode.isSkip
    member r.VerifySkipAndIncludeNull =
        Assert.True r.Line1.isSkip
        match r.Line2 with
        | Skip -> fail "Expected Line2 to be 'Include ValueNone'"
        | Include ValueNone -> ()
        | Include _ -> fail "Expected Line2 to be 'Include ValueNone'"
        Assert.True r.City.isSkip
        Assert.True r.State.isSkip
        Assert.True r.ZipCode.isSkip

type AddressClass(zipCode, city, state, line1, line2) =
    member _.Line1 : AddressLine1 Skippable = line1
    member _.Line2 : AddressLine2 voption Skippable = line2
    member _.City : City Skippable = city
    member _.State : State Skippable = state
    member _.ZipCode : ZipCode Skippable = zipCode

    member _.VerifyAllSkip =
        Assert.True line1.isSkip
        Assert.True line2.isSkip
        Assert.True city.isSkip
        Assert.True state.isSkip
        Assert.True zipCode.isSkip
    member _.VerifySkipAndIncludeNull =
        Assert.True line1.isSkip
        match line2 with
        | Skip -> fail "Expected Line2 to be 'Include ValueNone'"
        | Include ValueNone -> ()
        | Include _ -> fail "Expected Line2 to be 'Include ValueNone'"
        Assert.True city.isSkip
        Assert.True state.isSkip
        Assert.True zipCode.isSkip

[<Struct>]
type AddressStruct (
    zipCode : ZipCode Skippable,
    city : City Skippable,
    state : State Skippable,
    line1 : AddressLine1 Skippable,
    line2 : AddressLine2 voption Skippable
) =
    member _.Line1 = line1
    member _.Line2 = line2
    member _.City = city
    member _.State = state
    member _.ZipCode = zipCode

    member _.VerifyAllSkip =
        Assert.True line1.isSkip
        Assert.True line2.isSkip
        Assert.True city.isSkip
        Assert.True state.isSkip
        Assert.True zipCode.isSkip
    member _.VerifySkipAndIncludeNull =
        Assert.True line1.isSkip
        match line2 with
        | Skip -> fail "Expected Line2 to be 'Include ValueNone'"
        | Include ValueNone -> ()
        | Include _ -> fail "Expected Line2 to be 'Include ValueNone'"
        Assert.True city.isSkip
        Assert.True state.isSkip
        Assert.True zipCode.isSkip


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

    let createLine1 : Validator<string, AddressLine1> =
        (Check.String.lessThanLen 1000 <+> validateStringCharacters) *|* ValidString

    let createLine2 : Validator<string, AddressLine2 voption> =
        (allowEmpty ?=> (Check.String.lessThanLen 1000 <+> validateStringCharacters)) *|* ValueOption.map ValidString

    let createZipCode : Validator<string, ZipCode> =
        (Check.String.lessThanLen 100 <+> validateStringCharacters) *|* ValidString

    let createCity : Validator<string, City> =
        (Check.String.lessThanLen 100 <+> validateStringCharacters) *|* ValidString


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
            Define.SkippableInput("line1", Address.Line1Type)
            Define.SkippableInput("line2", Nullable Address.Line2Type)
            Define.SkippableInput("zipCode", Address.ZipCodeType)
            Define.SkippableInput("city", Address.CityType)
            Define.SkippableInput("state", Address.StateType)
        ]
    )

let InputAddressClassType =
    Define.InputObject<AddressClass>(
        name = "InputAddressObject",
        fields = [
            Define.SkippableInput("line1", Address.Line1Type)
            Define.SkippableInput("line2", Nullable Address.Line2Type)
            Define.SkippableInput("zipCode", Address.ZipCodeType)
            Define.SkippableInput("city", Address.CityType)
            Define.SkippableInput("state", Address.StateType)
        ]
    )

let InputAddressStructType =
    Define.InputObject<AddressStruct>(
        name = "InputAddressStruct",
        fields = [
            Define.SkippableInput("line1", Address.Line1Type)
            Define.SkippableInput("line2", Nullable Address.Line2Type)
            Define.SkippableInput("zipCode", Address.ZipCodeType)
            Define.SkippableInput("city", Address.CityType)
            Define.SkippableInput("state", Address.StateType)
        ]
    )

open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Validation
open FSharp.Data.GraphQL.Validation.ValidationResult
open ErrorHelpers

let createSingleError message =
    [{ new IGQLError with member _.Message = message }]

type InputRecordNested = { HomeAddress : AddressRecord; WorkAddress : AddressRecord Skippable; MailingAddress : AddressRecord Skippable }

let InputRecordNestedType =
    Define.InputObject<InputRecordNested> (
        "InputRecordNested",
        [ Define.Input ("homeAddress", InputAddressRecordType)
          Define.SkippableInput ("workAddress", InputAddressRecordType)
          Define.SkippableInput ("mailingAddress", InputAddressRecordType) ],
        fun inputRecord ->
            match inputRecord.MailingAddress, inputRecord.WorkAddress with
            | Skip, Skip -> ValidationError <| createSingleError "MailingAddress or WorkAddress must be provided"
            | _ -> Success
            @@
            if inputRecord.MailingAddress.isInclude && inputRecord.HomeAddress = (inputRecord.MailingAddress |> Skippable.toValueOption).Value then
                ValidationError <| createSingleError "HomeAddress and MailingAddress must be different"
            else
                Success
    )

type Verify =
    | Nothing
    | Skip
    | SkipAndIncludeNull

let schema verify =
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
                            (fun ctx name ->
                                let record = ctx.Arg<AddressRecord>("record")
                                let recordOptional = ctx.TryArg<AddressRecord>("recordOptional")
                                let recordNested = ctx.Arg<InputRecordNested>("recordNested")
                                match verify with
                                | Nothing -> ()
                                | Skip ->
                                    record.VerifyAllSkip
                                    recordOptional |> Option.iter _.VerifyAllSkip
                                    recordNested.HomeAddress.VerifyAllSkip
                                | SkipAndIncludeNull ->
                                    record.VerifySkipAndIncludeNull
                                    recordOptional |> Option.iter _.VerifySkipAndIncludeNull
                                    recordNested.HomeAddress.VerifySkipAndIncludeNull
                                stringifyInput ctx name
                            )
                      ) // TODO: add all args stringificaiton
                      Define.Field (
                          "objectInputs",
                          StringType,
                          [ Define.Input ("object", InputAddressClassType)
                            Define.Input ("objectOptional", Nullable InputAddressClassType) ],
                            (fun ctx name ->
                                let obj = ctx.Arg<AddressClass>("object")
                                let objOptional = ctx.TryArg<AddressClass>("objectOptional")
                                match verify with
                                | Nothing -> ()
                                | Skip ->
                                    obj.VerifyAllSkip
                                    objOptional |> Option.iter _.VerifyAllSkip
                                | SkipAndIncludeNull ->
                                    obj.VerifySkipAndIncludeNull
                                    objOptional |> Option.iter _.VerifySkipAndIncludeNull
                                stringifyInput ctx name
                            )
                      ) // TODO: add all args stringificaiton
                      Define.Field (
                          "structInputs",
                          StringType,
                          [ Define.Input ("struct", InputAddressStructType)
                            Define.Input ("structOptional", Nullable InputAddressStructType) ],
                            (fun ctx name ->
                                let obj = ctx.Arg<AddressStruct>("struct")
                                let objOptional = ctx.TryArg<AddressStruct>("structOptional")
                                match verify with
                                | Nothing -> ()
                                | Skip ->
                                    obj.VerifyAllSkip
                                    objOptional |> Option.iter _.VerifyAllSkip
                                | SkipAndIncludeNull ->
                                    obj.VerifySkipAndIncludeNull
                                    objOptional |> Option.iter _.VerifySkipAndIncludeNull
                                stringifyInput ctx name
                            )
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
    let result = sync <| (schema Nothing).AsyncExecute(parse query)
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
    let result = sync <| (schema Nothing).AsyncExecute(parse query)
    ensureDirect result <| fun data errors -> empty errors

[<Fact>]
let ``Execute handles validation of valid inline input records with null mandatory skippable fields`` () =
    let query =
        """{
      recordInputs(
        record: { zipCode: null, city: null },
        recordOptional: { zipCode: null, city: null },
        recordNested: { homeAddress: { zipCode: null, city: null }, workAddress: { zipCode: null, city: null } }
      )
      objectInputs(
        object: { zipCode: null, city: null },
        objectOptional: { zipCode: null, city: null }
      )
      structInputs(
        struct: { zipCode: null, city: null },
        structOptional: { zipCode: null, city: null }
      )
    }"""
    let result = sync <| (schema Skip).AsyncExecute(parse query)
    ensureDirect result <| fun data errors -> empty errors

[<Fact>]
let ``Execute handles validation of valid inline input records with null optional field`` () =
    let query =
        """{
      recordInputs(
        record: { line2: null },
        recordOptional: { line2: null },
        recordNested: { homeAddress: { line2: null }, workAddress: { line2: null }, mailingAddress: { line1: "", line2: null } }
      )
      objectInputs(
        object: { line2: null },
        objectOptional: { line2: null }
      )
      structInputs(
        struct: { line2: null },
        structOptional: { line2: null }
      )
    }"""
    let result = sync <| (schema SkipAndIncludeNull).AsyncExecute(parse query)
    ensureDirect result <| fun data errors -> empty errors
