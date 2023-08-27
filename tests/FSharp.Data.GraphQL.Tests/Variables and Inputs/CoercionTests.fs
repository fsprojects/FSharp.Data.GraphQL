// The MIT License (MIT)
/// Copyright (c) 2015-Mar 2016 Kevin Thompson @kthompson
// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.CoercionTests

#nowarn "25"

open System
open System.Text.Json
open Xunit
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns

let private testCoercion graphQLType (expected: 't) actual =
    let (Scalar scalar) = graphQLType
    let result = (scalar.CoerceInput actual) |> Result.map (fun x -> downcast x)
    match result with
    | Ok x -> equals expected x
    | Error _ -> Assert.Fail $"Expected %A{actual} to be able to be coerced to %A{expected}"

let private testCoercionError graphQLType (expectedErrorMeesage: string) actual =
    let (Scalar scalar) = graphQLType
    let result = (scalar.CoerceInput actual) |> Result.map (fun x -> downcast x)
    match result with
    | Ok _ -> Assert.Fail($"Expected %A{actual} to not be able to be coerced to %A{graphQLType.Type.Name}")
    | Error errs -> equals expectedErrorMeesage errs.Head.Message


[<Fact>]
let ``Int coerces input`` () =
    testCoercion IntType 123 (Variable (JsonDocument.Parse "123").RootElement)
    testCoercion IntType 123 (InlineConstant (IntValue 123L))
    testCoercionError IntType "JSON value '123.4' of kind 'Number' cannot be deserialized into integer of range from -2147483648 to 2147483647" (Variable (JsonDocument.Parse "123.4").RootElement)
    testCoercionError IntType "Inline value '123,4' of type float cannot be converted into integer of range from -2147483648 to 2147483647" (InlineConstant (FloatValue 123.4))
    testCoercion IntType 1 (Variable (JsonDocument.Parse "true").RootElement)
    testCoercion IntType 1 (InlineConstant (BooleanValue true))
    testCoercion IntType 0 (Variable (JsonDocument.Parse "false").RootElement)
    testCoercion IntType 0 (InlineConstant (BooleanValue false))
    testCoercionError IntType "JSON value 'enum' of kind 'String' cannot be deserialized into integer of range from -2147483648 to 2147483647" (Variable (JsonDocument.Parse "\"enum\"").RootElement)
    testCoercionError IntType "Inline value 'enum' of type enum cannot be converted into integer of range from -2147483648 to 2147483647" (InlineConstant (EnumValue "enum"))

[<Fact>]
let ``Long coerces input`` () =
    testCoercion LongType 123L (Variable (JsonDocument.Parse "123").RootElement)
    testCoercion LongType 123L (InlineConstant (IntValue 123L))
    testCoercionError LongType "JSON value '123.4' of kind 'Number' cannot be deserialized into integer of range from -9223372036854775808 to 9223372036854775807" (Variable (JsonDocument.Parse "123.4").RootElement)
    testCoercionError LongType "Inline value '123,4' of type float cannot be converted into integer of range from -9223372036854775808 to 9223372036854775807" (InlineConstant (FloatValue 123.4))
    testCoercion LongType 1L (Variable (JsonDocument.Parse "true").RootElement)
    testCoercion LongType 1L (InlineConstant (BooleanValue true))
    testCoercion LongType 0L (Variable (JsonDocument.Parse "false").RootElement)
    testCoercion LongType 0L (InlineConstant (BooleanValue false))
    testCoercionError LongType "JSON value 'enum' of kind 'String' cannot be deserialized into integer of range from -9223372036854775808 to 9223372036854775807" (Variable (JsonDocument.Parse "\"enum\"").RootElement)
    testCoercionError LongType "Inline value 'enum' of type enum cannot be converted into integer of range from -9223372036854775808 to 9223372036854775807" (InlineConstant (EnumValue "enum"))

[<Fact>]
let ``Float coerces input`` () =
    testCoercion FloatType 123. (Variable (JsonDocument.Parse "123").RootElement)
    testCoercion FloatType 123. (InlineConstant (IntValue 123L))
    testCoercion FloatType 123.4 (Variable (JsonDocument.Parse "123.4").RootElement)
    testCoercion FloatType 123.4 (InlineConstant (FloatValue 123.4))
    testCoercion FloatType 1. (Variable (JsonDocument.Parse "true").RootElement)
    testCoercion FloatType 1. (InlineConstant (BooleanValue true))
    testCoercion FloatType 0. (Variable (JsonDocument.Parse "false").RootElement)
    testCoercion FloatType 0. (InlineConstant (BooleanValue false))
    testCoercionError FloatType "JSON value 'enum' of kind 'String' cannot be deserialized into float of range from -1,7976931348623157E+308 to 1,7976931348623157E+308" (Variable (JsonDocument.Parse "\"enum\"").RootElement)
    testCoercionError FloatType "Inline value 'enum' of type enum cannot be converted into float of range from -1,7976931348623157E+308 to 1,7976931348623157E+308" (InlineConstant (EnumValue "enum"))

[<Fact>]
let ``Boolean coerces input`` () =
    testCoercion BooleanType false (Variable (JsonDocument.Parse "0").RootElement)
    testCoercion BooleanType false (InlineConstant (IntValue 0L))
    testCoercion BooleanType true (Variable (JsonDocument.Parse "123").RootElement)
    testCoercion BooleanType true (InlineConstant (IntValue 123L))
    testCoercion BooleanType true (Variable (JsonDocument.Parse "123.4").RootElement)
    testCoercion BooleanType true (InlineConstant (FloatValue 123.4))
    testCoercion BooleanType true (Variable (JsonDocument.Parse "true").RootElement)
    testCoercion BooleanType true (InlineConstant (BooleanValue true))
    testCoercion BooleanType false (Variable (JsonDocument.Parse "false").RootElement)
    testCoercion BooleanType false (InlineConstant (BooleanValue false))
    testCoercionError BooleanType "JSON value 'enum' of kind 'String' cannot be deserialized into boolean" (Variable (JsonDocument.Parse "\"enum\"").RootElement)
    testCoercionError BooleanType "Inline value 'enum' of type enum cannot be converted into boolean" (InlineConstant (EnumValue "enum"))

[<Fact>]
let ``String coerces input`` () =
    testCoercion StringType "123" (Variable (JsonDocument.Parse "123").RootElement)
    testCoercion StringType "123" (InlineConstant (IntValue 123L))
    testCoercion StringType "123.4" (Variable (JsonDocument.Parse "123.4").RootElement)
    testCoercion StringType "123.4" (InlineConstant (FloatValue 123.4))
    testCoercion StringType "abc123.4" (Variable (JsonDocument.Parse "\"abc123.4\"").RootElement)
    testCoercion StringType "acb123.4" (InlineConstant (StringValue "acb123.4"))
    testCoercion StringType "true" (Variable (JsonDocument.Parse "true").RootElement)
    testCoercion StringType "true" (InlineConstant (BooleanValue true))
    testCoercion StringType "false" (Variable (JsonDocument.Parse "false").RootElement)
    testCoercion StringType "false" (InlineConstant (BooleanValue false))
    testCoercion StringType "enum" (Variable (JsonDocument.Parse "\"enum\"").RootElement)
    testCoercion StringType "enum" (InlineConstant (EnumValue "enum"))

[<Fact>]
let ``ID coerces input`` () =
    testCoercion IDType "123" (Variable (JsonDocument.Parse "123").RootElement)
    testCoercion IDType "123" (InlineConstant (IntValue 123L))
    testCoercionError IDType "JSON value '123.4' of kind 'Number' cannot be deserialized into identifier of range from -9223372036854775808 to 9223372036854775807" (Variable (JsonDocument.Parse "123.4").RootElement)
    testCoercionError IDType "Inline value '123,4' of type float cannot be converted into identifier of range from -9223372036854775808 to 9223372036854775807" (InlineConstant (FloatValue 123.4))
    testCoercion IDType "abc123.4" (Variable (JsonDocument.Parse "\"abc123.4\"").RootElement)
    testCoercion IDType "acb123.4" (InlineConstant (StringValue "acb123.4"))
    testCoercionError IDType "JSON value 'true' of kind 'True' cannot be deserialized into identifier" (Variable (JsonDocument.Parse "true").RootElement)
    testCoercionError IDType "Inline value 'True' of type boolean cannot be converted into identifier" (InlineConstant (BooleanValue true))
    testCoercionError IDType "JSON value 'false' of kind 'False' cannot be deserialized into identifier" (Variable (JsonDocument.Parse "false").RootElement)
    testCoercionError IDType "Inline value 'False' of type boolean cannot be converted into identifier" (InlineConstant (BooleanValue false))
    // We have no idea that it is an enum
    testCoercion IDType "enum" (Variable (JsonDocument.Parse "\"enum\"").RootElement)
    testCoercionError IDType "Inline value 'enum' of type enum cannot be converted into identifier" (InlineConstant (EnumValue "enum"))
