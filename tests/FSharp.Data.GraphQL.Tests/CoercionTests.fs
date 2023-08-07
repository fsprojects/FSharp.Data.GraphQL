// The MIT License (MIT)
/// Copyright (c) 2015-Mar 2016 Kevin Thompson @kthompson
// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.CoercionTests

#nowarn "25"

open System
open Xunit
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns

let private testCoercion graphQLType (expected: 't) actual =
    let (Scalar scalar) = graphQLType
    let result = (scalar.CoerceInput actual) |> Option.map (fun x -> downcast x)
    match result with
    | Some x -> equals expected x
    | None -> raise (Exception(sprintf "Expected %A to be able to be coerced to %A" actual expected))


[<Fact>]
let ``Int coerces input`` () =
    testCoercion IntType 123 (IntValue 123L)
    testCoercion IntType 123 (FloatValue 123.4)
    testCoercion IntType 123 (StringValue "123")
    testCoercion IntType 1 (BooleanValue true)
    testCoercion IntType 0 (BooleanValue false)

[<Fact>]
let ``Float coerces input`` () =
    testCoercion FloatType 123. (IntValue 123L)
    testCoercion FloatType 123.4 (FloatValue 123.4)
    testCoercion FloatType 123.4 (StringValue "123.4")
    testCoercion FloatType 1. (BooleanValue true)
    testCoercion FloatType 0. (BooleanValue false)

[<Fact>]
let ``Long coerces input`` () =
    testCoercion LongType 123L (IntValue 123L)
    testCoercion LongType 123L (FloatValue 123.4)
    testCoercion LongType 123L (StringValue "123")
    testCoercion LongType 1L (BooleanValue true)
    testCoercion LongType 0L (BooleanValue false)

[<Fact>]
let ``Boolean coerces input`` () =
    testCoercion BooleanType true (IntValue 123L)
    testCoercion BooleanType false (IntValue 0L)
    testCoercion BooleanType true (FloatValue 123.4)
    testCoercion BooleanType true (BooleanValue true)
    testCoercion BooleanType false (BooleanValue false)

[<Fact>]
let ``String coerces input`` () =
    testCoercion StringType "123" (IntValue 123L)
    testCoercion StringType "123.4" (FloatValue 123.4)
    testCoercion StringType "acb123.4" (StringValue "acb123.4")
    testCoercion StringType "true" (BooleanValue true)
    testCoercion StringType "false" (BooleanValue false)
