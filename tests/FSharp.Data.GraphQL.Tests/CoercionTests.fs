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
    testCoercion IntType 123 (InlineConstant (IntValue 123L))
    testCoercion IntType 123 (InlineConstant (FloatValue 123.4))
    testCoercion IntType 123 (InlineConstant (StringValue "123"))
    testCoercion IntType 1 (InlineConstant (BooleanValue true))
    testCoercion IntType 0 (InlineConstant (BooleanValue false))

[<Fact>]
let ``Float coerces input`` () =
    testCoercion FloatType 123. (InlineConstant (IntValue 123L))
    testCoercion FloatType 123.4 (InlineConstant (FloatValue 123.4))
    testCoercion FloatType 123.4 (InlineConstant (StringValue "123.4"))
    testCoercion FloatType 1. (InlineConstant (BooleanValue true))
    testCoercion FloatType 0. (InlineConstant (BooleanValue false))

[<Fact>]
let ``Long coerces input`` () =
    testCoercion LongType 123L (InlineConstant (IntValue 123L))
    testCoercion LongType 123L (InlineConstant (FloatValue 123.4))
    testCoercion LongType 123L (InlineConstant (StringValue "123"))
    testCoercion LongType 1L (InlineConstant (BooleanValue true))
    testCoercion LongType 0L (InlineConstant (BooleanValue false))

[<Fact>]
let ``Boolean coerces input`` () =
    testCoercion BooleanType true (InlineConstant (IntValue 123L))
    testCoercion BooleanType false (InlineConstant (IntValue 0L))
    testCoercion BooleanType true (InlineConstant (FloatValue 123.4))
    testCoercion BooleanType true (InlineConstant (BooleanValue true))
    testCoercion BooleanType false (InlineConstant (BooleanValue false))

[<Fact>]
let ``String coerces input`` () =
    testCoercion StringType "123" (InlineConstant (IntValue 123L))
    testCoercion StringType "123.4" (InlineConstant (FloatValue 123.4))
    testCoercion StringType "acb123.4" (InlineConstant (StringValue "acb123.4"))
    testCoercion StringType "true" (InlineConstant (BooleanValue true))
    testCoercion StringType "false" (InlineConstant (BooleanValue false))
