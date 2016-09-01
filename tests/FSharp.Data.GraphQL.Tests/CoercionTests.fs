/// The MIT License (MIT)
/// Copyright (c) 2015-Mar 2016 Kevin Thompson @kthompson
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.CoercionTests

#nowarn "25"

open System
open Xunit
open FsCheck
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns

let private testCoercion graphQLType (expected: 't) actual =
    let (Scalar scalar) = graphQLType
    let result = (scalar.CoerceInput actual) |> Option.map (fun x -> downcast x)
    match result with
    | Some x -> equals expected x
    

[<Fact>]
let ``Int coerces input`` () = 
    testCoercion Int 123 (IntValue 123)
    testCoercion Int 123 (FloatValue 123.4)
    testCoercion Int 123 (StringValue "123")
    testCoercion Int 1 (BooleanValue true)
    testCoercion Int 0 (BooleanValue false)
    
[<Fact>]
let ``Float coerces input`` () = 
    testCoercion Float 123. (IntValue 123)
    testCoercion Float 123.4 (FloatValue 123.4)
    testCoercion Float 123.4 (StringValue "123.4")
    testCoercion Float 1. (BooleanValue true)
    testCoercion Float 0. (BooleanValue false)

[<Fact>]
let ``Boolean coerces input`` () = 
    testCoercion Boolean true (IntValue 123)
    testCoercion Boolean false (IntValue 0)
    testCoercion Boolean true (FloatValue 123.4)
    testCoercion Boolean true (BooleanValue true)
    testCoercion Boolean false (BooleanValue false)

[<Fact>]
let ``String coerces input`` () = 
    testCoercion String "123" (IntValue 123)
    testCoercion String "123.4" (FloatValue 123.4)
    testCoercion String "acb123.4" (StringValue "acb123.4")
    testCoercion String "true" (BooleanValue true)
    testCoercion String "false" (BooleanValue false)
