/// The MIT License (MIT)
/// Copyright (c) 2015-Mar 2016 Kevin Thompson @kthompson
/// Copyright (c) 2016 Bazinga Technologies Inc

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
    testCoercion Int 123 (IntValue 123L)
    testCoercion Int 123 (FloatValue 123.4)
    testCoercion Int 123 (StringValue "123")
    testCoercion Int 1 (BooleanValue true)
    testCoercion Int 0 (BooleanValue false)
        
[<Fact>]
let ``Float coerces input`` () = 
    testCoercion Float 123. (IntValue 123L)
    testCoercion Float 123.4 (FloatValue 123.4)
    testCoercion Float 123.4 (StringValue "123.4")
    testCoercion Float 1. (BooleanValue true)
    testCoercion Float 0. (BooleanValue false)

[<Fact>]
let ``Long coerces input`` () = 
    testCoercion Long 123L (IntValue 123L)
    testCoercion Long 123L (FloatValue 123.4)
    testCoercion Long 123L (StringValue "123")
    testCoercion Long 1L (BooleanValue true)
    testCoercion Long 0L (BooleanValue false)

[<Fact>]
let ``Boolean coerces input`` () = 
    testCoercion Boolean true (IntValue 123L)
    testCoercion Boolean false (IntValue 0L)
    testCoercion Boolean true (FloatValue 123.4)
    testCoercion Boolean true (BooleanValue true)
    testCoercion Boolean false (BooleanValue false)

[<Fact>]
let ``String coerces input`` () = 
    testCoercion String "123" (IntValue 123L)
    testCoercion String "123.4" (FloatValue 123.4)
    testCoercion String "acb123.4" (StringValue "acb123.4")
    testCoercion String "true" (BooleanValue true)
    testCoercion String "false" (BooleanValue false)
