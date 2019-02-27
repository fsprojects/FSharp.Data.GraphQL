/// The MIT License (MIT)
/// Copyright (c) 2015-Mar 2016 Kevin Thompson @kthompson
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.SerializationTests

open System
open FSharp.Data
open FSharp.Data.GraphQL.Client

type TestRecord =
    { Prop1 : string
      Prop2 : int }

[<Fact>]
let ``serialize should correctly serialize basic types`` () =
    let input = [| 
        box 1y
        upcast 2uy
        upcast 3s
        upcast 4us
        upcast 5
        upcast 6u
        upcast 7l
        upcast 8ul
        upcast 9.0f
        upcast 10.0
        upcast 11.0m
        upcast "test"
        upcast Guid.Parse("e433c15b-f435-4574-bcbf-a1e35b73fff7")
        null
        upcast { Prop1 = "test"; Prop2 = 1 } |]
    let actual = Serialization.serialize input
    let expected =
        JsonValue.Array [|
            JsonValue.Number 1m |]
    actual |> seqEquals expected