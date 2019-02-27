/// The MIT License (MIT)
/// Copyright (c) 2015-Mar 2016 Kevin Thompson @kthompson
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.SerializationTests

open Xunit
open System
open FSharp.Data
open FSharp.Data.GraphQL.Client

type TestRecord =
    { Prop1 : string
      Prop2 : int }

let private normalize (json : string) =
  json.Replace("\r\n", "\n")

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
        upcast DateTime(2016, 1, 1)
        upcast DateTime(2016, 1, 1, 10, 50, 25, 326)
        upcast DateTimeOffset(DateTime(2016, 1, 1, 10, 50, 25, 326))
        upcast "test"
        upcast Guid.Parse("e433c15b-f435-4574-bcbf-a1e35b73fff7")
        null
        upcast { Prop1 = "test"; Prop2 = 1 } |]
    let actual = Serialization.serialize input |> normalize
    let expected = normalize """[
  1,
  2,
  3,
  4,
  5,
  6,
  7,
  8,
  9,
  10,
  11,
  "2016-01-01",
  "2016-01-01T10:50:25.3260000",
  "2016-01-01T10:50:25.3260000-02:00",
  "test",
  "e433c15b-f435-4574-bcbf-a1e35b73fff7",
  null,
  {
    "Prop1": "test",
    "Prop2": 1
  }
]"""
    actual |> equals expected