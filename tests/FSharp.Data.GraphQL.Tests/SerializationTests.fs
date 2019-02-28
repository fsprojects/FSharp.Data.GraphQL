/// The MIT License (MIT)
/// Copyright (c) 2015-Mar 2016 Kevin Thompson @kthompson
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.SerializationTests

open Xunit
open System
open FSharp.Data
open FSharp.Data.GraphQL.Client

let private normalize (json : string) =
  json.Replace("\r\n", "\n")

type TestEnum =
    | Choice = 1

type TestRecord1 =
  { Prop1 : sbyte
    Prop2 : byte
    Prop3 : int16
    Prop4 : uint16
    Prop5 : int
    Prop6 : uint32
    Prop7 : int64
    Prop8 : uint64
    Prop9 : single
    Prop10 : double
    Prop11 : decimal
    Prop12 : DateTime
    Prop13 : DateTime
    Prop14 : DateTimeOffset
    Prop15 : string
    Prop16 : Guid
    Prop17 : string option
    Prop18 : int []
    Prop19 : int list
    Prop20 : seq<int>
    Prop21 : TestEnum }

type TestRecord2 =
  { Prop1 : sbyte option
    Prop2 : byte option
    Prop3 : int16 option
    Prop4 : uint16 option
    Prop5 : int option
    Prop6 : uint32 option
    Prop7 : int64 option
    Prop8 : uint64 option
    Prop9 : single option
    Prop10 : double option
    Prop11 : decimal option
    Prop12 : DateTime option
    Prop13 : DateTime option
    Prop14 : DateTimeOffset option
    Prop15 : string option
    Prop16 : Guid option
    Prop17 : string option
    Prop18 : int [] option
    Prop19 : int list option
    Prop20 : seq<int> option
    Prop21 : TestEnum option }

let subject1 =
    { TestRecord1.Prop1 = 1y
      Prop2 = 2uy
      Prop3 = 3s
      Prop4 = 4us
      Prop5 = 5
      Prop6 = 6u
      Prop7 = 7L
      Prop8 = 8UL
      Prop9 = 9.0f
      Prop10 = 10.0
      Prop11 = 11.0m
      Prop12 = DateTime(2016, 1, 1)
      Prop13 = DateTime(2016, 1, 1, 10, 50, 25, 326)
      Prop14 = DateTimeOffset(DateTime(2016, 1, 1, 10, 50, 25, 326))
      Prop15 = "test"
      Prop16 = Guid.Parse("e433c15b-f435-4574-bcbf-a1e35b73fff7")
      Prop17 = None
      Prop18 = [| 1; 2; 3 |]
      Prop19 = [ 1; 2; 3 ]
      Prop20 = [| 1; 2; 3 |]
      Prop21 = TestEnum.Choice }

let subject2 =
    { Prop1 = Some 1y
      Prop2 = Some 2uy
      Prop3 = Some 3s
      Prop4 = Some 4us
      Prop5 = Some 5
      Prop6 = Some 6u
      Prop7 = Some 7L
      Prop8 = Some 8UL
      Prop9 = Some 9.0f
      Prop10 = Some 10.0
      Prop11 = Some 11.0m
      Prop12 = Some (DateTime(2016, 1, 1))
      Prop13 = Some (DateTime(2016, 1, 1, 10, 50, 25, 326))
      Prop14 = Some (DateTimeOffset(DateTime(2016, 1, 1, 10, 50, 25, 326)))
      Prop15 = Some "test"
      Prop16 = Some (Guid.Parse("e433c15b-f435-4574-bcbf-a1e35b73fff7"))
      Prop17 = None
      Prop18 = Some [| 1; 2; 3 |]
      Prop19 = Some [ 1; 2; 3 ]
      Prop20 = Some (upcast [| 1; 2; 3 |])
      Prop21 = Some (TestEnum.Choice) }

let subject3 =
    { Prop1 = None
      Prop2 = None
      Prop3 = None
      Prop4 = None
      Prop5 = None
      Prop6 = None
      Prop7 = None
      Prop8 = None
      Prop9 = None
      Prop10 = None
      Prop11 = None
      Prop12 = None
      Prop13 = None
      Prop14 = None
      Prop15 = None
      Prop16 = None
      Prop17 = None
      Prop18 = None
      Prop19 = None
      Prop20 = None
      Prop21 = None }

let json = normalize """{
  "Prop1": 1,
  "Prop2": 2,
  "Prop3": 3,
  "Prop4": 4,
  "Prop5": 5,
  "Prop6": 6,
  "Prop7": 7,
  "Prop8": 8,
  "Prop9": 9,
  "Prop10": 10,
  "Prop11": 11,
  "Prop12": "2016-01-01",
  "Prop13": "2016-01-01T10:50:25.3260000",
  "Prop14": "2016-01-01T10:50:25.3260000-02:00",
  "Prop15": "test",
  "Prop16": "e433c15b-f435-4574-bcbf-a1e35b73fff7",
  "Prop17": null,
  "Prop18": [
    1,
    2,
    3
  ],
  "Prop19": [
    1,
    2,
    3
  ],
  "Prop20": [
    1,
    2,
    3
  ],
  "Prop21": "Choice"
}"""

let nullJson = normalize """{
  "Prop1": null,
  "Prop2": null,
  "Prop3": null,
  "Prop4": null,
  "Prop5": null,
  "Prop6": null,
  "Prop7": null,
  "Prop8": null,
  "Prop9": null,
  "Prop10": null,
  "Prop11": null,
  "Prop12": null,
  "Prop13": null,
  "Prop14": null,
  "Prop15": null,
  "Prop16": null,
  "Prop17": null,
  "Prop18": null,
  "Prop19": null,
  "Prop20": null,
  "Prop21": null
}"""

let outOfOrderJson = normalize """{
  "Prop2": 2,
  "Prop3": 3,
  "Prop4": 4,
  "Prop1": 1,
  "Prop5": 5,
  "Prop6": 6,
  "Prop7": 7,
  "Prop14": "2016-01-01T10:50:25.3260000-02:00",
  "Prop8": 8,
  "Prop9": 9,
  "Prop10": 10,
  "Prop11": 11,
  "Prop12": "2016-01-01",
  "Prop13": "2016-01-01T10:50:25.3260000",
  "Prop15": "test",
  "Prop16": "e433c15b-f435-4574-bcbf-a1e35b73fff7",
  "Prop17": null,
  "Prop19": [
    1,
    2,
    3
  ],
  "Prop21": "Choice",
  "Prop18": [
    1,
    2,
    3
  ],
  "Prop20": [
    1,
    2,
    3
  ]
}"""

let outOfOrderNullJson = normalize """{
  "Prop20": null,
  "Prop2": null,
  "Prop3": null,
  "Prop4": null,
  "Prop5": null,
  "Prop1": null,
  "Prop6": null,
  "Prop7": null,
  "Prop9": null,
  "Prop10": null,
  "Prop11": null,
  "Prop17": null,
  "Prop12": null,
  "Prop13": null,
  "Prop14": null,
  "Prop15": null,
  "Prop21": null,
  "Prop16": null,
  "Prop18": null,
  "Prop8": null,
  "Prop19": null
}"""



[<Fact>]
let ``deserializeRecord should correctly deserialize basic types`` () =
  let actual = Serialization.deserializeRecord<TestRecord1> json
  actual |> equals subject1

[<Fact>]
let ``deserializeRecord should correctly deserialize basic types with out of order properties`` () =
  let actual = Serialization.deserializeRecord<TestRecord1> outOfOrderJson
  actual |> equals subject1

[<Fact>]
let ``deserializeRecord should correctly deserialize option types`` () =
  let actual = [|
    Serialization.deserializeRecord<TestRecord2> json
    Serialization.deserializeRecord<TestRecord2> nullJson |]
  actual |> seqEquals [|subject2; subject3|]

[<Fact>]
let ``deserializeRecord should correctly deserialize option types with out of order properties`` () =
  let actual = [|
    Serialization.deserializeRecord<TestRecord2> outOfOrderJson
    Serialization.deserializeRecord<TestRecord2> outOfOrderNullJson |]
  actual |> seqEquals [|subject2; subject3|]

[<Fact>]
let ``serialize should correctly serialize basic types`` () =
    let actual = Serialization.serialize subject1 |> normalize
    actual |> equals json

[<Fact>]
let ``serialize should correctly serialize option types`` () =
    let actual = [|
      Serialization.serialize subject2 |> normalize
      Serialization.serialize subject3 |> normalize |]
    actual |> seqEquals [|json; nullJson|]