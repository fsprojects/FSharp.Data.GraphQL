module FSharp.Data.GraphQL.Tests.DecodingTests

open Xunit
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Decoding

type private FooBar =
  {
    Foo : int
    Bar : string
  }

type private Qux =
  {
    Foo : string
    Baz : string option
  }

let private checkDecoding decoder input expected =
  let actual = decoder input

  // Give better error messages for decoding failures
  match expected, actual with
  | Ok x, Error error ->
    equals (Some x, None) (None, Some error)
  | _ -> ()

  equals expected actual

[<Fact>]
let ``Decode.string works`` () =
  checkDecoding Decode.string (StringValue "foo") (Ok "foo")
  checkDecoding Decode.string (StringValue "abc") (Ok "abc")
  checkDecoding Decode.string (StringValue "") (Ok "")
  checkDecoding Decode.string (StringValue "1234") (Ok "1234")

[<Fact>]
let ``Decode.auto works for options`` () =
  let dec : Decoder<string option> = Decode.auto None

  checkDecoding dec (StringValue "foo") (Ok (Some "foo"))
  checkDecoding dec (StringValue "") (Ok (Some ""))
  checkDecoding dec (StringValue "1234") (Ok (Some "1234"))
  checkDecoding dec (NullValue) (Ok None)

[<Fact>]
let ``Decode.auto works for simple records`` () =
  let dec : Decoder<FooBar> = Decode.auto None

  checkDecoding dec (ObjectValue (Map.ofList [ "Foo", IntValue 1L; "Bar", StringValue "abc" ])) (Ok { Foo = 1; Bar = "abc" })

[<Fact>]
let ``Decode.auto works with customizations 1`` () =
  let stringTwiceDecoder : Decoder<string> = Decode.string |> Decoder.map (fun x -> x + x)
  let intPlusOneDecoder : Decoder<int> = Decode.int |> Decoder.map (fun x -> x + 1)

  let settings  =
    AutoDecoderSettings.empty
    |> AutoDecoderSettings.customizeBoxed typeof<string> (Decoder.box stringTwiceDecoder)
    |> AutoDecoderSettings.customize intPlusOneDecoder

  let dec = Decode.autoWithSettings settings

  checkDecoding dec (ObjectValue (Map.ofList [ "Foo", IntValue 3L; "Bar", StringValue "abc" ])) (Ok { Foo = 4; Bar = "abcabc" })

[<Fact>]
let ``Decode.auto works with customizations 2`` () =
  let string : Decoder<string> = Decode.auto None
  let stringOption : Decoder<string option> = Decode.auto None

  let settings  =
    AutoDecoderSettings.empty
    |> AutoDecoderSettings.customizeBoxed typeof<string> (Decoder.box string)
    |> AutoDecoderSettings.customizeBoxed typeof<string option> (Decoder.box stringOption)

  let dec : Decoder<Qux> = Decode.autoWithSettings settings

  checkDecoding dec (ObjectValue (Map.ofList [ "Foo", StringValue "x"; "Baz", NullValue ])) (Ok { Foo = "x"; Baz = None })
  checkDecoding dec (ObjectValue (Map.ofList [ "Foo", StringValue "x"; "Baz", StringValue "y" ])) (Ok { Foo = "x"; Baz = Some "y" })

[<Fact>]
let ``Decode.autoRecordFromFields works`` () =
  let dec : Decoder<FooBar> =
    Decode.autoRecordFromFields
      (
        AutoDecoderSettings.empty
        |> AutoDecoderSettings.casing DropCamelCase
      )
      (
        Map.ofSeq [
          "Foo", Decoder.box Decode.int
          "Bar", Decoder.box Decode.string
        ]
      )

  checkDecoding dec (ObjectValue (Map.ofList [ "foo", IntValue 1L; "bar", StringValue "abc" ])) (Ok { Foo = 1; Bar = "abc" })

[<Fact>]
let ``Decode.auto works for Option<string * string>`` () =
  let dec : Decoder<Option<string * string>> = Decode.auto None

  checkDecoding dec NullValue (Ok None)
  checkDecoding dec (ListValue [ StringValue "abc"; StringValue "def" ]) (Ok (Some ("abc", "def")))

[<Fact>]
let ``Decoder Computation Expression works`` () =
  let dec =
    decoder {
      let! foo = Decode.field "foo" Decode.int
      let! bar = Decode.field "bar" Decode.string

      return
        {
          Foo = foo
          Bar = bar
        }
    }

  checkDecoding dec (ObjectValue (Map.ofList [ "foo", IntValue 123L; "bar", StringValue "hello" ])) (Ok { Foo = 123; Bar = "hello" })
