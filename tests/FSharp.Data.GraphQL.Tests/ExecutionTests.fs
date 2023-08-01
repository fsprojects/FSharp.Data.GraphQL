// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.ExecutionTests

open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Samples.StarWarsApi
open System.Text.Json
open System.Collections.Immutable

#nowarn "40"

type TestSubject = {
    a: string
    b: string
    c: string
    d: string
    e: string
    f: string
    deep: DeepTestSubject
    pic: int option -> string
    promise: Async<TestSubject>
}
and DeepTestSubject = {
    a: string
    b: string
    c: string option list
}

and DUArg =
    | Case1
    | Case2

and EnumArg =
    | Enum1 = 1
    | Enum2 = 2

[<Fact>]
let ``Execution handles basic tasks: executes arbitrary code`` () =
    let rec data =
        {
            a = "Apple"
            b = "Banana"
            c = "Cookie"
            d = "Donut"
            e = "Egg"
            f = "Fish"
            pic = (fun size -> "Pic of size: " + (if size.IsSome then size.Value else 50).ToString())
            promise = async { return data }
            deep = deep
        }
    and deep =
        {
            a = "Already Been Done"
            b = "Boring"
            c = [Some "Contrived"; None; Some "Confusing"]
        }

    let ast = parse """query Example($size: Int) {
          a,
          b,
          x: c
          ...c
          f
          ...on DataType {
            pic(size: $size)
            promise {
              a
            }
          }
          deep {
            a
            b
            c
          }
        }

        fragment c on DataType {
          d
          e
        }"""

    let expected =
        NameValueLookup.ofList [
            "a", upcast "Apple"
            "b", upcast "Banana"
            "x", upcast "Cookie"
            "d", upcast "Donut"
            "e", upcast "Egg"
            "f", upcast "Fish"
            "pic", upcast "Pic of size: 100"
            "promise", upcast NameValueLookup.ofList [ "a", upcast "Apple" ]
            "deep", upcast NameValueLookup.ofList [
               "a", "Already Been Done" :> obj
               "b", upcast "Boring"
               "c", upcast ["Contrived" :> obj; null; upcast "Confusing"]
            ]
        ]

    let DeepDataType =
        Define.Object<DeepTestSubject>(
            "DeepDataType", [
                Define.Field("a", StringType, (fun _ dt -> dt.a))
                Define.Field("b", StringType, (fun _ dt -> dt.b))
                Define.Field("c", (ListOf (Nullable StringType)), (fun _ dt -> dt.c))
            ])
    let rec DataType =
      Define.Object<TestSubject>(
          "DataType",
          fieldsFn = fun () ->
          [
            Define.Field("a", StringType, resolve = fun _ dt -> dt.a)
            Define.Field("b", StringType, resolve = fun _ dt -> dt.b)
            Define.Field("c", StringType, resolve = fun _ dt -> dt.c)
            Define.Field("d", StringType, fun _ dt -> dt.d)
            Define.Field("e", StringType, fun _ dt -> dt.e)
            Define.Field("f", StringType, fun _ dt -> dt.f)
            Define.Field("pic", StringType, "Picture resizer", [ Define.Input("size", Nullable IntType) ], fun ctx dt -> dt.pic(ctx.TryArg("size")))
            Define.AsyncField("promise", DataType, fun _ dt -> dt.promise)
            Define.Field("deep", DeepDataType, fun _ dt -> dt.deep)
        ])

    let schema = Schema(DataType)
    let schemaProcessor = Executor(schema)
    let params' = JsonDocument.Parse("""{"size":100}""").RootElement.Deserialize<ImmutableDictionary<string, JsonElement>>(Json.serializerOptions)
    let result = sync <| schemaProcessor.AsyncExecute(ast, data, variables = params', operationName = "Example")
    match result with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

type TestThing = { mutable Thing: string }

[<Fact>]
let ``Execution handles basic tasks: merges parallel fragments`` () =
    let ast = parse """{ a, ...FragOne, ...FragTwo }

      fragment FragOne on Type {
        b
        deep { b, deeper: deep { b } }
      }

      fragment FragTwo on Type {
        c
        deep { c, deeper: deep { c } }
      }"""

    let rec Type =
      Define.Object(
        name = "Type",
        fieldsFn = fun () ->
        [
            Define.Field("a", StringType, fun _ _ -> "Apple")
            Define.Field("b", StringType, fun _ _ -> "Banana")
            Define.Field("c", StringType, fun _ _ -> "Cherry")
            Define.Field("deep", Type, fun _ v -> v)
        ])

    let schema = Schema(Type)
    let schemaProcessor = Executor(schema)
    let expected =
      NameValueLookup.ofList [
        "a", upcast "Apple"
        "b", upcast "Banana"
        "deep", upcast NameValueLookup.ofList [
            "b", upcast "Banana"
            "deeper", upcast NameValueLookup.ofList [
                "b", "Banana" :> obj
                "c", upcast "Cherry"
            ]
            "c", upcast "Cherry"
        ]
        "c", upcast "Cherry"
    ]
    let result = sync <| schemaProcessor.AsyncExecute(ast, obj())
    match result with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execution handles basic tasks: threads root value context correctly`` () =
    let query = "query Example { a }"
    let data = { Thing = "" }
    let Thing = Define.Object<TestThing>("Type", [  Define.Field("a", StringType, fun _ value -> value.Thing <- "thing"; value.Thing) ])
    let result = sync <| Executor(Schema(Thing)).AsyncExecute(parse query, data)
    match result with
    | Direct(data, errors) ->
      empty errors
    | _ -> fail "Expected Direct GQResponse"
    equals "thing" data.Thing

type TestTarget =
    { mutable Num: int option
      mutable Str: string option }

[<Fact>]
let ``Execution handles basic tasks: correctly threads arguments`` () =
    let query = """query Example {
        b(numArg: 123, stringArg: "foo")
      }"""
    let data = { Num = None; Str = None }
    let Type =
        Define.Object("Type",
            [ Define.Field("b", Nullable StringType, "", [ Define.Input("numArg", IntType); Define.Input("stringArg", StringType) ],
                 fun ctx value ->
                     value.Num <- ctx.TryArg("numArg")
                     value.Str <- ctx.TryArg("stringArg")
                     value.Str) ])

    let result = sync <| Executor(Schema(Type)).AsyncExecute(parse query, data)
    match result with
    | Direct(data, errors) ->
      empty errors
    | _ -> fail "Expected Direct GQResponse"
    equals (Some 123) data.Num
    equals (Some "foo") data.Str

type InlineTest = { A: string }

[<Fact>]
let ``Execution handles basic tasks: correctly handles discriminated union arguments`` () =
    let query = """query Example {
          b(enumArg: Case1)
        }"""
    let EnumType =
        Define.Enum(
            name = "EnumArg",
            options =
                [ Define.EnumValue("Case1", DUArg.Case1, "Case 1")
                  Define.EnumValue("Case2", DUArg.Case2, "Case 2") ])
    let data = { Num = None; Str = None }
    let Type =
        Define.Object("Type",
            [ Define.Field("b", Nullable StringType, "", [ Define.Input("enumArg", EnumType) ],
                 fun ctx value ->
                 let arg = ctx.TryArg("enumArg")
                 match arg with
                 | Some (Case1) ->
                     value.Str <- Some "foo"
                     value.Num <- Some 123
                     value.Str
                 | _ -> None) ])
    let result = sync <| Executor(Schema(Type)).AsyncExecute(parse query, data)
    match result with
    | Direct(data, errors) ->
      empty errors
    | _ -> fail "Expected Direct GQResponse"
    equals (Some 123) data.Num
    equals (Some "foo") data.Str

[<Fact>]
let ``Execution handles basic tasks: correctly handles Enum arguments`` () =
    let query = """query Example {
          b(enumArg: Enum1)
        }"""
    let EnumType =
        Define.Enum(
            name = "EnumArg",
            options =
                [ Define.EnumValue("Enum1", EnumArg.Enum1, "Enum 1")
                  Define.EnumValue("Enum2", EnumArg.Enum2, "Enum 2") ])
    let data = { Num = None; Str = None }
    let Type =
        Define.Object("Type",
            [ Define.Field("b", Nullable StringType, "", [ Define.Input("enumArg", EnumType) ],
                  fun ctx value ->
                  let arg = ctx.TryArg("enumArg")
                  match arg with
                  | Some _ ->
                      value.Str <- Some "foo"
                      value.Num <- Some 123
                      value.Str
                  | _ -> None) ])
    let result = sync <| Executor(Schema(Type)).AsyncExecute(parse query, data)
    match result with
    | Direct(data, errors) ->
      empty errors
    | _ -> fail "Expected Direct GQResponse"
    equals (Some 123) data.Num
    equals (Some "foo") data.Str


[<Fact>]
let ``Execution handles basic tasks: uses the inline operation if no operation name is provided`` () =
    let schema =
        Schema(Define.Object<InlineTest>(
                "Type", [
                    Define.Field("a", StringType, fun _ x -> x.A)
                ]))
    let result = sync <| Executor(schema).AsyncExecute(parse "{ a }", { A = "b" })
    match result with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast NameValueLookup.ofList ["a", "b" :> obj])
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execution handles basic tasks: uses the only operation if no operation name is provided`` () =
    let schema =
        Schema(Define.Object<InlineTest>(
                "Type", [
                    Define.Field("a", StringType, fun _ x -> x.A)
                ]))
    let result = sync <| Executor(schema).AsyncExecute(parse "query Example { a }", { A = "b" })
    match result with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast NameValueLookup.ofList ["a", "b" :> obj])
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execution handles basic tasks: uses the named operation if operation name is provided`` () =
    let schema =
        Schema(Define.Object<InlineTest>(
                "Type", [
                    Define.Field("a", StringType, fun _ x -> x.A)
                ]))
    let query = "query Example { first: a } query OtherExample { second: a }"
    let result = sync <| Executor(schema).AsyncExecute(parse query, { A = "b" }, operationName = "OtherExample")
    match result with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast NameValueLookup.ofList ["second", "b" :> obj])
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execution handles basic tasks: list of scalars`` () =
    let schema =
        Schema(Define.Object<InlineTest>(
                "Type", [
                    Define.Field("strings", ListOf StringType, fun _ _ -> ["foo"; "bar"; "baz"])
                ]))
    let result = sync <| Executor(schema).AsyncExecute("query Example { strings }")
    match result with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast NameValueLookup.ofList ["strings", box [ box "foo"; upcast "bar"; upcast "baz" ]])
    | _ -> fail "Expected Direct GQResponse"

type TwiceTest = { A : string; B : int }

[<Fact>]
let ``Execution when querying the same field twice will return it`` () =
    let schema =
      Schema(Define.Object<TwiceTest>(
                "Type", [
                    Define.Field("a", StringType, fun _ x -> x.A)
                    Define.Field("b", IntType, fun _ x -> x.B)
                ]))
    let query = "query Example { a, b, a }"
    let result = sync <| Executor(schema).AsyncExecute(query, { A = "aa"; B = 2 });
    let expected =
      NameValueLookup.ofList [
        "a", upcast "aa"
        "b", upcast 2]
    match result with
    | Direct(data, errors) ->
      empty errors
      data |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execution when querying returns unique document id with response`` () =
    let schema =
      Schema(Define.Object<TwiceTest>(
                "Type", [
                    Define.Field("a", StringType, fun _ x -> x.A)
                    Define.Field("b", IntType, fun _ x -> x.B)
                ]))
    let result1 = sync <| Executor(schema).AsyncExecute("query Example { a, b, a }", { A = "aa"; B = 2 })
    let result2 = sync <| Executor(schema).AsyncExecute("query Example { a, b, a }", { A = "aa"; B = 2 })
    result1.DocumentId |> notEquals Unchecked.defaultof<int>
    result1.DocumentId |> equals result2.DocumentId
    match result1,result2 with
    | Direct(data1, errors1), Direct(data2, errors2) ->
        equals data1 data2
        equals errors1 errors2
    | _ -> fail "Expected Direct GQResponse"

type InnerNullableTest = { Kaboom : string }
type NullableTest = { Inner : InnerNullableTest }

[<Fact>]
let ``Execution handles errors: properly propagates errors`` () =
    let InnerObj =
        Define.Object<InnerNullableTest>(
            "Inner", [
                Define.Field("kaboom", StringType, fun _ x -> x.Kaboom)
            ])
    let schema =
        Schema(Define.Object<NullableTest>(
                 "Type", [
                     Define.Field("inner", Nullable InnerObj, fun _ x -> Some x.Inner)
                 ]))
    let expectedData =
        NameValueLookup.ofList [
            "inner", null
        ]
    let expectedErrors = [ GQLProblemDetails.Create ("Non-Null field kaboom resolved as a null!", [ box "inner"; "kaboom" ]) ]
    let result = sync <| Executor(schema).AsyncExecute("query Example { inner { kaboom } }", { Inner = { Kaboom = null } })
    match result with
    | Direct(data, errors) ->
        result.DocumentId |> notEquals Unchecked.defaultof<int>
        data |> equals (upcast expectedData)
        errors |> equals expectedErrors
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execution handles errors: exceptions`` () =
    let schema =
        Schema(Define.Object<unit>(
                 "Type", [
                     Define.Field("a", StringType, fun _ _ -> failwith "Resolver Error!")
                 ]))
    let expectedErrors = [ GQLProblemDetails.Create ("Resolver Error!", [ box "a" ]) ]
    let result = sync <| Executor(schema).AsyncExecute("query Test { a }", ())
    ensureDirect result <| fun data errors ->
        data |> equals null
        errors |> equals expectedErrors


[<Fact>]
let ``Execution handles errors: nullable list fields`` () =
    let InnerObject =
        Define.Object<int>(
            "Inner", [
                Define.Field("error", StringType, fun _ _ -> failwith "Resolver Error!")
            ])
    let schema =
        Schema(Define.Object<unit>(
                 "Type", [
                     Define.Field("list", ListOf (Nullable InnerObject), fun _ _ -> [Some 1; Some 2; None])
                 ]))
    let expectedData =
        NameValueLookup.ofList [
            "list", upcast [null; null; null]
        ]
    let expectedErrors =
        [
            GQLProblemDetails.Create ("Resolver Error!", [ box "list"; 0; "error" ])
            GQLProblemDetails.Create ("Resolver Error!", [ box "list"; 1; "error" ])
        ]
    let result = sync <| Executor(schema).AsyncExecute("query Test { list { error } }", ())
    ensureDirect result <| fun data errors ->
        result.DocumentId |> notEquals Unchecked.defaultof<int>
        data |> equals (upcast expectedData)
        errors |> equals expectedErrors
