/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.ExecutionTests

open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution

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
                Define.Field("a", String, (fun _ dt -> dt.a))
                Define.Field("b", String, (fun _ dt -> dt.b))
                Define.Field("c", (ListOf (Nullable String)), (fun _ dt -> dt.c))
            ])
    let rec DataType =
      Define.Object<TestSubject>(
          "DataType",
          fieldsFn = fun () ->
          [
            Define.Field("a", String, resolve = fun _ dt -> dt.a)
            Define.Field("b", String, resolve = fun _ dt -> dt.b)
            Define.Field("c", String, resolve = fun _ dt -> dt.c)
            Define.Field("d", String, fun _ dt -> dt.d)
            Define.Field("e", String, fun _ dt -> dt.e)
            Define.Field("f", String, fun _ dt -> dt.f)
            Define.Field("pic", String, "Picture resizer", [ Define.Input("size", Nullable Int) ], fun ctx dt -> dt.pic(ctx.Arg("size")))
            Define.AsyncField("promise", DataType, fun _ dt -> dt.promise)
            Define.Field("deep", DeepDataType, fun _ dt -> dt.deep) 
        ])

    let schema = Schema(DataType)
    let schemaProcessor = Executor(schema)
    let result = sync <| schemaProcessor.AsyncExecute(ast, data, variables = Map.ofList [ "size", 100 :> obj], operationName = "Example")
    match result with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
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
            Define.Field("a", String, fun _ _ -> "Apple")
            Define.Field("b", String, fun _ _ -> "Banana")
            Define.Field("c", String, fun _ _ -> "Cherry")
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
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"
    
[<Fact>]
let ``Execution handles basic tasks: threads root value context correctly`` () = 
    let query = "query Example { a }"
    let data = { Thing = "" }
    let Thing = Define.Object<TestThing>("Type", [  Define.Field("a", String, fun _ value -> value.Thing <- "thing"; value.Thing) ])
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
            [ Define.Field("b", Nullable String, "", [ Define.Input("numArg", Int); Define.Input("stringArg", String) ], 
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
            [ Define.Field("b", Nullable String, "", [ Define.Input("enumArg", EnumType) ],
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
            [ Define.Field("b", Nullable String, "", [ Define.Input("enumArg", EnumType) ],
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
                    Define.Field("a", String, fun _ x -> x.A)
                ]))
    let result = sync <| Executor(schema).AsyncExecute(parse "{ a }", { A = "b" })
    match result with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast NameValueLookup.ofList ["a", "b" :> obj]) 
    | _ -> fail "Expected Direct GQResponse"
    
[<Fact>]
let ``Execution handles basic tasks: uses the only operation if no operation name is provided`` () =
    let schema =
        Schema(Define.Object<InlineTest>(
                "Type", [
                    Define.Field("a", String, fun _ x -> x.A)
                ]))
    let result = sync <| Executor(schema).AsyncExecute(parse "query Example { a }", { A = "b" })
    match result with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast NameValueLookup.ofList ["a", "b" :> obj]) 
    | _ -> fail "Expected Direct GQResponse"
    
[<Fact>]
let ``Execution handles basic tasks: uses the named operation if operation name is provided`` () =
    let schema =
        Schema(Define.Object<InlineTest>(
                "Type", [
                    Define.Field("a", String, fun _ x -> x.A)
                ]))
    let query = "query Example { first: a } query OtherExample { second: a }"
    let result = sync <| Executor(schema).AsyncExecute(parse query, { A = "b" }, operationName = "OtherExample")
    match result with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast NameValueLookup.ofList ["second", "b" :> obj])
    | _ -> fail "Expected Direct GQResponse"

type TwiceTest = { A : string; B : int }

[<Fact>]
let ``Execution when querying the same field twice will return it`` () =
    let schema =
      Schema(Define.Object<TwiceTest>(
                "Type", [
                    Define.Field("a", String, fun _ x -> x.A)
                    Define.Field("b", Int, fun _ x -> x.B)
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
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"
    
[<Fact>]
let ``Execution when querying returns unique document id with response`` () =
    let schema =
      Schema(Define.Object<TwiceTest>(
                "Type", [
                    Define.Field("a", String, fun _ x -> x.A)
                    Define.Field("b", Int, fun _ x -> x.B)
                ]))
    let result1 = sync <| Executor(schema).AsyncExecute("query Example { a, b, a }", { A = "aa"; B = 2 })
    let result2 = sync <| Executor(schema).AsyncExecute("query Example { a, b, a }", { A = "aa"; B = 2 })
    match result1,result2 with
    | Direct(data1, errors1), Direct(data2, errors2) ->
      data1.["documentId"] |> notEquals (null)
      data1.["documentId"] |> notEquals (upcast Unchecked.defaultof<int>)
      data1.["documentId"] |> equals (data2.["documentId"])
    | _ -> fail "Expected Direct GQResponse"

    