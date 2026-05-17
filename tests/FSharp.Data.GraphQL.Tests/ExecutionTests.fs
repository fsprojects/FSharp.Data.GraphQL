// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.ExecutionTests

open Xunit
open System
open System.Buffers.Binary
open System.Security.Cryptography
open System.Text
open System.Text.Json
open System.Text.Json.Serialization
open System.Collections.Immutable

#nowarn "0025"
#nowarn "0040"

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Shared
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Ast.Extensions

type TestSubject = {
    a: string
    b: string
    c: string
    d: string
    e: string
    f: string
    deep: DeepTestSubject
    pic: int voption -> string
    promise: Async<TestSubject>
}

and DeepTestSubject = {
    a: string
    b: string
    c: string option
    d: string voption
    l: string option list
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
            c = Some "Contrived"
            d = ValueSome "Donut"
            l = [Some "Contrived"; None; Some "Confusing"]
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
            d
            l
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
               "c", upcast "Contrived"
               "d", upcast "Donut"
               "l", upcast ["Contrived" :> obj; null; upcast "Confusing"]
            ]
        ]

    let DeepDataType =
        Define.Object<DeepTestSubject>(
            "DeepDataType", [
                Define.Field("a", StringType, (fun _ dt -> dt.a))
                Define.Field("b", StringType, (fun _ dt -> dt.b))
                Define.Field("c", Nullable StringType, (fun _ dt -> dt.c))
                Define.Field("d", StructNullable StringType, (fun _ dt -> dt.d))
                Define.Field("l", (ListOf (Nullable StringType)), (fun _ dt -> dt.l))
            ])

    let rec DataType =
      DefineRec.Object<TestSubject>(
          "DataType",
          fieldsFn = fun () ->
          [
            Define.Field("a", StringType, resolve = fun _ dt -> dt.a)
            Define.Field("b", StringType, resolve = fun _ dt -> dt.b)
            Define.Field("c", StringType, resolve = fun _ dt -> dt.c)
            Define.Field("d", StringType, resolve = fun _ dt -> dt.d)
            Define.Field("e", StringType, fun _ dt -> dt.e)
            Define.Field("f", StringType, fun _ dt -> dt.f)
            Define.Field("pic", StringType, "Picture resizer", [ Define.Input("size", Nullable IntType) ], fun ctx dt -> dt.pic(ctx.TryArg("size")))
            Define.AsyncField("promise", DataType, fun _ dt -> dt.promise)
            Define.Field("deep", DeepDataType, fun _ dt -> dt.deep)
        ])

    let schema = Schema(DataType)
    let schemaProcessor = Executor(schema)
    let params' = JsonDocument.Parse("""{"size":100}""").RootElement.Deserialize<ImmutableDictionary<string, JsonElement>>(serializerOptions)
    let result = sync <| schemaProcessor.AsyncExecute(ast, getMockInputContext, data, variables = params', operationName = "Example")
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)

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
      DefineRec.Object(
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
    let result = sync <| schemaProcessor.AsyncExecute(ast, getMockInputContext, obj())
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)

[<Fact>]
let ``Execution handles basic tasks: threads root value context correctly`` () =
    let query = "query Example { a }"
    let data = { Thing = "" }
    let Thing = Define.Object<TestThing>("Type", [  Define.Field("a", StringType, fun _ value -> value.Thing <- "thing"; value.Thing) ])
    let result = sync <| Executor(Schema(Thing)).AsyncExecute(parse query, getMockInputContext, data)
    ensureDirect result <| fun _ errors -> empty errors
    equals "thing" data.Thing

type TestTarget =
    { mutable Num: int voption
      mutable Str: string voption }

[<Fact>]
let ``Execution handles basic tasks: correctly threads arguments`` () =
    let query = """query Example {
        b(numArg: 123, stringArg: "foo")
      }"""
    let data = { Num = ValueNone; Str = ValueNone }
    let Type =
        Define.Object("Type",
            [ Define.Field("b", StructNullable StringType, "", [ Define.Input("numArg", IntType); Define.Input("stringArg", StringType) ],
                 fun ctx value ->
                     value.Num <- ctx.TryArg("numArg")
                     value.Str <- ctx.TryArg("stringArg")
                     value.Str) ])

    let result = sync <| Executor(Schema(Type)).AsyncExecute(parse query, getMockInputContext,data)
    ensureDirect result <| fun _ errors -> empty errors
    equals (ValueSome 123) data.Num
    equals (ValueSome "foo") data.Str

[<Fact>]
let ``Execution handles basic tasks: correctly handles null arguments`` () =
    let query = """query Example {
        b(numArg: null, stringArg: null)
      }"""
    let data = { Num = ValueNone; Str = ValueNone }
    let Type =
        Define.Object("Type",
            [ Define.Field("b", StructNullable StringType, "", [ Define.Input("numArg", Nullable IntType); Define.Input("stringArg", Nullable StringType) ],
                 fun ctx value ->
                     value.Num <- ctx.TryArg("numArg")
                     value.Str <- ctx.TryArg("stringArg")
                     value.Str) ])

    let result = sync <| Executor(Schema(Type)).AsyncExecute(parse query, getMockInputContext, data)
    ensureDirect result <| fun _ errors -> empty errors
    equals ValueNone data.Num
    equals ValueNone data.Str

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
    let data = { Num = ValueNone; Str = ValueNone }
    let Type =
        Define.Object("Type",
            [ Define.Field("b", StructNullable StringType, "", [ Define.Input("enumArg", EnumType) ],
                 fun ctx value ->
                 let arg = ctx.TryArg("enumArg")
                 match arg with
                 | ValueSome (Case1) ->
                     value.Str <- ValueSome "foo"
                     value.Num <- ValueSome 123
                     value.Str
                 | _ -> ValueNone) ])
    let result = sync <| Executor(Schema(Type)).AsyncExecute(parse query, getMockInputContext, data)
    ensureDirect result <| fun _ errors -> empty errors
    equals (ValueSome 123) data.Num
    equals (ValueSome "foo") data.Str

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
    let data = { Num = ValueNone; Str = ValueNone }
    let Type =
        Define.Object("Type",
            [ Define.Field("b", StructNullable StringType, "", [ Define.Input("enumArg", EnumType) ],
                  fun ctx value ->
                  let arg = ctx.TryArg("enumArg")
                  match arg with
                  | ValueSome _ ->
                      value.Str <- ValueSome "foo"
                      value.Num <- ValueSome 123
                      value.Str
                  | _ -> ValueNone) ])
    let result = sync <| Executor(Schema(Type)).AsyncExecute(parse query, getMockInputContext, data)
    ensureDirect result <| fun _ errors -> empty errors
    equals (ValueSome 123) data.Num
    equals (ValueSome "foo") data.Str


[<Fact>]
let ``Execution handles basic tasks: uses the inline operation if no operation name is provided`` () =
    let schema =
        Schema(Define.Object<InlineTest>(
                "Type", [
                    Define.Field("a", StringType, fun _ x -> x.A)
                ]))
    let result = sync <| Executor(schema).AsyncExecute(parse "{ a }", getMockInputContext, { A = "b" })
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast NameValueLookup.ofList ["a", "b" :> obj])

[<Fact>]
let ``Execution handles basic tasks: uses the only operation if no operation name is provided`` () =
    let schema =
        Schema(Define.Object<InlineTest>(
                "Type", [
                    Define.Field("a", StringType, fun _ x -> x.A)
                ]))
    let result = sync <| Executor(schema).AsyncExecute(parse "query Example { a }", getMockInputContext, { A = "b" })
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast NameValueLookup.ofList ["a", "b" :> obj])

[<Fact>]
let ``Execution handles basic tasks: uses the named operation if operation name is provided`` () =
    let schema =
        Schema(Define.Object<InlineTest>(
                "Type", [
                    Define.Field("a", StringType, fun _ x -> x.A)
                ]))
    let query = "query Example { first: a } query OtherExample { second: a }"
    let result = sync <| Executor(schema).AsyncExecute(parse query, getMockInputContext, { A = "b" }, operationName = "OtherExample")
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast NameValueLookup.ofList ["second", "b" :> obj])

[<Fact>]
let ``Execution handles basic tasks: list of scalars`` () =
    let schema =
        Schema(Define.Object<InlineTest>(
                "Type", [
                    Define.Field("strings", ListOf StringType, fun _ _ -> ["foo"; "bar"; "baz"])
                ]))
    let result = sync <| Executor(schema).AsyncExecute("query Example { strings }", getMockInputContext)
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast NameValueLookup.ofList ["strings", box [ box "foo"; upcast "bar"; upcast "baz" ]])

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
    let result = sync <| Executor(schema).AsyncExecute(query, getMockInputContext, { A = "aa"; B = 2 });
    let expected =
      NameValueLookup.ofList [
        "a", upcast "aa"
        "b", upcast 2]
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)

[<Fact>]
let ``Execution when querying returns unique document id with response`` () =
    let schema =
      Schema(Define.Object<TwiceTest>(
                "Type", [
                    Define.Field("a", StringType, fun _ x -> x.A)
                    Define.Field("b", IntType, fun _ x -> x.B)
                ]))
    let query = "query Example { a, b, a }"
    let expectedDocumentId =
        let canonicalQuery =
            let ast = parse query
            ast.ToQueryString()
        let hash = SHA256.HashData(Encoding.UTF8.GetBytes canonicalQuery)
        BinaryPrimitives.ReadInt32BigEndian(ReadOnlySpan<byte>(hash, 0, 4))
    let result1 = sync <| Executor(schema).AsyncExecute(query, getMockInputContext, { A = "aa"; B = 2 })
    let result2 = sync <| Executor(schema).AsyncExecute(query, getMockInputContext, { A = "aa"; B = 2 })
    result1.DocumentId |> notEquals Unchecked.defaultof<int>
    result1.DocumentId |> equals expectedDocumentId
    result1.DocumentId |> equals result2.DocumentId
    match result1,result2 with
    | Direct(data1, errors1), Direct(data2, errors2) ->
        equals data1 data2
        equals errors1 errors2
    | response -> fail $"Expected a 'Direct' GQLResponse but got\n{response}"

type InnerNullableTest = { Kaboom : string }
type NullableTest = {
    Inner : InnerNullableTest
    InnerPartialSuccess : InnerNullableTest
}

[<Fact>]
let ``Execution handles errors: properly propagates errors`` () =
    let InnerObjType =
        Define.Object<InnerNullableTest>(
            "Inner", [
                Define.Field("kaboom", StringType, fun _ x -> x.Kaboom)
            ])
    let InnerPartialSuccessObjType =
        // executeResolvers/resolveWith, case 5
        let resolvePartialSuccess (ctx : ResolveFieldContext) (_ : InnerNullableTest) =
            ctx.AddError { new IGQLError with member _.Message = "Some non-critical error" }
            "Yes, Rico, Kaboom"
        Define.Object<InnerNullableTest>(
            "InnerPartialSuccess", [
                Define.Field("kaboom", StringType, resolvePartialSuccess)
            ])
    let schema =
        Schema(Define.Object<NullableTest>(
            "Type", [
                Define.Field("inner", Nullable InnerObjType, fun _ x -> Some x.Inner)
                Define.Field("partialSuccess", Nullable InnerPartialSuccessObjType, fun _ x -> Some x.InnerPartialSuccess)
            ]))
    let expectedData =
        NameValueLookup.ofList [
            "inner", null
            "partialSuccess", NameValueLookup.ofList [
                "kaboom", "Yes, Rico, Kaboom"
            ]
        ]
    let expectedErrors = [
        GQLProblemDetails.CreateWithKind ("Non-Null field kaboom resolved as a null!", Execution, [ box "inner"; "kaboom" ])
        GQLProblemDetails.CreateWithKind ("Some non-critical error", Execution, [ box "partialSuccess"; "kaboom" ])
    ]
    let result =
        let variables = { Inner = { Kaboom = null }; InnerPartialSuccess = { Kaboom = "Yes, Rico, Kaboom" } }
        sync <| Executor(schema).AsyncExecute("query Example { inner { kaboom } partialSuccess { kaboom } }", getMockInputContext, variables)
    ensureDirect result <| fun data errors ->
        result.DocumentId |> notEquals Unchecked.defaultof<int>
        data |> equals (upcast expectedData)
        errors |> equals expectedErrors

[<Fact>]
let ``Execution handles errors: exceptions`` () =
    let schema =
        Schema(Define.Object<unit>(
            "Type", [
                Define.Field("a", StringType, fun _ _ -> failwith "Resolver Error!")
            ]))
    let expectedError = GQLProblemDetails.CreateWithKind ("Resolver Error!", Execution, [ box "a" ])
    let result = sync <| Executor(schema).AsyncExecute("query Test { a }", getMockInputContext, ())
    ensureRequestError result <| fun [ error ] -> error |> equals expectedError

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
            GQLProblemDetails.CreateWithKind ("Resolver Error!", Execution, [ box "list"; 0; "error" ])
            GQLProblemDetails.CreateWithKind ("Resolver Error!", Execution, [ box "list"; 1; "error" ])
        ]
    let result = sync <| Executor(schema).AsyncExecute("query Test { list { error } }", getMockInputContext, ())
    ensureDirect result <| fun data errors ->
        result.DocumentId |> notEquals Unchecked.defaultof<int>
        data |> equals (upcast expectedData)
        errors |> equals expectedErrors


[<Fact>]
let ``Execution handles errors: additional error added when exception is raised in a nullable field resolver`` () =
    let InnerNullableExceptionObjType =
        // executeResolvers/resolveWith, case 1
        let resolveWithException (ctx : ResolveFieldContext) (_ : InnerNullableTest) : string option =
            ctx.AddError { new IGQLError with member _.Message = "Non-critical error" }
            raise (Exception "Unexpected error")
        Define.Object<InnerNullableTest>(
            "InnerNullableException", [
                Define.Field("kaboom", Nullable StringType, resolve = resolveWithException)
            ])
    let schema =
        Schema(Define.Object<NullableTest>(
            "Type", [
                Define.Field("inner", Nullable InnerNullableExceptionObjType, fun _ x -> Some x.Inner)
            ]))
    let expectedData =
        NameValueLookup.ofList [
            "inner", NameValueLookup.ofList [
                "kaboom", null
            ]
        ]
    let expectedErrors =
        [
             GQLProblemDetails.CreateWithKind ("Unexpected error", Execution, [ box "inner"; "kaboom" ])
             GQLProblemDetails.CreateWithKind ("Non-critical error", Execution, [ box "inner"; "kaboom" ])
        ]
    let result =
        let variables = { Inner = { Kaboom = null }; InnerPartialSuccess = { Kaboom = "Yes, Rico, Kaboom" } }
        sync <| Executor(schema).AsyncExecute("query Example { inner { kaboom } }", getMockInputContext, variables)
    ensureDirect result <| fun data errors ->
        result.DocumentId |> notEquals Unchecked.defaultof<int>
        data |> equals (upcast expectedData)
        errors |> equals expectedErrors

[<Fact>]
let ``Execution handles errors: additional error added when None returned from a nullable field resolver`` () =
    let InnerNullableNoneObjType =
        // executeResolvers/resolveWith, case 2
        let resolveWithNone (ctx : ResolveFieldContext) (_ : InnerNullableTest) : string option =
            ctx.AddError { new IGQLError with member _.Message = "Non-critical error" }
            None
        Define.Object<InnerNullableTest>(
            "InnerNullableException", [
                Define.Field("kaboom", Nullable StringType, resolve = resolveWithNone)
            ])
    let schema =
        Schema(Define.Object<NullableTest>(
            "Type", [
                Define.Field("inner", Nullable InnerNullableNoneObjType, fun _ x -> Some x.Inner)
            ]))
    let expectedData =
        NameValueLookup.ofList [
            "inner", NameValueLookup.ofList [
                "kaboom", null
            ]
        ]
    let expectedErrors =
        [
             GQLProblemDetails.CreateWithKind ("Non-critical error", Execution, [ box "inner"; "kaboom" ])
        ]
    let result =
        let variables = { Inner = { Kaboom = null }; InnerPartialSuccess = { Kaboom = "Yes, Rico, Kaboom" } }
        sync <| Executor(schema).AsyncExecute("query Example { inner { kaboom } }", getMockInputContext, variables)
    ensureDirect result <| fun data errors ->
        result.DocumentId |> notEquals Unchecked.defaultof<int>
        data |> equals (upcast expectedData)
        errors |> equals expectedErrors

[<Fact>]
let ``Execution handles errors: additional error added when exception is rised in a non-nullable field resolver`` () =
    let InnerNonNullableExceptionObjType =
        // executeResolvers/resolveWith, case 3
        let resolveWithException (ctx : ResolveFieldContext) (_ : InnerNullableTest) : string =
            ctx.AddError { new IGQLError with member _.Message = "Non-critical error" }
            raise (Exception "Fatal error")
        Define.Object<InnerNullableTest>(
            "InnerNonNullableException", [
                Define.Field("kaboom", StringType, resolve = resolveWithException)
            ])
    let schema =
        Schema(Define.Object<NullableTest>(
            "Type", [
                Define.Field("inner", InnerNonNullableExceptionObjType, fun _ x -> x.Inner)
            ]))
    let expectedErrors =
        [
             GQLProblemDetails.CreateWithKind ("Fatal error", Execution, [ box "inner"; "kaboom" ])
             GQLProblemDetails.CreateWithKind ("Non-critical error", Execution, [ box "inner"; "kaboom" ])
        ]
    let result =
        let variables = { Inner = { Kaboom = "Yes, Rico, Kaboom" }; InnerPartialSuccess = { Kaboom = "Yes, Rico, Kaboom" } }
        sync <| Executor(schema).AsyncExecute("query Example { inner { kaboom } }", getMockInputContext, variables)
    ensureRequestError result <| fun  errors ->
        result.DocumentId |> notEquals Unchecked.defaultof<int>
        errors |> equals expectedErrors

[<Fact>]
let ``Execution handles errors: additional error added and when null returned from a non-nullable field resolver`` () =
    let InnerNonNullableNullObjType =
        // executeResolvers/resolveWith, case 4
        let resolveWithNull (ctx : ResolveFieldContext) (_ : InnerNullableTest) : string =
            ctx.AddError { new IGQLError with member _.Message = "Non-critical error" }
            null
        Define.Object<InnerNullableTest>(
            "InnerNonNullableNull", [
                Define.Field("kaboom", StringType, resolveWithNull)
            ])
    let schema =
        Schema(Define.Object<NullableTest>(
            "Type", [
                Define.Field("inner", InnerNonNullableNullObjType, fun _ x -> x.Inner)
            ]))
    let expectedErrors =
        [
            GQLProblemDetails.CreateWithKind ("Non-Null field kaboom resolved as a null!", Execution, [ box "inner"; "kaboom" ])
            GQLProblemDetails.CreateWithKind ("Non-critical error", Execution, [ box "inner"; "kaboom" ])
        ]
    let result =
        let variables = { Inner = { Kaboom = "Yes, Rico, Kaboom" }; InnerPartialSuccess = { Kaboom = "Yes, Rico, Kaboom" } }
        sync <| Executor(schema).AsyncExecute("query Example { inner { kaboom } }", getMockInputContext, variables)
    ensureRequestError result <| fun errors ->
        result.DocumentId |> notEquals Unchecked.defaultof<int>
        errors |> equals expectedErrors
