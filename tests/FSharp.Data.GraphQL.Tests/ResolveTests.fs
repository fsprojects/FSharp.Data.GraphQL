// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.ResolveTests

open System
open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution

type TestData =
    { Test: string }
    member x.TestMethod (a: string) (b: int) = x.Test + a + b.ToString()
    member x.AsyncTestMethod (a: string) (b: int) = async { return x.Test + a + b.ToString() }
let testSchema testFields : Schema<TestData> = Schema(Define.Object("Query", fields = testFields))

[<Fact>]
let ``Execute uses default resolve to accesses properties`` () =
    let schema = testSchema [ Define.AutoField("test", String) ]
    let expected = NameValueLookup.ofList [ "test", "testValue" :> obj ]
    let actual = sync <| Executor(schema).AsyncExecute(parse "{ test }", { Test = "testValue" })
    match actual with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

[<Fact>]
let ``Execute uses provided resolve function to accesses properties`` () =
    let schema = testSchema [ Define.Field("test", String, "", [ Define.Input("a", String) ], resolve = fun ctx d -> d.Test + ctx.Arg("a")) ]
    let expected = NameValueLookup.ofList [ "test", "testValueString" :> obj ]
    let actual = sync <| Executor(schema).AsyncExecute(parse "{ test(a: \"String\") }", { Test = "testValue" })
    match actual with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"

type private Fruit =
  | Apple
  | Banana
  | Cherry

let private enumType =
  Define.Enum(
    "Fruit",
    [
      Define.EnumValue("APPLE", Apple)
      Define.EnumValue("BANANA", Banana)
      Define.EnumValue("CHERRY", Cherry)
    ]
  )

[<Fact>]
let ``Execute resolves enums to their names`` () =
    let schema = testSchema [ Define.Field("fruits", ListOf enumType, "", [], resolve = fun ctx d -> [ Apple; Banana; Cherry ]) ]
    let expected = NameValueLookup.ofList [ "fruits", [ "APPLE"; "BANANA"; "CHERRY" ] :> obj ]
    let actual = sync <| Executor(schema).AsyncExecute(parse "{ fruits() }")
    match actual with
    | Direct(data, errors) ->
      empty errors
      data.["data"] |> equals (upcast expected)
    | _ -> fail "Expected Direct GQResponse"
