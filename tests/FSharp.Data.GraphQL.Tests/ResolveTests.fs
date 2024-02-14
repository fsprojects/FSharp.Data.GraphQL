// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.ResolveTests

open System
open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution

type TestData = {
    Test : string
} with

    member x.TestMethod (a : string) (b : int) = x.Test + a + b.ToString ()
    member x.AsyncTestMethod (a : string) (b : int) = async { return x.Test + a + b.ToString () }

let testSchema testFields : Schema<TestData> = Schema (Define.Object ("Query", fields = testFields))

[<Fact>]
let ``Execute uses default resolve to accesses properties`` () =
    let schema = testSchema [ Define.AutoField ("test", StringType) ]
    let expected = NameValueLookup.ofList [ "test", "testValue" :> obj ]

    let result = sync <| Executor(schema).AsyncExecute (parse "{ test }", { Test = "testValue" })
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)

[<Fact>]
let ``Execute uses provided resolve function to accesses properties`` () =
    let schema =
        testSchema [
            Define.Field ("test", StringType, "", [ Define.Input ("a", StringType) ], resolve = (fun ctx d -> d.Test + ctx.Arg ("a")))
        ]

    let expected = NameValueLookup.ofList [ "test", "testValueString" :> obj ]
    let result = sync <| Executor(schema) .AsyncExecute (parse "{ test(a: \"String\") }", { Test = "testValue" })
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)

type private Fruit =
    | Apple
    | Banana
    | Cherry
    | DragonFruit

module private Fruit =

    let show =
        function
        | Apple -> "apple"
        | Banana -> "banana"
        | Cherry -> "cherry"
        | DragonFruit -> "dragon fruit"


let private fruitType =
    Define.Enum (
        "Fruit",
        [
            Define.EnumValue ("APPLE", Apple)
            Define.EnumValue ("BANANA", Banana)
            Define.EnumValue ("CHERRY", Cherry)
            Define.EnumValue ("DRAGON_FRUIT", DragonFruit)
        ]
    )

[<Fact>]
let ``Execute resolves enums to their names`` () =
    let schema =
        testSchema [
            Define.Field ("fruits", ListOf fruitType, "", [], resolve = (fun ctx d -> [ Apple; Banana; Cherry; DragonFruit ]))
        ]

    let expected = NameValueLookup.ofList [ "fruits", [ "APPLE"; "BANANA"; "CHERRY"; "DRAGON_FRUIT" ] :> obj ]
    let result = sync <| Executor(schema).AsyncExecute (parse "{ fruits() }")
    ensureDirect result
    <| fun data errors ->
        empty errors
        data |> equals (upcast expected)

[<Fact>]
let ``Execute resolves enums arguments from their names`` () =
    let schema =
        testSchema [
            Define.Field (
                "foo",
                StringType,
                "",
                [ Define.Input ("fruit", fruitType, defaultValue = Cherry) ],
                resolve =
                    fun ctx _ ->
                        let fruit = ctx.Arg "fruit"
                        $"You asked for {Fruit.show fruit}"
            )
        ]

    let expected = NameValueLookup.ofList [ "foo", box "You asked for dragon fruit" ]
    let result = sync <| Executor(schema).AsyncExecute (parse "{ foo(fruit: DRAGON_FRUIT) }")
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expected)
