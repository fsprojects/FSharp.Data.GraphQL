/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.ResolveTests

open System
open Xunit
open FsCheck
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution

type TestData = 
    { Test: string }
    member x.TestMethod (a: string) (b: int) = x.Test + a + b.ToString()
    member x.AsyncTestMethod (a: string) (b: int) = async { return x.Test + a + b.ToString() }
let testSchema testFields = Schema(objdef "Query" testFields)

[<Fact>]
let ``Execute uses default resolve to accesses properties`` () =
    let schema = testSchema [ Define.Field("test", String) ]
    let expected = Map.ofList [ "test", "testValue" :> obj ]
    let actual = sync <| schema.AsyncExecute(parse "{ test }", { Test = "testValue" })
    noErrors actual
    equals expected actual.Data.Value
    
[<Fact>]
let ``Execute uses default resolve to invoke methods`` () =
    let schema = testSchema [ Define.Field("testMethod", String, args = [
        Define.Arg("a", String)
        Define.Arg("b", Int)
    ]) ]
    let expected = Map.ofList [ "testMethod", "testValueArg123" :> obj ]
    let actual = sync <| schema.AsyncExecute(parse "{ testMethod(a: \"Arg\", b: 123) }", { Test = "testValue" })
    noErrors actual
    equals expected actual.Data.Value
        
[<Fact>]
let ``Execute uses provided resolve function to accesses properties`` () =
    let schema = testSchema [ 
        Define.Field("test", String, args = [Define.Arg("a", String)], resolve = fun ctx d -> d.Test + ctx.Arg("a").Value) ]
    let expected = Map.ofList [ "test", "testValueString" :> obj ]
    let actual = sync <| schema.AsyncExecute(parse "{ test(a: \"String\") }", { Test = "testValue" })
    noErrors actual
    equals expected actual.Data.Value