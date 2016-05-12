/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.MutationTests

open System
open Xunit
open FsCheck
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution

type NumberHolder = { mutable Number: int }
type Root = 
    {
        NumberHolder: NumberHolder
    }
    member x.ChangeImmediatelly num = 
        x.NumberHolder.Number <- num
        x.NumberHolder
    member x.AsyncChange num = 
        async { 
            x.NumberHolder.Number <- num 
            return x.NumberHolder
        }    
    member x.ChangeFail(num): NumberHolder = 
        failwith "Cannot change number"
    member x.AsyncChangeFail(num): Async<NumberHolder> = 
        async { 
            return failwith "Cannot change number"
        }

let NumberHolder = Define.Object("NumberHolder", [ Define.Field("theNumber", Int, fun _ x -> x.Number)])
let schema = Schema(
    query = Define.Object("Query", [ Define.Field("numberHolder", NumberHolder, fun _ x -> x.NumberHolder) ]),
    mutation = Define.Object("Mutation", [
        Define.Field("immediatelyChangeTheNumber", NumberHolder, "", [Define.Input("newNumber", Int)], fun ctx (x:Root) -> x.ChangeImmediatelly(ctx.Arg("newNumber").Value))
        Define.AsyncField("promiseToChangeTheNumber", NumberHolder, "", [Define.Input("newNumber", Int)], fun ctx (x:Root) -> x.AsyncChange(ctx.Arg("newNumber").Value))
        Define.Field("failToChangeTheNumber", NumberHolder, "", [Define.Input("newNumber", Int)], fun ctx (x:Root) -> x.ChangeFail(ctx.Arg("newNumber").Value))
        Define.AsyncField("promiseAndFailToChangeTheNumber", NumberHolder, "", [Define.Input("newNumber", Int)], fun ctx (x:Root) -> x.AsyncChangeFail(ctx.Arg("newNumber").Value))
    ]))

[<Fact>]
let ``Execute handles mutation execution ordering: evaluates mutations serially`` () =
    let query = """mutation M {
      first: immediatelyChangeTheNumber(newNumber: 1) {
        theNumber
      },
      second: promiseToChangeTheNumber(newNumber: 2) {
        theNumber
      },
      third: immediatelyChangeTheNumber(newNumber: 3) {
        theNumber
      }
      fourth: promiseToChangeTheNumber(newNumber: 4) {
        theNumber
      },
      fifth: immediatelyChangeTheNumber(newNumber: 5) {
        theNumber
      }
    }"""

    let mutationResult = sync <| schema.AsyncExecute(parse query, {NumberHolder = {Number = 6}})
    let expected: Map<string, obj> = Map.ofList [
        "first",  upcast Map.ofList [ "theNumber", 1 :> obj]
        "second", upcast Map.ofList [ "theNumber", 2 :> obj]
        "third",  upcast Map.ofList [ "theNumber", 3 :> obj]
        "fourth", upcast Map.ofList [ "theNumber", 4 :> obj]
        "fifth",  upcast Map.ofList [ "theNumber", 5 :> obj]
    ]
    noErrors mutationResult
    equals expected mutationResult.Data.Value
    
[<Fact>]
let ``Execute handles mutation execution ordering: evaluates mutations correctly in the presense of failures`` () =
    let query = """mutation M {
      first: immediatelyChangeTheNumber(newNumber: 1) {
        theNumber
      },
      second: promiseToChangeTheNumber(newNumber: 2) {
        theNumber
      },
      third: failToChangeTheNumber(newNumber: 3) {
        theNumber
      }
      fourth: promiseToChangeTheNumber(newNumber: 4) {
        theNumber
      },
      fifth: immediatelyChangeTheNumber(newNumber: 5) {
        theNumber
      }
      sixth: promiseAndFailToChangeTheNumber(newNumber: 6) {
        theNumber
      }
    }"""

    let data = {NumberHolder = {Number = 6}}
    let mutationResult = sync <| schema.AsyncExecute(parse query, data)
    let expected: Map<string, obj> = Map.ofList [
        "first",  upcast Map.ofList [ "theNumber", 1 :> obj]
        "second", upcast Map.ofList [ "theNumber", 2 :> obj]
        "third",  null
        "fourth", upcast Map.ofList [ "theNumber", 4 :> obj]
        "fifth",  upcast Map.ofList [ "theNumber", 5 :> obj]
        "sixth",  null
    ]
    equals expected mutationResult.Data.Value
    equals 2 mutationResult.Errors.Value.Length