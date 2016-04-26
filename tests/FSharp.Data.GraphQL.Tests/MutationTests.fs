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
        x.NumberHolder.Number
    member x.ChangeAsync num = 
        async { 
            x.NumberHolder.Number <- num 
            return x.NumberHolder.Number
        }    
    member x.ChangeFail(num): int = 
        failwith "Cannot change number"
    member x.ChangeFailAsync(num): Async<int> = 
        async { 
            return failwith "Cannot change number"
        }

let NumberHolder = objdef "NumberHolder" [ field "theNumber" Int (fun x -> x.Number)]
let schema = Schema(
    query = objdef "Query" [ field "numberHolder" NumberHolder (fun x -> x.NumberHolder) ],
    mutation = objdef "Mutation" [
        fieldA "immediatelyChangeTheNumber" NumberHolder [arg "newNumber" Int] (fun ctx (x:Root) -> x.ChangeImmediatelly(ctx.Arg("newNumber").Value))
        fieldA "promiseToChangeTheNumber" NumberHolder [arg "newNumber" Int] (fun ctx (x:Root) -> x.ChangeAsync(ctx.Arg("newNumber").Value))
        fieldA "failToChangeTheNumber" NumberHolder [arg "newNumber" Int] (fun ctx (x:Root) -> x.ChangeFail(ctx.Arg("newNumber").Value))
        fieldA "promiseAndFailToChangeTheNumber" NumberHolder [arg "newNumber" Int] (fun ctx (x:Root) -> x.ChangeFailAsync(ctx.Arg("newNumber").Value))
    ])

[<Fact>]
let ``Execute: Handles mutation execution ordering: evaluates mutations serially`` () =
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

    let mutationResult = schema.Execute(parse query, {NumberHolder = {Number = 6}})
    let expected: Map<string, obj> = Map.ofList [
        "first",  upcast Map.ofList [ "theNumber", 1 :> obj]
        "second", upcast Map.ofList [ "theNumber", 2 :> obj]
        "third",  upcast Map.ofList [ "theNumber", 3 :> obj]
        "fourth", upcast Map.ofList [ "theNumber", 4 :> obj]
        "fifth",  upcast Map.ofList [ "theNumber", 5 :> obj]
    ]
    equals expected mutationResult.Data.Value
    
[<Fact>]
let ``Execute: Handles mutation execution ordering: evaluates mutations correctly in the presense of a failed mutation`` () =
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

    let mutationResult = schema.Execute(parse query, {NumberHolder = {Number = 6}})
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