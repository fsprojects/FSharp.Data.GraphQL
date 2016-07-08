module FSharp.Data.GraphQL.Tests.SubscriptionTests

open System
open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution


type NumberHolder = { Number: int }
type NumberChanged = { NumberHolder: NumberHolder }
type Root =
    { NumberHolder: NumberHolder
      NumberChangedSubscribe: IObservable<NumberChanged> }

let event = Event<NumberChanged>()
let observable =
    event.Publish
    |> Observable.map id
let mutable observed : NameValueLookup list = []

let NumberHolder = Define.Object("NumberHolder", [ Define.Field("theNumber", Int, fun _ x -> x.Number) ])
let NumberChanged = Define.Object("NumberChanged", [ Define.Field("number", NumberHolder, fun _ (x : NumberChanged) -> x.NumberHolder) ])

let schema = Schema(
    query = Define.Object("Query", [ Define.Field("numberHolder", NumberHolder, fun _ x -> x.NumberHolder) ]),
    subscription = Define.Object("Subscription", [
        Define.Subscription("numberChangedSubscribe", NumberChanged, (fun _ _ o -> o.Add(fun x -> observed <- x :: observed)))
    ]))

[<Fact>]
let ``Execute subscription returns null`` () =
    let query = """subscription M {
      numberChangedSubscribe {
        number {
          theNumber
        }
      }
    }"""
    let root =
        { NumberHolder = { Number = 1 }
          NumberChangedSubscribe = observable }

    let result = sync <| schema.AsyncExecute(parse query, root)

    let expected = NameValueLookup.ofList ["numberChangedSubscribe", null]
    noErrors result
    result.["data"] |> equals (upcast expected)