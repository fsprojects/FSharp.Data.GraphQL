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

let NumberHolder = Define.Object("NumberHolder", [ Define.Field("theNumber", Int, fun _ x -> x.Number) ])
let NumberChanged = Define.Object("NumberChanged", [ Define.Field("number", NumberHolder, fun _ (x : NumberChanged) -> x.NumberHolder) ])
let Query = Define.Object("Query", [ Define.Field("numberHolder", NumberHolder, fun _ x -> x.NumberHolder) ])

let schema f = Schema(
    query = Query,
    subscription = Define.Object("Subscription", [
        Define.Subscription("numberChangedSubscribe", NumberChanged, (fun _ _ o -> o.Add f))
    ]))

let rec toLookup x =
    let convert x =
        match box x with
        | :? ((string * _) list) as l -> toLookup l :> obj
        | _ -> x

    let rec traverse = function
        | [] -> []
        | (x,l) :: xs -> (x, convert l) :: traverse xs
        
    traverse x
    |> NameValueLookup.ofList

[<Fact>]
let ``Execute subscription returns null`` () =
    let mutable observed : NameValueLookup list = []
    let schema = schema (fun x -> observed <- x :: observed)
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

[<Fact>]
let ``Execute subscription triggers observable when event is triggered after subscription`` () =
    let mutable observed : NameValueLookup list = []
    let schema = schema (fun x -> observed <- x :: observed)
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

    event.Trigger { NumberHolder = { Number = 1 } }

    let result = sync <| schema.AsyncExecute(parse query, root)

    event.Trigger { NumberHolder = { Number = 2 } }
    event.Trigger { NumberHolder = { Number = 3 } }

    let expected = NameValueLookup.ofList ["numberChangedSubscribe", null]
    noErrors result
    result.["data"] |> equals (upcast expected)

    let expected =
        [ [ "numberChangedSubscribe", box ["number", box [ "theNumber", box 2 ] ] ]
          [ "numberChangedSubscribe", box ["number", box [ "theNumber", box 3 ] ] ] ]
        |> List.map toLookup
    observed |> List.rev |> equals expected

[<Fact>]
let ``Execute subscription handles label`` () =
    let mutable observed : NameValueLookup list = []
    let schema = schema (fun x -> observed <- x :: observed)
    let query = """subscription M {
      label: numberChangedSubscribe {
        number {
          tn: theNumber
        }
      }
    }"""
    let root =
        { NumberHolder = { Number = 1 }
          NumberChangedSubscribe = observable }

    let result = sync <| schema.AsyncExecute(parse query, root)

    event.Trigger { NumberHolder = { Number = 1 } }
    event.Trigger { NumberHolder = { Number = 2 } }

    let expected = NameValueLookup.ofList ["label", null]
    noErrors result
    result.["data"] |> equals (upcast expected)

    let expected =
        [ [ "label", box ["number", box [ "tn", box 1 ] ] ]
          [ "label", box ["number", box [ "tn", box 2 ] ] ] ]
        |> List.map toLookup
    observed |> List.rev |> equals expected

[<Fact>]
let ``Execute resolves correctly`` () =
    let mutable observed : NameValueLookup list = []
    let anotherEvent = Event<NumberChanged>()
    let anotherObservable =
        anotherEvent.Publish
        |> Observable.map id
    let schema = Schema(
        query = Query,
        subscription = Define.Object("Subscription", [
            Define.Subscription("numberChangedSubscribe", NumberChanged,
                (fun _ _ o -> o.Add (fun x -> observed <- x :: observed)),
                (fun _ _ -> anotherObservable))
        ]))
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

    schema.AsyncExecute(parse query, root) |> sync |> ignore

    event.Trigger { NumberHolder = { Number = 1 } }
    event.Trigger { NumberHolder = { Number = 2 } }
    anotherEvent.Trigger { NumberHolder = { Number = 3 } }
    anotherEvent.Trigger { NumberHolder = { Number = 4 } }

    let expected =
        [ [ "numberChangedSubscribe", box ["number", box [ "theNumber", box 3 ] ] ]
          [ "numberChangedSubscribe", box ["number", box [ "theNumber", box 4 ] ] ] ]
        |> List.map toLookup
    observed |> List.rev |> equals expected

[<Fact>]
let ``Execute handles subscription with multiple arguments`` () =
    let mutable observed : NameValueLookup list = []
    let schema = Schema(
        query = Query,
        subscription = Define.Object("Subscription", [
            Define.Subscription("numberChangedSubscribe", NumberChanged,
                (fun ctx _ o ->
                    o.Add (fun x -> observed <- x :: observed)
                    event.Trigger { NumberHolder = { Number = ctx.Arg("id") } }
                    event.Trigger { NumberHolder = { Number = ctx.Arg("client") } } ),
                args = [Define.Input("id", Int); Define.Input("client", Int)])
        ]))
    let query = """subscription M ($arg1: Int!, $arg2: Int!) {
      numberChangedSubscribe(id: $arg1, client: 4) {
        number {
          theNumber
        }
      }
    }"""
    let root =
        { NumberHolder = { Number = 1 }
          NumberChangedSubscribe = observable }

    schema.AsyncExecute(parse query, root, Map.ofList [ "arg1", box 5; "arg2", box 33]) |> sync |> ignore

    event.Trigger { NumberHolder = { Number = 1 } }
    event.Trigger { NumberHolder = { Number = 2 } }

    let expected =
        [ [ "numberChangedSubscribe", box ["number", box [ "theNumber", box 5 ] ] ]
          [ "numberChangedSubscribe", box ["number", box [ "theNumber", box 4 ] ] ]
          [ "numberChangedSubscribe", box ["number", box [ "theNumber", box 1 ] ] ]
          [ "numberChangedSubscribe", box ["number", box [ "theNumber", box 2 ] ] ] ]
        |> List.map toLookup
    observed |> List.rev |> equals expected