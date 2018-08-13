module FSharp.Data.GraphQL.Tests.SubscriptionTests

open Xunit
open FSharp.Control
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution
open System.Threading
open System.Collections.Concurrent
open FSharp.Data.GraphQL.Types

type Value =
    { Id : int
      mutable Data : string }

type Root =
    { ClientId : string }

let ValueType =
    Define.Object<Value>(
        name = "Value", 
        fieldsFn = fun () -> 
        [
            Define.Field("id", Int, (fun _ d -> d.Id))
            Define.Field("data", String, (fun _ d -> d.Data)) 
        ])

let RootType =
    Define.Object<Root>(
        name = "Query",
        description = "Root object",
        isTypeOf = (fun o -> o :? Root),
        fieldsFn = fun () -> [ Define.Field("clientId", String, (fun _ r -> r.ClientId)) ]
    )

let values = [ { Id = 1; Data = "Value 1" }; { Id = 2; Data = "Value 2" } ]

let getValue id =
    values |> Seq.tryFind (fun x -> x.Id = id)

let Query = 
    Define.Object<Root>(
        name = "Query",
        fieldsFn = fun () -> [ Define.Field("values", ListOf ValueType, (fun _ _ -> values)) ] )

let SubscriptionField =
    Define.SubscriptionField(
        "watchData", 
        RootType, 
        ValueType, 
        "Get's updated data", 
        [ Define.Input("id", Int) ], 
        fun ctx _ v -> if ctx.Arg("id") = v.Id then Some v else None)

let AsyncSubscriptionField =
    Define.AsyncSubscriptionField(
        "watchDataAsync", 
        RootType, 
        ValueType, 
        "Get's updated data asynchronously on the server", 
        [ Define.Input("id", Int) ], 
        fun ctx _ v -> async { return (if ctx.Arg("id") = v.Id then Some v else None) })

let schemaConfig = SchemaConfig.Default

let updateValue id data =
    getValue id
    |> Option.map (fun value ->
        value.Data <- data
        schemaConfig.SubscriptionProvider.Publish "watchData" value
        schemaConfig.SubscriptionProvider.Publish "watchDataAsync" value)
    |> ignore

let Subscription =
    Define.SubscriptionObject<Root>(
        name = "Subscription",
        fields = [ SubscriptionField; AsyncSubscriptionField ])

let schema = Schema(Query, subscription = Subscription, config = schemaConfig)

let executor = Executor(schema)

[<Fact>]
let ``Should be able to subscribe to sync field and get results``() =
    let expected = NameValueLookup.ofList [
        "data", upcast NameValueLookup.ofList [
            "id", upcast 1
            "data", upcast "Updated value 1"
        ]
    ]
    let query = parse """subscription Test {
        watchData(id: 1) {
            id
            data
        }
    }"""
    use mre = new ManualResetEvent(false)
    let actual = ConcurrentBag<Output>()
    let result = executor.AsyncExecute(query) |> sync
    match result with
    | Stream data ->
        data |> Observable.add (fun x -> actual.Add(x); set mre)
        updateValue 1 "Updated value 1"
        wait mre "Timeout while waiting for Stream GQLResponse"
        actual
        |> Seq.cast<NameValueLookup>
        |> contains expected
        |> ignore
    | _ -> failwith "Expected Stream GQLResponse"

[<Fact>]
let ``Should be able to subscribe to async field and get results``() =
    let expected = NameValueLookup.ofList [
        "data", upcast NameValueLookup.ofList [
            "id", upcast 1
            "data", upcast "Updated value 1"
        ]
    ]
    let query = parse """subscription Test {
  watchDataAsync(id: 1) {
    id
    data
  }
}"""
    use mre = new ManualResetEvent(false)
    let actual = ConcurrentBag<Output>()
    let result = executor.AsyncExecute(query) |> sync
    match result with
    | Stream data ->
        data |> Observable.add (fun x -> actual.Add(x); set mre)
        updateValue 1 "Updated value 1"
        wait mre "Timeout while waiting for Stream GQLResponse"
        actual
        |> Seq.cast<NameValueLookup>
        |> contains expected
        |> ignore
    | _ -> failwith "Expected Stream GQLResponse"