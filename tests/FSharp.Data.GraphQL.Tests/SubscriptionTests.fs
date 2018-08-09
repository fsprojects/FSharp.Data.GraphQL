module FSharp.Data.GraphQL.Tests.SubscriptionTests

open System
open Xunit
open FSharp.Control
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution
open System.Threading
open System.Collections.Concurrent
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Tests
open FSharp.Data.GraphQL.Tests

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

let root = { ClientId = "5" }

let values = [ { Id = 1; Data = "Value 1" }; { Id = 2; Data = "Value 2" } ]

let Query = 
    Define.Object<Root>(
        name = "Query",
        fieldsFn = fun () -> [ Define.Field("values", ListOf ValueType, (fun _ _ -> values)) ] )

let SubscriptionField =
    Define.SubscriptionField(
        "updatedData", 
        RootType, 
        ValueType, 
        "Get's updated data", 
        [ Define.Input("id", String) ], 
        fun ctx _ v -> if ctx.Arg("id") = v.Id then Some v else None)

let AsyncSubscriptionField =
    Define.SubscriptionAsyncField(
        "updatedDataAsync", 
        RootType, 
        ValueType, 
        "Get's updated data asynchronously on the server", 
        [ Define.Input("id", String) ], 
        fun ctx _ v -> async { return (if ctx.Arg("id") = v.Id then Some v else None) })

let Subscription =
    Define.SubscriptionObject<Root>(
        name = "Subscription",
            fields = [ SubscriptionField; AsyncSubscriptionField ])

let schemaConfig = SchemaConfig.Default

let updateValue id data =
    let value = values |> List.find (fun x -> x.Id = id)
    value.Data <- data
    schemaConfig.SubscriptionProvider.Publish SubscriptionField value
    schemaConfig.SubscriptionProvider.Publish AsyncSubscriptionField value

let schema = Schema(Query, subscription = Subscription, config = schemaConfig)

let executor = Executor(schema)

[<Fact>]
let ``Should be able to perform normal query in a schema with Subscriptions``() =
    let expected =
        NameValueLookup.ofList [
            "values", upcast [ 
                box <| NameValueLookup.ofList [
                    "id", upcast 1
                    "data", upcast "Value 1" 
                ]
                upcast NameValueLookup.ofList [
                    "id", upcast 2
                    "data", upcast "Value 2"
                ]
            ]
        ]
    let query = parse """{
        values {
            id
            data
        }
    }"""
    let result = executor.AsyncExecute(query) |> sync
    match result with
    | Direct (data, errors) ->
        empty errors
        data.["data"] |> equals (upcast expected)
    | _ -> failwith "Expected Direct GQLResponse"

[<Fact>]
let ``Should be able to subscribe to sync field and get results``() =
    let expected = NameValueLookup.ofList [
        "id", upcast 1
        "data", upcast "Value 1"
    ]
    let query = parse """"{
            updatedData (id : 1) {
                id
                data
            }
        }"""
    use mre = new ManualResetEvent(false)
    let actual = ConcurrentBag<Output>()
    let plan = executor.CreateExecutionPlan(query)
    let result = executor.AsyncExecute(plan, data = root) |> sync
    match result with
    | Stream data ->
        data |> Observable.add (fun x -> actual.Add(x); set mre)
        wait mre "Timeout while waiting for Deferred GQLResponse"
        actual
        |> Seq.cast<NameValueLookup>
        |> contains expected
        |> ignore
    | _ -> failwith "Expected Stream GQLResponse"