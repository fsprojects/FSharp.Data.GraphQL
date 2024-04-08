module FSharp.Data.GraphQL.Tests.SubscriptionTests

open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution
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
            Define.Field("id", IntType, (fun _ d -> d.Id))
            Define.Field("data", StringType, (fun _ d -> d.Data))
        ])

let RootType =
    Define.Object<Root>(
        name = "Query",
        description = "Root object",
        isTypeOf = (fun o -> o :? Root),
        fieldsFn = fun () -> [ Define.Field("clientId", StringType, (fun _ r -> r.ClientId)) ]
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
        [ Define.Input("id", IntType) ],
        fun ctx _ v -> if ctx.Arg("id") = v.Id then Some v else None)

let TaggedSubscriptionField =
    Define.SubscriptionField(
        "watchDataByKey",
        RootType,
        ValueType,
        "Get's updated data if key is correct",
        [ Define.Input("id", IntType); Define.Input("key", StringType) ],
        (fun ctx _ v -> if ctx.Arg("id") = v.Id then Some v else None),
        tagsResolver = (fun ctx -> Tags.from (ctx.Arg<string>("key"))))

let AsyncSubscriptionField =
    Define.AsyncSubscriptionField(
        "watchDataAsync",
        RootType,
        ValueType,
        "Get's updated data asynchronously on the server",
        [ Define.Input("id", IntType) ],
        fun ctx _ v -> async { return (if ctx.Arg("id") = v.Id then Some v else None) })

let AsyncTaggedSubscriptionField =
    Define.AsyncSubscriptionField(
        "watchDataByKeyAsync",
        RootType,
        ValueType,
        "Get's updated data asynchronously on the server if key is correct",
        [ Define.Input("id", IntType); Define.Input("key", StringType) ],
        (fun ctx _ v -> async { return (if ctx.Arg("id") = v.Id then Some v else None) }),
        tagsResolver = (fun ctx -> Tags.from (ctx.Arg<string>("key"))))

let schemaConfig = SchemaConfig.Default

let updateValue id data =
    let tag = "tag1"
    getValue id
    |> Option.map (fun value ->
        value.Data <- data
        schemaConfig.SubscriptionProvider.Publish "watchData" value
        schemaConfig.SubscriptionProvider.Publish "watchDataAsync" value
        schemaConfig.SubscriptionProvider.PublishTag "watchDataByKey" tag value
        schemaConfig.SubscriptionProvider.PublishTag "watchDataByKeyAsync" tag value)
    |> ignore

let Subscription =
    Define.SubscriptionObject<Root>(
        name = "Subscription",
        fields = [ SubscriptionField; AsyncSubscriptionField; TaggedSubscriptionField; AsyncTaggedSubscriptionField ])

let schema = Schema(Query, subscription = Subscription, config = schemaConfig)

let executor = Executor(schema)

[<Fact>]
let ``Can subscribe to sync field and get results``() =
    let expected = SubscriptionResult (NameValueLookup.ofList [
            "watchData", upcast NameValueLookup.ofList [
                "id", upcast 1
                "data", upcast "Updated value 1"
            ]
        ]
    )
    let query = parse """subscription Test {
        watchData(id: 1) {
            id
            data
        }
    }"""
    let result = executor.AsyncExecute(query) |> sync
    match result with
    | Stream data ->
        use sub = Observer.create data
        updateValue 1 "Updated value 1"
        sub.WaitForItem()
        sub.Received
        |> Seq.cast<GQLSubscriptionResponseContent>
        |> contains expected
        |> ignore
    | _ -> failwith "Expected Stream GQLResponse"

[<Fact>]
let ``Can subscribe to tagged sync field and get results with expected tag``() =
    let expected = SubscriptionResult (NameValueLookup.ofList [
            "watchDataByKey", upcast NameValueLookup.ofList [
                "id", upcast 1
                "data", upcast "Updated value 1"
            ]
        ]
    )
    let query = parse """subscription Test {
        watchDataByKey(id: 1, key: "tag1") {
            id
            data
        }
    }"""
    let result = executor.AsyncExecute(query) |> sync
    match result with
    | Stream data ->
        use sub = Observer.create data
        updateValue 1 "Updated value 1"
        sub.WaitForItem()
        sub.Received
        |> Seq.cast<GQLSubscriptionResponseContent>
        |> contains expected
        |> ignore
    | _ -> failwith "Expected Stream GQLResponse"

[<Fact>]
let ``Can subscribe to tagged sync field and do not get results with unexpected tag``() =
    let query = parse """subscription Test {
        watchDataByKey(id: 1, key: "tag2") {
            id
            data
        }
    }"""
    let result = executor.AsyncExecute(query) |> sync
    match result with
    | Stream data ->
        use sub = Observer.create data
        updateValue 1 "Updated value 1"
        ensureThat (fun () -> Seq.isEmpty sub.Received) 50 "Should not get results with given tag"
    | _ -> failwith "Expected Stream GQLResponse"

[<Fact>]
let ``Can subscribe to async field and get results``() =
    let expected = SubscriptionResult (NameValueLookup.ofList [
        "watchDataAsync", upcast NameValueLookup.ofList [
            "id", upcast 1
            "data", upcast "Updated value 1"
        ]
    ])
    let query = parse """subscription Test {
  watchDataAsync(id: 1) {
    id
    data
  }
}"""
    let result = executor.AsyncExecute(query) |> sync
    match result with
    | Stream data ->
        use sub = Observer.create data
        updateValue 1 "Updated value 1"
        sub.WaitForItem()
        sub.Received
        |> Seq.cast<GQLSubscriptionResponseContent>
        |> contains expected
        |> ignore
    | _ -> failwith "Expected Stream GQLResponse"

[<Fact>]
let ``Can subscribe to tagged async field and get results with expected tag``() =
    let expected = SubscriptionResult (NameValueLookup.ofList [
            "watchDataByKeyAsync", upcast NameValueLookup.ofList [
                "id", upcast 1
                "data", upcast "Updated value 1"
            ]
        ]
    )
    let query = parse """subscription Test {
  watchDataByKeyAsync(id: 1, key: "tag1") {
    id
    data
  }
}"""
    let result = executor.AsyncExecute(query) |> sync
    match result with
    | Stream data ->
        use sub = Observer.create data
        updateValue 1 "Updated value 1"
        sub.WaitForItem()
        sub.Received
        |> Seq.cast<GQLSubscriptionResponseContent>
        |> contains expected
        |> ignore
    | _ -> failwith "Expected Stream GQLResponse"

[<Fact>]
let ``Can subscribe to tagged async field and do not get results with unexpected tag``() =
    let query = parse """subscription Test {
        watchDataByKeyAsync(id: 1, key: "tag2") {
            id
            data
        }
    }"""
    let result = executor.AsyncExecute(query) |> sync
    match result with
    | Stream data ->
        use sub = Observer.create data
        updateValue 1 "Updated value 1"
        ensureThat (fun () -> Seq.isEmpty sub.Received) 50 "Should not get results with given tag"
    | _ -> failwith "Expected Stream GQLResponse"
