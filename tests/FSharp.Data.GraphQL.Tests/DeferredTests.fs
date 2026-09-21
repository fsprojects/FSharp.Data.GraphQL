module FSharp.Data.GraphQL.Tests.DeferredTests

open System
open System.Collections.Immutable
open System.Text.Json
open Xunit
open System.Threading
open FSharp.Control
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Shared
open FSharp.Data.GraphQL.Types

#nowarn "40"

let ms x =
    let factor =
        match Environment.ProcessorCount with
        | x when x >= 8 -> 1
        | x when x >= 4 -> 4
        | _ -> 6
    x * factor

let delay time x = async {
    do! Async.Sleep(ms time)
    return x }

type TestSubject = {
    id: string
    a: string
    b: string
    union: UnionTestSubject
    list: UnionTestSubject list
    innerList: InnerTestSubject list
    iface : InterfaceSubject
    ifaceList : InterfaceSubject list
    mutable live: string
    delayed : AsyncTestSubject
    delayedList : AsyncTestSubject list
    resolverError : NonNullAsyncTestSubject
    resolverListError : NonNullAsyncTestSubject list
    nullableError : NonNullAsyncTestSubject
    nullableListError : NonNullAsyncTestSubject list
    bufferedList : AsyncTestSubject list
    /// A nullable object that resolves to null, so nothing deferred below it can ever be delivered
    nullObject : AsyncTestSubject option
    /// An object whose nested non-null field fails, to observe error bubbling inside a deferred payload
    container : ContainerSubject
}

and AsyncTestSubject = {
    value : Async<string option>
}

and NonNullAsyncTestSubject = {
    value : Async<string>
}

and ContainerSubject = {
    name : string
    inner : NonNullAsyncTestSubject
}

and InnerTestSubject = {
    a : string
    innerList : InnerTestSubject list
}

and UnionTestSubject =
   | A of A
   | B of B

and A = {
    id: string
    a: string
}

and B = {
    id: string
    b: int
}

and C =
    { id : string
      value : string }
    interface InterfaceSubject with
        member this.Id = this.id
        member this.Value = this.value
and D =
    { id : string
      value : string }
    interface InterfaceSubject with
        member this.Id = this.id
        member this.Value = this.value

and InterfaceSubject =
    abstract member Id : string
    abstract member Value : string

let AType =
    Define.Object<A>(
        "A", [
            Define.Field("a", Nullable StringType, resolve = fun _ a -> Some a.a)
            Define.Field("id", Nullable StringType, resolve = fun _ a -> Some a.id)
        ])

let BType =
    Define.Object<B>(
        "B", [
            Define.Field("id", StringType, (fun _ (b : B) -> b.id))
            Define.Field("b", IntType, (fun _ b -> b.b))
        ])

let InterfaceType =
    Define.Interface(
        "TestInterface", [
            Define.Field("id", StringType, resolve = fun _ (x : InterfaceSubject) -> x.Id)
            Define.Field("value", Nullable StringType, resolve = fun _ (x : InterfaceSubject) -> Some x.Value)
        ])

let CType =
    Define.Object<C>(
        name ="C",
        fields = [
            Define.Field("id", StringType, (fun _ (c : C) -> c.id))
            Define.Field("value", Nullable StringType, (fun _ (c: C) -> Some c.value))
        ],
        interfaces = [ InterfaceType ],
        isTypeOf = (fun o -> o :? C))

let DType =
    Define.Object<D>(
        name = "D",
        fields = [
            Define.Field("id", StringType, (fun _ (d : D) -> d.id))
            Define.Field("value", Nullable StringType, (fun _ d -> Some d.value))
        ],
        interfaces = [ InterfaceType ],
        isTypeOf = (fun o -> o :? D))

let UnionType =
    Define.Union(
        name = "Union",
        options = [ AType; BType ] ,
        resolveValue = (fun u ->
            match u with
            | A a -> box a
            | B b -> box b),
        resolveType = (fun u ->
            match u with
            | A _ -> upcast AType
            | B _ -> upcast BType))

let rec InnerDataType =
    DefineRec.Object<InnerTestSubject>(
        name = "InnerData",
        fieldsFn = fun () ->
        [
            Define.Field("a", StringType, (fun _ (d: InnerTestSubject) -> d.a))
            Define.Field("innerList", Nullable (ListOf InnerDataType), (fun _ d -> Some d.innerList))
        ])

let AsyncDataType =
    Define.Object<AsyncTestSubject>(
        name = "AsyncData",
        fields = [ Define.AsyncField("value", Nullable StringType, (fun _ d -> d.value )) ])

let NonNullAsyncDataType =
    Define.Object<NonNullAsyncTestSubject>(
        name = "NonNullAsyncData",
        fields = [ Define.AsyncField("value", StringType, (fun _ d -> d.value )) ])

let ContainerType =
    Define.Object<ContainerSubject>(
        name = "Container",
        fields = [
            Define.Field("name", StringType, (fun _ (c : ContainerSubject) -> c.name))
            // Nullable, so an error in its non-null `value` nulls `inner` and stops there, not at the container
            Define.Field("inner", Nullable NonNullAsyncDataType, (fun _ (c : ContainerSubject) -> Some c.inner))
        ])

let DataType =
    DefineRec.Object<TestSubject>(
        name = "Data",
        fieldsFn = fun () ->
        [
            Define.Field("id", StringType, (fun _ (d: TestSubject) -> d.id))
            Define.Field("a", Nullable StringType, (fun _ (d: TestSubject) -> Some d.a))
            Define.Field("b", Nullable StringType, (fun _ (d: TestSubject) -> Some d.b))
            Define.Field("union", Nullable UnionType, (fun _ d -> Some d.union))
            Define.Field("list", Nullable (ListOf UnionType), (fun _ d -> Some d.list))
            Define.Field("innerList", Nullable (ListOf InnerDataType), (fun _ (d: TestSubject) -> Some d.innerList))
            Define.Field("live", StringType, (fun _ d -> d.live))
            Define.Field("iface", Nullable InterfaceType, (fun _ d -> Some d.iface))
            Define.Field("ifaceList", Nullable (ListOf InterfaceType), (fun _ d -> Some d.ifaceList))
            Define.Field("delayed", Nullable AsyncDataType, (fun _ d -> Some d.delayed))
            Define.Field("delayedList", ListOf AsyncDataType, (fun _ d -> d.delayedList))
            Define.Field("resolverError", Nullable NonNullAsyncDataType, (fun _ d -> Some d.resolverError))
            Define.Field("nullableError", Nullable NonNullAsyncDataType, (fun _ d -> Some d.nullableError))
            Define.Field("resolverListError", Nullable (ListOf NonNullAsyncDataType), (fun _ d -> Some d.resolverListError))
            Define.Field("nullableListError", Nullable (ListOf NonNullAsyncDataType), (fun _ d -> Some d.nullableListError))
            Define.Field("bufferedList", ListOf AsyncDataType, (fun _ d -> d.bufferedList))
            Define.Field("nullObject", Nullable AsyncDataType, (fun _ (d: TestSubject) -> d.nullObject))
            Define.Field("container", Nullable ContainerType, (fun _ (d: TestSubject) -> Some d.container))
            // A non-null field whose failure propagates to the object containing it
            Define.Field("nonNullError", StringType, (fun _ (_ : TestSubject) -> failwith "Non-null field error!"))
        ])

let data = {
       id = "1"
       a = "Apple"
       b = "Banana"
       union = A {
           id = "1"
           a = "Union A"
       }
       list = [
           A {
               id = "2"
               a = "Union A"
           };
           B {
               id = "3"
               b = 4
           }
       ]
       innerList = [
           { a = "Inner A"; innerList = [ { a = "Inner B"; innerList = [] }; { a = "Inner C"; innerList = [] } ] }
       ]
       live = "some value"
       iface = { C.id = "1000"; value = "C" }
       ifaceList = [
            { D.id = "2000"; value = "D" }; { C.id = "3000"; value = "C2" }
       ]
       delayed = { value = delay 5000 (Some "Delayed value") }
       delayedList = [
           { value = delay 5000 (Some "Slow") }
           { value = async { return (Some "Fast") } }
       ]
       resolverError = { value = async { return failwith "Resolver error!" } }
       resolverListError = [
           { value = async { return failwith "Resolver error!" } }
           { value = async { return failwith "Resolver error!" } }
       ]
       nullableError = { value = async { return null } }
       nullableListError = [
           { value = async { return null } }
           { value = async { return null } }
       ]
       bufferedList = [
            { value = delay 5000 (Some "Buffered 1") }
            { value = delay 1000 (Some "Buffered 2") }
            { value = async { return (Some "Buffered 3") } }
       ]
       nullObject = None
       container = { name = "Container"; inner = { value = async { return null } } }
   }

let Query =
    DefineRec.Object<TestSubject>(
        name = "Query",
        fieldsFn = fun () ->
        [
            Define.Field("listData", ListOf UnionType, (fun _ _ -> data.list))
            Define.Field("testData", DataType, (fun _ _ -> data))
            Define.Field("nullableTestData", Nullable DataType, (fun _ _ -> Some data))
        ])

let Mutation =
    Define.Object<TestSubject>(
        name = "Mutation",
        fields = [ Define.Field("touch", Nullable DataType, (fun _ _ -> Some data)) ])

let schemaConfig =
    { SchemaConfig.DefaultWithBufferedStream(streamOptions = { Interval = ValueNone; PreferredBatchSize = ValueNone }) with Types = [ CType; DType ] }


let sub =
    { FieldName = "live"
      TypeName = "Data"
      Filter = (fun (x : TestSubject) (y : TestSubject) -> x.id = y.id)
      Project = _.live }

schemaConfig.LiveFieldSubscriptionProvider.Register sub

let schema = Schema(Query, Mutation, config = schemaConfig)

let executor = Executor(schema)

let hasSubscribers () =
    schemaConfig.LiveFieldSubscriptionProvider.HasSubscribers "Data" "live"

let resetLiveData () =
    data.live <- "some value"

let updateLiveData () =
    data.live <- "another value"
    schemaConfig.LiveFieldSubscriptionProvider.Publish "Data" "live" data

[<Fact>]
let ``Resolver error`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "resolverError", null
            ]
        ]
    let expectedDeferred =
        DeferredErrors (
            null,
            [ GQLProblemDetails.CreateWithKind ("Resolver error!", Execution, [ box "testData"; "resolverError"; "value" ]) ],
            [ "testData"; "resolverError" ]
        )
    let query = parse """{
        testData {
            resolverError @defer {
                value
            }
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        (sub.Received |> withoutCompleted) |> single |> equals expectedDeferred

[<Fact>]
let ``Resolver list error`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "resolverListError", upcast []
            ]
        ]
    let expectedDeferred1 =
        DeferredErrors (
            null,
            [ GQLProblemDetails.CreateWithKind ("Resolver error!", Execution, [ box "testData"; "resolverListError"; 0; "value" ]) ],
            [ box "testData"; "resolverListError"; 0 ]
        )
    let expectedDeferred2 =
        DeferredErrors (
            null,
            [ GQLProblemDetails.CreateWithKind ("Resolver error!", Execution, [ box "testData"; "resolverListError"; 1; "value" ]) ],
            [ box "testData"; "resolverListError"; 1 ]
        )
    let query = parse """{
        testData {
            resolverListError @stream {
                value
            }
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted(2)
        (sub.Received |> withoutCompleted)
        |> Seq.cast<GQLDeferredResponseContent>
        |> contains expectedDeferred1
        |> contains expectedDeferred2
        |> ignore

[<Fact>]
let ``Nullable error`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "nullableError", null
            ]
        ]
    let expectedDeferred =
        DeferredErrors (
            null,
            [ GQLProblemDetails.CreateWithKind ("Non-Null field value resolved as a null!", Execution, [ box "testData"; "nullableError"; "value" ]) ],
            [ "testData"; "nullableError" ]
        )
    let query = parse """{
        testData {
            nullableError @defer {
                value
            }
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        (sub.Received |> withoutCompleted) |> single |> equals expectedDeferred

[<Fact>]
let ``Single Root object field - Defer and Stream`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "iface", null
            ]
        ]
    let expectedDeferred =
        DeferredResult (
            NameValueLookup.ofList [
                "id", upcast "1000"
                "value", upcast "C"
            ],
            [ "testData"; "iface" ]
        )
    let query = """{
        testData {
            iface @defer {
                id
                value
            }
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        (sub.Received |> withoutCompleted) |> single |> equals expectedDeferred

[<Fact>]
let ``Single Root object list field - Defer`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "ifaceList", upcast null
            ]
        ]
    let expectedDeferred =
        DeferredResult ([|
                NameValueLookup.ofList [
                    "id", upcast "2000"
                    "value", upcast "D"
                ]
                NameValueLookup.ofList [
                    "id", upcast "3000"
                    "value", upcast "C2"
                ]
            |],
            [ "testData"; "ifaceList" ]
        )
    let query = parse """{
        testData {
            ifaceList @defer {
                id
                value
            }
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        (sub.Received |> withoutCompleted) |> single |> equals expectedDeferred

[<Fact>]
let ``Single Root object list field - Stream`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "ifaceList", upcast [ ]
            ]
        ]
    let expectedDeferred1 =
        DeferredResult ([|
                NameValueLookup.ofList [
                    "id", upcast "2000"
                    "value", upcast "D"
                ]
            |],
            [ "testData"; "ifaceList"; 0 ]
        )
    let expectedDeferred2 =
        DeferredResult ([|
                NameValueLookup.ofList [
                    "id", upcast "3000"
                    "value", upcast "C2"
                ]
            |],
            [ "testData"; "ifaceList"; 1 ]
        )
    let query = parse """{
        testData {
            ifaceList @stream {
                id
                value
            }
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted(2)
        (sub.Received |> withoutCompleted)
        |> Seq.cast<GQLDeferredResponseContent>
        |> contains expectedDeferred1
        |> contains expectedDeferred2
        |> ignore

[<Fact>]
let ``Interface field - Defer`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "iface", upcast NameValueLookup.ofList [
                    "id", upcast "1000"
                    "value", null
                ]
            ]
        ]
    let expectedDeferred = DeferredResult ("C", [ "testData"; "iface"; "value" ] )
    let query = """{
        testData {
            iface {
                id
                value @defer
            }
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        (sub.Received |> withoutCompleted) |> single |> equals expectedDeferred

[<Fact>]
let ``Interface list field - Defer`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "ifaceList", upcast [
                    box <| NameValueLookup.ofList [
                        "id", upcast "2000"
                        "value", null
                    ]
                    upcast NameValueLookup.ofList [
                        "id", upcast "3000"
                        "value", null
                    ]
                ]
            ]
        ]
    let expectedDeferred1 = DeferredResult ("D", [ "testData"; "ifaceList"; 0; "value" ])
    let expectedDeferred2 = DeferredResult ("C2", [ "testData"; "ifaceList"; 1; "value" ])
    let query = """{
        testData {
            ifaceList {
                id
                value @defer
            }
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted(2)
        (sub.Received |> withoutCompleted)
        |> Seq.cast<GQLDeferredResponseContent>
        |> contains expectedDeferred1
        |> contains expectedDeferred2
        |> ignore

[<Fact>]
let ``Each live result should be sent as soon as it is computed`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "live", upcast "some value"
                "delayed", null
            ]
        ]
    let expectedLive = DeferredResult ("another value", [ "testData"; "live" ])
    let expectedDeferred =
        DeferredResult (
            NameValueLookup.ofList [
                "value", upcast "Delayed value"
            ],
            [ "testData"; "delayed" ]
        )
    let query = parse """{
        testData {
            live @live
            delayed @defer {
                value
            }
        }
    }"""
    use mre1 = new ManualResetEvent(false)
    use mre2 = new ManualResetEvent(false)
    resetLiveData()
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = deferred |> Observer.createWithCallback (fun sub _ ->
            if Seq.length (sub.Received |> withoutCompleted) = 1 then mre1.Set() |> ignore
            elif Seq.length (sub.Received |> withoutCompleted) = 2 then mre2.Set() |> ignore)
        waitFor hasSubscribers 10 "Timeout while waiting for subscribers on GQLResponse"
        updateLiveData()
        // The second result is a delayed async field, which is set to compute the value for 5 seconds.
        // The first result should come as soon as the live value is updated, which sould be almost instantly.
        // Therefore, let's assume that if it does not come in at least 3 seconds, test has failed.
        if TimeSpan.FromSeconds(float (ms 3)) |> mre1.WaitOne |> not
        then fail "Timeout while waiting for first deferred result"
        if TimeSpan.FromSeconds(float (ms 10)) |> mre2.WaitOne |> not
        then fail "Timeout while waiting for second deferred result"
        (sub.Received |> withoutCompleted)
        |> Seq.cast<GQLDeferredResponseContent>
        |> itemEquals 0 expectedLive
        |> itemEquals 1 expectedDeferred
        |> ignore

[<Fact>]
let ``Live Query`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "id", upcast "1"
                "live", upcast "some value"
            ]
        ]
    let expectedLive = DeferredResult ("another value", [ "testData"; "live" ])
    let query = parse """{
        testData {
            id
            live @live
        }
    }"""
    resetLiveData()
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        waitFor hasSubscribers 10 "Timeout while waiting for subscribers on GQLResponse"
        updateLiveData()
        sub.WaitForItem()
        (sub.Received |> withoutCompleted)
        |> Seq.cast<GQLDeferredResponseContent>
        |> contains expectedLive
        |> ignore

[<Fact>]
let ``Parallel Defer`` () =
    let expectedDirect =
        NameValueLookup.ofList [
           "testData", upcast NameValueLookup.ofList [
                "a", null
                "b", upcast "Banana"
                "innerList", upcast null
            ]
        ]
    let expectedDeferred1 = DeferredResult ("Apple", [ "testData"; "a" ])
    let expectedDeferred2 =
        DeferredResult ([|
                NameValueLookup.ofList [
                    "a", upcast "Inner A"
                ]
            |],
            [ "testData"; "innerList" ]
        )
    let query =
        parse """{
            testData {
                a @defer
                b
                innerList @defer {
                    a
                }
            }
        }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted(2)
        (sub.Received |> withoutCompleted)
        |> Seq.cast<GQLDeferredResponseContent>
        |> contains expectedDeferred1
        |> contains expectedDeferred2
        |> ignore

[<Fact>]
let ``Parallel Stream`` () =
    let expectedDirect =
        NameValueLookup.ofList [
           "testData", upcast NameValueLookup.ofList [
                "a", upcast "Apple"
                "b", upcast "Banana"
                "innerList", upcast [||]
            ]
        ]
    let expectedDeferred1 =
        DeferredResult ([|
            NameValueLookup.ofList [
                    "a", upcast "Inner A"
                    "innerList", upcast [||]
                ]
            |],
            [ "testData"; "innerList"; 0 ]
        )
    let expectedDeferred2 =
        DeferredResult ([|
                NameValueLookup.ofList [
                    "a", upcast "Inner B"
                ]
            |],
            [ "testData"; "innerList"; 0; "innerList"; 0 ]
        )
    let query =
        parse """{
            testData {
                a
                b
                innerList @stream {
                    a
                    innerList @stream {
                         a
                    }
                }
            }
        }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted(2)
        (sub.Received |> withoutCompleted)
        |> Seq.cast<GQLDeferredResponseContent>
        |> contains expectedDeferred1
        |> contains expectedDeferred2
        |> ignore

[<Fact>]
let ``Inner Object List Defer`` () =
    let expectedDirect =
        NameValueLookup.ofList [
           "testData", upcast NameValueLookup.ofList [
                "b", upcast "Banana"
                "innerList", upcast null
            ]
        ]
    let expectedDeferred =
        DeferredResult ([|
                NameValueLookup.ofList [
                    "a", upcast "Inner A"
                ]
            |],
            [ "testData"; "innerList" ]
        )
    let query = parse """{
            testData {
                b
                innerList @defer {
                    a
                }
            }
        }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        (sub.Received |> withoutCompleted) |> single |> equals expectedDeferred

[<Fact>]
let ``Inner Object List Stream`` () =
    let expectedDirect =
        NameValueLookup.ofList [
           "testData", upcast NameValueLookup.ofList [
                "b", upcast "Banana"
                "innerList", upcast []
            ]
        ]
    let expectedDeferred =
        DeferredResult ([|
                NameValueLookup.ofList [
                    "a", upcast "Inner A"
                ]
            |],
            [ "testData"; "innerList"; 0 ]
        )
    let query = parse """{
            testData {
                b
                innerList @stream {
                    a
                }
            }
        }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        (sub.Received |> withoutCompleted) |> single |> equals expectedDeferred

[<Fact>]
let ``Nested Inner Object List Defer`` () =
    let expectedDirect =
        NameValueLookup.ofList [
           "testData", upcast NameValueLookup.ofList [
                "b", upcast "Banana"
                "innerList", upcast null
            ]
        ]
    let expectedDeferred1 =
        DeferredResult ([|
                NameValueLookup.ofList [
                    "a", upcast "Inner A"
                    "innerList", upcast null
                ]
            |],
            [ "testData"; "innerList" ]
        )
    let expectedDeferred2 =
        DeferredResult ([|
                NameValueLookup.ofList [
                    "a", upcast "Inner B"
                ]
                NameValueLookup.ofList [
                    "a", upcast "Inner C"
                ]
            |],
            [ "testData"; "innerList"; 0; "innerList" ]
        )
    let query = parse """{
            testData {
                b
                innerList @defer {
                    a
                    innerList @defer {
                        a
                    }
                }
            }
        }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted(2)
        (sub.Received |> withoutCompleted)
        |> Seq.cast<GQLDeferredResponseContent>
        |> contains expectedDeferred1
        |> contains expectedDeferred2
        |> ignore

[<Fact>]
let ``Nested defer completes the parent before nested deferred payloads`` () =
    let query = parse """{
            testData {
                b
                innerList @defer {
                    a
                    innerList @defer {
                        a
                    }
                }
            }
        }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun _ errors deferred ->
        empty errors
        use sub = Observer.create deferred
        sub.WaitCompleted(2)
        sub.Received
        |> Seq.toList
        |> equals [
            DeferredResult ([| NameValueLookup.ofList [
                    "a", upcast "Inner A"
                    "innerList", upcast null
                ] |], [ "testData"; "innerList" ])
            DeferredCompleted [ "testData"; "innerList" ]
            DeferredResult ([|
                    NameValueLookup.ofList [
                        "a", upcast "Inner B"
                    ]
                    NameValueLookup.ofList [
                        "a", upcast "Inner C"
                    ]
                |], [ "testData"; "innerList"; 0; "innerList" ])
            DeferredCompleted [ "testData"; "innerList"; 0; "innerList" ]
        ]

[<Fact>]
let ``Deferred field with a label emits a pending marker before its payload`` () =
    let query = parse """{
            testData {
                a @defer(label: "hero")
            }
        }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun _ errors deferred ->
        empty errors
        use sub = Observer.create deferred
        sub.WaitCompleted(2)
        sub.Received
        |> Seq.toList
        |> equals [
            DeferredPending ([ "testData"; "a" ], ValueSome "hero", false, 0)
            DeferredResult ("Apple", [ "testData"; "a" ])
            DeferredCompleted [ "testData"; "a" ]
        ]
        |> ignore

[<Fact>]
let ``Nested Inner Object List Stream`` () =
    let expectedDirect =
        NameValueLookup.ofList [
           "testData", upcast NameValueLookup.ofList [
                "b", upcast "Banana"
                "innerList", upcast null
            ]
        ]
    let expectedDeferred1 =
        DeferredResult ([|
                NameValueLookup.ofList [
                    "a", upcast "Inner A"
                    "innerList", upcast []
                ]
            |],
            [ "testData"; "innerList" ]
        )
    let expectedDeferred2 =
        DeferredResult ([|
                NameValueLookup.ofList [
                    "a", upcast "Inner B"
                ]
            |],
            [ "testData"; "innerList"; 0; "innerList"; 0 ]
        )
    let expectedDeferred3 =
        DeferredResult ([|
                NameValueLookup.ofList [
                    "a", upcast "Inner C"
                ]
            |],
            [ "testData"; "innerList"; 0; "innerList"; 1 ]
        )
    let query = parse """{
            testData {
                b
                innerList @defer {
                    a
                    innerList @stream {
                        a
                    }
                }
            }
        }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted(3)
        (sub.Received |> withoutCompleted)
        |> Seq.cast<GQLDeferredResponseContent>
        |> contains expectedDeferred1
        |> contains expectedDeferred2
        |> contains expectedDeferred3
        |> ignore

[<Fact>]
let ``Nested stream pending is emitted before the deferred payload that exposes it`` () =
    let expectedDirect =
        NameValueLookup.ofList [
           "testData", upcast NameValueLookup.ofList [
                "b", upcast "Banana"
                "innerList", upcast null
            ]
        ]
    let expectedDeferred =
        DeferredResult ([|
                NameValueLookup.ofList [
                    "a", upcast "Inner A"
                    "innerList", upcast []
                ]
            |],
            [ "testData"; "innerList" ]
        )
    let query = parse """{
            testData {
                b
                innerList @defer {
                    a
                    innerList @stream {
                        a
                    }
                }
            }
        }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted(3)
        let expectedPending = DeferredPending ([ box "testData"; box "innerList"; box 0; box "innerList" ], ValueNone, true, 0)
        match sub.Received |> Seq.toList with
        | actualPending :: actualDeferred :: _ ->
            Assert.Equal (expectedPending, actualPending)
            Assert.Equal (expectedDeferred, actualDeferred)
        | received -> fail $"Expected the nested stream announcement before the containing deferred payload, but received %A{received}"

[<Fact>]
let ``Simple Defer and Stream`` () =
    let expectedDirect =
        NameValueLookup.ofList [
           "testData", upcast NameValueLookup.ofList [
                "a", null
                "b", upcast "Banana"
            ]
        ]
    let expectedDeferred = DeferredResult ("Apple",  [ "testData"; "a" ])
    let query = """{
        testData {
            a @defer
            b
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        (sub.Received |> withoutCompleted) |> single |> equals expectedDeferred

[<Fact>]
let ``List Defer``() =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "a", upcast "Apple"
                "list", upcast null
            ]
        ]
    let expectedDeferred =
        DeferredResult (
            [|
                box <| NameValueLookup.ofList [
                    "id", upcast "2"
                    "a", upcast "Union A"
                ]
                upcast NameValueLookup.ofList [
                    "id", upcast "3"
                    "b", upcast 4
                ]
            |],
            [ "testData"; "list" ]
        )
    let query = parse """{
        testData {
            a
            list @defer {
                ... on A {
                    id
                    a
                }
                ... on B {
                    id
                    b
                }
            }
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        (sub.Received |> withoutCompleted) |> single |> equals expectedDeferred

[<Fact>]
let ``List Fragment Defer and Stream - Exclusive``() =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "a", upcast "Apple"
                "list", upcast [
                    box <| NameValueLookup.ofList [
                        "id", upcast "2"
                        "a", null
                    ]
                    upcast NameValueLookup.ofList [
                        "id", upcast "3"
                        "b", upcast 4
                    ]
                ]
            ]
        ]
    let expectedDeferred = DeferredResult ("Union A", [ "testData"; "list"; 0; "a" ])
    let query = """{
        testData {
            a
            list {
                ... on A {
                    id
                    a @defer
                }
                ... on B {
                    id
                    b
                }
            }
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        (sub.Received |> withoutCompleted) |> single |> equals expectedDeferred

[<Fact>]
let ``List Fragment Defer and Stream - Common``() =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "a", upcast "Apple"
                "list", upcast [
                    box <| NameValueLookup.ofList [
                        "id", null
                        "a", upcast "Union A"
                    ]
                    upcast NameValueLookup.ofList [
                        "id", upcast "3"
                        "b", upcast 4
                    ]
                ]
            ]
        ]
    let expectedDeferred = DeferredResult ("2", [ "testData"; "list"; 0; "id" ])
    let query = """{
        testData {
            a
            list {
                ... on A {
                    id @defer
                    a
                }
                ... on B {
                    id
                    b
                }
            }
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        (sub.Received |> withoutCompleted) |> single |> equals expectedDeferred

[<Fact>]
let ``List inside root - Stream``() =
    let expectedDirect =
        NameValueLookup.ofList [
            "listData", upcast []
        ]
    let expectedDeferred1 =
        DeferredResult ([|
                NameValueLookup.ofList [
                    "id", upcast "2"
                    "a", upcast "Union A"
                ]
            |],
            [ "listData"; 0 ]
        )
    let expectedDeferred2 =
        DeferredResult ([|
                NameValueLookup.ofList [
                    "id", upcast "3"
                    "b", upcast 4
                ]
            |],
            [ "listData"; 1 ]
        )
    let query = parse """{
        listData @stream {
            ... on A {
                id
                a
            }
            ... on B {
                id
                b
            }
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted(2)
        (sub.Received |> withoutCompleted)
        |> Seq.cast<GQLDeferredResponseContent>
        |> contains expectedDeferred1
        |> contains expectedDeferred2
        |> ignore

[<Fact>]
let ``List Stream``() =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "a", upcast "Apple"
                "list", upcast []
            ]
        ]
    let expectedDeferred1 =
        DeferredResult ([|
                NameValueLookup.ofList [
                    "id", upcast "2"
                    "a", upcast "Union A"
                ]
            |],
            [ "testData"; "list"; 0 ]
        )
    let expectedDeferred2 =
        DeferredResult ([|
                NameValueLookup.ofList [
                    "id", upcast "3"
                    "b", upcast 4
                ]
            |],
            [ "testData"; "list"; 1 ]
        )
    let query = parse """{
        testData {
            a
            list @stream {
                ... on A {
                    id
                    a
                }
                ... on B {
                    id
                    b
                }
            }
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted(2)
        (sub.Received |> withoutCompleted)
        |> Seq.cast<GQLDeferredResponseContent>
        |> contains expectedDeferred1
        |> contains expectedDeferred2
        |> ignore

[<Fact>]
let ``Should buffer stream list correctly by timing information``() =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "bufferedList", upcast []
            ]
        ]
    let expectedDeferred1 =
        DeferredResult ([|
                NameValueLookup.ofList [
                    "value", upcast "Buffered 3"
                ]
                NameValueLookup.ofList [
                    "value", upcast "Buffered 2"
                ]
            |],
            [box "testData"; "bufferedList"; [box 2; 1]]
        )
    let expectedDeferred2 =
        DeferredResult ([|
                NameValueLookup.ofList [
                    "value", upcast "Buffered 1"
                ]
            |],
            [box "testData"; "bufferedList"; 0]
        )
    let query =
        ms 3000
        |> sprintf """{
            testData {
                bufferedList @stream(interval : %i) {
                    value
                }
            }
        }"""
        |> parse
    use mre1 = new ManualResetEvent(false)
    use mre2 = new ManualResetEvent(false)
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = deferred |> Observer.createWithCallback (fun sub _ ->
            if Seq.length (sub.Received |> withoutCompleted) = 1 then mre1.Set() |> ignore
            elif Seq.length (sub.Received |> withoutCompleted) = 2 then mre2.Set() |> ignore)
        // The first result is a delayed async field, which is set to compute the value for 5 seconds.
        // The second result is also a delayed async field, computed for 1 second.
        // Third result is a instant returning async field.
        // As the buffer has a time limit of 3 seconds, the expected behavior is
        // to buffer results 3 and 2 (in this order), as together they take less than 3 seconds to compute,
        // and send them together on the first batch.
        // First result should come in a second batch, as it takes 5 seconds to compute, more than the time limit of the buffer.
        if TimeSpan.FromSeconds(float (ms 4)) |> mre1.WaitOne |> not
        then fail "Timeout while waiting for first Deferred GQLResponse"
        if TimeSpan.FromSeconds(float (ms 10)) |> mre2.WaitOne |> not
        then fail "Timeout while waiting for second Deferred GQLResponse"
        sub.WaitCompleted(timeout = ms 10)
        (sub.Received |> withoutCompleted)
        |> Seq.cast<GQLDeferredResponseContent>
        |> itemEquals 0 expectedDeferred1
        |> itemEquals 1 expectedDeferred2
        |> ignore

[<Fact>]
let ``Should buffer stream list correctly by count information``() =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "bufferedList", upcast []
            ]
        ]
    let expectedDeferred1 =
        DeferredResult ([|
                NameValueLookup.ofList [
                    "value", upcast "Buffered 3"
                ]
                NameValueLookup.ofList [
                    "value", upcast "Buffered 2"
                ]
            |],
            [box "testData"; "bufferedList"; [box 2; 1]]
        )
    let expectedDeferred2 =
        DeferredResult ([|
                NameValueLookup.ofList [
                    "value", upcast "Buffered 1"
                ]
            |],
            [box "testData"; "bufferedList"; 0]
        )
    let query = parse """{
        testData {
            bufferedList @stream(preferredBatchSize : 2) {
                value
            }
        }
    }"""
    use mre1 = new ManualResetEvent(false)
    use mre2 = new ManualResetEvent(false)
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = deferred |> Observer.createWithCallback (fun sub _ ->
            if Seq.length (sub.Received |> withoutCompleted) = 1 then mre1.Set() |> ignore
            elif Seq.length (sub.Received |> withoutCompleted) = 2 then mre2.Set() |> ignore)
        // The first result is a delayed async field, which is set to compute the value for 5 seconds.
        // The second result is also a delayed async field, computed for 1 second.
        // Third result is a instant returning async field.
        // As the preferred batch size is configured to have a maximum of two items, the expected behavior is
        // to buffer results 3 and 2 (in this order), as together they take should be computed before than the fist result,
        // and send them together on the first batch.
        // First result should come in a second batch, as it takes 5 seconds to compute, which should be enough
        // to put the two other results in a batch with the preferred size.
        if TimeSpan.FromSeconds(float (ms 4)) |> mre1.WaitOne |> not
        then fail "Timeout while waiting for first Deferred GQLResponse"
        if TimeSpan.FromSeconds(float (ms 10)) |> mre2.WaitOne |> not
        then fail "Timeout while waiting for second Deferred GQLResponse"
        sub.WaitCompleted(timeout = ms 10)
        (sub.Received |> withoutCompleted)
        |> Seq.cast<GQLDeferredResponseContent>
        |> itemEquals 0 expectedDeferred1
        |> itemEquals 1 expectedDeferred2
        |> ignore

[<Fact>]
let ``Union Defer`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "a", upcast "Apple"
                "b", upcast "Banana"
                "union", null
            ]
        ]
    let expectedDeferred =
        DeferredResult (
            NameValueLookup.ofList [ "id", upcast "1"; "a", upcast "Union A" ],
            [ "testData"; "union" ]
        )
    let query = """{
        testData {
            a
            b
            union @defer {
                ... on A {
                    id
                    a
                }
                ... on B {
                    id
                    b
                }
            }
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        (sub.Received |> withoutCompleted) |> single |> equals expectedDeferred

[<Fact>]
let ``Each deferred result should be sent as soon as it is computed``() =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "delayed", null
                "b", null
            ]
        ]
    let expectedDeferred1 = DeferredResult ("Banana", [ "testData"; "b" ])
    let expectedDeferred2 =
        DeferredResult (NameValueLookup.ofList [ "value", upcast "Delayed value" ], [ "testData"; "delayed" ])
    let query = parse """{
        testData {
            delayed @defer {
                value
            }
            b @defer
        }
    }"""
    use mre1 = new ManualResetEvent(false)
    use mre2 = new ManualResetEvent(false)
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = deferred |> Observer.createWithCallback (fun sub _ ->
            if Seq.length (sub.Received |> withoutCompleted) = 1 then mre1.Set() |> ignore
            elif Seq.length (sub.Received |> withoutCompleted) = 2 then mre2.Set() |> ignore)
        // The second result is a delayed async field, which is set to compute the value for 5 seconds.
        // The first result should come almost instantly, as it is not a delayed computed field.
        // Therefore, let's assume that if it does not come in at least 3 seconds, the test has failed.
        if TimeSpan.FromSeconds(float (ms 3)) |> mre1.WaitOne |> not
        then fail "Timeout while waiting for first deferred result"
        if TimeSpan.FromSeconds(float (ms 10)) |> mre2.WaitOne |> not
        then fail "Timeout while waiting for second deferred result"
        sub.WaitCompleted(timeout = ms 10)
        (sub.Received |> withoutCompleted)
        |> Seq.cast<GQLDeferredResponseContent>
        |> itemEquals 0 expectedDeferred1
        |> itemEquals 1 expectedDeferred2
        |> ignore

[<Fact(Skip="Flaky test")>]
let ``Each deferred result of a list should be sent as soon as it is computed`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "delayedList", upcast [
                    box <| NameValueLookup.ofList [
                        "value", null
                    ]
                    upcast NameValueLookup.ofList [
                        "value", null
                    ]
                ]
            ]
        ]
    let expectedDeferred1 = DeferredResult ("Fast", [ "testData"; "delayedList"; 1; "value" ])
    let expectedDeferred2 = DeferredResult ("Slow", [ "testData"; "delayedList"; 0; "value" ])
    let query = parse """{
        testData {
            delayedList {
                value @defer
            }
        }
    }"""
    use mre1 = new ManualResetEvent(false)
    use mre2 = new ManualResetEvent(false)
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = deferred |> Observer.createWithCallback (fun sub _ ->
            if Seq.length (sub.Received |> withoutCompleted) = 1 then mre1.Set() |> ignore
            elif Seq.length (sub.Received |> withoutCompleted) = 2 then mre2.Set() |> ignore)
        // The first result is a delayed async field, which is set to compute the value for 5 seconds.
        // The second result should come first, almost instantly, as it is not a delayed computed field.
        // Therefore, let's assume that if it does not come in at least 4 seconds, the test has failed.
        if TimeSpan.FromSeconds(float (ms 4)) |> mre1.WaitOne |> not
        then fail "Timeout while waiting for first deferred result"
        if TimeSpan.FromSeconds(float (ms 10)) |> mre2.WaitOne |> not
        then fail "Timeout while waiting for second deferred result"
        sub.WaitCompleted(timeout = ms 10)
        (sub.Received |> withoutCompleted)
        |> Seq.cast<GQLDeferredResponseContent>
        |> itemEquals 0 expectedDeferred1
        |> itemEquals 1 expectedDeferred2
        |> ignore

[<Fact>]
let ``Each streamed result should be sent as soon as it is computed - async seq``() =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "delayedList", upcast []
            ]
        ]
    let expectedDeferred1 =
        DeferredResult ([| NameValueLookup.ofList [ "value", upcast "Fast" ] |], [ "testData"; "delayedList"; 1 ])
    let expectedDeferred2 =
        DeferredResult ([| NameValueLookup.ofList [ "value", upcast "Slow" ] |], [ "testData"; "delayedList"; 0 ])
    let query = parse """{
        testData {
            delayedList @stream {
                value
            }
        }
    }"""
    use mre1 = new ManualResetEvent(false)
    use mre2 = new ManualResetEvent(false)
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = deferred |> Observer.createWithCallback (fun sub _ ->
            if Seq.length (sub.Received |> withoutCompleted) = 1 then mre1.Set() |> ignore
            elif Seq.length (sub.Received |> withoutCompleted) = 2 then mre2.Set() |> ignore)
        // The first result is a delayed async field, which is set to compute the value for 5 seconds.
        // The second result should come first, almost instantly, as it is not a delayed computed field.
        // Therefore, let's assume that if it does not come in at least 4 seconds, test has failed.
        if TimeSpan.FromSeconds(float (ms 4)) |> mre1.WaitOne |> not
        then fail "Timeout while waiting for first deferred result"
        if TimeSpan.FromSeconds(float (ms 10)) |> mre2.WaitOne |> not
        then fail "Timeout while waiting for second deferred result"
        sub.WaitCompleted(timeout = ms 10)
        (sub.Received |> withoutCompleted)
        |> Seq.cast<GQLDeferredResponseContent>
        |> itemEquals 0 expectedDeferred1
        |> itemEquals 1 expectedDeferred2
        |> ignore

// ---------------------------------------------------------------------------------------------------------------------
// Incremental delivery spec v0.2 coverage. Tests marked Skip capture behaviour the spec requires but the engine does not
// implement yet; each names the missing feature in its Skip reason and is turned on when that feature lands.
// ---------------------------------------------------------------------------------------------------------------------

[<Fact>]
let ``Deferred field inside a streamed item is delivered after its item with its own completion`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "innerList", upcast []
            ]
        ]
    let query = parse """{
        testData {
            innerList @stream {
                a
                innerList @defer {
                    a
                }
            }
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        sub.Received
        |> Seq.toList
        |> equals [
            DeferredPending ([ "testData"; "innerList" ], ValueNone, true, 0)
            // The item carries the deferred child as null; the child's own payload and completion follow it
            DeferredResult ([| NameValueLookup.ofList [ "a", upcast "Inner A"; "innerList", null ] |], [ "testData"; "innerList"; 0 ])
            DeferredResult ([|
                    NameValueLookup.ofList [ "a", upcast "Inner B" ]
                    NameValueLookup.ofList [ "a", upcast "Inner C" ]
                |], [ "testData"; "innerList"; 0; "innerList" ])
            DeferredCompleted [ "testData"; "innerList"; 0; "innerList" ]
            DeferredCompleted [ "testData"; "innerList" ]
        ]

[<Fact>]
let ``Errors inside a deferred payload bubble to the nearest nullable boundary within that payload`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "container", null
            ]
        ]
    let expectedError =
        GQLProblemDetails.CreateWithKind (
            "Non-Null field value resolved as a null!",
            Execution,
            [ box "testData"; "container"; "inner"; "value" ]
        )
    let query = parse """{
        testData {
            container @defer {
                name
                inner {
                    value
                }
            }
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        sub.Received
        |> Seq.toList
        |> equals [
            // `inner` is the nearest nullable ancestor of the failing `value`, so the payload keeps `name` and nulls `inner`
            DeferredErrors (
                NameValueLookup.ofList [ "name", upcast "Container"; "inner", null ],
                [ expectedError ],
                [ "testData"; "container" ]
            )
            DeferredCompleted [ "testData"; "container" ]
        ]

[<Fact>]
let ``Deferred field under a parent that resolves to null is never delivered`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "nullObject", null
            ]
        ]
    let query = parse """{
        testData {
            nullObject {
                value @defer
            }
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expectedDirect)

[<Fact>]
let ``Root-level deferred object field`` () =
    let expectedDirect = NameValueLookup.ofList [ "nullableTestData", null ]
    let query = parse """{
        nullableTestData @defer {
            id
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        sub.Received
        |> Seq.toList
        |> equals [
            DeferredResult (NameValueLookup.ofList [ "id", upcast "1" ], [ "nullableTestData" ])
            DeferredCompleted [ "nullableTestData" ]
        ]

[<Fact>]
let ``Deferred field inside a mutation payload`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "touch", upcast NameValueLookup.ofList [
                "id", upcast "1"
                "a", null
            ]
        ]
    let query = parse """mutation {
        touch {
            id
            a @defer
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        sub.Received
        |> Seq.toList
        |> equals [
            DeferredResult ("Apple", [ "touch"; "a" ])
            DeferredCompleted [ "touch"; "a" ]
        ]

[<Fact>]
let ``Defer directive with if false executes the field inline as if the directive were absent`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "a", upcast "Apple"
                "b", upcast "Banana"
            ]
        ]
    let query = parse """{
        testData {
            a @defer(if: false)
            b
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expectedDirect)

[<Fact>]
let ``Defer directive with if given through a true variable still defers the field`` () =
    let query = parse """query ($d: Boolean!) {
        testData {
            a @defer(if: $d)
        }
    }"""
    let variables = ImmutableDictionary<string, JsonElement>.Empty.Add ("d", JsonDocument.Parse("true").RootElement)
    let result = executor.AsyncExecute(query, getMockInputContext, variables = variables) |> sync
    ensureDeferred result <| fun _ errors deferred ->
        empty errors
        use sub = Observer.create deferred
        sub.WaitCompleted()
        (sub.Received |> withoutCompleted)
        |> single
        |> equals (DeferredResult ("Apple", [ "testData"; "a" ]))

[<Fact>]
let ``Stream directive with if false returns the whole list inline`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "ifaceList", upcast [
                    NameValueLookup.ofList [ "id", upcast "2000" ]
                    NameValueLookup.ofList [ "id", upcast "3000" ]
                ]
            ]
        ]
    let query = parse """{
        testData {
            ifaceList @stream(if: false) {
                id
            }
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expectedDirect)

[<Fact>]
let ``Stream directive initialCount delivers the first items in the initial payload and streams the rest`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "ifaceList", upcast [ NameValueLookup.ofList [ "id", upcast "2000"; "value", upcast "D" ] ]
            ]
        ]
    let query = parse """{
        testData {
            ifaceList @stream(initialCount: 1) {
                id
                value
            }
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        sub.Received
        |> Seq.toList
        |> equals [
            // Item 0 went out in the initial payload, so streaming starts at index 1, as the announcement says
            DeferredPending ([ "testData"; "ifaceList" ], ValueNone, true, 1)
            DeferredResult ([| NameValueLookup.ofList [ "id", upcast "3000"; "value", upcast "C2" ] |], [ "testData"; "ifaceList"; 1 ])
            DeferredCompleted [ "testData"; "ifaceList" ]
        ]

[<Fact>]
let ``Stream directive label is announced in the stream's pending marker`` () =
    let query = parse """{
        testData {
            ifaceList @stream(label: "friends") {
                id
            }
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun _ errors deferred ->
        empty errors
        use sub = Observer.create deferred
        sub.WaitCompleted()
        sub.Received
        |> Seq.head
        |> equals (DeferredPending ([ "testData"; "ifaceList" ], ValueSome "friends", true, 0))

[<Fact>]
let ``Defer directive on an inline fragment defers the fragment's fields as one payload at the parent's path`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "id", upcast "1"
            ]
        ]
    let query = parse """{
        testData {
            id
            ... @defer(label: "rest") {
                a
                b
            }
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        sub.Received
        |> Seq.toList
        |> equals [
            DeferredFragmentPending ([ "testData" ], ValueSome "rest", 0)
            DeferredFragmentResult (ValueSome (upcast NameValueLookup.ofList [ "a", upcast "Apple"; "b", upcast "Banana" ]), [], [ "testData" ], 0)
            DeferredFragmentCompleted ([ "testData" ], 0)
        ]

[<Fact>]
let ``Defer directive on a fragment spread defers the fragment's fields as one payload`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "id", upcast "1"
            ]
        ]
    let query = parse """query {
        testData {
            id
            ...Rest @defer
        }
    }
    fragment Rest on Data {
        a
        b
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        sub.Received
        |> Seq.toList
        |> equals [
            DeferredFragmentResult (ValueSome (upcast NameValueLookup.ofList [ "a", upcast "Apple"; "b", upcast "Banana" ]), [], [ "testData" ], 0)
            DeferredFragmentCompleted ([ "testData" ], 0)
        ]

[<Fact>]
let ``The same fragment deferred twice at the same path is delivered once`` () =
    let query = parse """query {
        testData {
            ...Rest @defer
            ...Rest @defer
        }
    }
    fragment Rest on Data {
        a
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun _ errors deferred ->
        empty errors
        use sub = Observer.create deferred
        sub.WaitCompleted()
        sub.Received
        |> Seq.toList
        |> equals [
            DeferredFragmentResult (ValueSome (upcast NameValueLookup.ofList [ "a", upcast "Apple" ]), [], [ "testData" ], 0)
            DeferredFragmentCompleted ([ "testData" ], 0)
        ]

[<Fact>]
let ``Two labeled fragments deferred at the same path are delivered as separate payloads`` () =
    let query = parse """{
        testData {
            id
            ... @defer(label: "first") {
                a
            }
            ... @defer(label: "second") {
                b
            }
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun _ errors deferred ->
        empty errors
        use sub = Observer.create deferred
        sub.WaitCompleted()
        sub.Received
        |> Seq.toList
        |> equals [
            // Both fragments are announced before either delivers
            DeferredFragmentPending ([ "testData" ], ValueSome "first", 0)
            DeferredFragmentPending ([ "testData" ], ValueSome "second", 1)
            DeferredFragmentResult (ValueSome (upcast NameValueLookup.ofList [ "a", upcast "Apple" ]), [], [ "testData" ], 0)
            DeferredFragmentCompleted ([ "testData" ], 0)
            DeferredFragmentResult (ValueSome (upcast NameValueLookup.ofList [ "b", upcast "Banana" ]), [], [ "testData" ], 1)
            DeferredFragmentCompleted ([ "testData" ], 1)
        ]

[<Fact>]
let ``A field selected both directly and in a deferred fragment is executed with the object`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "a", upcast "Apple"
            ]
        ]
    let query = parse """{
        testData {
            a
            ... @defer {
                a
                b
            }
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        sub.Received
        |> Seq.toList
        |> equals [
            // The fragment delivers only the field the object did not
            DeferredFragmentResult (ValueSome (upcast NameValueLookup.ofList [ "b", upcast "Banana" ]), [], [ "testData" ], 0)
            DeferredFragmentCompleted ([ "testData" ], 0)
        ]

[<Fact>]
let ``A deferred fragment selecting under a field selected directly adds its selection to that field`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "innerList", upcast [
                    NameValueLookup.ofList [
                        "a", upcast "Inner A"
                        "innerList", upcast [
                            NameValueLookup.ofList [ "a", upcast "Inner B" ]
                            NameValueLookup.ofList [ "a", upcast "Inner C" ]
                        ]
                    ]
                ]
            ]
        ]
    let query = parse """{
        testData {
            innerList {
                a
            }
            ... @defer {
                innerList {
                    innerList {
                        a
                    }
                }
            }
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    // The fragment selects nothing but the field the object selects itself, so it delivers nothing
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expectedDirect)

[<Fact>]
let ``A deferred fragment standing before the field it selects under still adds its selection to that field`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "innerList", upcast [
                    NameValueLookup.ofList [
                        "a", upcast "Inner A"
                        "innerList", upcast [
                            NameValueLookup.ofList [ "a", upcast "Inner B" ]
                            NameValueLookup.ofList [ "a", upcast "Inner C" ]
                        ]
                    ]
                ]
            ]
        ]
    let query = parse """{
        testData {
            ... @defer {
                innerList {
                    innerList {
                        a
                    }
                }
            }
            innerList {
                a
            }
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast expectedDirect)

[<Fact>]
let ``A deferred fragment skipped with a directive is not delivered`` () =
    let query = parse """{
        testData {
            id
            ... @defer @skip(if: true) {
                a
            }
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast NameValueLookup.ofList [ "testData", upcast NameValueLookup.ofList [ "id", upcast "1" ] ])

[<Fact>]
let ``A deferred root fragment excluded with a directive is not delivered`` () =
    let query = parse """{
        testData {
            id
        }
        ... @defer @include(if: false) {
            nullableTestData {
                id
            }
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDirect result <| fun data errors ->
        empty errors
        data |> equals (upcast NameValueLookup.ofList [ "testData", upcast NameValueLookup.ofList [ "id", upcast "1" ] ])

[<Fact>]
let ``A fragment spread directly is resolved with the object whichever spread of it comes first`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "a", upcast "Apple"
                "b", upcast "Banana"
            ]
        ]
    let execute (selection : string) =
        let query = parse $"""query {{
            testData {{
                {selection}
            }}
        }}
        fragment Rest on Data {{
            a
            b
        }}"""
        executor.AsyncExecute(query, getMockInputContext) |> sync
    for selection in [ "...Rest @defer ...Rest"; "...Rest ...Rest @defer" ] do
        ensureDirect (execute selection) <| fun data errors ->
            empty errors
            data |> equals (upcast expectedDirect)

[<Fact>]
let ``Defer directive on a fragment with if false through a variable resolves the fragment's fields with the object`` () =
    let query = parse """query ($d: Boolean!) {
        testData {
            id
            ... @defer(if: $d) {
                a
                b
            }
        }
    }"""
    let variables = ImmutableDictionary<string, JsonElement>.Empty.Add ("d", JsonDocument.Parse("false").RootElement)
    let result = executor.AsyncExecute(query, getMockInputContext, variables = variables) |> sync
    ensureDirect result <| fun data errors ->
        empty errors
        data
        |> equals (
            upcast NameValueLookup.ofList [
                "testData", upcast NameValueLookup.ofList [
                    "id", upcast "1"
                    "a", upcast "Apple"
                    "b", upcast "Banana"
                ]
            ]
        )

[<Fact>]
let ``Defer directive on a fragment with if true through a variable defers the fragment's fields`` () =
    let query = parse """query ($d: Boolean!) {
        testData {
            id
            ... @defer(if: $d) {
                a
            }
        }
    }"""
    let variables = ImmutableDictionary<string, JsonElement>.Empty.Add ("d", JsonDocument.Parse("true").RootElement)
    let result = executor.AsyncExecute(query, getMockInputContext, variables = variables) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast NameValueLookup.ofList [ "testData", upcast NameValueLookup.ofList [ "id", upcast "1" ] ])
        use sub = Observer.create deferred
        sub.WaitCompleted()
        sub.Received
        |> Seq.toList
        |> equals [
            DeferredFragmentResult (ValueSome (upcast NameValueLookup.ofList [ "a", upcast "Apple" ]), [], [ "testData" ], 0)
            DeferredFragmentCompleted ([ "testData" ], 0)
        ]

[<Fact>]
let ``A root fragment with if false through a variable resolves its root fields with the root`` () =
    let query = parse """query ($d: Boolean!) {
        testData {
            id
        }
        ... @defer(if: $d) {
            nullableTestData {
                id
            }
        }
    }"""
    let variables = ImmutableDictionary<string, JsonElement>.Empty.Add ("d", JsonDocument.Parse("false").RootElement)
    let result = executor.AsyncExecute(query, getMockInputContext, variables = variables) |> sync
    ensureDirect result <| fun data errors ->
        empty errors
        data
        |> equals (
            upcast NameValueLookup.ofList [
                "testData", upcast NameValueLookup.ofList [ "id", upcast "1" ]
                "nullableTestData", upcast NameValueLookup.ofList [ "id", upcast "1" ]
            ]
        )

[<Fact>]
let ``A fragment deferred at the operation root delivers root fields`` () =
    let query = parse """{
        ... @defer {
            nullableTestData {
                id
            }
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast NameValueLookup.ofList [])
        use sub = Observer.create deferred
        sub.WaitCompleted()
        sub.Received
        |> Seq.toList
        |> equals [
            DeferredFragmentResult (
                ValueSome (upcast NameValueLookup.ofList [ "nullableTestData", upcast NameValueLookup.ofList [ "id", upcast "1" ] ]),
                [],
                [],
                0
            )
            DeferredFragmentCompleted ([], 0)
        ]

[<Fact>]
let ``A deferred fragment on an abstract type delivers the fields of the matching type`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "iface", upcast NameValueLookup.ofList [
                    "id", upcast "1000"
                ]
            ]
        ]
    let query = parse """{
        testData {
            iface {
                id
                ... on C @defer(label: "c") {
                    value
                }
            }
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        sub.Received
        |> Seq.toList
        |> equals [
            DeferredFragmentPending ([ "testData"; "iface" ], ValueSome "c", 0)
            DeferredFragmentResult (ValueSome (upcast NameValueLookup.ofList [ "value", upcast "C" ]), [], [ "testData"; "iface" ], 0)
            DeferredFragmentCompleted ([ "testData"; "iface" ], 0)
        ]

[<Fact>]
let ``An error propagating up to a deferred fragment completes it with the errors and no data`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "id", upcast "1"
            ]
        ]
    let expectedError = GQLProblemDetails.CreateWithKind ("Non-null field error!", Execution, [ box "testData"; "nonNullError" ])
    let query = parse """{
        testData {
            id
            ... @defer {
                nonNullError
            }
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        sub.Received
        |> Seq.toList
        |> equals [
            // The object was already delivered with its own fields, so the fragment has nothing to null: it fails as a whole
            DeferredFragmentResult (ValueNone, [ expectedError ], [ "testData" ], 0)
            DeferredFragmentCompleted ([ "testData" ], 0)
        ]

[<Fact>]
let ``A deferred label given through a variable is rejected instead of being dropped`` () =
    // The spec forbids variables for `label`; today the executor asserts in Debug and silently drops the label in Release
    let query = parse """query ($l: String) {
        testData {
            a @defer(label: $l)
        }
    }"""
    let variables = ImmutableDictionary<string, JsonElement>.Empty.Add ("l", JsonDocument.Parse("\"hero\"").RootElement)
    let result = executor.AsyncExecute(query, getMockInputContext, variables = variables) |> sync
    ensureRequestError result <| fun errors ->
        errors |> hasError "label"

[<Fact>]
let ``Top-level announcements of several deferred and streamed fields precede every payload in field order`` () =
    let query = parse """{
        testData {
            a @defer(label: "first")
            ifaceList @stream {
                id
            }
            b @defer(label: "third")
        }
    }"""
    let result = executor.AsyncExecute(query, getMockInputContext) |> sync
    ensureDeferred result <| fun _ errors deferred ->
        empty errors
        use sub = Observer.create deferred
        sub.WaitCompleted()
        sub.Received
        |> Seq.toList
        |> List.take 3
        |> equals [
            DeferredPending ([ "testData"; "a" ], ValueSome "first", false, 0)
            DeferredPending ([ "testData"; "ifaceList" ], ValueNone, true, 0)
            DeferredPending ([ "testData"; "b" ], ValueSome "third", false, 0)
        ]
