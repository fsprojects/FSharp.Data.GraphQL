module FSharp.Data.GraphQL.Tests.DeferredTests

open System
open Xunit
open System.Threading
open FSharp.Control
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution
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
}

and AsyncTestSubject = {
    value : Async<string option>
}

and NonNullAsyncTestSubject = {
    value : Async<string>
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
    Define.ObjectRec<InnerTestSubject>(
        name = "InnerData",
        fieldsFn = fun () ->
        [
            Define.Field("a", StringType, (fun _ (d: InnerTestSubject) -> d.a))
            Define.Field("innerList", Nullable (ListOf InnerDataType), (fun _ d -> Some d.innerList))
        ])

let AsyncDataType =
    Define.ObjectRec<AsyncTestSubject>(
        name = "AsyncData",
        fieldsFn = fun () -> [ Define.AsyncField("value", Nullable StringType, (fun _ d -> d.value )) ])

let NonNullAsyncDataType =
    Define.ObjectRec<NonNullAsyncTestSubject>(
        name = "NonNullAsyncData",
        fieldsFn = fun () -> [ Define.AsyncField("value", StringType, (fun _ d -> d.value )) ])

let DataType =
    Define.ObjectRec<TestSubject>(
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
   }

let Query =
    Define.ObjectRec<TestSubject>(
        name = "Query",
        fieldsFn = fun () ->
        [
            Define.Field("listData", ListOf UnionType, (fun _ _ -> data.list))
            Define.Field("testData", DataType, (fun _ _ -> data))
        ])

let schemaConfig =
    { SchemaConfig.DefaultWithBufferedStream(streamOptions = { Interval = None; PreferredBatchSize = None }) with Types = [ CType; DType ] }


let sub =
    { FieldName = "live"
      TypeName = "Data"
      Filter = (fun (x : TestSubject) (y : TestSubject) -> x.id = y.id)
      Project = (fun x -> x.live) }

schemaConfig.LiveFieldSubscriptionProvider.Register sub

let schema = Schema(Query, config = schemaConfig)

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
    let result = query |> executor.AsyncExecute |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        sub.Received |> single |> equals expectedDeferred

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
    let result = query |> executor.AsyncExecute |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted(2)
        sub.Received
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
    let result = query |> executor.AsyncExecute |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        sub.Received |> single |> equals expectedDeferred

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
    let result = query |> executor.AsyncExecute |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        sub.Received |> single |> equals expectedDeferred

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
    let result = query |> executor.AsyncExecute |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        sub.Received |> single |> equals expectedDeferred

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
    let result = query |> executor.AsyncExecute |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted(2)
        sub.Received
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
    let result = query |> executor.AsyncExecute |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        sub.Received |> single |> equals expectedDeferred

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
    let result = query |> executor.AsyncExecute |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted(2)
        sub.Received
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
    let result = query |> executor.AsyncExecute |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = deferred |> Observer.createWithCallback (fun sub _ ->
            if Seq.length sub.Received = 1 then mre1.Set() |> ignore
            elif Seq.length sub.Received = 2 then mre2.Set() |> ignore)
        waitFor hasSubscribers 10 "Timeout while waiting for subscribers on GQLResponse"
        updateLiveData()
        // The second result is a delayed async field, which is set to compute the value for 5 seconds.
        // The first result should come as soon as the live value is updated, which sould be almost instantly.
        // Therefore, let's assume that if it does not come in at least 3 seconds, test has failed.
        if TimeSpan.FromSeconds(float (ms 3)) |> mre1.WaitOne |> not
        then fail "Timeout while waiting for first deferred result"
        if TimeSpan.FromSeconds(float (ms 10)) |> mre2.WaitOne |> not
        then fail "Timeout while waiting for second deferred result"
        sub.Received
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
    let result = query |> executor.AsyncExecute |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        waitFor hasSubscribers 10 "Timeout while waiting for subscribers on GQLResponse"
        updateLiveData()
        sub.WaitForItem()
        sub.Received
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
    let result = query |> executor.AsyncExecute |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted(2)
        sub.Received
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
    let result = query |> executor.AsyncExecute |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted(2)
        sub.Received
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
    let result = query |> executor.AsyncExecute |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        sub.Received |> single |> equals expectedDeferred

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
    let result = query |> executor.AsyncExecute |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        sub.Received |> single |> equals expectedDeferred

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
    let result = query |> executor.AsyncExecute |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted(2)
        sub.Received
        |> Seq.cast<GQLDeferredResponseContent>
        |> contains expectedDeferred1
        |> contains expectedDeferred2
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
    let result = query |> executor.AsyncExecute |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted(3)
        sub.Received
        |> Seq.cast<GQLDeferredResponseContent>
        |> contains expectedDeferred1
        |> contains expectedDeferred2
        |> contains expectedDeferred3
        |> ignore

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
    let result = query |> executor.AsyncExecute |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        sub.Received |> single |> equals expectedDeferred

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
    let result = query |> executor.AsyncExecute |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        sub.Received |> single |> equals expectedDeferred

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
    let query = sprintf """{
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
    let result = query |> executor.AsyncExecute |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        sub.Received |> single |> equals expectedDeferred

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
    let query = sprintf """{
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
    let result = query |> executor.AsyncExecute |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        sub.Received |> single |> equals expectedDeferred

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
    let result = query |> executor.AsyncExecute |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted(2)
        sub.Received
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
    let result = query |> executor.AsyncExecute |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted(2)
        sub.Received
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
    let result = query |> executor.AsyncExecute |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = deferred |> Observer.createWithCallback (fun sub _ ->
            if Seq.length sub.Received = 1 then mre1.Set() |> ignore
            elif Seq.length sub.Received = 2 then mre2.Set() |> ignore)
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
        sub.Received
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
    let result = query |> executor.AsyncExecute |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = deferred |> Observer.createWithCallback (fun sub _ ->
            if Seq.length sub.Received = 1 then mre1.Set() |> ignore
            elif Seq.length sub.Received = 2 then mre2.Set() |> ignore)
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
        sub.Received
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
    let query = sprintf """{
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
    let result = query |> executor.AsyncExecute |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = Observer.create deferred
        sub.WaitCompleted()
        sub.Received |> single |> equals expectedDeferred

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
    let result = query |> executor.AsyncExecute |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = deferred |> Observer.createWithCallback (fun sub _ ->
            if Seq.length sub.Received = 1 then mre1.Set() |> ignore
            elif Seq.length sub.Received = 2 then mre2.Set() |> ignore)
        // The second result is a delayed async field, which is set to compute the value for 5 seconds.
        // The first result should come almost instantly, as it is not a delayed computed field.
        // Therefore, let's assume that if it does not come in at least 3 seconds, test has failed.
        if TimeSpan.FromSeconds(float (ms 3)) |> mre1.WaitOne |> not
        then fail "Timeout while waiting for first deferred result"
        if TimeSpan.FromSeconds(float (ms 10)) |> mre2.WaitOne |> not
        then fail "Timeout while waiting for second deferred result"
        sub.WaitCompleted(timeout = ms 10)
        sub.Received
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
    let result = query |> executor.AsyncExecute |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = deferred |> Observer.createWithCallback (fun sub _ ->
            if Seq.length sub.Received = 1 then mre1.Set() |> ignore
            elif Seq.length sub.Received = 2 then mre2.Set() |> ignore)
        // The first result is a delayed async field, which is set to compute the value for 5 seconds.
        // The second result should come first, almost instantly, as it is not a delayed computed field.
        // Therefore, let's assume that if it does not come in at least 4 seconds, test has failed.
        if TimeSpan.FromSeconds(float (ms 4)) |> mre1.WaitOne |> not
        then fail "Timeout while waiting for first deferred result"
        if TimeSpan.FromSeconds(float (ms 10)) |> mre2.WaitOne |> not
        then fail "Timeout while waiting for second deferred result"
        sub.WaitCompleted(timeout = ms 10)
        sub.Received
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
    let result = query |> executor.AsyncExecute |> sync
    ensureDeferred result <| fun data errors deferred ->
        empty errors
        data |> equals (upcast expectedDirect)
        use sub = deferred |> Observer.createWithCallback (fun sub _ ->
            if Seq.length sub.Received = 1 then mre1.Set() |> ignore
            elif Seq.length sub.Received = 2 then mre2.Set() |> ignore)
        // The first result is a delayed async field, which is set to compute the value for 5 seconds.
        // The second result should come first, almost instantly, as it is not a delayed computed field.
        // Therefore, let's assume that if it does not come in at least 4 seconds, test has failed.
        if TimeSpan.FromSeconds(float (ms 4)) |> mre1.WaitOne |> not
        then fail "Timeout while waiting for first deferred result"
        if TimeSpan.FromSeconds(float (ms 10)) |> mre2.WaitOne |> not
        then fail "Timeout while waiting for second deferred result"
        sub.WaitCompleted(timeout = ms 10)
        sub.Received
        |> Seq.cast<GQLDeferredResponseContent>
        |> itemEquals 0 expectedDeferred1
        |> itemEquals 1 expectedDeferred2
        |> ignore
