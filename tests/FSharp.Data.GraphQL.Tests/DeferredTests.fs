module FSharp.Data.GraphQL.Tests.DeferredTests

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

#nowarn "40"

type TestSubject = {
    id: string
    a: string
    b: string
    union: UnionTestSubject
    list: UnionTestSubject list
    innerList: InnerTestSubject list
    mutable live: string
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

let AType =
    Define.Object<A>(
        "A", [
            Define.Field("a", String, resolve = fun _ a -> a.a)
            Define.Field("id", String, resolve = fun _ a -> a.id)
        ])

let BType =
    Define.Object<B>(
        "B", [
            Define.Field("id", String, (fun _ b -> b.id))
            Define.Field("b", Int, (fun _ b -> b.b))
        ])

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
    Define.Object<InnerTestSubject>(
        name = "InnerData",
        fieldsFn = fun () ->
        [
            Define.Field("a", String, (fun _ d -> d.a))
            Define.Field("innerList", ListOf InnerDataType, (fun _ d -> d.innerList))
        ])

let DataType =
    Define.Object<TestSubject>(
        name = "Data",
        fieldsFn = fun () ->
        [
            Define.Field("id", String, (fun _ d -> d.id))
            Define.Field("a", String, (fun _ d -> d.a))
            Define.Field("b", String, (fun _ d -> d.b))
            Define.Field("union", UnionType, (fun _ d -> d.union))
            Define.Field("list", ListOf UnionType, (fun _ d -> d.list))
            Define.Field("innerList", ListOf InnerDataType, (fun _ d -> d.innerList))
            Define.Field("live", String, (fun _ d -> d.live))
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
   }

let Query =
    Define.Object<TestSubject>(
        name = "Query",
        fieldsFn = fun () -> [ Define.Field("testData", DataType, (fun _ _ -> data)) ] )

let config = SchemaConfig.Default

let sub =
    { FieldName = "live"
      TypeName = "Data"
      Identity = fun (x : TestSubject) -> x.id }

config.LiveFieldSubscriptionProvider.Register sub

let schema = Schema(Query, config = config)

let executor = Executor(schema)

let hasSubscribers () =
    config.LiveFieldSubscriptionProvider.HasSubscribers "Data" "live"

let updateData () =
    data.live <- "another value"
    config.LiveFieldSubscriptionProvider.Publish "Data" "live" data

[<Fact>]
let ``Live Query`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "id", upcast "1"
            ]
        ]
    let expectedLive2 =
        NameValueLookup.ofList [
            "data", upcast "some value"
            "path", upcast ["testData"; "live"]
        ]
    let expectedLive1 =
        NameValueLookup.ofList [
            "data", upcast "another value"
            "path", upcast ["testData"; "live"]
        ]
    let query = parse """{
        testData {
            id
            live @live
        }
    }"""
    use mre = new ManualResetEvent(false)
    let actualDeferred = ConcurrentBag<Output>()
    let result = query |> executor.AsyncExecute |> sync
    match result with
    | Deferred(data, errors, deferred) ->
        empty errors
        data.["data"] |> equals (upcast expectedDirect)
        deferred
        |> Observable.add (fun x ->
            actualDeferred.Add(x)
            if actualDeferred.Count = 2 then set mre)
        waitFor hasSubscribers 10 "Timeout while waiting for subscribers on GQLResponse"
        updateData ()
        wait mre "Timeout while waiting for Deferred GQLResponse"
        actualDeferred
        |> Seq.cast<NameValueLookup>
        |> contains expectedLive1
        |> contains expectedLive2
        |> ignore
    | _ -> fail "Expected Deferred GQLResponse"

[<Fact>]
let ``Parallell Defer and Stream`` () =
    let expectedDirect =
        NameValueLookup.ofList [
           "testData", upcast NameValueLookup.ofList [
                "b", upcast "Banana"
            ]
        ]
    let expectedDeferred1 =
        NameValueLookup.ofList [
            "data", upcast "Apple"
            "path", upcast ["testData"; "a"]
        ]
    let expectedDeferred2 =
        NameValueLookup.ofList [
            "data", [
                NameValueLookup.ofList [
                    "a", upcast "Inner A"
                ] :> obj
            ] :> obj
            "path", upcast ["testData"; "innerList"]
        ]
    let query s =
        sprintf """{
            testData {
                a @%s
                b
                innerList @%s {
                    a
                }
            }
        }""" s s
    asts query
    |> Seq.iter (fun query ->
        use mre = new ManualResetEvent(false)
        let actualDeferred = ConcurrentBag<Output>()
        let result = query |> executor.AsyncExecute |> sync
        match result with
        | Deferred(data, errors, deferred) ->
            empty errors
            data.["data"] |> equals (upcast expectedDirect)
            deferred
            |> Observable.add (fun x ->
                actualDeferred.Add(x)
                if actualDeferred.Count = 2 then set mre)
            wait mre "Timeout while waiting for Deferred GQLResponse"
            actualDeferred
            |> Seq.cast<NameValueLookup>
            |> contains expectedDeferred1
            |> contains expectedDeferred2
            |> ignore
        | _ -> fail "Expected Deferred GQLResponse")

[<Fact>]
let ``Inner Object List Defer and Stream`` () =
    let expectedDirect =
        NameValueLookup.ofList [
           "testData", upcast NameValueLookup.ofList [
                "b", upcast "Banana"
            ]
        ]
    let expectedDeferred =
        NameValueLookup.ofList [
            "data", [
                NameValueLookup.ofList [
                    "a", upcast "Inner A"
                ] :> obj
            ] :> obj
            "path", upcast ["testData"; "innerList"]
        ]
    let query = sprintf """{
            testData {
                b
                innerList @%s {
                    a
                }
            }
        }"""
    asts query
    |> Seq.iter (fun query ->
        use mre = new ManualResetEvent(false)
        let actualDeferred = ConcurrentBag<Output>()
        let result = query |> executor.AsyncExecute |> sync
        match result with
        | Deferred(data, errors, deferred) ->
            empty errors
            data.["data"] |> equals (upcast expectedDirect)
            deferred |> Observable.add (fun x -> actualDeferred.Add(x); set mre)
            wait mre "Timeout while waiting for Deferred GQLResponse"
            actualDeferred |> single |> equals (upcast expectedDeferred)
        | _ -> fail "Expected Deferred GQLResponse")

[<Fact>]
let ``Nested Inner Object List Defer`` () =
    let expectedDirect =
        NameValueLookup.ofList [
           "testData", upcast NameValueLookup.ofList [
                "b", upcast "Banana"
            ]
        ]
    let expectedDeferred1 =
        NameValueLookup.ofList [
            "data", [
                NameValueLookup.ofList [
                    "a", upcast "Inner A"
                ] :> obj
            ] :> obj
            "path", upcast ["testData"; "innerList"]
        ]
    let expectedDeferred2 =
        NameValueLookup.ofList [
            "data", [
                NameValueLookup.ofList [
                    "a", upcast "Inner B"
                ] :> obj
                NameValueLookup.ofList [
                    "a", upcast "Inner C"
                ] :> obj
            ] :> obj
            "path", upcast ["testData" :> obj; "innerList" :> obj; 0 :> obj; "innerList" :> obj]
        ]
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
    use mre = new ManualResetEvent(false)
    let actualDeferred = ConcurrentBag<Output>()
    let result = query |> executor.AsyncExecute |> sync
    match result with
    | Deferred(data, errors, deferred) ->
        empty errors
        data.["data"] |> equals (upcast expectedDirect)
        deferred
        |> Observable.add (fun x ->
            actualDeferred.Add(x)
            if actualDeferred.Count = 2 then set mre)
        wait mre "Timeout while waiting for Deferred GQLResponse"
        actualDeferred
        |> Seq.cast<NameValueLookup>
        |> contains expectedDeferred1
        |> contains expectedDeferred2
        |> ignore
    | _ -> fail "Expected Deferred GQLResponse"

[<Fact>]
let ``Nested Inner Object List Stream`` () =
    let expectedDirect =
        NameValueLookup.ofList [
           "testData", upcast NameValueLookup.ofList [
                "b", upcast "Banana"
            ]
        ]
    let expectedDeferred1 =
        NameValueLookup.ofList [
            "data", [
                NameValueLookup.ofList [
                    "a", upcast "Inner A"
                ] :> obj
            ] :> obj
            "path", upcast ["testData"; "innerList"]
        ]
    let expectedDeferred2 =
        NameValueLookup.ofList [
            "data", [
                NameValueLookup.ofList [
                    "a", upcast "Inner B"
                ] :> obj
                NameValueLookup.ofList [
                    "a", upcast "Inner C"
                ] :> obj
            ] :> obj
            "path", upcast ["testData" :> obj; "innerList" :> obj; 0 :> obj; "innerList" :> obj; 0 :> obj]
        ]
    let expectedDeferred3 =
        NameValueLookup.ofList [
            "data", [
                NameValueLookup.ofList [
                    "a", upcast "Inner C"
                ] :> obj
            ] :> obj
            "path", upcast ["testData" :> obj; "innerList" :> obj; 0 :> obj; "innerList" :> obj; 1 :> obj]
        ]
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
    use mre = new ManualResetEvent(false)
    let actualDeferred = ConcurrentBag<Output>()
    let result = query |> executor.AsyncExecute |> sync
    match result with
    | Deferred(data, errors, deferred) ->
        empty errors
        data.["data"] |> equals (upcast expectedDirect)
        deferred
        |> Observable.add (fun x ->
            actualDeferred.Add(x)
            if actualDeferred.Count = 3 then set mre)
        wait mre "Timeout while waiting for Deferred GQLResponse"
        actualDeferred
        |> Seq.cast<NameValueLookup>
        |> contains expectedDeferred1
        |> contains expectedDeferred2
        |> contains expectedDeferred3
        |> ignore
    | _ -> fail "Expected Deferred GQLResponse"

[<Fact>]
let ``Simple Defer and Stream`` () =
    let expectedDirect =
        NameValueLookup.ofList [
           "testData", upcast NameValueLookup.ofList [
                "b", upcast "Banana"
            ]
        ]
    let expectedDeferred =
        NameValueLookup.ofList [
            "data", upcast "Apple"
            "path", upcast ["testData"; "a"]
        ]
    let query = sprintf """{
        testData {
            a @%s
            b
        }
    }"""
    asts query
    |> Seq.iter (fun query ->
        use mre = new ManualResetEvent(false)
        let actualDeferred = ConcurrentBag<Output>()
        let result = query |> executor.AsyncExecute |> sync
        match result with
        | Deferred(data, errors, deferred) ->
            empty errors
            data.["data"] |> equals (upcast expectedDirect)
            deferred |> Observable.add (fun x -> actualDeferred.Add(x); mre.Set() |> ignore)
            if TimeSpan.FromSeconds(float 30) |> mre.WaitOne |> not
            then fail "Timeout while waiting for Deferred GQLResponse"
            actualDeferred |> single |> equals (upcast expectedDeferred)
        | _ -> fail "Expected Deferred GQLResponse")

[<Fact>]
let ``List Defer``() =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "a", upcast "Apple"
            ]
        ]
    let expectedDeferred =
        NameValueLookup.ofList [
            "data", [
                NameValueLookup.ofList [
                    "id", upcast "2"
                    "a", upcast "Union A"
                ] :> obj
                NameValueLookup.ofList [
                    "id", upcast "3"
                    "b", upcast 4
                ] :> obj
            ] :> obj
            "path", upcast ["testData"; "list"]
        ]
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
    use mre = new ManualResetEvent(false)
    let actualDeferred = ConcurrentBag<Output>()
    let result = query |> executor.AsyncExecute |> sync
    match result with
    | Deferred(data, errors, deferred) ->
        empty errors
        data.["data"] |> equals (upcast expectedDirect)
        deferred |> Observable.add (fun x -> actualDeferred.Add(x); mre.Set() |> ignore)
        if TimeSpan.FromSeconds(float 30) |> mre.WaitOne |> not
        then fail "Timeout while waiting for Deferred GQLResponse"
        actualDeferred |> single |> equals (upcast expectedDeferred)
    | _ -> fail "Expected Deferred GQLResponse"

[<Fact>]
let ``List Fragment Defer and Stream - Exclusive``() =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "a", upcast "Apple"
                "list", [
                    NameValueLookup.ofList [
                        "id", upcast "2"
                    ]
                    NameValueLookup.ofList [
                        "id", upcast "3"
                        "b", upcast 4
                    ]
                ] :> obj
            ]
        ]
    let expectedDeferred =
        NameValueLookup.ofList [
            "data", upcast "Union A"
            "path", upcast ["testData" :> obj; "list" :> obj; 0 :> obj; "a" :> obj]
        ]
    let query = sprintf """{
        testData {
            a
            list {
                ... on A {
                    id
                    a @%s
                }
                ... on B {
                    id
                    b
                }
            }
        }
    }"""
    asts query
    |> Seq.iter (fun query ->
        use mre = new ManualResetEvent(false)
        let actualDeferred = ConcurrentBag<Output>()
        let result = query |> executor.AsyncExecute |> sync
        match result with
        | Deferred(data, errors, deferred) ->
            empty errors
            data.["data"] |> equals (upcast expectedDirect)
            deferred |> Observable.add (fun x -> actualDeferred.Add(x); mre.Set() |> ignore)
            if TimeSpan.FromSeconds(float 30) |> mre.WaitOne |> not
            then fail "Timeout while waiting for Deferred GQLResponse"
            actualDeferred |> single |> equals (upcast expectedDeferred)
        | _ -> fail "Expected Deferred GQLResponse")

[<Fact>]
let ``List Fragment Defer and Stream - Common``() =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "a", upcast "Apple"
                "list", [
                    NameValueLookup.ofList [
                        "a", upcast "Union A"
                    ]
                    NameValueLookup.ofList [
                        "id", upcast "3"
                        "b", upcast 4
                    ]
                ] :> obj
            ]
        ]
    let expectedDeferred =
        NameValueLookup.ofList [
            "data", upcast "2"
            "path", upcast ["testData" :> obj; "list" :> obj; 0 :> obj; "id" :> obj]
        ]
    let query = sprintf """{
        testData {
            a
            list {
                ... on A {
                    id @%s
                    a
                }
                ... on B {
                    id
                    b
                }
            }
        }
    }"""
    asts query
    |> Seq.iter (fun query ->
        use mre = new ManualResetEvent(false)
        let actualDeferred = ConcurrentBag<Output>()
        let result = query |> executor.AsyncExecute |> sync
        match result with
        | Deferred(data, errors, deferred) ->
            empty errors
            data.["data"] |> equals (upcast expectedDirect)
            deferred |> Observable.add (fun x -> actualDeferred.Add(x); mre.Set() |> ignore)
            if TimeSpan.FromSeconds(float 30) |> mre.WaitOne |> not
            then fail "Timeout while waiting for Deferred GQLResponse"
            actualDeferred |> single |> equals (upcast expectedDeferred)
        | _ -> fail "Expected Deferred GQLResponse")

[<Fact>]
let ``List Stream``() =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "a", upcast "Apple"
            ]
        ]
    let expectedDeferred1 =
        NameValueLookup.ofList [
            "data", [
                NameValueLookup.ofList [
                    "id", upcast "2"
                    "a", upcast "Union A"
                ] :> obj
            ] :> obj
            "path", upcast ["testData" :> obj; "list" :> obj; 0 :> obj]
        ]
    let expectedDeferred2 =
        NameValueLookup.ofList [
            "data", [
                NameValueLookup.ofList [
                    "id", upcast "3"
                    "b", upcast 4
                ] :> obj
            ] :> obj
            "path", upcast ["testData" :> obj; "list" :> obj; 1 :> obj]
        ]
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
    use mre = new ManualResetEvent(false)
    let actualDeferred = ConcurrentBag<Output>()
    let result = query |> executor.AsyncExecute |> sync
    match result with
    | Deferred(data, errors, deferred) ->
        empty errors
        data.["data"] |> equals (upcast expectedDirect)
        deferred
        |> Observable.add (fun x ->
            actualDeferred.Add(x)
            if actualDeferred.Count = 2 then mre.Set() |> ignore)
        if TimeSpan.FromSeconds(float 30) |> mre.WaitOne |> not
        then fail "Timeout while waiting for Deferred GQLResponse"
        actualDeferred
        |> Seq.cast<NameValueLookup>
        |> contains expectedDeferred1
        |> contains expectedDeferred2
        |> ignore
    | _ -> fail "Expected Deferred GQLResponse"

[<Fact>]
let ``Union Defer and Stream`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "a", upcast "Apple"
                "b", upcast "Banana"
            ]
        ]
    let expectedDeferred =
        NameValueLookup.ofList [
            "data", upcast NameValueLookup.ofList [ "id", upcast "1"; "a", upcast "Union A" ]
            "path", upcast ["testData"; "union"]
        ]
    let query = sprintf """{
        testData {
            a
            b
            union @%s {
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
    asts query
    |> Seq.iter (fun query ->
        use mre = new ManualResetEvent(false)
        let actualDeferred = ConcurrentBag<Output>()
        let result = query |> executor.AsyncExecute |> sync
        match result with
        | Deferred(data, errors, deferred) ->
            empty errors
            data.["data"] |> equals (upcast expectedDirect)
            deferred |> Observable.add (fun x -> actualDeferred.Add(x); mre.Set() |> ignore)
            if TimeSpan.FromSeconds(float 30) |> mre.WaitOne |> not
            then fail "Timeout while waiting for Deferred GQLResponse"
            actualDeferred |> single |> equals (upcast expectedDeferred)
        | _ -> fail "Expected Deferred GQLRespnse")