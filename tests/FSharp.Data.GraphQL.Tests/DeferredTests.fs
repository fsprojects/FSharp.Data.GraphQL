module FSharp.Data.GraphQL.Tests.DeferredTests

open System
open Xunit
open FSharp.Control
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution
open System.Threading
open System.Collections.Generic
open System.Collections.Concurrent
open FSharp.Data.GraphQL.Types

#nowarn "40"

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
    resolverError : AsyncTestSubject
    resolverListError : AsyncTestSubject list
    nullableError : AsyncTestSubject
    nullableListError : AsyncTestSubject list
}

and AsyncTestSubject = {
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
            Define.Field("a", String, resolve = fun _ a -> a.a)
            Define.Field("id", String, resolve = fun _ a -> a.id)
        ])

let BType =
    Define.Object<B>(
        "B", [
            Define.Field("id", String, (fun _ (b : B) -> b.id))
            Define.Field("b", Int, (fun _ b -> b.b))
        ])

let InterfaceType =
    Define.Interface(
        "TestInterface", [
            Define.Field("id", String, resolve = fun _ (x : InterfaceSubject) -> x.Id)
            Define.Field("value", String, resolve = fun _ (x : InterfaceSubject) -> x.Value)
        ])

let CType =
    Define.Object<C>(
        name ="C",
        fields = [ Define.Field("id", String, (fun _ (c : C) -> c.id)); Define.Field("value", String, (fun _ c -> c.value)) ],
        interfaces = [ InterfaceType ],
        isTypeOf = (fun o -> o :? C))

let DType =
    Define.Object<D>(
        name = "D",
        fields = [ Define.Field("id", String, (fun _ (d : D) -> d.id)); Define.Field("value", String, (fun _ d -> d.value)) ],
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
    Define.Object<InnerTestSubject>(
        name = "InnerData",
        fieldsFn = fun () ->
        [
            Define.Field("a", String, (fun _ d -> d.a))
            Define.Field("innerList", ListOf InnerDataType, (fun _ d -> d.innerList))
        ])

let AsyncDataType =
    Define.Object<AsyncTestSubject>(
        name = "AsyncData",
        fieldsFn = fun () -> [ Define.AsyncField("value", String, (fun _ d -> d.value)) ])

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
            Define.Field("iface", InterfaceType, (fun _ d -> d.iface))
            Define.Field("ifaceList", ListOf InterfaceType, (fun _ d -> d.ifaceList))
            Define.Field("delayed", AsyncDataType, (fun _ d -> d.delayed))
            Define.Field("delayedList", ListOf AsyncDataType, (fun _ d -> d.delayedList))
            Define.Field("resolverError", AsyncDataType, (fun _ d -> d.resolverError))
            Define.Field("nullableError", AsyncDataType, (fun _ d -> d.nullableError))
            Define.Field("resolverListError", ListOf AsyncDataType, (fun _ d -> d.resolverListError))
            Define.Field("nullableListError", ListOf AsyncDataType, (fun _ d -> d.nullableListError))
        ])

let delay secs x = async {
    do! Async.Sleep(secs * 1000)
    return x
}

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
       delayed = { value = delay 15 "Delayed value" }
       delayedList = [
           { value = async { return "Delayed value 1" } }
           { value = delay 5 "Delayed value 2" }
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
   }

let Query =
    Define.Object<TestSubject>(
        name = "Query",
        fieldsFn = fun () -> [ Define.Field("testData", DataType, (fun _ _ -> data)) ] )

let schemaConfig = 
    { SchemaConfig.Default with Types = [ CType; DType ] }


let sub =
    { FieldName = "live"
      TypeName = "Data"
      Identity = fun (x : TestSubject) -> x.id }

schemaConfig.LiveFieldSubscriptionProvider.Register sub

let schema = Schema(Query, config = schemaConfig)

let executor = Executor(schema)

let hasSubscribers () =
    schemaConfig.LiveFieldSubscriptionProvider.HasSubscribers "Data" "live"

let updateData () =
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
        NameValueLookup.ofList [
            "data", upcast NameValueLookup.ofList [
                "resolverError", null
            ]
            "errors", upcast [
                box <| NameValueLookup.ofList [
                    "message", upcast "Resolver error!"
                    "path", upcast [ "testData"; "resolverError"; "value" ]
                ]
            ]
            "path", upcast [ "testData"; "resolverError" ]
        ]
    let query = parse """{
        testData {
            resolverError @defer {
                value
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
        |> Observable.add (fun x -> actualDeferred.Add(x); set mre)
        wait mre "Timeout while waiting for Deferred GQLResponse"
        actualDeferred |> single |> equals (upcast expectedDeferred)
    | _ -> fail "Expected Deferred GQLResponse"

[<Fact>]
let ``Resolver list error`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "resolverListError", upcast []
            ]
        ]
    let expectedDeferred1 =
        NameValueLookup.ofList [
            "data", null
            "errors", upcast [
                box <| NameValueLookup.ofList [
                    "message", upcast "Resolver error!"
                    "path", upcast [ "testData"; "resolverListError"; "value" ]
                ]
            ]
            "path", upcast [ box "testData"; upcast "resolverListError"; upcast 0 ]
        ]
    let expectedDeferred2 =
        NameValueLookup.ofList [
            "data", null
            "errors", upcast [
                box <| NameValueLookup.ofList [
                    "message", upcast "Resolver error!"
                    "path", upcast [ "testData"; "resolverListError"; "value" ]
                ]
            ]
            "path", upcast [ box "testData"; upcast "resolverListError"; upcast 1 ]
        ]
    let query = parse """{
        testData {
            resolverListError @stream {
                value
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
let ``Nullable error`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "nullableError", null
            ]
        ]
    let expectedDeferred =
        NameValueLookup.ofList [
            "data", upcast NameValueLookup.ofList [
                "nullableError", null
            ]
            "errors", upcast [
                box <| NameValueLookup.ofList [
                    "message", upcast "Non-Null field value resolved as a null!"
                    "path", upcast [ "testData"; "nullableError"; "value" ]
                ]
            ]
            "path", upcast [ "testData"; "nullableError" ]
        ]
    let query = parse """{
        testData {
            nullableError @defer {
                value
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
        |> Observable.add (fun x -> actualDeferred.Add(x); set mre)
        wait mre "Timeout while waiting for Deferred GQLResponse"
        actualDeferred |> single |> equals (upcast expectedDeferred)
    | _ -> fail "Expected Deferred GQLResponse"

[<Fact>]
let ``Single Root object field - Defer and Stream`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "iface", null
            ]
        ]
    let expectedDeferred =
        NameValueLookup.ofList [
            "data", upcast NameValueLookup.ofList [
                "id", upcast "1000"
                "value", upcast "C"
            ]
            "path", upcast [ "testData"; "iface" ]
        ]
    let query = sprintf """{
        testData {
            iface @%s {
                id
                value
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
let ``Single Root object list field - Defer`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "ifaceList", upcast [ ]
            ]
        ]
    let expectedDeferred =
        NameValueLookup.ofList [
            "data", upcast [
                box <| NameValueLookup.ofList [
                    "id", upcast "2000"
                    "value", upcast "D"
                ]
                box <| NameValueLookup.ofList [
                    "id", upcast "3000"
                    "value", upcast "C2"
                ]
            ]
            "path", upcast [ box "testData"; upcast "ifaceList" ]
        ]
    let query = parse """{
        testData {
            ifaceList @defer {
                id
                value
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
        |> Observable.add (fun x -> actualDeferred.Add(x); set mre)
        wait mre "Timeout while waiting for Deferred GQLResponse"
        actualDeferred |> single |> equals (upcast expectedDeferred)
    | _ -> fail "Expected Deferred GQLResponse"

[<Fact>]
let ``Single Root object list field - Stream`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "ifaceList", upcast [ ]
            ]
        ]
    let expectedDeferred1 =
        NameValueLookup.ofList [
            "data", upcast [
                box <| NameValueLookup.ofList [
                    "id", upcast "2000"
                    "value", upcast "D"
                ]
            ]
            "path", upcast [ box "testData"; upcast "ifaceList"; upcast 0 ]
        ]
    let expectedDeferred2 =
        NameValueLookup.ofList [
            "data", upcast [
                box <| NameValueLookup.ofList [
                    "id", upcast "3000"
                    "value", upcast "C2"
                ]
            ]
            "path", upcast [ box "testData"; upcast "ifaceList"; upcast 1 ]
        ]
    let query = parse """{
        testData {
            ifaceList @stream {
                id
                value
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
let ``Interface field - Defer and Stream`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "iface", upcast NameValueLookup.ofList [
                    "id", upcast "1000"
                    "value", null
                ]
            ]
        ]
    let expectedDeferred =
        NameValueLookup.ofList [
            "data", upcast "C"
            "path", upcast [ "testData"; "iface"; "value" ]
        ]
    let query = sprintf """{
        testData {
            iface {
                id
                value @%s
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
let ``Interface list field - Defer and Stream`` () =
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
    let expectedDeferred1 =
        NameValueLookup.ofList [
            "data", upcast "D"
            "path", upcast [ box "testData"; upcast "ifaceList"; upcast 0; upcast "value" ]
        ]
    let expectedDeferred2 =
        NameValueLookup.ofList [
            "data", upcast "C2"
            "path", upcast [ box "testData"; upcast "ifaceList"; upcast 1; upcast "value" ]
        ]
    let query = sprintf """{
        testData {
            ifaceList {
                id
                value @%s
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
let ``Live Query`` () =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "id", upcast "1"
                "live", upcast "some value"
            ]
        ]
    let expectedLive =
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
        |> Observable.add (fun x -> actualDeferred.Add(x); set mre)
        waitFor hasSubscribers 10 "Timeout while waiting for subscribers on GQLResponse"
        updateData ()
        wait mre "Timeout while waiting for Deferred GQLResponse"
        actualDeferred
        |> Seq.cast<NameValueLookup>
        |> contains expectedLive
        |> ignore
    | _ -> fail "Expected Deferred GQLResponse"

[<Fact>]
let ``Parallel Defer`` () =
    let expectedDirect =
        NameValueLookup.ofList [
           "testData", upcast NameValueLookup.ofList [
                "a", null
                "b", upcast "Banana"
                "innerList", upcast []
            ]
        ]
    let expectedDeferred1 =
        NameValueLookup.ofList [
            "data", upcast "Apple"
            "path", upcast ["testData"; "a"]
        ]
    let expectedDeferred2 =
        NameValueLookup.ofList [
            "data", upcast [
                box <| NameValueLookup.ofList [
                    "a", upcast "Inner A"
                ]
            ]
            "path", upcast ["testData"; "innerList"]
        ]
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
let ``Parallell Stream`` () =
    let expectedDirect =
        NameValueLookup.ofList [
           "testData", upcast NameValueLookup.ofList [
                "a", null
                "b", upcast "Banana"
                "innerList", upcast []
            ]
        ]
    let expectedDeferred1 =
        NameValueLookup.ofList [
            "data", upcast "Apple"
            "path", upcast ["testData"; "a"]
        ]
    let expectedDeferred2 =
        NameValueLookup.ofList [
            "data", upcast [
                box <| NameValueLookup.ofList [
                    "a", upcast "Inner A"
                ]
            ]
            "path", upcast [box "testData"; upcast "innerList"; upcast 0]
        ]
    let query = 
        parse """{
            testData {
                a @stream
                b
                innerList @stream {
                    a                    
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
let ``Inner Object List Defer`` () =
    let expectedDirect =
        NameValueLookup.ofList [
           "testData", upcast NameValueLookup.ofList [
                "b", upcast "Banana"
                "innerList", upcast []
            ]
        ]
    let expectedDeferred =
        NameValueLookup.ofList [
            "data", upcast [
                box <| NameValueLookup.ofList [
                    "a", upcast "Inner A"
                ]
            ]
            "path", upcast ["testData"; "innerList"]
        ]
    let query = parse """{
            testData {
                b
                innerList @defer {
                    a                    
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
        deferred |> Observable.add (fun x -> actualDeferred.Add(x); set mre)
        wait mre "Timeout while waiting for Deferred GQLResponse"
        actualDeferred |> single |> equals (upcast expectedDeferred)
    | _ -> fail "Expected Deferred GQLResponse"

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
        NameValueLookup.ofList [
            "data", upcast [
                box <| NameValueLookup.ofList [
                    "a", upcast "Inner A"
                ]
            ]
            "path", upcast [box "testData"; upcast "innerList"; upcast 0]
        ]
    let query = parse """{
            testData {
                b
                innerList @stream {
                    a                    
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
        deferred |> Observable.add (fun x -> actualDeferred.Add(x); set mre)
        wait mre "Timeout while waiting for Deferred GQLResponse"
        actualDeferred |> single |> equals (upcast expectedDeferred)
    | _ -> fail "Expected Deferred GQLResponse"

[<Fact>]
let ``Nested Inner Object List Defer`` () =
    let expectedDirect =
        NameValueLookup.ofList [
           "testData", upcast NameValueLookup.ofList [
                "b", upcast "Banana"
                "innerList", upcast []
            ]
        ]
    let expectedDeferred1 =
        NameValueLookup.ofList [
            "data", upcast [
                box <| NameValueLookup.ofList [
                    "a", upcast "Inner A"
                    "innerList", upcast []
                ]
            ]
            "path", upcast ["testData"; "innerList"]
        ]
    let expectedDeferred2 =
        NameValueLookup.ofList [
            "data", upcast [
                box <| NameValueLookup.ofList [
                    "a", upcast "Inner B"
                ]
                upcast NameValueLookup.ofList [
                    "a", upcast "Inner C"
                ]
            ]
            "path", upcast [box "testData"; upcast "innerList"; upcast 0; upcast "innerList"]
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
                "innerList", upcast []
            ]
        ]
    let expectedDeferred1 =
        NameValueLookup.ofList [
            "data", upcast [
                box <| NameValueLookup.ofList [
                    "a", upcast "Inner A"
                    "innerList", upcast []
                ]
            ]
            "path", upcast ["testData"; "innerList"]
        ]
    let expectedDeferred2 =
        NameValueLookup.ofList [
            "data", upcast [
                box <| NameValueLookup.ofList [
                    "a", upcast "Inner B"
                ]
            ]
            "path", upcast [box "testData"; upcast "innerList"; upcast 0; upcast "innerList"; upcast 0]
        ]
    let expectedDeferred3 =
        NameValueLookup.ofList [
            "data", upcast [
                box <| NameValueLookup.ofList [
                    "a", upcast "Inner C"
                ]
            ]
            "path", upcast [box "testData"; upcast "innerList"; upcast 0; upcast "innerList"; upcast 1]
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
                "a", null
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
                "list", upcast []
            ]
        ]
    let expectedDeferred =
        NameValueLookup.ofList [
            "data", upcast [
                box <| NameValueLookup.ofList [
                    "id", upcast "2"
                    "a", upcast "Union A"
                ]
                upcast NameValueLookup.ofList [
                    "id", upcast "3"
                    "b", upcast 4
                ]
            ]
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
    let expectedDeferred =
        NameValueLookup.ofList [
            "data", upcast "Union A"
            "path", upcast [box "testData"; upcast "list"; upcast 0; upcast "a"]
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
    let expectedDeferred =
        NameValueLookup.ofList [
            "data", upcast "2"
            "path", upcast [box "testData"; upcast "list"; upcast 0; upcast "id"]
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
                "list", upcast []
            ]
        ]
    let expectedDeferred1 =
        NameValueLookup.ofList [
            "data", upcast [
                box <| NameValueLookup.ofList [
                    "id", upcast "2"
                    "a", upcast "Union A"
                ]
            ]
            "path", upcast [box "testData"; upcast "list"; upcast 0]
        ]        
    let expectedDeferred2 =
        NameValueLookup.ofList [
            "data", upcast [
                box <| NameValueLookup.ofList [
                    "id", upcast "3"
                    "b", upcast 4
                ]
            ]
            "path", upcast [box "testData"; upcast "list"; box 1]
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
                "union", null
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

[<Fact(Skip="Needs at least two cores to work, not consistent in CI")>]
let ``Each deferred result should be sent as soon as it is computed``() =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "b", null
                "delayed", null
            ]
        ]
    let expectedDeferred1 =
        NameValueLookup.ofList [
            "data", upcast "Banana"
            "path", upcast ["testData"; "b"]
        ]
    let expectedDeferred2 =
        NameValueLookup.ofList [
            "data", upcast NameValueLookup.ofList [
                "value", upcast "Delayed value"
            ]
            "path", upcast ["testData"; "delayed"]
        ]
    let query = parse """{
        testData {
            b @defer
            delayed @defer {
                value
            }
        }
    }"""
    use mre1 = new ManualResetEvent(false)
    use mre2 = new ManualResetEvent(false)
    let actualDeferred = List<Output>()
    let result = query |> executor.AsyncExecute |> sync
    match result with
    | Deferred(data, errors, deferred) ->
        empty errors
        data.["data"] |> equals (upcast expectedDirect)
        deferred |> Observable.add (fun x -> 
            if actualDeferred.Count < 2 then actualDeferred.Add(x)
            if actualDeferred.Count = 1 then mre1.Set() |> ignore
            if actualDeferred.Count = 2 then mre2.Set() |> ignore)
        // The second result is a delayed async field, which is set to compute the value for 5 seconds.
        // The first result should come almost instantly, as it is not a delayed computed field.
        // Therefore, let's assume that if it does not come in at least 4 seconds, test has failed.
        if TimeSpan.FromSeconds(float 10) |> mre1.WaitOne |> not
        then fail "Timeout while waiting for first deferred result"
        if TimeSpan.FromSeconds(float 30) |> mre2.WaitOne |> not
        then fail "Timeout while waiting for second deferred result"
        actualDeferred
        |> Seq.cast<NameValueLookup>
        |> itemEquals 0 expectedDeferred1
        |> itemEquals 1 expectedDeferred2
        |> ignore
    | _ -> fail "Expected Deferred GQLRespnse"

[<Fact>]
let ``Each streamed result should be sent as soon as it is computed``() =
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "delayedList", upcast []
            ]
        ]
    let expectedDeferred1 =
        NameValueLookup.ofList [
            "data", upcast [
                box <| NameValueLookup.ofList [
                    "value", upcast "Delayed value 1"
                ]
            ]
            "path", upcast [box "testData"; upcast "delayedList"; upcast 0]
        ]
    let expectedDeferred2 =
        NameValueLookup.ofList [
            "data", upcast [
                box <| NameValueLookup.ofList [
                    "value", upcast "Delayed value 2"
                ]
            ]
            "path", upcast [box "testData"; upcast "delayedList"; upcast 1]
        ]
    let query = parse """{
        testData {
            delayedList @stream {
                value
            }
        }
    }"""
    use mre1 = new ManualResetEvent(false)
    use mre2 = new ManualResetEvent(false)
    let actualDeferred = List<Output>()
    let result = query |> executor.AsyncExecute |> sync
    match result with
    | Deferred(data, errors, deferred) ->
        empty errors
        data.["data"] |> equals (upcast expectedDirect)
        deferred |> Observable.add (fun x -> 
            if actualDeferred.Count < 2 then actualDeferred.Add(x)
            if actualDeferred.Count = 1 then mre1.Set() |> ignore
            if actualDeferred.Count = 2 then mre2.Set() |> ignore)
        // The second result is a delayed async field, which is set to compute the value for 5 seconds.
        // The first result should come almost instantly, as it is not a delayed computed field.
        // Therefore, let's assume that if it does not come in at least 4 seconds, test has failed.
        if TimeSpan.FromSeconds(float 4) |> mre1.WaitOne |> not
        then fail "Timeout while waiting for first deferred result"
        if TimeSpan.FromSeconds(float 30) |> mre2.WaitOne |> not
        then fail "Timeout while waiting for second deferred result"
        actualDeferred
        |> Seq.cast<NameValueLookup>
        |> itemEquals 0 expectedDeferred1
        |> itemEquals 1 expectedDeferred2
        |> ignore
    | _ -> fail "Expected Deferred GQLRespnse"