module FSharp.Data.GraphQL.Tests.DeferredTests

open System
open Xunit
open FSharp.Control
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution

#nowarn "40"

type TestSubject  = {
    a: string
    b: string
    union: UnionTestSubject
    list: UnionTestSubject list
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
let executor =     
    let AType =
        Define.Object<A>(
            "A", [
                Define.Field("a", String, (fun _ a -> a.a))
                Define.Field("id", String, (fun _ a -> a.id))
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
    let DataType =
        Define.Object<TestSubject>(
            name = "Data", 
            fieldsFn = fun () -> [
                Define.Field("a", String, (fun _ d -> d.a))
                Define.Field("b", String, (fun _ d -> d.b))
                Define.Field("union", UnionType, (fun _ d -> d.union))
                Define.Field("list", ListOf UnionType, (fun _ d -> d.list))
            ])
    let data = {
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
       }


    let Query = 
        Define.Object<TestSubject>(
            name = "Query",
            fieldsFn = fun () -> [
                Define.Field("testData", DataType, (fun _ d -> data))
            ]
        )
    let schema = Schema(Query)
    Executor(schema)

// [<Fact>]
let ``Simple Defer`` () =
    let expectedDirect =
        NameValueLookup.ofList [
           "testData", upcast NameValueLookup.ofList [
                "b", upcast "Banana"
            ]
        ]
    let expectedDeferred =
        NameValueLookup.ofList [
            "data", upcast NameValueLookup.ofList [ "a", upcast "Apple" ]
            "path", upcast ["Data"; "a"]
        ]
    let ast = parse"""{
        testData {
            a @defer
            b
        }
    }"""
    let result = sync <| executor.AsyncExecute(ast)
    match result with
    | Deferred(data, errors, deferred) -> 
        empty errors
        data.["data"] |> equals (upcast expectedDirect)
        deferred |> Observable.add(equals (upcast expectedDeferred))
    | _ -> fail "Expected Deferred GQLResponse"

[<Fact>]
let ``Partial Union Defer`` () = 
    let ast = parse """{
        testData {
            a
            union {
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
    let expectedDirect = 
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "a", upcast "Apple"
                "union", upcast NameValueLookup.ofList[
                    "id", upcast "1"
                ]
            ]
        ]
    let result = sync <| executor.AsyncExecute(ast)
    match result with
    | Deferred(data, errors, deferred) ->
        empty errors
        data.["data"] |> equals (upcast expectedDirect)
        // deferred |> Observable.add(printfn "Deferred: %A")
    | _ -> fail "Expected Deferred GQLRespnse"

[<Fact>]
let ``List Union Defer``() =
    let ast = parse """{
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
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "a", upcast "Apple"
                "list", upcast [
                    NameValueLookup.ofList [
                        "id", upcast "2"
                    ]
                    NameValueLookup.ofList [
                        "id", upcast "3"
                        "b", upcast 4
                    ]
                ]
            ]
        ]
    let result = sync <| executor.AsyncExecute(ast)
    match result with
    | Deferred(data, errors, deferred) ->
        empty errors
        data.["data"] |> equals (upcast expectedDirect)
        // deferred |> Observable.add(printfn "Deferred: %A")
    | _ -> fail "Expected Deferred GQLRespnse"

[<Fact>]
let ``Complex Defer`` () =

    let ast = parse"""{
        testData {
            a
            b
            union {
                ... on A {
                    id
                    a @defer
                }
                ... on B {
                    id
                    b @defer
                }
            }
        }
    }"""
    let expectedDirect =
        NameValueLookup.ofList [
            "testData", upcast NameValueLookup.ofList [
                "a", upcast "Apple"
                "b", upcast "Banana"
                "union", upcast NameValueLookup.ofList [ "id", upcast "1" ]
            ]
        ]
    let expectedDeferred =
        NameValueLookup.ofList [
            "data", upcast NameValueLookup.ofList [ "a", upcast "Union A" ]
            "path", upcast ["Data"; "union"; "a"]
        ]

    let result = sync <| executor.AsyncExecute(ast)
    match result with
    | Deferred(data, errors, deferred) -> 
        empty errors
        data.["data"] |> equals (upcast expectedDirect)
        deferred |> Observable.add(equals (upcast expectedDeferred))
    | _ -> fail "Expected Deferred GQLRespnse"