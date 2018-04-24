module FSharp.Data.GraphQL.Tests.Subject

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types

#nowarn "40"

type TestSubject = {
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
    let DataType =
        Define.Object<TestSubject>(
            name = "Data", 
            fieldsFn = fun () -> 
                [ Define.Field("a", String, resolve = fun _ d -> d.a)
                  Define.Field("b", String, resolve = fun _ d -> d.b)
                  Define.Field("union", UnionType, resolve = fun _ d -> d.union)
                  Define.Field("list", ListOf UnionType, resolve = fun _ d -> d.list) ])
    let data = {
           a = "Apple"
           b = "Banana"
           union = A {
               id = "1"
               a = "Union A"
           }
           list = 
            [ A { 
                   id = "2"
                   a = "Union A" 
               }; 
               B { 
                   id = "3"
                   b = 4
               } ]
       }
    let Query = 
        Define.Object<TestSubject>(
            name = "Query",
            fieldsFn = fun () -> [ Define.Field("testData", DataType, (fun _ _ -> data)) ] )
    let schema = Schema(Query)
    Executor(schema)