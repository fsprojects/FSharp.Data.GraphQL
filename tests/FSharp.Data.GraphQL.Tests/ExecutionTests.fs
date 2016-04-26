/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.ExecutionTests

open System
open Xunit
open FsCheck
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution

type TestSubject = {
    a: string
    b: string
    c: string
    d: string
    e: string
    f: string
    deep: DeepTestSubject
    pic: int option -> string
    promise: Async<TestSubject>
}
and DeepTestSubject = {
    a: string
    b: string
    c: string list
}

[<Fact>]
let ``Execution handles basic tasks: executes arbitrary code`` () =
    let rec data = 
        {
            a = "Apple"
            b = "Banana"
            c = "Cookie"
            d = "Donut"
            e = "Egg"
            f = "Fish"
            pic = (fun size -> "Pic of size: " + (if size.IsSome then size.Value else 50).ToString())
            promise = async { return data }
            deep = deep
        }
    and deep = 
        {
            a = "Already Been Done"
            b = "Boring"
            c = ["Contrived"; null; "Confusing"]
        }

    let ast = parse """query Example($size: Int) {
          a,
          b,
          x: c
          ...c
          f
          ...on DataType {
            pic(size: $size)
            promise {
              a
            }
          }
          deep {
            a
            b
            c
          }
        }

        fragment c on DataType {
          d
          e
        }"""

    let (expected: Map<string, obj>) = 
        Map.ofList [
            "a", upcast "Apple" 
            "b", upcast "Banana"
            "d", upcast "Donut"
            "deep", upcast Map.ofList [
               "a", "Already Been Done" :> obj
               "b", upcast "Boring"
               "c", upcast ["Contrived" :> obj; null; upcast "Confusing"]
            ] 
            "e", upcast "Egg"
            "f", upcast "Fish"
            "pic", upcast "Pic of size: 100"
            "promise", null
            "x", upcast "Cookie"
        ]

    let DeepDataType = objdef "DeepDataType" [
        field "a" String (fun dt -> dt.a)
        field "b" String (fun dt -> dt.b)
        field "c" (ListOf String) (fun dt -> dt.c)
    ]
    let DataType = objdef "DataType" [
        field "a" String (fun (dt: TestSubject) -> dt.a);
        field "b" String (fun (dt: TestSubject) -> dt.b);
        field "c" String (fun (dt: TestSubject) -> dt.c);
        field "d" String (fun (dt: TestSubject) -> dt.d);
        field "e" String (fun (dt: TestSubject) -> dt.e);
        field "f" String (fun (dt: TestSubject) -> dt.f);
        fieldA "pic" String [arg "size" Int] (fun ctx dt -> dt.pic(ctx.Arg("size")));
        field "deep" DeepDataType (fun (dt: TestSubject) -> dt.deep);
    ]
    let schema = Schema(DataType)
    let result = schema.Execute(ast, data, variables = Map.ofList [ "size", upcast 100 ], operationName = "Example")
    equals expected result.Data.Value

type TestThing = { Thing: string }

[<Fact>]
let ``Execution handles basic tasks: merges parallel fragments`` () = 
    let ast = parse """{ a, ...FragOne, ...FragTwo }

      fragment FragOne on Type {
        b
        deep { b, deeper: deep { b } }
      }

      fragment FragTwo on Type {
        c
        deep { c, deeper: deep { c } }
      }"""

    let mutable Type = objdef "Type" [
        field "a" String (fun () -> "Apple")
        field "b" String (fun () -> "Banana")
        field "c" String (fun () -> "Cherry")
    ]
    //TODO: API fix - self referencing data type
    let (Object x) = Type
    x.AddField (field "deep" Type id)

    let schema = Schema(Type)
    let expected: Map<string, obj> = Map.ofList [
        "a", upcast "Apple"
        "b", upcast "Banana"
        "c", upcast "Cherry"
        "deep", upcast Map.ofList [
            "b", "Banana" :> obj
            "c", upcast "Cherry"
            "deeper", upcast Map.ofList [
                "b", "Banana" :> obj
                "c", upcast "Cherry"
            ]
        ]
    ]
    let result = schema.Execute(ast, obj())
    equals expected result.Data.Value
    
[<Fact>]
let ``Execution handles basic tasks: threads root value context correctly`` () = 
    let query = "query Example { a }"
    let data = { Thing = "thing" }
    let mutable resolved = {Thing = ""};
    let Thing = objdef "Type" [
        field "a" String (fun r -> resolved <- data)
    ]
    let result = Schema(Thing).Execute(parse query, data)
    equals "thing" resolved.Thing
    
[<Fact>]
let ``Execution handles basic tasks: correctly threads arguments`` () =
    let query = """query Example {
        b(numArg: 123, stringArg: "foo")
      }"""
    let mutable numArg = None;
    let mutable stringArg = None;
    let Type = objdef "Type" [
        fieldA "b" String [arg "numArg" Int; arg "stringArg" String] 
            (fun ctx _ -> 
                numArg <- ctx.Arg("numArg")
                stringArg <- ctx.Arg("stringArg")) 
    ]

    let result = Schema(Type).Execute(parse query, ())
    equals (Some 123) numArg
    equals (Some "foo") stringArg
    
type InlineTest = { A: string }

[<Fact>]
let ``Execution handles basic tasks: uses the inline operation if no operation name is provided`` () =
    let schema =  Schema(objdef "Type" [
        field "a" String (fun x -> x.A)
    ])
    let result = schema.Execute(parse "{ a }", { A = "b" })
    equals (Map.ofList ["a", "b" :> obj]) result.Data.Value
    
[<Fact>]
let ``Execution handles basic tasks: uses the only operation if no operation name is provided`` () =
    let schema =  Schema(objdef "Type" [
        field "a" String (fun x -> x.A)
    ])
    let result = schema.Execute(parse "query Example { a }", { A = "b" })
    equals (Map.ofList ["a", "b" :> obj]) result.Data.Value
    
[<Fact>]
let ``Execution handles basic tasks: uses the named operation if operation name is provided`` () =
    let schema =  Schema(objdef "Type" [
        field "a" String (fun x -> x.A)
    ])
    let query = "query Example { first: a } query OtherExample { second: a }"
    let result = schema.Execute(parse query, { A = "b" }, operationName = "OtherExample")
    equals (Map.ofList ["second", "b" :> obj]) result.Data.Value
