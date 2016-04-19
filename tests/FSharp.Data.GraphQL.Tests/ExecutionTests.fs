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

let private sync = Async.RunSynchronously
let private field name typedef (resolve: 'a -> 'b) = Schema.Field(name = name, schema = typedef, resolve = resolve)
let private arg name typedef = Schema.Argument(name, typedef)
let private objdef name fields = Schema.ObjectType(name, fields)
let private (<??) opt other =
    match opt with
    | None -> Some other
    | _ -> opt


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
let ``Execution executes arbitrary code`` () =
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
            ("a", upcast "Apple"); 
            ("b", upcast "Banana"); 
            ("d", upcast "Donut");
            ("deep", upcast Map.ofList [
                ("a", "Already Been Done" :> obj); 
                ("b", upcast "Boring");
                ("c", upcast ["Contrived" :> obj; null; upcast "Confusing"])]); 
            ("e", upcast "Egg"); 
            ("f", upcast "Fish");
            ("pic", upcast "Pic of size: 100"); 
            ("promise", null); 
            ("x", upcast "Cookie")
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
        Schema.Field<TestSubject, string>("pic", String, arguments = [arg "size" Int], resolve = (fun (dt, args:Args) -> dt.pic(args.Arg("size"))));
        field "deep" DeepDataType (fun (dt: TestSubject) -> dt.deep);
    ]
    let schema = Schema(DataType)
    let result = schema.Execute(ast, data, variables = Map.ofList [ "size", upcast 100 ], operationName = "Example")
    equals result.Data.Value expected
