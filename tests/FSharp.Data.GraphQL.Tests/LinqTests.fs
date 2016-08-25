/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
module FSharp.Data.GraphQL.Tests.LinqTests

open Xunit
open FsCheck
open System
open System.Linq
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Linq
open FSharp.Data.GraphQL.Execution

type NestedDataType = { D: string }
type DataType = { ID: int; A: string; B: string; C: NestedDataType; E: NestedDataType list }

let NestedType = Define.Object("NestedType", [
    Define.Field("d", String, fun _ x -> x.D)])

let DataType = Define.Object<DataType>("DataType", [
    Define.Field("id", ID, fun _ x -> x.ID)
    Define.AutoField("a", String)
    Define.Field("b", String, fun _ x -> x.B)
    Define.Field("c", NestedType, fun _ x -> x.C)
    Define.Field("e", ListOf NestedType, fun _ x -> x.E) ])

let data = [
    { ID = 1; 
      A = "aa"; 
      B = "bb"; 
      C = { D = "dd"}; 
      E = [ 
          { D = "dd2" }; 
          { D = "dd3" } ]}
          
    { ID = 2; 
      A = "aa2"; 
      B = "bb2"; 
      C = { D = "dd2"}; 
      E = []}
]

let undefined<'t> = Unchecked.defaultof<'t>

let resolveRoot ctx () =
    let info = ctx.ExecutionPlan
    let queryable = data.AsQueryable()
    let result = info.ToLinq(queryable) |> Seq.toList
    result

let schema = Schema(Define.Object("RootQuery", [
    Define.Field("root", ListOf DataType, "", [ Define.Input("id", Nullable ID<int>) ], resolveRoot)]))

[<Fact>]
let ``LINQ interpreter works with auto-fields`` () = 
    let plan = schema.CreateExecutionPlan """
    query Example {
        root {
            a
        }
    }
    """
    let info = plan.["root"]
    let result = info.ToLinq(data.AsQueryable()).First()
    result.A |> equals "aa"
    result.B |> equals undefined
    result.C |> equals undefined 
    result.E |> equals undefined

[<Fact>]
let ``LINQ interpreter works with fields with defined resolvers`` () = 
    let plan = schema.CreateExecutionPlan """
    query Example {
        root {
            b
        }
    }
    """
    let info = plan.["root"]
    let result = info.ToLinq(data.AsQueryable()).First()
    result.A |> equals undefined
    result.B |> equals "bb"; 
    result.C |> equals undefined 
    result.E |> equals undefined 

[<Fact>]
let ``LINQ interpreter works with fields referring to nested property resolver`` () = 
    let plan = schema.CreateExecutionPlan """
    query Example {
        root {
            c { d }
        }
    }
    """
    let info = plan.["root"]
    let result = info.ToLinq(data.AsQueryable()).First()
    result.A |> equals undefined
    result.B |> equals undefined 
    result.C |> equals { D = "dd" } 
    result.E |> equals undefined 

[<Fact>]
let ``LINQ interpreter works with nested collections`` () = 
    let plan = schema.CreateExecutionPlan """
    query Example {
        root {
            e { d }
        }
    }
    """
    let info = plan.["root"]
    let result = info.ToLinq(data.AsQueryable()).First()
    result.A |> equals undefined
    result.B |> equals undefined 
    result.C |> equals undefined 
    result.E |> equals [
            { D = "dd2" }
            { D = "dd3" } ]

[<Fact>]
let ``LINQ interpreter works with id arg`` () = 
    let query = """
        query Example {
            root(id: 2) {
                id
                a
            }
        }
        """
    let expected = NameValueLookup.ofList [
        "root", upcast [ 
            box <| NameValueLookup.ofList [
                "id", upcast 2
                "a", upcast "aa2" ]]]
    let actual = sync <| schema.AsyncExecute(query)
    noErrors actual
    actual.["data"] |> equals (upcast expected)
//    let plan = schema.CreateExecutionPlan """
//    query Example {
//        root(id: 2) {
//            id
//            a
//        }
//    }
//    """
//    let info = plan.["root"]
//    let result = info.ToLinq(data.AsQueryable()).First()
//    result.ID |> equals 2
//    result.A |> equals "aa2"
//    result.B |> equals undefined 
//    result.C |> equals undefined 
//    result.E |> equals undefined

[<Fact(Skip="TODO")>]
let ``LINQ interpreter works with first/after args`` () = ()

[<Fact(Skip="TODO")>]
let ``LINQ interpreter works with last/before args`` () = ()

[<Fact(Skip="TODO")>]
let ``LINQ interpreter works with Relay Connection`` () = ()