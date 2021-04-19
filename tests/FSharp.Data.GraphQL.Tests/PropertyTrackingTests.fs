/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
module FSharp.Data.GraphQL.Tests.PropertyTrackingTests

open System
open System.Linq
open System.Linq.Expressions
open FSharp.Linq
open Microsoft.FSharp.Quotations
open FSharp.Data.GraphQL.Linq
open Xunit

type DeepestType =
    { Z : string }

type DeepType =
    { Y : DeepestType }

type OuterType =
    { X : DeepType
      FirstName : string
      LastName : string
      NestedCollection : DeepType list }
//
//let private complex name nodes = Direct(name, Set.ofList nodes)
//let private leaf name = Direct(name, Set.empty)
//let private collection name nodes = Collection(name, Set.ofList nodes)
//
//let private test expected (Patterns.Lambda(arg, expr))=
//    let actual = tracker arg expr
//    actual |> equals expected
//
//[<Fact>]
//let ``Collect getters from properties``() = <@ fun o -> o.X @> |> test (complex "o" [ leaf "X"])
//
//[<Fact>]
//let ``Collect getters from nested properties``() = <@ fun o -> o.X.Y.Z @> |> test (complex "o" [ (complex "X" [ (complex "Y" [ leaf "Z" ])])])
//
//[<Fact>]
//let ``Collect getters from mutliple properties``() = <@ fun o -> o.FirstName + " " + o.LastName @> |> test (complex "o" [ leaf "FirstName"; leaf "LastName" ])
//
//[<Fact>]
//let ``Collect getters from function calls``() = <@ fun o -> string o.X @> |> test (complex "o" [ leaf "X"])
//
//[<Fact>]
//let ``Collect getters from type coercions``() =
//    let e : Expr<OuterType->obj> = <@ fun o -> upcast o.X @>
//    e |> test (complex "o" [ leaf "X"])
//
//[<Fact>]
//let ``Collect getters from for loops``() =
//    <@ fun o ->
//        for i=1 to o.NestedCollection.Length do
//            ()
//    @> |> test (complex "o" [ (collection "NestedCollection" [ leaf "Length" ])])
//
//[<Fact>]
//let ``Collect getters from if-else expressions``() =
//    <@ fun o ->
//        if o.FirstName = "John"
//        then o.FirstName
//        else o.LastName
//    @> |> test (complex "o" [ leaf "FirstName"; leaf "LastName" ])
//
//[<Fact>]
//let ``Collect getters from let statements``() =
//    <@ fun o ->
//        let x = o.X
//        x.Y
//    @> |> test (complex "o" [ (complex "X" [ leaf "Y" ])])
//
//[<Fact>]
//let ``Collect getters from mutable assignments``() =
//    <@ fun o ->
//        let mutable x = o.FirstName
//        x <- o.LastName
//    @> |> test (complex "o" [ leaf "FirstName"; leaf "LastName" ])
//
//[<Fact>]
//let ``Collect getters from recursive let statements``() =
//    <@ fun o ->
//        let rec loop n a =
//            match n with
//            | 0 -> a.X
//            | _ -> loop (n-1) a
//        loop 4 o
//    @> |> test (complex "o" [ leaf "X"])
//
//[<Fact>]
//let ``Collect getters from new array``() =
//    <@ fun o ->  [| o.X |] @> |> test (complex "o" [ leaf "X"])
//
//[<Fact>]
//let ``Collect getters from new delegates``() =
//    <@ fun o ->
//        let a y = o.FirstName + y
//        a
//    @> |> test (complex "o" [ leaf "FirstName"])
//
//type TestRecord = { Contained: string }
//
//[<Fact>]
//let ``Collect getters from new records``() =
//    <@ fun o -> { Contained = o.FirstName } @> |> test (complex "o" [ leaf "FirstName"])
//
//[<Fact>]
//let ``Collect getters from tuples``() =
//    <@ fun o -> (o.FirstName, o.LastName) @> |> test (complex "o" [ leaf "FirstName"; leaf "LastName" ])
//
//type TestDU = TestDU of string * string
//
//[<Fact>]
//let ``Collect getters from discriminated unions``() =
//    <@ fun o -> TestDU(o.FirstName, o.LastName) @> |> test (complex "o" [ leaf "FirstName"; leaf "LastName" ])
//
//[<Fact>]
//let ``Collect getters from typed sub-quotes``() =
//    <@ fun o -> <@ o.FirstName + "x" @> @> |> test (complex "o" [ leaf "FirstName"])
//
//[<Fact>]
//let ``Collect getters from untyped sub-quotes``() =
//    <@ fun o -> <@@ o.FirstName + "x" @@> @> |> test (complex "o" [ leaf "FirstName"])
//
//[<Fact>]
//let ``Collect getters from sequential expressions``() =
//    <@ fun o ->
//        o.FirstName
//        o.LastName
//    @> |> test (complex "o" [ leaf "FirstName"; leaf "LastName" ])
//
//[<Fact>]
//let ``Collect getters from try-finally``() =
//    <@ fun o ->
//        try
//            o.FirstName
//        finally
//            o.LastName
//    @> |> test (complex "o" [ leaf "FirstName"; leaf "LastName" ])
//
//[<Fact>]
//let ``Collect getters from try-with``() =
//    <@ fun o ->
//        try
//            o.FirstName
//        with
//        | e -> o.LastName + e.Message
//    @> |> test (complex "o" [ leaf "FirstName"; leaf "LastName" ])
//
//[<Fact>]
//let ``Collect getters from pattern matches``() =
//    <@ fun o ->
//        match o.X with
//        | { Y = y } when o.FirstName = "" -> y
//        | { Y = y } -> y
//    @> |> test (complex "o" [ (complex "X" [ leaf "Y" ]); leaf "FirstName"; ])
//
//[<Fact>]
//let ``Collect getters from while loops``() =
//    <@ fun o ->
//        while o.FirstName = "" do
//            o.LastName
//    @> |> test (complex "o" [ leaf "FirstName"; leaf "LastName" ])
//
//[<Fact>]
//let ``Collect getters from foreach loops``() =
//    <@ fun o ->
//        for i in o.NestedCollection do
//            let x = i.Y
//            ()
//    @> |> test (complex "o" [ (collection "NestedCollection" [ leaf "Y" ])])
//
//[<Fact>]
//let ``Collect getters from repinned field``() =
//    <@ fun o ->
//        let x = o.X
//        let y = x.Y
//        y
//    @> |> test (complex "o" [ (complex "X" [ leaf "Y" ])])
//
//[<Fact>]
//let ``Collect getters from multiple repinned fields``() =
//    <@ fun o ->
//        let x1 = o.X
//        let x2 = o.X
//        let y = x1.Y
//        let z = x2.Y.Z
//        y
//    @> |> test (complex "o" [ (complex "X" [ (complex "Y" [ leaf "Z" ])])])
//
//type OmittedType = { X: int; Y: string }
//
//[<Fact>]
//let ``Doesn't collect getters of objects not being used from root``() =
//    <@ fun o ->
//        let omitted = { X = 1; Y = o.FirstName }
//        let x = omitted.X
//        x
//    @> |> test (complex "o" [ leaf "FirstName" ])
