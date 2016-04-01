/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.SchemaTests

open System
open Xunit
open FsCheck
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types

[<Fact>]
let ``Object type should be able to merge fields with matching signatures from different interfaces`` () = 
    let person = Schema.ObjectType("Person", [
        Schema.Field("name", String)
    ])
    let movable = Schema.Interface("Movable", [
        Schema.Field("speed", Int)
    ])
    let movable2 = Schema.Interface("Movable2", [
        Schema.Field("speed", Int)
        Schema.Field("acceleration", Int)
    ])
    let objectType = implements person [ movable; movable2 ]
    let expected = Schema.ObjectType("Person", [
        Schema.Field("name", String)
        Schema.Field("speed", Int)
        Schema.Field("acceleration", Int)
    ], interfaces = [movable; movable2])
    equals expected objectType


[<Fact>]
let ``Object type should not be able to merge fields with matching names but different types from different interfaces`` () = 
    let person = Schema.ObjectType("Person", [
        Schema.Field("name", String)
    ])
    let movable = Schema.Interface("Movable", [
        Schema.Field("speed", String)
    ])
    let movable2 = Schema.Interface("Movable2", [
        Schema.Field("speed", Int)
        Schema.Field("acceleration", Int)
    ])

    (fun () -> implements person [ movable; movable2 ] |> ignore)
    |> throws<TypeViolationException>