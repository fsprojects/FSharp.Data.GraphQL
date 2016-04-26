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
    let person = Define.ObjectType("Person", [
        Define.Field("name", String)
    ])
    let movable = Define.Interface("Movable", [
        Define.Field("speed", Int)
    ])
    let movable2 = Define.Interface("Movable2", [
        Define.Field("speed", Int)
        Define.Field("acceleration", Int)
    ])
    let objectType = implements person [ movable; movable2 ]
    let expected = Define.ObjectType("Person", [
        Define.Field("name", String)
        Define.Field("speed", Int)
        Define.Field("acceleration", Int)
    ], interfaces = [movable; movable2])
    equals expected objectType


[<Fact>]
let ``Object type should not be able to merge fields with matching names but different types from different interfaces`` () = 
    let person = Define.ObjectType("Person", [
        Define.Field("name", String)
    ])
    let movable = Define.Interface("Movable", [
        Define.Field("speed", String)
    ])
    let movable2 = Define.Interface("Movable2", [
        Define.Field("speed", Int)
        Define.Field("acceleration", Int)
    ])

    (fun () -> implements person [ movable; movable2 ] |> ignore)
    |> throws<GraphQLException>