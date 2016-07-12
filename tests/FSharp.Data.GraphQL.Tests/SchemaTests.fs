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
    let MovableType = Define.Interface("Movable", [
        Define.Field("speed", Int)
    ])
    let Movable2Type = Define.Interface("Movable2", [
        Define.Field("speed", Int)
        Define.Field("acceleration", Int)
    ])
    let PersonType = Define.Object(
        name = "Person",
        interfaces = [ MovableType; Movable2Type ],
        fields = [
            Define.Field("name", String)
            Define.Field("speed", Int)
            Define.Field("acceleration", Int) ])
    equals [ MovableType :> InterfaceDef; upcast Movable2Type ] (PersonType.Implements |> Array.toList )
    equals [ Define.Field("name", String) :> FieldDef; upcast Define.Field("speed", Int); upcast Define.Field("acceleration", Int) ] (( PersonType :> ObjectDef).Fields |> Map.toList |> List.map snd)