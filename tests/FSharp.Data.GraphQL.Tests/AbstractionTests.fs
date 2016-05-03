/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.AbstractionTests

open System
open Xunit
open FsCheck
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution

type Dog = { Name: string; Woofs: bool }
type Cat = { Name: string; Meows: bool }
type Human = { Name: string; }

[<Fact>]
let ``Execute handles execution of abstract types: isTypeOf is used to resolve runtime type for Interface`` () = 
    let PetType = Define.Interface("Pet", fun () -> [ Define.Field("name", String) ])
    let DogType = Define.Object(
        name = "Dog", 
        isTypeOf = is<Dog>,
        interfaces = [ PetType ],
        fields = fun () -> [
            Define.Field("name", String)
            Define.Field("woofs", Boolean)
        ])
    let CatType = Define.Object(
        name = "Cat", 
        isTypeOf = is<Cat>,
        interfaces = [ PetType ],
        fields = fun () -> [
            Define.Field("name", String)
            Define.Field("meows", Boolean)
        ])
    let schema = Schema(
        types = [CatType; DogType],
        query = Define.Object("Query", fun () -> [
            Define.Field("pets", ListOf PetType, resolve = fun _ _ -> [ { Name = "Odie"; Woofs = true } :> obj; upcast { Name = "Garfield"; Meows = false } ])
        ]))
    let query = """{
      pets {
        name
        ... on Dog {
          woofs
        }
        ... on Cat {
          meows
        }
      }
    }"""
    let result = sync <| schema.AsyncExecute(parse query)
    let expected: Map<string, obj> = Map.ofList [
        "pets", upcast [
            Map.ofList [
                "name", "Odie" :> obj
                "woofs", upcast true ] :> obj
            upcast Map.ofList [
                "name", "Garfield" :> obj
                "meows", upcast false]]]
    noErrors result
    equals expected result.Data.Value
    
[<Fact>]
let ``Execute handles execution of abstract types: isTypeOf is used to resolve runtime type for Union`` () = 
    let DogType = Define.Object(
        name = "Dog", 
        isTypeOf = is<Dog>,
        fields = fun () -> [
            Define.Field("name", String)
            Define.Field("woofs", Boolean)
        ])
    let CatType = Define.Object(
        name = "Cat", 
        isTypeOf = is<Cat>,
        fields = fun () -> [
            Define.Field("name", String)
            Define.Field("meows", Boolean)
        ])
    let PetType = Define.Union("Pet", [ DogType; CatType ])
    let schema = Schema(
        query = Define.Object("Query", fun () -> [
            Define.Field("pets", ListOf PetType, resolve = fun _ _ -> 
                [ { Name = "Odie"; Woofs = true } :> obj; upcast { Name = "Garfield"; Meows = false } ])
        ]))
    let query = """{
      pets {
        ... on Dog {
          name
          woofs
        }
        ... on Cat {
          name
          meows
        }
      }
    }"""
    let result = sync <| schema.AsyncExecute(parse query)
    let expected: Map<string, obj> = Map.ofList [
        "pets", upcast [
            Map.ofList [
                "name", "Odie" :> obj
                "woofs", upcast true ] :> obj
            upcast Map.ofList [
                "name", "Garfield" :> obj
                "meows", upcast false]]]
    noErrors result
    equals expected result.Data.Value
