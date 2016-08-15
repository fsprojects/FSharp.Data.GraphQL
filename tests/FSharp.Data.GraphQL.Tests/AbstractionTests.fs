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

type IPet =
    interface
        abstract Name : string
    end 

type Dog = 
    { Name: string; Woofs: bool }
    interface IPet with
        member x.Name = x.Name

type Cat = 
    { Name: string; Meows: bool }
    interface IPet with
        member x.Name = x.Name

type Human = { Name: string; }

type Pet =
    | DogCase of Dog
    | CatCase of Cat

let resolvePet = function
    | DogCase d -> box d
    | CatCase c -> upcast c

[<Fact>]
let ``Execute handles execution of abstract types: isTypeOf is used to resolve runtime type for Interface`` () = 
    let PetType = Define.Interface("Pet", fun () -> [ Define.Field("name", String) ])
    let DogType = Define.Object<Dog>(
        name = "Dog", 
        isTypeOf = is<Dog>,
        interfaces = [ PetType ],
        fields = [
            Define.Field("name", String)
            Define.Field("woofs", Boolean)
        ])
    let CatType = Define.Object<Cat>(
        name = "Cat", 
        isTypeOf = is<Cat>,
        interfaces = [ PetType ],
        fields = [
            Define.Field("name", String)
            Define.Field("meows", Boolean)
        ])
    let schema = Schema(
        query = Define.Object("Query", fun () -> [
            Define.Field("pets", ListOf PetType, fun _ _ -> [ { Name = "Odie"; Woofs = true } :> IPet ; upcast { Name = "Garfield"; Meows = false } ])
        ]), 
        config = { SchemaConfig.Default with Types = [CatType; DogType] })
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
    let expected = NameValueLookup.ofList [
        "pets", upcast [
            NameValueLookup.ofList [
                "name", "Odie" :> obj
                "woofs", upcast true ] :> obj
            upcast NameValueLookup.ofList [
                "name", "Garfield" :> obj
                "meows", upcast false]]]
    noErrors result
    result.["data"] |> equals (upcast expected)
    
[<Fact>]
let ``Execute handles execution of abstract types: isTypeOf is used to resolve runtime type for Union`` () = 
    let DogType = Define.Object<Dog>(
        name = "Dog", 
        isTypeOf = is<Dog>,
        fields = [
            Define.Field("name", String)
            Define.Field("woofs", Boolean)
        ])
    let CatType = Define.Object<Cat>(
        name = "Cat", 
        isTypeOf = is<Cat>,
        fields = [
            Define.Field("name", String)
            Define.Field("meows", Boolean)
        ])
    let PetType = Define.Union("Pet", [ DogType; CatType ], resolvePet)
    let schema = Schema(
        query = Define.Object("Query", fun () -> [
            Define.Field("pets", ListOf PetType, fun _ _ -> [ DogCase { Name = "Odie"; Woofs = true }; CatCase { Name = "Garfield"; Meows = false } ])
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
    let expected = NameValueLookup.ofList [
        "pets", upcast [
            NameValueLookup.ofList [
                "name", "Odie" :> obj
                "woofs", upcast true ] :> obj
            upcast NameValueLookup.ofList [
                "name", "Garfield" :> obj
                "meows", upcast false]]]
    noErrors result
    result.["data"] |> equals (upcast expected)
