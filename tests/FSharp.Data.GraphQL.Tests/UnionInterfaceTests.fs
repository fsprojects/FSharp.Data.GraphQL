/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.UnionInterfaceTests

open System
open Xunit
open FsCheck
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution

type Dog = { Name: string; Barks: bool }
type Cat = { Name: string; Meows: bool }
type Pet =
    | Cat of Cat
    | Dog of Dog
type Person = { Name: string; Pets: Pet list; Friends: obj list }

let NamedType = Define.Interface(
    name = "Named",
    fields = [ Define.Field("name", String) ])

let DogType = Define.Object(
    name = "Dog",
    isTypeOf = is<Dog>,
    interfaces = [ NamedType ],
    fields = [
        Define.Field("name", String)
        Define.Field("barks", Boolean)
    ])
    
let CatType = Define.Object(
    name = "Cat",
    isTypeOf = is<Cat>,
    interfaces = [ NamedType ],
    fields = [
        Define.Field("name", String)
        Define.Field("meows", Boolean)
    ])

let PetType = Define.Union(
    name = "Pet",
    options = [ CatType; DogType ],
    resolveType = fun pet ->
        match pet with
        | :? Cat -> CatType
        | :? Dog -> DogType)

let PersonType = Define.Object(
    name = "Person",
    isTypeOf = is<Person>,
    interfaces = [ NamedType ],
    fields = [
        Define.Field("name", String)
        Define.Field("pets", ListOf PetType, 
            resolve = fun _ person -> person.Pets |> List.map (fun pet -> match pet with Dog d -> box d | Cat c -> box c))
        Define.Field("friends", ListOf NamedType)
    ])

let schema = Schema(query = PersonType, config = { SchemaConfig.Default with Types = [ PetType ] })

let garfield = { Name = "Garfield"; Meows = false }
let odie = { Name = "Odie"; Barks = true }
let liz = { Name = "Liz"; Pets = []; Friends = [] }
let john = { Name = "John"; Pets = [ Cat garfield; Dog odie ]; Friends = [ liz; odie ] }

[<Fact>]
let ``Execute can introspect on union and intersection types`` () =
    let ast = parse """{
        Named: __type(name: "Named") {
          kind
          name
          fields { name }
          interfaces { name }
          possibleTypes { name }
          enumValues { name }
          inputFields { name }
        }
        Pet: __type(name: "Pet") {
          kind
          name
          fields { name }
          interfaces { name }
          possibleTypes { name }
          enumValues { name }
          inputFields { name }
        }
      }"""
    let actual = sync <| schema.AsyncExecute(ast)
    let expected: Map<string, obj> = Map.ofList [
        "Named", upcast Map.ofList [
            "kind", box "INTERFACE"
            "name", upcast "Named"
            "fields", upcast [
                box (Map.ofList [ "name", box "name"])]
            "interfaces", null
            "possibleTypes", upcast [
                box (Map.ofList ["name", box "Person"])
                upcast Map.ofList ["name", box "Dog"]
                upcast Map.ofList ["name", box "Cat"]]
            "enumValues", null
            "inputFields", null]
        "Pet", upcast Map.ofList [
            "kind", box "UNION"
            "name", upcast "Pet"
            "fields", null
            "interfaces", null
            "possibleTypes", upcast [
                box (Map.ofList ["name", box "Cat"])
                upcast Map.ofList ["name", box "Dog"]]
            "enumValues", null
            "inputFields", null]]
    noErrors actual
    equals expected actual.Data.Value

[<Fact>]
let ``Executes union types`` () =
    // NOTE: This is an *invalid* query, but it should be an *executable* query.
    let ast = parse """{
        __typename
        name
        pets {
          __typename
          name
          barks
          meows
        }
      }"""
    let actual = sync <| schema.AsyncExecute(ast, john)
    let expected: Map<string, obj> = Map.ofList [
        "__typename", box "Person"
        "name", upcast "John"
        "pets", upcast [
            box <| Map.ofList [
                "__typename", box "Cat"
                "name", upcast "Garfield"
                "barks", null
                "meows", upcast false]
            upcast Map.ofList [
                "__typename", box "Dog"
                "name", upcast "Odie"
                "barks", upcast true
                "meows", null]]]
    noErrors actual
    equals expected actual.Data.Value
    
[<Fact>]
let ``Executes union types with inline fragments`` () =
    // This is the valid version of the query in the above test.
    let ast = parse """{
        __typename
        name
        pets {
          __typename
          ... on Dog {
            name
            barks
          }
          ... on Cat {
            name
            meows
          }
        }
      }"""
    let actual = sync <| schema.AsyncExecute(ast, john)
    let expected: Map<string, obj> = Map.ofList [
        "__typename", box "Person"
        "name", upcast "John"
        "pets", upcast [
            box <| Map.ofList [
                "__typename", box "Cat"
                "name", upcast "Garfield"
                "meows", upcast false]
            upcast Map.ofList [
                "__typename", box "Dog"
                "name", upcast "Odie"
                "barks", upcast true]]]
    noErrors actual
    equals expected actual.Data.Value
    
[<Fact>]
let ``Executes interface types`` () =
    // NOTE: This is an *invalid* query, but it should be an *executable* query.
    let ast = parse """{
        __typename
        name
        friends {
          __typename
          name
          barks
          meows
        }
      }"""
    let actual = sync <| schema.AsyncExecute(ast, john)
    let expected: Map<string, obj> = Map.ofList [
        "__typename", box "Person"
        "name", upcast "John"
        "friends", upcast [
            box <| Map.ofList [
                "__typename", box "Person"
                "name", upcast "Liz"
                "barks", null
                "meows", null]
            upcast Map.ofList [
                "__typename", box "Dog"
                "name", upcast "Odie"
                "barks", upcast true
                "meows", null]]]
    noErrors actual
    equals expected actual.Data.Value
    
[<Fact>]
let ``Executes interface types with inline fragments`` () =
    // This is the valid version of the query in the above test.
    let ast = parse """{
        __typename
        name
        friends {
          __typename
          name
          ... on Dog {
            barks
          }
          ... on Cat {
            meows
          }
        }
      }"""
    let actual = sync <| schema.AsyncExecute(ast, john)
    let expected: Map<string, obj> = Map.ofList [
        "__typename", box "Person"
        "name", upcast "John"
        "friends", upcast [
            box <| Map.ofList [
                "__typename", box "Person"
                "name", upcast "Liz"]
            upcast Map.ofList [
                "__typename", box "Dog"
                "name", upcast "Odie"
                "barks", upcast true]]]
    noErrors actual
    equals expected actual.Data.Value

[<Fact>]
let ``Execute allows fragment conditions to be abstract types`` () =
    let ast = parse """{
        __typename
        name
        pets { ...PetFields }
        friends { ...FriendFields }
      }

      fragment PetFields on Pet {
        __typename
        ... on Dog {
          name
          barks
        }
        ... on Cat {
          name
          meows
        }
      }

      fragment FriendFields on Named {
        __typename
        name
        ... on Dog {
          barks
        }
        ... on Cat {
          meows
        }
      }"""
    let actual = sync <| schema.AsyncExecute(ast, john)
    let expected: Map<string, obj> = Map.ofList [
        "__typename", box "Person"
        "name", upcast "John"
        "pets", upcast [
            box <| Map.ofList [
                "__typename", box "Cat"
                "name", upcast "Garfield"
                "meows", upcast false]
            upcast Map.ofList [
                "__typename", box "Dog"
                "name", upcast "Odie"
                "barks", upcast true]]
        "friends", upcast [
            box <| Map.ofList [
                "__typename", box "Person"
                "name", upcast "Liz"]
            upcast Map.ofList [
                "__typename", box "Dog"
                "name", upcast "Odie"
                "barks", upcast true]]]
    noErrors actual
    equals expected actual.Data.Value
    