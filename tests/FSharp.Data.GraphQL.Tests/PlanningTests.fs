/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
module FSharp.Data.GraphQL.Tests.PlanningTests

#nowarn "25"

open System
open Xunit
open FsCheck
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Planning
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Client.Serialization

type Person = 
    { firstName : string
      lastName : string
      age : int }

type Animal = 
    { name : string
      species : string }

type Named = 
    | Animal of Animal
    | Person of Person

let people = 
    [ { firstName = "John"
        lastName = "Doe"
        age = 21 } ]

let animals = 
    [ { name = "Max"
        species = "Dog" } ]

let rec Person = 
    Define.Object
        (name = "Person", 
         fields = [ Define.Field("firstName", String, fun _ person -> person.firstName)
                    Define.Field("lastName", String, fun _ person -> person.lastName)
                    Define.Field("age", Int, fun _ person -> person.age)
                    Define.Field("name", String, fun _ person -> person.firstName + " " + person.lastName) ], 
         interfaces = [ INamed ])

and Animal = 
    Define.Object(name = "Animal", 
                  fields = [ Define.Field("name", String, fun _ animal -> animal.name)
                             Define.Field("species", String, fun _ animal -> animal.species) ], interfaces = [ INamed ])

and INamed = Define.Interface<obj>("INamed", [ Define.Field("name", String) ])

and UNamed = 
    Define.Union("UNamed", [ Person; Animal ], 
                 function 
                 | Animal a -> box a
                 | Person p -> upcast p)

[<Fact>]
let ``Planning should work for a simple case``() = 
    let schema = Schema(Person)
    let query = """{
        firstName
        lastName
        age
    }"""
    let plan = schema.CreateExecutionPlan(query)
    plan.RootDef |> equals (upcast Person)
    equals 3 plan.Fields.Length
    plan.Fields
    |> List.map (fun info -> (info.Identifier, info.ParentDef, info.Definition.Type))
    |> equals [ ("firstName", upcast Person, upcast String)
                ("lastName", upcast Person, upcast String)
                ("age", upcast Person, upcast Int) ]

[<Fact>]
let ``Planning should work with fragments``() = 
    let schema = Schema(Person)
    let query = """query Example {
        ...named
        age
    }
    fragment named on Person {
        firstName
        lastName
    }"""
    let plan = schema.CreateExecutionPlan(query)
    plan.RootDef |> equals (upcast Person) 
    equals 3 plan.Fields.Length
    plan.Fields
    |> List.map (fun info -> (info.Identifier, info.ParentDef, info.Definition.Type))
    |> equals [ ("firstName", upcast Person, upcast String)
                ("lastName", upcast Person, upcast String)
                ("age", upcast Person, upcast Int) ]

[<Fact>]
let ``Planning should work with parallel fragments``() = 
    let schema = Schema(Person)
    let query = """query Example {
        ...fnamed
        ...lnamed
        age
    }
    fragment fnamed on Person {
        firstName
    }
    fragment lnamed on Person {
        lastName
    }
    """
    let plan = schema.CreateExecutionPlan(query)
    plan.RootDef |> equals (upcast Person) 
    equals 3 plan.Fields.Length
    plan.Fields
    |> List.map (fun info -> (info.Identifier, info.ParentDef, info.Definition.Type))
    |> equals [ ("firstName", upcast Person, upcast String)
                ("lastName", upcast Person, upcast String)
                ("age", upcast Person, upcast Int) ]

[<Fact>]
let ``Planning should work with lists``() = 
    let Query = Define.Object("Query", [ Define.Field("people", ListOf Person, fun _ () -> people) ])
    let schema = Schema(Query)
    let query = """{
        people {
            firstName
            lastName
        }
    }"""
    let plan = schema.CreateExecutionPlan(query)
    equals 1 plan.Fields.Length
    let listInfo = plan.Fields.Head
    let (ResolveCollection(info)) = listInfo.Kind
    let (SelectFields(innerFields)) = info.Kind
    equals Person (downcast info.ParentDef)
    equals 2 innerFields.Length
    innerFields
    |> List.map (fun i -> (i.Identifier, i.ParentDef, i.Definition.Type))
    |> equals [ ("firstName", upcast Person, upcast String)
                ("lastName", upcast Person, upcast String) ]

[<Fact>]
let ``Planning should work with interfaces``() = 
    let Query = Define.Object("Query", [ Define.Field("names", ListOf INamed, fun _ () -> []) ])
    let schema = Schema(query = Query, config = { SchemaConfig.Default with Types = [ Person; Animal ] })
    let query = """query Example {
        names {
            name
            ... on Animal {
                species
            }
            ...ageFragment
        }
    }
    fragment ageFragment on Person {
        age
    }"""
    let plan = schema.CreateExecutionPlan(query)
    equals 1 plan.Fields.Length
    let listInfo = plan.Fields.Head
    let (ResolveCollection(info)) = listInfo.Kind
    let (ResolveAbstraction(innerFields)) = info.Kind
    equals INamed (downcast info.ParentDef)
    innerFields
    |> Map.map (fun typeName fields -> fields |> List.map (fun i -> (i.Identifier, i.ParentDef, i.Definition.Type)))
    |> equals (Map.ofList [ "Person", 
                            [ ("name", upcast INamed, upcast String)
                              ("age", upcast INamed, upcast Int) ]
                            "Animal", 
                            [ ("name", upcast INamed, upcast String)
                              ("species", upcast INamed, upcast String) ] ])

[<Fact>]
let ``Planning should work with unions``() = 
    let Query = Define.Object("Query", [ Define.Field("names", ListOf UNamed, fun _ () -> []) ])
    let schema = Schema(Query)
    let query = """query Example {
        names {
            ... on Animal {
                name
                species
            }
            ... on Person {
                name
                age
            }
        }
    }"""
    let plan = schema.CreateExecutionPlan(query)
    equals 1 plan.Fields.Length
    let listInfo = plan.Fields.Head
    let (ResolveCollection(info)) = listInfo.Kind
    let (ResolveAbstraction(innerFields)) = info.Kind
    equals UNamed (downcast info.ParentDef)
    innerFields
    |> Map.map (fun typeName fields -> fields |> List.map (fun i -> (i.Identifier, i.ParentDef, i.Definition.Type)))
    |> equals (Map.ofList [ "Animal", 
                            [ ("name", upcast UNamed, upcast String)
                              ("species", upcast UNamed, upcast String) ]
                            "Person", 
                            [ ("name", upcast UNamed, upcast String)
                              ("age", upcast UNamed, upcast Int) ] ])
