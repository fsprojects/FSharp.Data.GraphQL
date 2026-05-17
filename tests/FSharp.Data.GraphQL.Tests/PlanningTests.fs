// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc
module FSharp.Data.GraphQL.Tests.PlanningTests

#nowarn "25"
#nowarn "40"

open System
open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Planning
open FSharp.Data.GraphQL.Execution

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
    DefineRec.Object(
        name = "Person",
        fieldsFn = (fun () ->
            [ Define.Field("firstName", StringType, fun _ person -> person.firstName)
              Define.Field("lastName", StringType, fun _ person -> person.lastName)
              Define.Field("age", IntType, fun _ person -> person.age)
              Define.Field("name", StringType, fun _ person -> person.firstName + " " + person.lastName)
              Define.Field("friends", ListOf Person, fun _ _ -> []) ]), interfaces = [ INamed ])

and Animal =
    Define.Object(name = "Animal",
                  fields = [ Define.Field("name", StringType, fun _ animal -> animal.name)
                             Define.Field("species", StringType, fun _ animal -> animal.species) ], interfaces = [ INamed ])

and INamed = Define.Interface<obj>("INamed", [ Define.Field("name", StringType) ])

and UNamed =
    Define.Union(
        "UNamed", [ Person; Animal ],
        function
        | Animal a -> box a
        | Person p -> upcast p)

[<Fact>]
let ``Planning must retain correct types for leafs``() =
    let schema = Schema(Person)
    let schemaProcessor = Executor(schema)
    let query = """{
        firstName
        lastName
        age
    }"""
    let plan = schemaProcessor.CreateExecutionPlanOrFail(query)
    plan.RootDef |> equals (upcast Person)
    equals 3 plan.Fields.Length
    plan.Fields
    |> List.map (fun info -> (info.Identifier, info.ParentDef, info.ReturnDef))
    |> equals [ ("firstName", upcast Person, upcast StringType)
                ("lastName", upcast Person, upcast StringType)
                ("age", upcast Person, upcast IntType) ]

[<Fact>]
let ``Planning must work with fragments``() =
    let schema = Schema(Person)
    let schemaProcessor = Executor(schema)
    let query = """query Example {
        ...named
        age
    }
    fragment named on Person {
        firstName
        lastName
    }"""
    let plan = schemaProcessor.CreateExecutionPlanOrFail(query)
    plan.RootDef |> equals (upcast Person)
    equals 3 plan.Fields.Length
    plan.Fields
    |> List.map (fun info -> (info.Identifier, info.ParentDef, info.ReturnDef))
    |> equals [ ("firstName", upcast Person, upcast StringType)
                ("lastName", upcast Person, upcast StringType)
                ("age", upcast Person, upcast IntType) ]

[<Fact>]
let ``Planning must work with parallel fragments``() =
    let schema = Schema(Person)
    let schemaProcessor = Executor(schema)
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
    let plan = schemaProcessor.CreateExecutionPlanOrFail(query)
    plan.RootDef |> equals (upcast Person)
    equals 3 plan.Fields.Length
    plan.Fields
    |> List.map (fun info -> (info.Identifier, info.ParentDef, info.ReturnDef))
    |> equals [ ("firstName", upcast Person, upcast StringType)
                ("lastName", upcast Person, upcast StringType)
                ("age", upcast Person, upcast IntType) ]

[<Fact>]
let ``Planning must retain correct types for lists``() =
    let Query = Define.Object("Query", [ Define.Field("people", ListOf Person, fun _ () -> people) ])
    let schema = Schema(Query)
    let schemaProcessor = Executor(schema)
    let query = """{
        people {
            firstName
            lastName
            friends {
                firstName
                lastName
            }
        }
    }"""
    let PersonList : OutputDef<Person list> = ListOf Person
    let plan = schemaProcessor.CreateExecutionPlanOrFail(query)
    equals 1 plan.Fields.Length
    let listInfo = plan.Fields.Head
    listInfo.Identifier |> equals "people"
    listInfo.ReturnDef |> equals (upcast PersonList)
    let (ResolveCollection(info)) = listInfo.Kind
    info.ParentDef |> equals (upcast PersonList)
    info.ReturnDef |> equals (upcast Person)
    let (SelectFields(innerFields)) = info.Kind
    equals 3 innerFields.Length
    innerFields
    |> List.map (fun i -> (i.Identifier, i.ParentDef, i.ReturnDef))
    |> equals [ ("firstName", upcast Person, upcast StringType)
                ("lastName", upcast Person, upcast StringType)
                ("friends", upcast Person, upcast PersonList) ]
    let (ResolveCollection(friendInfo)) = (innerFields |> List.find (fun i -> i.Identifier = "friends")).Kind
    friendInfo.ParentDef |> equals (upcast PersonList)
    friendInfo.ReturnDef |> equals (upcast Person)

[<Fact>]
let ``Planning must work with interfaces``() =
    let Query = Define.Object("Query", [ Define.Field("names", ListOf INamed, fun _ () -> []) ])
    let schema = Schema(query = Query, config = { SchemaConfig.Default with Types = [ Person; Animal ] })
    let schemaProcessor = Executor(schema)
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
    let plan = schemaProcessor.CreateExecutionPlanOrFail(query)
    equals 1 plan.Fields.Length
    let INamedList : OutputDef<obj list> = ListOf INamed
    let listInfo = plan.Fields.Head
    listInfo.Identifier |> equals "names"
    listInfo.ReturnDef |> equals (upcast INamedList)
    let (ResolveCollection(info)) = listInfo.Kind
    info.ParentDef |> equals (upcast INamedList)
    info.ReturnDef |> equals (upcast INamed)
    let (ResolveAbstraction(innerFields)) = info.Kind
    innerFields
    |> Map.map (fun typeName fields -> fields |> List.map (fun i -> (i.Identifier, i.ParentDef, i.ReturnDef)))
    |> equals (Map.ofList [ "Person",
                            [ ("name", upcast INamed, upcast StringType)
                              ("age", upcast INamed, upcast IntType) ]
                            "Animal",
                            [ ("name", upcast INamed, upcast StringType)
                              ("species", upcast INamed, upcast StringType) ] ])

[<Fact>]
let ``Planning must work with unions``() =
    let Query = Define.Object("Query", [ Define.Field("names", ListOf UNamed, fun _ () -> []) ])
    let schema = Schema(Query)
    let schemaProcessor = Executor(schema)
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
    let plan = schemaProcessor.CreateExecutionPlanOrFail(query)
    equals 1 plan.Fields.Length
    let listInfo = plan.Fields.Head
    let UNamedList : OutputDef<Named list> = ListOf UNamed
    listInfo.Identifier |> equals "names"
    listInfo.ReturnDef |> equals (upcast UNamedList)
    let (ResolveCollection(info)) = listInfo.Kind
    info.ParentDef |> equals (upcast UNamedList)
    info.ReturnDef |> equals (upcast UNamed)
    let (ResolveAbstraction(innerFields)) = info.Kind
    innerFields
    |> Map.map (fun typeName fields -> fields |> List.map (fun i -> (i.Identifier, i.ParentDef, i.ReturnDef)))
    |> equals (Map.ofList [ "Animal",
                            [ ("name", upcast UNamed, upcast StringType)
                              ("species", upcast UNamed, upcast StringType) ]
                            "Person",
                            [ ("name", upcast UNamed, upcast StringType)
                              ("age", upcast UNamed, upcast IntType) ] ])

[<Fact>]
let ``Planning must handle inline fragment with non-matching type condition in unions``() =
    // ═══════════════════════════════════════════════════════════════════════════
    // REGRESSION TEST for Planning_ResolveDeferred_Bug
    // ═══════════════════════════════════════════════════════════════════════════
    //
    // GraphQL SCENARIO:
    // =================
    // In GraphQL, inline fragments with type conditions are used to query fields
    // specific to certain types in a union or interface:
    //
    //   query {
    //     items {              # Union of [Animal, Person]
    //       ... on Animal { }  # ✓ Valid – Animal is in union
    //       ... on Person { }  # ✓ Valid – Person is in union
    //       ... on Robot { }   # ✓ Valid by spec! Robot is not in union
    //     }
    //   }
    //
    // THE PROBLEM:
    // ============
    // When an inline fragment's type condition does NOT match any type in the union,
    // the GraphQL spec says this is VALID – the fragment simply never matches.
    //
    // Example: Querying for Robot fields on a Person|Animal union
    // Expected: Empty result for Robot (no runtime error during planning)
    // Bug: Runtime error "Expected an Abstraction!" during query planning phase
    //
    // WHY IT MATTERS:
    // ===============
    // This commonly happens in real-world scenarios:
    // - Generic queries across multiple schema types
    // - Schema evolution (type removed from union, old queries still reference it)
    // - Client doesn't know exact union composition
    //
    // According to GraphQL spec, this must NOT fail – it should gracefully
    // produce an empty result set for non-matching fragments.
    // ═══════════════════════════════════════════════════════════════════════════

    // Create a third type that is NOT part of UNamed union
    let Robot =
        Define.Object(
            name = "Robot",
            fields =
                [ Define.Field("modelNumber", StringType, fun _ (robot: string) -> robot)
                  Define.Field("name", StringType, fun _ _ -> "Robot") ])

    let Query = Define.Object("Query", [ Define.Field("names", ListOf UNamed, fun _ () -> []) ])
    let schema = Schema(query = Query, config = { SchemaConfig.Default with Types = [ Person; Animal; Robot ] })
    let schemaProcessor = Executor(schema)

    // GraphQL Query:
    // UNamed union = Person | Animal (Robot is NOT in this union)
    // The "... on Robot" fragment below will never match any objects
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
            ... on Robot {
                modelNumber
            }
        }
    }"""

    // TEST ASSERTION:
    // This must succeed per GraphQL spec – non-matching fragments are valid
    // Bug would cause: "Expected an Abstraction!" runtime error during planning
    let plan = schemaProcessor.CreateExecutionPlanOrFail(query)

    // Verify the execution plan structure
    equals 1 plan.Fields.Length
    let listInfo = plan.Fields.Head
    let UNamedList : OutputDef<Named list> = ListOf UNamed
    listInfo.Identifier |> equals "names"
    listInfo.ReturnDef |> equals (upcast UNamedList)
    let (ResolveCollection(info)) = listInfo.Kind
    info.ParentDef |> equals (upcast UNamedList)
    info.ReturnDef |> equals (upcast UNamed)

    // Must successfully extract abstraction info
    // Bug would fail here with wrong execution info kind
    let (ResolveAbstraction(innerFields)) = info.Kind

    // Result: Only Animal and Person fields (Robot is filtered out)
    // This is correct GraphQL behavior – non-matching fragments produce no fields
    innerFields
    |> Map.map (fun typeName fields -> fields |> List.map (fun i -> (i.Identifier, i.ParentDef, i.ReturnDef)))
    |> equals (Map.ofList [ "Animal",
                            [ ("name", upcast UNamed, upcast StringType)
                              ("species", upcast UNamed, upcast StringType) ]
                            "Person",
                            [ ("name", upcast UNamed, upcast StringType)
                              ("age", upcast UNamed, upcast IntType) ] ])

[<Fact>]
let ``Planning must handle nested inline fragments with non-matching type conditions``() =
    // REGRESSION TEST for Planning_ResolveDeferred_Bug (nested scenario)
    //
    // GraphQL SCENARIO:
    // =================
    // Same issue as above, but with nested structure:
    //
    //   query {
    //     container {
    //       nested {            # Union of [Animal, Person]
    //         ... on Robot { }  # ✓ Valid – just never matches
    //       }
    //     }
    //   }
    //
    // Tests that deeply nested queries with non-matching fragments work correctly.
    // This is common in production with complex schema hierarchies.

    // Define Robot type (not part of UNamed union)
    let RobotType =
        Define.Object(
            name = "Robot",
            fields =
                [ Define.Field("modelNumber", StringType, fun _ (robot: string) -> robot)
                  Define.Field("name", StringType, fun _ _ -> "Robot") ])

    // Container type with nested union list – creates deeper nesting
    let ContainerType =
        Define.Object<unit>(
            name = "Container",
            fields = [ Define.Field("nested", ListOf UNamed, fun _ () -> []) ])

    let Query =
        Define.Object(
            "Query",
            [ Define.Field("container", ContainerType, fun _ () -> ()) ])

    let schema = Schema(query = Query, config = { SchemaConfig.Default with Types = [ Person; Animal; RobotType ] })
    let schemaProcessor = Executor(schema)

    // Nested query with non-matching fragment
    let query = """query Example {
        container {
            nested {
                ... on Animal {
                    name
                    species
                }
                ... on Person {
                    name
                    age
                }
                ... on Robot {
                    modelNumber
                }
            }
        }
    }"""

    // Must succeed – nested non-matching fragments are valid per GraphQL spec
    let plan = schemaProcessor.CreateExecutionPlanOrFail(query)

    // Verify the plan structure is correct
    equals 1 plan.Fields.Length
    plan.Fields.Head.Identifier |> equals "container"

[<Fact>]
let ``Planning must return ResolveAbstraction even when all fragments are non-matching``() =
    // REGRESSION TEST for Planning_ResolveDeferred_Bug (extreme case)
    //
    // GraphQL SCENARIO – EDGE CASE:
    // ==============================
    // What if ALL inline fragments in a query don't match the union?
    //
    //   query {
    //     items {              # Union of [Animal, Person]
    //       ... on Robot { }   # Doesn't match
    //     }
    //   }
    //
    // THE PROBLEM:
    // ============
    // With only non-matching fragments, the planner has zero fields to plan.
    // This is the MOST EXTREME case of the bug.
    //
    // Expected GraphQL behavior: Valid query, returns empty result set
    // Bug behavior: Runtime crash during planning with "Expected an Abstraction!"
    //
    // WHY THIS HAPPENS:
    // =================
    // In real-world scenarios:
    // - Client queries for types that were removed from union
    // - Conditional fragments based on client-side logic
    // - Generic queries against multiple schema versions
    //
    // Per GraphQL spec: This MUST work – it's just a query that matches nothing.

    // Robot is NOT in UNamed union
    let RobotType =
        Define.Object(
            name = "Robot",
            fields = [ Define.Field("modelNumber", StringType, fun _ (robot: string) -> robot) ])

    let Query = Define.Object("Query", [ Define.Field("names", ListOf UNamed, fun _ () -> []) ])
    let schema = Schema(query = Query, config = { SchemaConfig.Default with Types = [ Person; Animal; RobotType ] })
    let schemaProcessor = Executor(schema)

    // GraphQL Query – ONLY non-matching fragment!
    // UNamed union = Person | Animal (NOT Robot)
    // This query will match zero objects at runtime
    let query = """query Example {
        names {
            ... on Robot {
                modelNumber
            }
        }
    }"""

    // TEST ASSERTION:
    // Must succeed per GraphQL spec – empty result is valid, not an error
    // Bug would cause: Runtime crash "Expected an Abstraction!" during planning
    let plan = schemaProcessor.CreateExecutionPlanOrFail(query)

    // Verify the plan was created successfully
    equals 1 plan.Fields.Length
    let listInfo = plan.Fields.Head
    let (ResolveCollection(info)) = listInfo.Kind

    // Must successfully extract abstraction info
    let (ResolveAbstraction(innerFields)) = info.Kind

    // Result: Empty map – no matching types
    // This is CORRECT per GraphQL spec – valid query, just matches nothing
    innerFields |> Map.isEmpty |> equals true
