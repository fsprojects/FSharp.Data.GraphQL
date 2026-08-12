// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.InterfaceMissingImplementationTests

open Xunit
open System
open System.Threading.Tasks

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Shared
open FSharp.Data.GraphQL.Execution
open Helpers

#nowarn "25"

module internal GQLProblemDetails =
    let CreateSchemaValidation message = GQLProblemDetails.CreateWithKind (message, Validation)

/// Test data types for interface validation

type Person = { Name : string }

type Company = { Name : string; EmployeeCount : int }

/// Define interfaces and types for testing

let EntityInterface =
    Define.Interface<obj> (name = "Entity", fields = [ Define.Field ("name", StringType, resolve = (fun _ _ -> "")) ])

let PersonType =
    Define.Object<Person> (
        name = "Person",
        isTypeOf = (fun o -> o :? Person),
        interfaces = [ EntityInterface ],
        fields = [ Define.Field ("name", StringType, resolve = (fun _ person -> person.Name)) ]
    )

let CompanyType =
    Define.Object<Company> (
        name = "Company",
        isTypeOf = (fun o -> o :? Company),
        interfaces = [ EntityInterface ],
        fields = [
            Define.Field ("name", StringType, resolve = (fun _ company -> company.Name))
            Define.Field ("employeeCount", IntType, resolve = (fun _ company -> company.EmployeeCount))
        ]
    )

[<Fact>]
let ``Schema creation with orphaned interface returns validation error`` () =
    let orphanedInterface =
        Define.Interface<obj> (name = "OrphanedEntity", fields = [ Define.Field ("id", StringType, resolve = (fun _ _ -> "")) ])

    let simpleObject =
        Define.Object<Person> (
            name = "SimpleObject",
            isTypeOf = (fun o -> o :? Person),
            interfaces = [],
            fields = [ Define.Field ("name", StringType, resolve = (fun _ person -> person.Name)) ]
        )

    // According to GraphQL spec: interfaces must have at least one implementing type
    let schema =
        Schema (query = simpleObject, config = { SchemaConfig.Default with Types = [ orphanedInterface ] })

    // Validate the schema - should report orphaned interface error
    let validationErrors = schema.Validate ()
    let expected = [
        GQLProblemDetails.CreateSchemaValidation "Interface 'OrphanedEntity' has no implementing object types"
    ]
    equals expected (validationErrors |> Array.toList)

[<Fact>]
let ``Validation detects orphaned interface`` () =
    let orphanedInterface =
        Define.Interface<obj> (name = "Orphaned", fields = [ Define.Field ("field", StringType, resolve = (fun _ _ -> "")) ])

    let simpleObject =
        Define.Object<Person> (
            name = "SimpleObject",
            isTypeOf = (fun o -> o :? Person),
            interfaces = [],
            fields = [ Define.Field ("name", StringType, resolve = (fun _ person -> person.Name)) ]
        )

    let schema =
        Schema (query = simpleObject, config = { SchemaConfig.Default with Types = [ orphanedInterface ] })

    // Validation should detect the orphaned interface
    let validationErrors = schema.Validate ()
    nonEmpty validationErrors

[<Fact>]
let ``Interface with implementations passes validation`` () : Task = task {
    let schema = Schema (query = PersonType, config = { SchemaConfig.Default with Types = [ CompanyType ] })

    let introspectionQuery =
        """
        {
          __type(name: "Entity") {
            name
            kind
            possibleTypes {
              name
            }
          }
        }
        """

    let ast = parse introspectionQuery
    let! result = Executor(schema).AsyncExecute (ast, getMockInputContext)

    Assert.NotNull (result)

    let validationErrors = schema.Validate ()
    empty validationErrors
}

[<Fact>]
let ``Mixed schema validation detects unimplemented interface`` () =
    let implementedInterface =
        Define.Interface<obj> (name = "Implemented", fields = [ Define.Field ("name", StringType, resolve = (fun _ _ -> "")) ])

    let unimplementedInterface =
        Define.Interface<obj> (name = "Unimplemented", fields = [ Define.Field ("value", StringType, resolve = (fun _ _ -> "")) ])

    let implementingType =
        Define.Object<Person> (
            name = "ImplementsOne",
            isTypeOf = (fun o -> o :? Person),
            interfaces = [ implementedInterface ],
            fields = [ Define.Field ("name", StringType, resolve = (fun _ person -> person.Name)) ]
        )

    let schema =
        Schema (query = implementingType, config = { SchemaConfig.Default with Types = [ unimplementedInterface ] })

    // Schema validation should detect unimplemented interface
    let validationErrors = schema.Validate ()
    let expected = [
        GQLProblemDetails.CreateSchemaValidation "Interface 'Unimplemented' has no implementing object types"
    ]
    equals expected (validationErrors |> Array.toList)
