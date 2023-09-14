// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc
module FSharp.Data.GraphQL.Tests.AbstractionTests

#nowarn "40"

open Xunit
open System
open System.Text.Json.Serialization

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Execution

type IPet =
    interface
        abstract Name : string
    end

type Dog =
    { Name : string
      Woofs : bool }

    interface IPet with
        member x.Name = x.Name

type Cat =
    { Name : string
      Meows : bool }

    interface IPet with
        member x.Name = x.Name

type Human = { Name : string }

type Pet =
    | DogCase of Dog
    | CatCase of Cat

let resolvePet =
    function
    | DogCase d -> box d
    | CatCase c -> upcast c

let schemaWithInterface =
    lazy
        let PetType = Define.Interface ("Pet", (fun () -> [ Define.Field ("name", StringType) ]))

        let DogType =
            Define.Object<Dog> (
                name = "Dog",
                isTypeOf = is<Dog>,
                interfaces = [ PetType ],
                fields =
                    [ Define.Field ("name", StringType, resolve = fun _ d -> d.Name)
                      Define.Field ("woofs", BooleanType, (fun _ d -> d.Woofs)) ]
            )

        let CatType =
            Define.Object<Cat> (
                name = "Cat",
                isTypeOf = is<Cat>,
                interfaces = [ PetType ],
                fields =
                    [ Define.Field ("name", StringType, resolve = fun _ c -> c.Name)
                      Define.Field ("meows", BooleanType, (fun _ c -> c.Meows)) ]
            )

        let schema =
            Schema (
                query =
                    Define.Object (
                        "Query",
                        fun () ->
                            [ Define.Field (
                                  "pets",
                                  ListOf PetType,
                                  fun _ _ -> [ { Name = "Odie"; Woofs = true } :> IPet; upcast { Name = "Garfield"; Meows = false } ]
                              ) ]
                    ),
                config = { SchemaConfig.Default with Types = [ CatType; DogType ] }
            )

        Executor<obj> (schema)

[<Fact>]
let ``Execute handles execution of abstract types: isTypeOf is used to resolve runtime type for Interface`` () =
    let query =
        """{
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

    let result = sync <| schemaWithInterface.Value.AsyncExecute (parse query)

    let expected =
        NameValueLookup.ofList
            [ "pets",
              upcast
                  [ NameValueLookup.ofList [ "name", "Odie" :> obj; "woofs", upcast true ] :> obj
                    NameValueLookup.ofList [ "name", "Garfield" :> obj; "meows", upcast false ] ] ]

    match result with
    | Direct (data, errors) ->
        empty errors
        data |> equals (upcast expected)
    | response -> fail $"Expected a direct GQLResponse but got {Environment.NewLine}{response}"

[<Fact>]
let ``Execute handles execution of abstract types: absent field resolution produces errors for Interface`` () =
    let query =
        """{
      pets {
        name
        ... on Dog {
          woofs
          unknownField1
        }
        ... on Cat {
          meows
          unknownField2
        }
      }
    }"""

    let result = sync <| schemaWithInterface.Value.AsyncExecute (parse query)

    let expected =
        [ { Message = "Field 'unknownField1' is not defined in schema type 'Dog'."
            Path = Include [ "pets"; "unknownField1" ]
            Locations = Skip
            Extensions = Skip }
          { Message = "Field 'unknownField2' is not defined in schema type 'Cat'."
            Path = Include [ "pets"; "unknownField2" ]
            Locations = Skip
            Extensions = Skip } ]

    match result with
    | RequestError (errors) -> equals expected errors
    | _ -> fail "Expected a requets error GQLResponse"

[<Fact>]
let ``Execute handles execution of abstract types: absent type resolution produces errors for Interface`` () =
    let query =
        """{
      pets {
        name
        ... on UnknownDog {
          woofs
          unknownField1
        }
        ... on Cat {
          meows
          unknownField2
        }
      }
    }"""

    let result = sync <| schemaWithInterface.Value.AsyncExecute (parse query)

    let expected =
        [ { Message = "Field 'unknownField2' is not defined in schema type 'Cat'."
            Path = Include [ "pets"; "unknownField2" ]
            Locations = Skip
            Extensions = Skip }
          { Message = "Inline fragment has type condition 'UnknownDog', but that type does not exist in the schema."
            Path = Include [ "pets" ]
            Locations = Skip
            Extensions = Skip } ]

    match result with
    | RequestError (errors) -> equals expected errors
    | _ -> fail "Expected a requets error GQLResponse"


let schemaWithUnion =
    lazy
        let DogType =
            Define.Object<Dog> (
                name = "Dog",
                isTypeOf = is<Dog>,
                fields = [ Define.AutoField ("name", StringType); Define.AutoField ("woofs", BooleanType) ]
            )

        let CatType =
            Define.Object<Cat> (
                name = "Cat",
                isTypeOf = is<Cat>,
                fields = [ Define.AutoField ("name", StringType); Define.AutoField ("meows", BooleanType) ]
            )

        let PetType = Define.Union ("Pet", [ DogType; CatType ], resolvePet)

        let schema =
            Schema (
                query =
                    Define.Object (
                        "Query",
                        fun () ->
                            [ Define.Field (
                                  "pets",
                                  ListOf PetType,
                                  fun _ _ -> [ DogCase { Name = "Odie"; Woofs = true }; CatCase { Name = "Garfield"; Meows = false } ]
                              ) ]
                    )
            )

        Executor<obj> (schema)

[<Fact>]
let ``Execute handles execution of abstract types: isTypeOf is used to resolve runtime type for Union`` () =
    let query =
        """{
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

    let result = sync <| schemaWithUnion.Value.AsyncExecute (parse query)

    let expected =
        NameValueLookup.ofList
            [ "pets",
              upcast
                  [ NameValueLookup.ofList [ "name", "Odie" :> obj; "woofs", upcast true ] :> obj
                    NameValueLookup.ofList [ "name", "Garfield" :> obj; "meows", upcast false ] ] ]

    match result with
    | Direct (data, errors) ->
        empty errors
        data |> equals (upcast expected)
    | response -> fail $"Expected a direct GQLResponse but got {Environment.NewLine}{response}"

[<Fact>]
let ``Execute handles execution of abstract types: absent field resolution produces errors for Union`` () =
    let query =
        """{
      pets {
        name
        ... on Dog {
          woofs
          unknownField1
        }
        ... on Cat {
          meows
          unknownField2
        }
      }
    }"""

    let result = sync <| schemaWithInterface.Value.AsyncExecute (parse query)

    let expected =
        [ { Message = "Field 'unknownField1' is not defined in schema type 'Dog'."
            Path = Include [ "pets"; "unknownField1" ]
            Locations = Skip
            Extensions = Skip }
          { Message = "Field 'unknownField2' is not defined in schema type 'Cat'."
            Path = Include [ "pets"; "unknownField2" ]
            Locations = Skip
            Extensions = Skip } ]

    match result with
    | RequestError (errors) -> equals expected errors
    | _ -> fail "Expected a requets error GQLResponse"

[<Fact>]
let ``Execute handles execution of abstract types: absent type resolution produces errors for Union`` () =
    let query =
        """{
      pets {
        name
        ... on Dog {
          woofs
          unknownField1
        }
        ... on UnknownCat {
          meows
          unknownField2
        }
      }
    }"""

    let result = sync <| schemaWithInterface.Value.AsyncExecute (parse query)

    let expected =
        [ { Message = "Field 'unknownField1' is not defined in schema type 'Dog'."
            Path = Include [ "pets"; "unknownField1" ]
            Locations = Skip
            Extensions = Skip }
          { Message = "Inline fragment has type condition 'UnknownCat', but that type does not exist in the schema."
            Path = Include [ "pets" ]
            Locations = Skip
            Extensions = Skip } ]

    match result with
    | RequestError (errors) -> equals expected errors
    | _ -> fail "Expected a requets error GQLResponse"
