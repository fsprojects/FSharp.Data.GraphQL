/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
module FSharp.Data.GraphQL.Tests.AstValidationTests

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Validation
open Xunit
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Ast

type Command =
    | SIT = 1
    | HEEL = 2
    | JUMP = 3

type Dog = 
    { Name : string
      BarkVolume : int
      Nickname : string
      KnownCommands : Command list }
    member x.DoesKnowCommand(cmd : Command) = x.KnownCommands |> List.contains cmd

type Cat = 
    { Name : string
      MeowVolume : int
      Nickname : string
      KnownCommands : Command list }
    member x.DoesKnowCommand(cmd : Command) = x.KnownCommands |> List.contains cmd

type CatOrDog =
    | Cat of Cat
    | Dog of Dog

type Root =
    { Cat : Cat
      Dog : Dog }

let Command =
    Define.Enum<Command>(
        name = "Command",
        options = 
            [ Define.EnumValue("SIT", Command.SIT)
              Define.EnumValue("HEEL", Command.HEEL)
              Define.EnumValue("JUMP", Command.JUMP) ])

let Pet = Define.Interface<obj>("Pet", [ Define.Field("name", String) ])

let Dog = 
    Define.Object<Dog>(
        name = "Dog", 
        fields = 
            [ Define.AutoField("name", String)
              Define.AutoField("barkVolume", Int)
              Define.AutoField("nickname", String)
              Define.Field("doesKnowCommand", Boolean, [ Define.Input("dogCommand", Command) ], fun ctx (dog : Dog) -> dog.DoesKnowCommand(ctx.Arg("dogCommand"))) ], 
        interfaces = [ Pet ])

let Cat = 
    Define.Object<Cat>(
        name = "Cat", 
        fields = 
            [ Define.AutoField("name", String)
              Define.AutoField("meowVolume", Int)
              Define.AutoField("nickname", String)
              Define.Field("doesKnowCommand", Boolean, [ Define.Input("catCommand", Command) ], fun ctx (dog : Cat) -> dog.DoesKnowCommand(ctx.Arg("catCommand")))], 
        interfaces = [ Pet ])

let CatOrDog =
    Define.Union(
        "CatOrDog",
        options = [ Cat; Dog ],
        resolveValue = (function | Cat c -> box c | Dog d -> upcast d),
        resolveType = (function | Cat _ -> upcast Cat | Dog _ -> upcast Dog))

let Root = Define.Object<Root>("Root", [ Define.Field("catOrDog", CatOrDog) ])

let schema : ISchema = upcast Schema(Root)
let schemaInfo = SchemaInfo.FromIntrospectionSchema(schema.Introspected)

[<Fact>]
let ``Validation should grant that each operation name is unique in the document`` () =
    let query =
        """query dogOperation {
  dog {
    name
  }
}

mutation dogOperation {
  mutateDog {
    id
  }
}"""
    let actual = Parser.parse query |> Validation.Ast.validateOperationNameUniqueness
    let expected = Error [ "Operation 'dogOperation' has 2 definitions. Each operation name must be unique." ]
    actual |> equals expected

[<Fact>]
let ``Validation should grant that, if the document has anonymous operations, all of its operations should be a set of one`` () =
    let query =
        """{
  dog {
    name
  }
}

query getName {
  dog {
    owner {
      name
    }
  }
}"""
    let actual = Parser.parse query |> Validation.Ast.validateLoneAnonymousOperation
    let expected = Error [ "An anonymous operation must be the only operation in a document. This document has at least one anonymous operation and more than one operation." ]
    actual |> equals expected

[<Fact>]
let ``Validation should grant that subscription operations contains only one root field`` () =
    let query1 =
        """subscription sub1 {
  newMessage {
    body
    sender
  }
  disallowedSecondRootField
}"""
    let query2 =
        """subscription sub2 {
  ...multipleSubscriptions
}

fragment multipleSubscriptions on Subscription {
  newMessage {
    body
    sender
  }
  disallowedSecondRootField
}"""
    let query3 =
        """subscription sub3 {
  newMessage {
    body
    sender
  }
  __typename
}"""
    let expected =
        Error [ "Subscription operations should have only one root field. Operation 'sub1' has 2 fields (disallowedSecondRootField,  newMessage)."
                "Subscription operations should have only one root field. Operation 'sub2' has 2 fields (disallowedSecondRootField,  newMessage)."
                "Subscription operations should have only one root field. Operation 'sub3' has 2 fields (__typename,  newMessage)." ]
    let actual = [query1; query2; query3] |> List.map (Parser.parse >> Validation.Ast.validateSubscriptionSingleRootField) |> List.reduce (@)
    actual |> equals expected

[<Fact>]
let ``Validation should grant that selections contains only fields of their scoped types`` () =
    let query1 =
        """fragment fieldNotDefined on Dog {
  meowVolume
}

fragment aliasedLyingFieldTargetNotDefined on Dog {
  barkVolume: kawVolume
}"""
    let query2 =
        """fragment definedOnImplementorsButNotInterface on Pet {
  nickname
}"""
    let query3 =
        """fragment directFieldSelectionOnUnion on CatOrDog {
  name
  barkVolume
}"""
    let expected =
        Error [ "Field 'meowVolume' is not defined in schema type 'Dog'."
                "Field 'kawVolume' is not defined in schema type 'Dog'."
                "Field 'nickname' is not defined in schema type 'Pet'."
                "Field 'name' is not defined in schema type 'CatOrDog'."
                "Field 'barkVolume' is not defined in schema type 'CatOrDog'." ]
    let actual = [query1; query2; query3] |> List.map (Parser.parse >> Validation.Ast.validateSelectionFieldTypes schemaInfo) |> List.reduce (@)
    actual |> equals expected

[<Fact>]
let ``Validation should grant that fields in any selection set can be merged`` () =
    let query1 =
        """fragment conflictingBecauseAlias on Dog {
  name: nickname
  name
}"""
    let query2 =
        """fragment conflictingArgsOnValues on Dog {
  doesKnowCommand(dogCommand: SIT)
  doesKnowCommand(dogCommand: HEEL)
}

fragment conflictingArgsValueAndVar on Dog {
  doesKnowCommand(dogCommand: SIT)
  doesKnowCommand(dogCommand: $dogCommand)
}

fragment conflictingArgsWithVars on Dog {
  doesKnowCommand(dogCommand: $varOne)
  doesKnowCommand(dogCommand: $varTwo)
}

fragment differingArgs on Dog {
  doesKnowCommand(dogCommand: SIT)
  doesKnowCommand
}"""
    let query3 =
        """fragment conflictingDifferingResponses on Pet {
  ... on Dog {
    someValue: nickname
  }
  ... on Cat {
    someValue: meowVolume
  }
}"""
    let expectedFailureResult = 
        Error [ "Field name or alias 'name' is referring to fields 'nickname' and 'name', but they are different fields in the scope of the parent type."
                "Field name or alias 'doesKnowCommand' refers to field 'doesKnowCommand' two times, but each reference has different argument sets."
                "Field name or alias 'doesKnowCommand' refers to field 'doesKnowCommand' two times, but each reference has different argument sets."
                "Field name or alias 'doesKnowCommand' refers to field 'doesKnowCommand' two times, but each reference has different argument sets."
                "Field name or alias 'doesKnowCommand' refers to field 'doesKnowCommand' two times, but each reference has different argument sets."
                "Field name or alias 'someValue' appears two times, but they do not have the same return types in the scope of the parent type."]
    let shouldFail = [query1; query2; query3] |> List.map (Parser.parse >> Validation.Ast.validateFieldSelectionMerging schemaInfo) |> List.reduce (@)
    shouldFail |> equals expectedFailureResult
    let query4 =
        """fragment mergeIdenticalFields on Dog {
  name
  name
}

fragment mergeIdenticalAliasesAndFields on Dog {
  otherName: name
  otherName: name
}"""
    let query5 =
        """fragment mergeIdenticalFieldsWithIdenticalArgs on Dog {
  doesKnowCommand(dogCommand: SIT)
  doesKnowCommand(dogCommand: SIT)
}

fragment mergeIdenticalFieldsWithIdenticalValues on Dog {
  doesKnowCommand(dogCommand: $dogCommand)
  doesKnowCommand(dogCommand: $dogCommand)
}"""
    let query6 =
        """fragment safeDifferingFields on Pet {
  ... on Dog {
    volume: barkVolume
  }
  ... on Cat {
    volume: meowVolume
  }
}

fragment safeDifferingArgs on Pet {
  ... on Dog {
    doesKnowCommand(dogCommand: SIT)
  }
  ... on Cat {
    doesKnowCommand(catCommand: JUMP)
  }
}"""
    let shouldPass = [query4; query5; query6] |> List.map (Parser.parse >> Validation.Ast.validateFieldSelectionMerging schemaInfo) |> List.reduce (@)
    shouldPass |> equals Success
    