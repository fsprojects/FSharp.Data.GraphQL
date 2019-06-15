/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
module FSharp.Data.GraphQL.Tests.AstValidationTests

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Validation
open Xunit
open FSharp.Data.GraphQL.Types

type Command =
    | SIT = 1
    | HEEL = 2
    | JUMP = 3
    | CLEAN_UP_HOUSE = 4

type IPet =
    abstract Name : string

type Human =
    { Name : string }

type Dog = 
    { Name : string
      BarkVolume : int
      Nickname : string
      KnownCommands : Command list
      IsHouseTrainedAtOtherHomes : bool
      IsHouseTrainedAtOwnHome : bool }
    member x.DoesKnowCommand(cmd : Command) = x.KnownCommands |> List.contains cmd
    member x.IsHouseTrained(atOtherHomes : bool) = if atOtherHomes then x.IsHouseTrainedAtOtherHomes else x.IsHouseTrainedAtOwnHome
    interface IPet with
        member x.Name = x.Name

type Cat = 
    { Name : string
      MeowVolume : int
      Nickname : string
      KnownCommands : Command list }
    member x.DoesKnowCommand(cmd : Command) = x.KnownCommands |> List.contains cmd
    interface IPet with
        member x.Name = x.Name

type CatOrDog =
    | Cat of Cat
    | Dog of Dog

type Root =
    { CatOrDog : CatOrDog
      Pet : IPet
      Human : Human }

let Command =
    Define.Enum<Command>(
        name = "Command",
        options = 
            [ Define.EnumValue("SIT", Command.SIT)
              Define.EnumValue("HEEL", Command.HEEL)
              Define.EnumValue("JUMP", Command.JUMP) ])

let Pet = Define.Interface<IPet>("Pet", [ Define.Field("name", String) ])

let Dog = 
    Define.Object<Dog>(
        name = "Dog", 
        fields = 
            [ Define.AutoField("name", String)
              Define.AutoField("barkVolume", Int)
              Define.AutoField("nickname", String)
              Define.Field("doesKnowCommand", Boolean, [ Define.Input("dogCommand", Command) ], fun ctx (dog : Dog) -> dog.DoesKnowCommand(ctx.Arg("dogCommand")))
              Define.Field("isHouseTrained", Boolean, [ Define.Input("atOtherHomes", Boolean) ], fun ctx (dog : Dog) -> dog.IsHouseTrained(ctx.Arg("atOtherHomes"))) ], 
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
        name = "CatOrDog",
        options = [ Cat; Dog ],
        resolveValue = (function | Cat c -> box c | Dog d -> upcast d),
        resolveType = (function | Cat _ -> upcast Cat | Dog _ -> upcast Dog))

let Human =
    Define.Object<Human>(
        name = "Human",
        fields =
            [ Define.AutoField("name", String) ])

let Arguments =
    Define.Object<obj>(
        name = "Arguments",
        fields =
            [ Define.Field("multipleReqs", Int, [ Define.Input("x", Int); Define.Input("y", Int) ], fun ctx _ -> ctx.Arg("x") + ctx.Arg("y"))
              Define.Field("booleanArgField", Nullable Boolean, [ Define.Input("booleanArg", Nullable Boolean) ], fun ctx _ -> ctx.Arg("booleanArg"))
              Define.Field("floatArgField", Nullable Float, [ Define.Input("floatArg", Nullable Float) ], fun ctx _ -> ctx.Arg("floatArg"))
              Define.Field("intArgField", Nullable Int, [ Define.Input("intArg", Nullable Int) ], fun ctx _ -> ctx.Arg("intArg"))
              Define.Field("nonNullBooleanArgField", Boolean, [ Define.Input("nonNullBooleanArg", Boolean) ], fun ctx _ -> ctx.Arg("nonNullBooleanArg"))
              Define.Field("booleanListArgField", Nullable (ListOf (Nullable Boolean)), [ Define.Input("booleanListArg", ListOf (Nullable Boolean)) ], fun ctx _ -> ctx.Arg("booleanListArg") |> Some)
              Define.Field("optionalNonNullBooleanArgField", Boolean, [ Define.Input("optionalBooleanArg", Boolean, false) ], fun ctx _ -> ctx.Arg("optionalBooleanArg")) ])

let Root = 
    Define.Object<Root>(
        name = "Root", 
        fields =
            [ Define.AutoField("catOrDog", CatOrDog)
              Define.AutoField("pet", Pet)
              Define.AutoField("human", Human)
              Define.Field("arguments", Arguments) ])

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
    
[<Fact>]
let ``Validation should grant that leaf fields have sub fields when necessary, and do not have if they are scalar or enums`` () =
    let query1 =
        """fragment scalarSelectionsNotAllowedOnInt on Dog {
  barkVolume {
    sinceWhen
  }
}"""
    let query2 =
        """query directQueryOnObjectWithoutSubFields {
  human
}

query directQueryOnInterfaceWithoutSubFields {
  pet
}

query directQueryOnUnionWithoutSubFields {
  catOrDog
}"""
    let expectedFailureResult = 
        Error [ "Field 'barkVolume' of 'Dog' type is of type kind SCALAR, and therefore should not contain inner fields in its selection."
                "Field 'human' of 'Root' type is of type kind OBJECT, and therefore should have inner fields in its selection."
                "Field 'pet' of 'Root' type is of type kind INTERFACE, and therefore should have inner fields in its selection."
                "Field 'catOrDog' of 'Root' type is of type kind UNION, and therefore should have inner fields in its selection." ]
    let shouldFail = [query1; query2] |> List.map (Parser.parse >> Validation.Ast.validateLeafFieldSelections schemaInfo) |> List.reduce (@)
    shouldFail |> equals expectedFailureResult
    let query4 =
        """fragment scalarSelection on Dog {
  barkVolume
}"""
    let shouldPass = Parser.parse query4 |> Validation.Ast.validateLeafFieldSelections schemaInfo
    shouldPass |> equals Success

[<Fact>]
let ``Validation should grant that arguments passed to fields exists in their definition`` () =
    let query1 =
        """fragment invalidArgName on Dog {
  doesKnowCommand(command: CLEAN_UP_HOUSE)
}"""
    let query2 =
        """fragment invalidArgName on Dog {
  isHouseTrained(atOtherHomes: true) @include(unless: false)
}"""
    let expectedFailureResult =
        Error [ "Field 'doesKnowCommand' of type 'Dog' does not have an input named 'command' in its definition."
                "Directive 'include' of field 'isHouseTrained' of type 'Dog' does not have an argument named 'unless' in its definition."]
    let shouldFail = [query1; query2] |> List.map (Parser.parse >> Validation.Ast.validateArgumentNames schemaInfo) |> List.reduce (@)
    shouldFail |> equals expectedFailureResult
    let query3 =
        """fragment argOnRequiredArg on Dog {
  doesKnowCommand(dogCommand: SIT)
}

fragment argOnOptional on Dog {
  isHouseTrained(atOtherHomes: true) @include(if: true)
}"""
    let shouldPass = Parser.parse query3 |> Validation.Ast.validateArgumentNames schemaInfo
    shouldPass |> equals Success

[<Fact>]
let ``Validation should grant that arguments passed to fields are unique between themselves`` () =
    let query =
        """fragment duplicatedArgs on Dog {
  doesKnowCommand(dogCommand: SIT, dogCommand: CLEAN_UP_HOUSE)
}"""
    let expectedFailureResult =
        Error [ "More than one argument named 'dogCommand' was defined in field 'doesKnowCommand'. Field arguments must be unique." ]
    let shouldFail = Parser.parse query |> Validation.Ast.validateArgumentUniqueness
    shouldFail |> equals expectedFailureResult

[<Fact>]
let ``Validation should grant that required arguments with no default values are passed`` () =
    let query1 =
        """fragment missingRequiredArg on Arguments {
  nonNullBooleanArgField
}"""
    let query2 =
        """fragment missingRequiredArg on Arguments {
  nonNullBooleanArgField(nonNullBooleanArg: null)
}"""
    let expectedFailureResult =
        Error [ "Argument 'nonNullBooleanArg' of field 'nonNullBooleanArgField' of type 'Arguments' is required and does not have a default value."
                "Argument 'nonNullBooleanArg' of field 'nonNullBooleanArgField' of type 'Arguments' is required and does not have a default value." ]
    let shouldFail = [query1; query2] |> List.map (Parser.parse >> Validation.Ast.validateRequiredArguments schemaInfo) |> List.reduce (@)
    shouldFail |> equals expectedFailureResult
    let query3 =
        """fragment goodBooleanArg on Arguments {
  booleanArgField(booleanArg: true)
}

fragment goodNonNullArg on Arguments {
  nonNullBooleanArgField(nonNullBooleanArg: true)
}"""
    let query4 =
        """fragment goodBooleanArgDefault on Arguments {
  booleanArgField
}"""
    let shouldPass = [query3; query4] |> List.map (Parser.parse >> Validation.Ast.validateRequiredArguments schemaInfo) |> List.reduce (@)
    shouldPass |> equals Success

[<Fact>]
let ``Validation should grant that fragment definitions have unique names`` () =
    let query1 =
        """{
  dog {
    ...fragmentOne
  }
}

fragment fragmentOne on Dog {
  name
}

fragment fragmentOne on Dog {
  owner {
    name
  }
}"""
    let shouldFail = Parser.parse query1 |> Validation.Ast.validateFragmentNameUniqueness
    shouldFail |> equals (Error [ "There are 2 fragments with name 'fragmentOne' in the document. Fragment definitions must have unique names." ])
    let query2 =
        """fragment correctType on Dog {
  name
}

fragment inlineFragment on Dog {
  ... on Dog {
    name
  }
}

fragment inlineFragment2 on Dog {
  ... @include(if: true) {
    name
  }
}"""
    let shouldPass = Parser.parse query2 |> Validation.Ast.validateFragmentNameUniqueness
    shouldPass |> equals Success

[<Fact>]
let ``Validation should grant that type conditions of fragments refers to existing types in the schema`` () =
    let query =
        """fragment notOnExistingType on NotInSchema {
  name
}

fragment inlineNotExistingType on Dog {
  ... on NotInSchema {
    name
  }
}"""
    let expectedFailureResult =
        Error [ "Fragment 'notOnExistingType' has type condition 'NotInSchema', but that type does not exist in the schema."
                "An inline fragment in the document has type condition 'NotInSchema', but that type does not exist in the schema." ]
    let shouldFail = Parser.parse query |> Validation.Ast.validateFragmentTypeExistence schemaInfo
    shouldFail |> equals expectedFailureResult

[<Fact>]
let ``Validation should grant that type fragments are only used in composite types`` () =
    let query =
        """fragment fragOnScalar on Int {
  something
}

fragment inlineFragOnScalar on Dog {
  ... on Boolean {
    somethingElse
  }
}"""
    let expectedFailureResult =
        Error [ "Fragment 'fragOnScalar' has type kind SCALAR, but fragments can only be defined in UNION, OBJECT or INTERFACE types."
                "An inline fragment has type kind SCALAR, but fragments can only be defined in UNION, OBJECT or INTERFACE types." ]
    let shouldFail = Parser.parse query |> Validation.Ast.validateFragmentsOnCompositeTypes schemaInfo
    shouldFail |> equals expectedFailureResult

[<Fact>]
let ``Validation should grant that fragment definitions are used in at least one operation`` () =
    let query =
        """fragment nameFragment on Dog {
  name
}

{
  dog {
    name
  }
}"""
    let shouldFail = Parser.parse query |> Validation.Ast.validateFragmentsMustBeUsed
    shouldFail |> equals (Error [ "Fragment 'nameFragment' is not used in any operation in the document. Fragments must be used in at least one operation." ])

[<Fact>]
let ``Validation should grant that fragment spreads exists in document`` () =
    let query =
        """{
  dog {
    ...undefinedFragment
  }
}"""
    let shouldFail = Parser.parse query |> Validation.Ast.validateFragmentSpreadTargetDefined
    shouldFail |> equals (Error [ "Fragment spread 'undefinedFragment' refers to a non-existent fragment definition in the document." ])