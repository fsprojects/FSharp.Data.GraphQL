/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
module FSharp.Data.GraphQL.Tests.AstValidationTests

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Validation
open Xunit

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
    let expected = Error [ "Anonymous operations must be the only operation in a document. This document has at least one anonymous operation and more than one operation." ]
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
