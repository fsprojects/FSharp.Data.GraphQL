/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
module FSharp.Data.GraphQL.Tests.AstValidationTests

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Validation
open Xunit
open FSharp.Data.GraphQL.Ast

let parseOperationDefinitions (query : string) =
    let ast = Parser.parse query
    ast.Definitions
    |> List.choose (function | OperationDefinition x -> Some x | _ -> None)

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
    let actual = parseOperationDefinitions query |> Validation.Ast.validateOperationNameUniqueness
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
    let actual = parseOperationDefinitions query |> Validation.Ast.validateLoneAnonymousOperation
    let expected = Error [ "Anonymous operations must be the only operation in a document. This document has at least one anonymous operation and more than one operation." ]
    actual |> equals expected
