/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.AstExtensionsTests

open Xunit
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Ast.Extensions

/// Generates an Ast.Document from a query string, prints it to another
/// query string and expects it to be equal.
let private printAndAssert (query : string) =
    let normalize (str : string) = str.Replace("\r\n", "\n")
    let document = parse query
    let expected = normalize query
    let actual = normalize <| document.ToQueryString()
    actual |> equals expected

[<Fact>]
let ``Should be able to print a simple query`` () =
    printAndAssert """query q {
  hero {
    name
  }
}"""

[<Fact>]
let ``Should be able to print a simple query with 2 fields`` () =
    printAndAssert """query q {
  hero {
    id
    name
  }
}"""

[<Fact>]
let ``Should be able to print a query with variables`` () =
    printAndAssert """query q($id: String!) {
  hero(id: $id) {
    id
    name
  }
}"""