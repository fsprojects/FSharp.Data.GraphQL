/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.AstExtensionsTests

open Xunit
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Ast.Extensions

/// Converts line breaks to a single standard to avoid different SO line break termination issues.
let normalize (str : string) = str.Replace("\r\n", "\n")

/// Generates an Ast.Document from a query string, prints it to another
/// query string and expects it to be equal. Input query must be formatted (with line breaks and identation).
/// Identation unit is two empty spaces.
let private printAndAssert (query : string) =
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

[<Fact>]
let ``Should be able to print a query with aliases`` () =
    printAndAssert """query q($myId: String!, $hisId: String!) {
  myHero: hero(id: $myId) {
    id
    name
  }
  hisHero: hero(id: $hisId) {
    id
    name
  }
  otherHero: hero(id: "1002") {
    name
  }
}"""

[<Fact>]
let ``Should be able to print a query with fragment spreads`` () =
    printAndAssert """query q($myId: String!, $hisId: String!) {
  myHero: hero(id: $myId) {
    id
    name
  }
  hisHero: hero(id: $hisId) {
    id
    name
  }
  otherHero: hero(id: "1002") {
    name
    friends {
      ...friend
    }
  }
}

fragment friend on Character {
  ... on Human {
    id
    homePlanet
  }
  ... on Droid {
    id
    primaryFunction
  }
}"""

[<Fact>]
let ``Should be able to print a short hand format query`` () =
    printAndAssert """{
  field1
  field2
}"""

[<Fact>]
let ``Should not print query without name in short hand format`` () =
    printAndAssert """query ($rId: Int) {
  answer(id: $rId) {
    id
    answer
  }
}"""

[<Fact>]
let ``Should be able to print a query with inline fragments`` () =
    printAndAssert """query q($myId: String!, $hisId: String!) {
  myHero: hero(id: $myId) {
    id
    name
  }
  hisHero: hero(id: $hisId) {
    id
    name
    friends {
      ... on Human {
        homePlanet
      }
      ... on Droid {
        primaryFunction
      }
    }
  }
  otherHero: hero(id: "1002") {
    name
    friends {
      ...friend
    }
  }
}

fragment friend on Character {
  ... on Human {
    id
    homePlanet
  }
  ... on Droid {
    id
    primaryFunction
  }
}"""

[<Fact>]
let ``Should be able to print arguments inside fragment spreads and default variable values`` () =
    printAndAssert """query HeroComparison($first: Int = 3) {
  leftComparison: hero(episode: EMPIRE) {
    ...comparisonFields
  }
  rightComparison: hero(episode: JEDI) {
    ...comparisonFields
  }
}

fragment comparisonFields on Character {
  name
  friendsConnection(first: $first) {
    totalCount
    edges {
      node {
        name
      }
    }
  }
}"""

[<Fact>]
let ``Should be able to print directives`` () =
    printAndAssert """query Hero($episode: Episode, $withFriends: Boolean!) {
  hero(episode: $episode) {
    name
    friends @include(if: $withFriends) {
      name
    }
  }
}"""

[<Fact>]
let ``Should be able to print multiple directives and arguments`` () =
    printAndAssert """query q($skip: Boolean!) {
  hero(id: "1000") {
    name
    friends(first: 1, name_starts_with: "D") @defer @skip(if: $skip) {
      ... on Human {
        id
        homePlanet
      }
      ... on Droid {
        id
        primaryFunction
      }
    }
  }
}"""

[<Fact>]
let ``Should be able to print a mutation`` () =
    printAndAssert """mutation CreateReviewForEpisode($ep: Episode!, $review: ReviewInput!) {
  createReview(episode: $ep, review: $review) {
    stars
    commentary
  }
}"""

[<Fact>]
let ``Should be able to print a subscription`` () =
    printAndAssert """subscription onCommentAdded($repoFullName: String!) {
  commentAdded(repoFullName: $repoFullName) {
    id
    content
  }
}"""

[<Fact>]
let ``Should be able to print type name meta field`` () =
    let expected = normalize """query q {
  hero(id: "1000") {
    name
    friends {
      ... on Human {
        id
        homePlanet
        __typename
      }
      ... on Droid {
        id
        primaryFunction
        __typename
      }
      __typename
    }
    __typename
  }
  __typename
}"""
    let query = """query q {
  hero(id: "1000") {
    name
    friends {
      ... on Human {
        id
        homePlanet
      }
      ... on Droid {
        id
        primaryFunction
      }
    }
  }
}
"""
    let document = parse query
    let actual = normalize <| document.ToQueryString(QueryStringPrintingOptions.IncludeTypeNames)
    actual |> equals expected

[<Fact>]
let ``Should generate information map correctly`` () =
    let query = """query q {
  hero(id: "1000") {
    name
    friends {
      ... on Human {
        id
        homePlanet
      }
      ... on Droid {
        id
        primaryFunction
      }
    }
  }
}
"""
    let document = parse query
    let actual = document.GetInfoMap() |> Map.toList
    let expected = [(Some "q", [TypeField
     {Name = "hero";
      Alias = None;
      Fields =
       [TypeField
          {Name = "friends";
           Alias = None;
           Fields =
            [FragmentField {Name = "primaryFunction";
                            Alias = None;
                            TypeCondition = "Droid";
                            Fields = [];};
             FragmentField {Name = "id";
                            Alias = None;
                            TypeCondition = "Droid";
                            Fields = [];};
             FragmentField {Name = "homePlanet";
                            Alias = None;
                            TypeCondition = "Human";
                            Fields = [];};
             FragmentField {Name = "id";
                            Alias = None;
                            TypeCondition = "Human";
                            Fields = [];}];}; TypeField {Name = "name";
                                                         Alias = None;
                                                         Fields = [];}];}])]
    actual |> equals expected