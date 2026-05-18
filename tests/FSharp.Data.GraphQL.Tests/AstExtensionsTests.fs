// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.AstExtensionsTests

open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Ast.Extensions

/// Converts line breaks to a single standard to avoid different SO line break termination issues.
let normalize (str : string) = str.Replace ("\r\n", "\n")

/// Generates an Ast.Document from a query string, prints it to another
/// query string and expects it to be equal. Input query must be formatted (with line breaks and identation).
/// Identation unit is two empty spaces.
let private printAndAssert (query : string) =
    let document = parse query
    let expected = normalize query
    let actual = normalize <| document.ToQueryString ()
    actual |> equals expected

[<Fact>]
let ``Can print a simple query`` () =
    printAndAssert
        """query q {
  hero {
    name
  }
}"""

[<Fact>]
let ``Can print a simple query with 2 fields`` () =
    printAndAssert
        """query q {
  hero {
    id
    name
  }
}"""

[<Fact>]
let ``Can print a query with variables`` () =
    printAndAssert
        """query q($id: String!) {
  hero(id: $id) {
    id
    name
  }
}"""

[<Fact>]
let ``Can parse a query with an object input in the internal method`` () =
    printAndAssert
        """mutation q($id: String!, $name: String!) {
  addHero(input: { id: $id, label: $name })
}"""

[<Fact>]
let ``Can parse a query with an object having array properties input in the internal method`` () =
    printAndAssert
        """mutation q($id: String!, $name: String!, $friend1: String!) {
  addHero(input: { friends: [ $friend1 ], id: $id, label: $name })
}"""

[<Fact>]
let ``Can parse a query with an object having multi-element array input in the internal method`` () =
    printAndAssert
        """mutation q($id: String!, $name: String!) {
  addHero(input: { friends: [ 7, 5, -3 ], id: $id, label: $name })
}"""

[<Fact>]
let ``Should be able print ObjectValue names properly`` () =
    printAndAssert
        """query GetCampaigns {
  campaigns(params: { limit: 100, offset: 0 }) {
    campaigns {
      code
    }
  }
}"""

[<Fact>]
let ``Can print a query with aliases`` () =
    printAndAssert
        """query q($myId: String!, $hisId: String!) {
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
let ``Can print a query with fragment spreads`` () =
    printAndAssert
        """query q($myId: String!, $hisId: String!) {
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
let ``Can print a short hand format query`` () =
    printAndAssert
        """{
  field1
  field2
}"""

[<Fact>]
let ``Should not print query without name in short hand format`` () =
    printAndAssert
        """query ($rId: Int) {
  answer(id: $rId) {
    id
    answer
  }
}"""

[<Fact>]
let ``Can print a query with inline fragments`` () =
    printAndAssert
        """query q($myId: String!, $hisId: String!) {
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
let ``Can print arguments inside fragment spreads and default variable values`` () =
    printAndAssert
        """query HeroComparison($first: Int = 3) {
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
let ``Can print directives`` () =
    printAndAssert
        """query Hero($episode: Episode, $withFriends: Boolean!) {
  hero(episode: $episode) {
    name
    friends @include(if: $withFriends) {
      name
    }
  }
}"""

[<Fact>]
let ``Can print multiple directives and arguments`` () =
    printAndAssert
        """query q($skip: Boolean!) {
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
let ``Can print a mutation`` () =
    printAndAssert
        """mutation CreateReviewForEpisode($ep: Episode!, $review: ReviewInput!) {
  createReview(episode: $ep, review: $review) {
    stars
    commentary
  }
}"""

[<Fact>]
let ``Can print a subscription`` () =
    printAndAssert
        """subscription onCommentAdded($repoFullName: String!) {
  commentAdded(repoFullName: $repoFullName) {
    id
    content
  }
}"""

[<Fact>]
let ``Can print type name meta field`` () =
    let expected =
        normalize
            """query q {
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

    let query =
        """query q {
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
    let actual =
        normalize
        <| document.ToQueryString (QueryStringPrintingOptions.IncludeTypeNames)
    actual |> equals expected

[<Fact>]
let ``Should generate information map correctly`` () =
    let query =
        """query q {
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
    let actual = document.GetInfoMap () |> Map.toList

    let expected = [
        (ValueSome "q",
         [
             TypeField {
                 Name = "hero"
                 Alias = ValueNone
                 Fields = [
                     TypeField {
                         Name = "friends"
                         Alias = ValueNone
                         Fields = [
                             FragmentField {
                                 Name = "primaryFunction"
                                 Alias = ValueNone
                                 TypeCondition = "Droid"
                                 Fields = []
                             }
                             FragmentField { Name = "id"; Alias = ValueNone; TypeCondition = "Droid"; Fields = [] }
                             FragmentField { Name = "homePlanet"; Alias = ValueNone; TypeCondition = "Human"; Fields = [] }
                             FragmentField { Name = "id"; Alias = ValueNone; TypeCondition = "Human"; Fields = [] }
                         ]
                     }
                     TypeField { Name = "name"; Alias = ValueNone; Fields = [] }
                 ]
             }
         ])
    ]

    actual |> equals expected

[<Fact>]
let ``ToQueryString escapes double quotes in string values`` () =
    let query = """query q { hero(name: "test\"quote") }"""
    let document = parse query
    let printed = document.ToQueryString ()
    // Verify the printed query contains the escaped quote
    Assert.Contains ("\\\"", printed)
    // Verify it can be parsed back
    let reparsed = parse printed
    equals (document.ToQueryString ()) (reparsed.ToQueryString ())

[<Fact>]
let ``ToQueryString escapes backslashes in string values`` () =
    let query = """query q { hero(path: "C:\\Users\\test") }"""
    let document = parse query
    let printed = document.ToQueryString ()
    // Verify the printed query contains escaped backslashes
    Assert.Contains ("\\\\", printed)
    // Verify it can be parsed back
    let reparsed = parse printed
    equals (document.ToQueryString ()) (reparsed.ToQueryString ())

[<Fact>]
let ``ToQueryString escapes newlines in string values`` () =
    let query = """query q { hero(text: "line1\nline2") }"""
    let document = parse query
    let printed = document.ToQueryString ()
    // Verify the printed query contains the escaped newline within the string value
    Assert.Contains ("\\n", printed)
    // Verify the string value itself doesn't contain an actual newline (it should be escaped)
    // The printed output will have formatting newlines, but the string value should have \n
    Assert.Contains ("\"line1\\nline2\"", printed)
    // Verify it can be parsed back
    let reparsed = parse printed
    equals (document.ToQueryString ()) (reparsed.ToQueryString ())

[<Fact>]
let ``ToQueryString escapes tabs in string values`` () =
    let query = """query q { hero(text: "col1\tcol2") }"""
    let document = parse query
    let printed = document.ToQueryString ()
    // Verify the printed query contains the escaped tab within the string value
    Assert.Contains ("\\t", printed)
    Assert.Contains ("\"col1\\tcol2\"", printed)
    // Verify it can be parsed back
    let reparsed = parse printed
    equals (document.ToQueryString ()) (reparsed.ToQueryString ())

[<Fact>]
let ``ToQueryString escapes carriage returns in string values`` () =
    let query = """query q { hero(text: "line1\rline2") }"""
    let document = parse query
    let printed = document.ToQueryString ()
    // Verify the printed query contains the escaped carriage return
    Assert.Contains ("\\r", printed)
    // Verify it can be parsed back
    let reparsed = parse printed
    equals (document.ToQueryString ()) (reparsed.ToQueryString ())

[<Fact>]
let ``ToQueryString escapes backspace in string values`` () =
    let query = """query q { hero(text: "test\bback") }"""
    let document = parse query
    let printed = document.ToQueryString ()
    // Verify the printed query contains the escaped backspace
    Assert.Contains ("\\b", printed)
    // Verify it can be parsed back
    let reparsed = parse printed
    equals (document.ToQueryString ()) (reparsed.ToQueryString ())

[<Fact>]
let ``ToQueryString escapes form feed in string values`` () =
    let query = """query q { hero(text: "page1\fpage2") }"""
    let document = parse query
    let printed = document.ToQueryString ()
    // Verify the printed query contains the escaped form feed
    Assert.Contains ("\\f", printed)
    // Verify it can be parsed back
    let reparsed = parse printed
    equals (document.ToQueryString ()) (reparsed.ToQueryString ())

[<Fact>]
let ``ToQueryString escapes control characters as unicode in string values`` () =
    // Test with a control character (e.g., ASCII 0x01)
    let query = "query q { hero(text: \"test\u0001control\") }"
    let document = parse query
    let printed = document.ToQueryString ()
    // Verify the printed query contains the unicode escape (lowercase hex)
    Assert.Contains ("\\u0001", printed)
    // Verify it can be parsed back
    let reparsed = parse printed
    equals (document.ToQueryString ()) (reparsed.ToQueryString ())

[<Fact>]
let ``ToQueryString escapes multiple special characters correctly`` () =
    let query = """query q { hero(text: "quote:\"newline:\nslash:\\tab:\t") }"""
    let document = parse query
    let printed = document.ToQueryString ()
    // Verify all escapes are present
    Assert.Contains ("\\\"", printed)
    Assert.Contains ("\\n", printed)
    Assert.Contains ("\\\\", printed)
    Assert.Contains ("\\t", printed)
    // Verify it can be parsed back
    let reparsed = parse printed
    equals (document.ToQueryString ()) (reparsed.ToQueryString ())

[<Fact>]
let ``ToQueryString produces deterministic output for escaped strings`` () =
    // This test verifies that the same query with escaped strings produces
    // the same canonical output, which is critical for documentId stability
    let query = """query Test { field(arg: "test\"quote\nline\ttab\\back") }"""
    let document = parse query
    let printed1 = document.ToQueryString ()
    let printed2 = document.ToQueryString ()
    equals printed1 printed2
    // Verify the documentId is deterministic
    let documentId = DocumentId.fromCanonicalQuery printed1
    equals 64 documentId.Length // SHA-256 hex string is always 64 chars
