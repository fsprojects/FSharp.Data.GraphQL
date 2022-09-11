// The MIT License (MIT)
/// Copyright (c) 2015-Mar 2016 Kevin Thompson @kthompson
// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.ParserTests

open System
open Xunit
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Parser

let test ast query =
    let result = parse query
    equals ast result

let docN definitions = { Document.Definitions = definitions }
let doc1 definition = docN [ definition ]

let namedQueryWithVariablesAndSelections name variables selections =
    OperationDefinition { OperationType = Query
                          Name = Some name
                          Directives = []
                          VariableDefinitions = variables
                          SelectionSet = selections }

let namedQueryWithVariableAndSelection name variable selection = namedQueryWithVariablesAndSelections name [ variable ] [ selection ]
let namedQueryWithSelections name = namedQueryWithVariablesAndSelections name []
let namedQuerWithSelection name selection = namedQueryWithSelections name [ selection ]
let queryWithSelections selections =
    OperationDefinition { OperationType = Query
                          Name = None
                          Directives = []
                          VariableDefinitions = []
                          SelectionSet = selections }
let queryWithSelection selection = queryWithSelections [ selection ]

let arg name value = { Argument.Name = name; Value = value }
let argInt name value = arg name (IntValue (int64 value))
let argString name value = arg name (StringValue value)
let argNull name = arg name NullValue
let fieldWithNameAndArgsAndSelections name arguments selections =
    Field { Name = name
            Alias = None
            Directives = []
            Arguments = arguments
            SelectionSet = selections }

let fieldWithNameArgsAndSelection name arguments selection = fieldWithNameAndArgsAndSelections name arguments [ selection ]
let fieldWithNameAndArgs name arguments = fieldWithNameAndArgsAndSelections name arguments []
let fieldWithNameAndSelections name = fieldWithNameAndArgsAndSelections name []
let fieldWithNameAndSelection name selection = fieldWithNameAndSelections name [ selection ]
let field name = fieldWithNameAndSelections name []

let withAlias alias x =
    match x with
    | Field f -> Field { f with Alias = Some alias }
    | _ -> x

let fragment name typeCondition selections =
    FragmentDefinition { Name = Some name
                         TypeCondition = Some typeCondition
                         Directives = []
                         SelectionSet = selections }

let fragmentWithCondAndSelection name typeCondition selection = fragment name typeCondition [ selection ]

let inlieFragment typeCondition selections =
    InlineFragment { Name = None
                     TypeCondition = Some typeCondition
                     Directives = []
                     SelectionSet = selections }

let inlineFragmentWithCondAndSelection typeCondition selection = inlieFragment typeCondition [ selection ]

let spreadWithDirectives name directives = FragmentSpread { Name = name; Directives = directives }

let spreadWithDirective name directive = spreadWithDirectives name [ directive ]
let spread name = spreadWithDirectives name []
let varType list nullable name = NamedType name
let valueType = varType false false
let refType = varType false true
let listType = varType true false

let var vtype defaultValue name = { VariableName = name; Type = vtype; DefaultValue = defaultValue }
let directive name arguments = { Directive.Name = name; Arguments = arguments }
let directive1 name argument = directive name [ argument ]

[<Fact>]
let ``parser should parse empty query``() =
    let expected = docN []
    test expected ""

[<Fact>]
let ``parser should parse empty query with whitespace``() =
    let expected = docN []
    test expected """
  """

[<Fact>]
let ``parser should parse simple query with single field``() =
    let expected =
        field "uri"
        |> queryWithSelection
        |> doc1
    test expected """{uri}"""

[<Fact>]
let ``parser should parse simple query with operation identifier, but no operation name``() =
    let expected =
        field "uri"
        |> queryWithSelection
        |> doc1
    test expected """query {uri}"""

[<Fact>]
let ``parser should parse simple query with single field with whitespace``() =
    let expected =
        field "uri"
        |> queryWithSelection
        |> doc1
    test expected """{
    uri
  }"""

[<Fact>]
let ``parser should parse simple fields``() =
    let expected =
        [ "uri"; "width"; "height" ]
        |> List.map field
        |> queryWithSelections
        |> doc1
    test expected """{uri,width,height}"""

[<Fact>]
let ``parser should parse simple fields with whitespace``() =
    let expected =
        [ "uri"; "width"; "height" ]
        |> List.map field
        |> queryWithSelections
        |> doc1
    test expected """{
    uri,
    width,
    height
  }"""

[<Fact>]
let ``parser should parse simple fields without commas whitespace``() =
    let expected =
        [ "uri"; "width"; "height" ]
        |> List.map field
        |> queryWithSelections
        |> doc1
    test expected """{
    uri
    width
    height
  }"""

[<Fact>]
let ``parser should parse nested field``() =
    let expected =
        [ "phone"; "name" ]
        |> List.map field
        |> fieldWithNameAndSelections "contact"
        |> queryWithSelection
        |> doc1
    test expected """{
    contact {
      phone,
      name
    }
  }"""

[<Fact>]
let ``parser should parse nested field no commas``() =
    let expected =
        [ "phone"; "name" ]
        |> List.map field
        |> fieldWithNameAndSelections "contact"
        |> queryWithSelection
        |> doc1
    test expected """{
    contact {
      phone
      name
    }
  }"""

[<Fact>]
let ``parser should parse nested fields``() =
    let contact =
        [ "phone"; "name" ]
        |> List.map field
        |> fieldWithNameAndSelections "contact"

    let expected =
        [ field "uri"; contact; field "height" ]
        |> queryWithSelections
        |> doc1

    test expected """{
    uri,
    contact {
      phone,
      name
    },
    height
  }"""

[<Fact>]
let ``parser should parse nested fields no commas``() =
    let contact =
        [ "phone"; "name" ]
        |> List.map field
        |> fieldWithNameAndSelections "contact"

    let expected =
        [ field "uri"; contact; field "height" ]
        |> queryWithSelections
        |> doc1

    test expected """{
    uri
    contact {
      phone
      name
    }
    height
  }"""

[<Fact>]
let ``parser should parse multi level nested fields``() =
    let profile = field "uri" |> fieldWithNameAndSelection "profile_picture"

    let author =
        [ field "name"; profile ]
        |> fieldWithNameAndSelections "author"

    let expected =
        [ author; field "text" ]
        |> namedQueryWithSelections "Story"
        |> doc1

    test expected """query Story {
        author {
          name,
          profile_picture {
            uri
          }
        },
        text
      }"""

[<Fact>]
let ``parser should parse GraphQL``() =
    let profile =
        [ field "uri"; field "width"; field "height" ]
        |> fieldWithNameAndArgsAndSelections "profilePicture" [ argInt "size" 50 ]

    let user =
        [ field "id"; field "name"; field "isViewerFriend"; profile ]
        |> fieldWithNameAndArgsAndSelections "user" [ argInt "id" 3500401 ]

    let expected =
        user
        |> queryWithSelection
        |> doc1

    test expected """{
    user(id: 3500401) {
      id,
      name,
      isViewerFriend,
      profilePicture(size: 50)  {
        uri,
        width,
        height
      }
    }
  }"""

[<Fact>]
let ``parser should parse query with null arguments``() =
    let expected =
        [ field "name" ]
        |> fieldWithNameAndArgsAndSelections "user" [ argNull "id" ]
        |> queryWithSelection
        |> doc1
    test expected """{
    user(id: null) {
      name
    }
  }"""

[<Fact>]
let ``parser should parse query with quoted arguments``() =
    let expected =
        [ field "id" ]
        |> fieldWithNameAndArgsAndSelections "search" [ argString "query" "the cow said \"moo\"!" ]
        |> queryWithSelection
        |> doc1
    test expected """{
    search(query: "the cow said \"moo\"!") {
      id
    }
  }"""

[<Fact>]
let ``parser should parse query with arguments``() =
    let expected =
        [ field "name" ]
        |> fieldWithNameAndArgsAndSelections "user" [ argInt "id" 4 ]
        |> queryWithSelection
        |> doc1
    test expected """{
      user(id: 4) {
        name
      }
  }"""

[<Fact>]
let ``parser should parse query with arguments 1``() =
    let expected =
        [ field "id"; field "name"; fieldWithNameAndArgs "profilePic" [ argInt "size" 100 ] ]
        |> fieldWithNameAndArgsAndSelections "user" [ argInt "id" 4 ]
        |> queryWithSelection
        |> doc1
    test expected """{
      user(id: 4) {
        id
        name
        profilePic(size: 100)
      }
    }"""

[<Fact>]
let ``parser should parse query with multiple arguments``() =
    let profilePic =
        [ argInt "width" 100; argInt "height" 50 ]
        |> fieldWithNameAndArgs "profilePic"

    let expected =
        [ field "id"; field "name"; profilePic ]
        |> fieldWithNameAndArgsAndSelections "user" [ argInt "id" 4 ]
        |> queryWithSelection
        |> doc1

    test expected """{
      user(id: 4) {
        id
        name
        profilePic(width: 100, height: 50)
      }
    }"""

[<Fact>]
let ``parser should parse query with field alias``() =
    let profilePic alias size =
        [ argInt "size" size ]
        |> fieldWithNameAndArgs "profilePic"
        |> withAlias alias

    let expected =
        [ field "id"; field "name"; profilePic "smallPic" 64; profilePic "bigPic" 1024 ]
        |> fieldWithNameAndArgsAndSelections "user" [ argInt "id" 4 ]
        |> queryWithSelection
        |> doc1

    test expected """{
      user(id: 4) {
        id
        name
        smallPic: profilePic(size: 64)
        bigPic:profilePic(size: 1024)
      }
    }"""

[<Fact>]
let ``parser should parse query with top level field alias``() =
    let expected =
        [ field "id"; field "name" ]
        |> fieldWithNameAndArgsAndSelections "user" [ argInt "id" 4 ]
        |> withAlias "zuck"
        |> queryWithSelection
        |> doc1
    test expected """{
      zuck: user(id: 4) {
        id
        name
      }
    }"""

[<Fact>]
let ``parser should parse query without fragments``() =
    let friends name =
        [ field "id"; field "name"; fieldWithNameAndArgs "profilePic" [ argInt "size" 50 ] ]
        |> fieldWithNameAndArgsAndSelections name [ argInt "first" 10 ]

    let expected =
        [ friends "friends"; friends "mutualFriends" ]
        |> fieldWithNameAndArgsAndSelections "user" [ argInt "id" 4 ]
        |> namedQuerWithSelection "noFragments"
        |> doc1

    test expected """query noFragments {
      user(id: 4) {
        friends(first: 10) {
          id
          name
          profilePic(size: 50)
        }
        mutualFriends(first: 10) {
          id
          name
          profilePic(size: 50)
        }
      }
    }"""

[<Fact>]
let ``parser should parse query with fragments ``() =
    let friends name = spread "friendFields" |> fieldWithNameArgsAndSelection name [ argInt "first" 10 ]

    let withFragments =
        [ friends "friends"; friends "mutualFriends" ]
        |> fieldWithNameAndArgsAndSelections "user" [ argInt "id" 4 ]
        |> namedQuerWithSelection "withFragments"

    let friendFields =
        [ field "id"; field "name"; fieldWithNameAndArgs "profilePic" [ argInt "size" 50 ] ]
        |> fragment "friendFields" "User"

    let expected = docN [ withFragments; friendFields ]
    test expected """query withFragments {
      user(id: 4) {
        friends(first: 10) {
          ...friendFields
        }
        mutualFriends(first: 10) {
          ...friendFields
        }
      }
    }
    fragment friendFields on User {
      id
      name
      profilePic(size: 50)
    }"""

[<Fact>]
let ``parser should parse query with nested fragments ``() =
    let friends name = spread "friendFields" |> fieldWithNameArgsAndSelection name [ argInt "first" 10 ]

    let withFragments =
        [ friends "friends"; friends "mutualFriends" ]
        |> fieldWithNameAndArgsAndSelections "user" [ argInt "id" 4 ]
        |> namedQuerWithSelection "withNestedFragments"

    let standardProfilePic = fieldWithNameAndArgs "profilePic" [ argInt "size" 50 ] |> fragmentWithCondAndSelection "standardProfilePic" "User"

    let friendFields =
        [ field "id"; field "name"; spread "standardProfilePic" ]
        |> fragment "friendFields" "User"

    let expected = docN [ withFragments; friendFields; standardProfilePic ]
    test expected """query withNestedFragments {
      user(id: 4) {
        friends(first: 10) {
          ...friendFields
        }
        mutualFriends(first: 10) {
          ...friendFields
        }
      }
    }
    fragment friendFields on User {
      id
      name
      ...standardProfilePic
    }
    fragment standardProfilePic on User {
      profilePic(size: 50)
    }"""

[<Fact>]
let ``parser should parse query with type conditions``() =
    let nestedFrag name typ fieldName =
        field "count"
        |> fieldWithNameAndSelection fieldName
        |> fragmentWithCondAndSelection name typ

    let userFragment = nestedFrag "userFragment" "User" "friends"
    let pageFragment = nestedFrag "pageFragment" "Page" "likers"

    let fragmentTyping =
        [ field "handle"
          spread "userFragment"
          spread "pageFragment" ]
        |> fieldWithNameAndArgsAndSelections "profiles" [ arg "handles" (ListValue [ StringValue "zuck"; StringValue "cocacola" ]) ]
        |> namedQuerWithSelection "FragmentTyping"

    let expected = [ fragmentTyping; userFragment; pageFragment ] |> docN
    test expected """query FragmentTyping {
      profiles(handles: ["zuck", "cocacola"]) {
        handle
        ...userFragment
        ...pageFragment
      }
    }
    fragment userFragment on User {
      friends {
        count
      }
    }
    fragment pageFragment on Page {
      likers {
        count
      }
    }"""

[<Fact>]
let ``parser should parse query with inline fragments``() =
    let nestedFragment typ fieldName =
        field "count"
        |> fieldWithNameAndSelection fieldName
        |> inlineFragmentWithCondAndSelection typ

    let userFragment = nestedFragment "User" "friends"
    let pageFragment = nestedFragment "Page" "likers"

    let inlineFragmentTyping =
        [ field "handle"; userFragment; pageFragment ]
        |> fieldWithNameAndArgsAndSelections "profiles" [ arg "handles" (ListValue [ StringValue "zuck"; StringValue "cocacola" ]) ]
        |> namedQuerWithSelection "inlineFragmentTyping"

    let expected = inlineFragmentTyping |> doc1
    test expected """query inlineFragmentTyping {
      profiles(handles: ["zuck", "cocacola"]) {
        handle
        ... on User {
          friends {
            count
          }
        }
        ... on Page {
          likers {
            count
          }
        }
      }
    }"""

[<Fact>]
let ``parser should parse query with fragment directives``() =
    let maybeFragment =
        field "name"
        |> fieldWithNameAndSelection "me"
        |> fragmentWithCondAndSelection "maybeFragment" "Query"

    let b = (refType "Boolean")
    let v = var b None "condition"

    let hasConditionalFragment =
        arg "if" (Variable "condition")
        |> directive1 "include"
        |> spreadWithDirective "maybeFragment"
        |> namedQueryWithVariableAndSelection "hasConditionalFragment" v

    let expected = docN [ hasConditionalFragment; maybeFragment ]
    test expected """query hasConditionalFragment($condition: Boolean) {
      ...maybeFragment @include(if: $condition)
    }
    fragment maybeFragment on Query {
      me {
        name
      }
    }
    """

[<Literal>]
let KitchenSink = """# Copyright (c) 2015, Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

query queryName($foo: ComplexType, $site: Site = MOBILE) {
  whoever123is: node(id: [123, 456]) {
    id ,
    ... on User @defer {
      field2 {
        id ,
        alias: field1(first:10, after:$foo,) @include(if: $foo) {
          id,
          ...frag
        }
      }
    }
    ... @skip(unless: $foo) {
      id
    }
    ... {
      id
    }
  }
}

mutation likeStory {
  like(story: 123) @defer {
    story {
      id
    }
  }
}

subscription StoryLikeSubscription($input: StoryLikeSubscribeInput) {
  storyLikeSubscribe(input: $input) {
    story {
      likers {
        count
      }
      likeSentence {
        text
      }
    }
  }
}

fragment frag on Friend {
  foo(size: $size, bar: $b, obj: {key: "value"})
}

{
  unnamed(truthy: true, falsey: false),
  query
}"""

[<Fact>]
let ``parser should parse kitchen sink``() =
    parse KitchenSink
