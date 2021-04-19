/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.ParsingBenchmark

open System
open BenchmarkDotNet
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Parser

let simpleQueryString = """{
    hero(id: "1000") {
        id
    }
}"""

let complexQuery = """query IntrospectionQuery {
    __schema {
      queryType { name }
      mutationType { name }
      subscriptionType { name }
      types {
        ...FullType
      }
      directives {
        name
        description
        locations
        args {
          ...InputValue
        }
      }
    }
  }

  fragment FullType on __Type {
    kind
    name
    description
    fields(includeDeprecated: true) {
      name
      description
      args {
        ...InputValue
      }
      type {
        ...TypeRef
      }
      isDeprecated
      deprecationReason
    }
    inputFields {
      ...InputValue
    }
    interfaces {
      ...TypeRef
    }
    enumValues(includeDeprecated: true) {
      name
      description
      isDeprecated
      deprecationReason
    }
    possibleTypes {
      ...TypeRef
    }
  }

  fragment InputValue on __InputValue {
    name
    description
    type { ...TypeRef }
    defaultValue
  }

  fragment TypeRef on __Type {
    kind
    name
    ofType {
      kind
      name
      ofType {
        kind
        name
        ofType {
          kind
          name
        }
      }
    }
  }"""


open BenchmarkDotNet.Attributes

[<Config(typeof<GraphQLBenchConfig>)>]
type ParsingBenchmark() =
    [<GlobalSetup>] member x.Setup () = ()
    [<Benchmark>] member x.ParseSimpleQuery () =  parse simpleQueryString
    [<Benchmark>] member x.ParseComplexQuery () = parse complexQuery