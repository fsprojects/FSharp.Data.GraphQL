# FSharp.Data.GraphQL

## WARNING: Work in progress
F# implementation of Facebook [GraphQL query language specification](https://facebook.github.io/graphql).

[![Build status](https://ci.appveyor.com/api/projects/status/yjsen9xyvqhyak4b?svg=true)](https://ci.appveyor.com/project/johnberzy-bazinga/fsharp-data-graphql)

## Quick start

```fsharp
type Person = 
    { FirstName: string
      LastName: string }

// define GraphQL type 
let PersonType = Define.Object(
    name = "Person",
    fields = [
        // property resolver will be auto-generated
        Define.Field("firstName", String)   
        // asynchronous explicit member resolver
        Define.AsyncField("lastName", String, resolve = fun context person -> async { return person.LastName })   
    ])
    
// include person as a root query of a schema
let schema = Schema(query = PersonType)

// retrieve person data
let johnSnow = { FirstName = "John"; LastName = "Snow" }
let reply = schema.AsyncExecute(parse "{ firstName, lastName }", johnSnow) |> Async.RunSynchronously
// #> map [("firstName", "John"); ("lastName", "Snow")] 
```

It's type safe. Things like invalid fields or invalid return types will be checked at compile time.

## Demo

You can checkout this project in work, by running [example Suave server](samples/server.fsx) from your FSI. It will run GraphQL server running of http://localhost:8083. You may decide to query it directly or by using [GraphiQL client](samples/graphiql-client). 

To run GraphiQL, simply run `npm i & npm start` from your console, and open your browser at http://localhost:8090/.

Example query:

```graphql
{
  hero(id:"1000") {
    id,
    name,
    appearsIn,
    homePlanet,
    friends {
      ... on Human {
        name
      }
      ... on Droid {
        name
      }
    }
  }
}
```

## Implementation progress

Missing parts:

- Complex objects as input arguments
- Friendly query validation messages