# FSharp.Data.GraphQL

## WARNING: Work in progress
F# implementation of Facebook [GraphQL query language specification](https://facebook.github.io/graphql).

[![Build Status](https://travis-ci.org/bazingatechnologies/FSharp.Data.GraphQL.svg?branch=dev)](https://travis-ci.org/bazingatechnologies/FSharp.Data.GraphQL)
[![Build status](https://ci.appveyor.com/api/projects/status/mkjwu1dg9xn3jwox/branch/dev?svg=true)](https://ci.appveyor.com/project/bazingatechnologies/fsharp-data-graphql-ydavv/branch/dev)

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
        Define.AutoField("firstName", String)   
        // asynchronous explicit member resolver
        Define.AsyncField("lastName", String, resolve = fun context person -> async { return person.LastName })   
    ])
    
// include person as a root query of a schema
let schema = Schema(query = PersonType)

// retrieve person data
let johnSnow = { FirstName = "John"; LastName = "Snow" }
let reply = schema.AsyncExecute(parse "{ firstName, lastName }", johnSnow) |> Async.RunSynchronously
// #> { data: { "firstName", "John", "lastName", "Snow" } } 
```

It's type safe. Things like invalid fields or invalid return types will be checked at compile time.

## Demos

### GraphiQL client

Go to [GraphiQL sample directory](https://github.com/bazingatechnologies/FSharp.Data.GraphQL/tree/dev/samples/graphiql-client). In order to run it, build `FSharp.Data.GraphQL` project on Debug settings and run `server.fsx` with FSI - this will create a Suave.IO server compatible with GraphQL spec, running on port 8083. Then what you need is to run node.js graphiql frontend. To do so, run `npm i` to get all dependencies, and then run `npm run serve | npm run dev` - this will start a webpack server running on [http://localhost:8090/](http://localhost:8090/) . Visit this link, and GraphiQL edito should appear. You may try it by applying following query:

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

### Relay.js starter kit

[Second sample](https://github.com/bazingatechnologies/FSharp.Data.GraphQL/tree/dev/samples/) is a F#-backed version of of popular Relay Starter Kit - an example application using React.js + Relay with Relay-compatible server API.

To run it, build a `FSharp.Data.GraphQL` and `FSharp.Data.GraphQL.Relay` projects using Debug settings. Then start server by running `server.fsx` script in your FSI - this will start relay-compatible F# server on port 8083. Then build node.js frontend by getting all dependencies (`npm i`) and running it (`npm run serve | npm run dev`) - this will start webpack server running React application using Relay for managing application state. You can visit it on [http://localhost:8083/](http://localhost:8083/) .

In order to update client schema, visit [http://localhost:8083/](http://localhost:8083/) and copy-paste the response (which is introspection query result from current F# server) into *data/schema.json*.