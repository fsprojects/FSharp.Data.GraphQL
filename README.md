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

You can checkout this project in work, by running [example Suave server](samples/graphiql/server.fsx) from your FSI, and calling it by sending example request:

    curl --form 'query={ hero(id: "1000") { id, name, appearsIn, friends { id,name } } }' http://localhost:8083/

## Implementation progress

Missing parts:

- Introspection API
- Query validation