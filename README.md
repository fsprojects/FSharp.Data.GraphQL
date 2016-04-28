# FSharp.Data.GraphQL

## WARNING: Work in progress
F# implementation of Facebook [GraphQL query language specification](https://facebook.github.io/graphql).

[![Build status](https://ci.appveyor.com/api/projects/status/yjsen9xyvqhyak4b?svg=true)](https://ci.appveyor.com/project/johnberzy-bazinga/fsharp-data-graphql)

## Example

```fsharp
type Person = {
    FirstName: string
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

## Implementation progress

Missing parts:

- Fully functional Introspection API
- Query validation