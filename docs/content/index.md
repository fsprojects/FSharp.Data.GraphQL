
FSharp.Data.GraphQL
======================

FSharp.Data.GraphQL is a client and server implementation of Facebook's [GraphQL](http://graphql.org/) query language.

It's a standard created for building web service APIs and a runtime for defining those APIs in statically typed, well
formed way. The core idea is to define web service in context of its capabilities in oposition to routees known from existing RESTful APIs. Capabilities are defined in form of GraphQL schema and describe all operations and data allowed to be requested by the client, without fragmenting it into particular routes.

The FSharp.Data.GraphQL library can be installed from NuGet on the [server](https://www.nuget.org/packages/FSharp.Data.GraphQL.Server) or a [client](https://www.nuget.org/packages/FSharp.Data.GraphQL.Client):
    
    PM> Install-Package FSharp.Data.GraphQL.Server -Pre
    PM> Install-Package FSharp.Data.GraphQL.Client -Pre
    
## Quick start

To use FSharp.Data.GraphQL on the server side, first define some data working as a source:

```fsharp
type Person = { FirstName: string; LastName: string }
let people = [ 
    { FirstName = "Jane"; LastName = "Milton" }
    { FirstName = "Travis"; LastName = "Smith" } ]
```

Then expose it through the schema - GraphQL language defines it's own type system, that can be integrated with any other programming language:

```fsharp
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types

// GraphQL type definition for Person type
let Person = Define.Object("Person", [
    Define.Field("firstName", String, fun ctx p -> p.FirstName)
    Define.Field("lastName", String, fun ctx p -> p.LastName)  
])

// Each schema must define so-called root query
let QueryRoot = Define.Object("Query", [
    Define.Field("people", ListOf Person, fun ctx () -> people)
])

// Then initialize everything as part of schema
let schema = Schema(QueryRoot)
```

With schema create we are now able to respond to any incoming GraphQL queries:

```fsharp
open FSharp.Data.GraphQL.Execution
let query = """
    query Example {
        people {
            firstName
        }
    }
    """
async {
    let! response = schema.AsyncExecute(query)
    printf "%A" response
}

```

## More examples

For more examples, clone [FSharp.Data.GraphQL github repository](https://github.com/bazingatechnologies/FSharp.Data.GraphQL) and see the **samples** folder. There, your can find:

- A mandatory Star Wars schema introduction using [GraphiQL](https://github.com/graphql/graphiql) client.
- An example using [RelayJS](https://facebook.github.io/relay/) data structures (which this library supports).
- A client example, using type providers to operate on any GraphQL schema available - worth noticing: it's compatbile with [Fable](https://fable-compiler.github.io/) compiler!
- A fully fledged ToDo application, where *FSharp.Data.GraphQL* is used for both server and client.

Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding a new public API, please also 
consider adding [samples][content] that can be turned into a documentation. You might
also want to read the [library design notes][readme] to understand how it works.

The library is available under Public Domain license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/bazingatechnologies/FSharp.Data.GraphQL/tree/master/docs/content
  [gh]: https://github.com/bazingatechnologies/FSharp.Data.GraphQL
  [issues]: https://github.com/bazingatechnologies/FSharp.Data.GraphQL/issues
  [readme]: https://github.com/bazingatechnologies/FSharp.Data.GraphQL/blob/master/README.md
  [license]: https://github.com/bazingatechnologies/FSharp.Data.GraphQL/blob/master/LICENSE.txt

