(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../src/FSharp.Data.GraphQL.Server/bin/Release"

(**
FSharp.Data.GraphQL
======================

Documentation

The FSharp.Data.GraphQL library can be installed from [NuGet](https://www.nuget.org/packages?q=FSharp.Data.Graphql):
    
    PM> Install-Package FSharp.Data.GraphQL
    
First steps
-------

This example demonstrates how to define your own GraphQL schema.

*)
#r "../../src/FSharp.Data.GraphQL.Server/bin/Release/FSharp.Data.GraphQL.Shared.dll"
#r "../../src/FSharp.Data.GraphQL.Server/bin/Release/FSharp.Data.GraphQL.Server.dll"
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Execution

// define your types and data source
type Person = { FirstName: string; LastName: string }
let people = [ 
    { FirstName = "Jane"; LastName = "Milton" }
    { FirstName = "Travis"; LastName = "Smith" } ]

// now define it's GraphQL representation
let Person = Define.Object("Person", [
    Define.Field("firstName", String, fun ctx p -> p.FirstName)
    Define.Field("lastName", String, fun ctx p -> p.LastName)  
])

let QueryRoot = Define.Object("Query", [
    Define.Field("people", ListOf Person, fun ctx () -> people)
])

// then initialize everything as part of schema
let schema = Schema(QueryRoot)

// now you can execute graphql schema
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

(**

More examples
-------------

For more examples, see the samples folder. There, your can find:

- A mandatory Star Wars schema introduction using [GraphiQL](https://github.com/graphql/graphiql) client.
- An example using Relay.JS data structures (which this library supports).
- A client example, using type providers to operate on any GraphQL schema available - worth noticing: it's compatbile with [Fable](https://fable-compiler.github.io/)!
- A fully fledged TODO application, where *FSharp.Data.GraphQL* is used for both server and client.

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
*)
