// Uncomment those to use build script client assembly
//#r "../../bin/FSharp.Data.GraphQL.Client/net47/FSharp.Data.GraphQL.Client.dll"
//#r "../../bin/FSharp.Data.GraphQL.Shared/net47/FSharp.Data.GraphQL.Shared.dll"

// Uncomment those to use build script client assembly using netstandard2.0
//#r "../../bin/FSharp.Data.GraphQL.Shared/netstandard2.0/FSharp.Data.GraphQL.Shared.dll"
//#r "../../bin/FSharp.Data.GraphQL.Client/netstandard2.0/netstandard.dll"
//#r "../../bin/FSharp.Data.GraphQL.Client/netstandard2.0/FSharp.Data.GraphQL.Client.dll"

// Uncomment those to use dotnet build command for the client assembly
// #r "../../src/FSharp.Data.GraphQL.Shared/bin/Debug/net47/FSharp.Data.GraphQL.Shared.dll"
// #r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/net47/FSharp.Data.GraphQL.Client.dll"

//Uncomment those to use dotnet build command for the client assembly using netstandard2.0
#r "../../src/FSharp.Data.GraphQL.Shared/bin/Debug/netstandard2.0/FSharp.Data.GraphQL.Shared.dll"
#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/netstandard2.0/netstandard.dll"
#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/netstandard2.0/FSharp.Data.GraphQL.Client.dll"

open FSharp.Data.GraphQL
open System.Diagnostics

// Some GraphQL API's gives access to their schema via GET method, whithout need to anthenticate via headers.
// The provider automatically tries to get the schema via GET method first. If it does not work,
// The classical way via POST is done.
type MyProvider = GraphQLProvider<"github_schema.json">

let context = MyProvider.GetContext("https://api.github.com/graphql")

let operation =
    context.Operation<"""query q { viewer { login } }""">()

let run () =
    let sw = Stopwatch()
    sw.Start()
    // You need to edit the headers file before making this call at runtime.
    // Be sure to check out how to generate GitHub access tokens first.
    let result = operation.Run("github_authorization_headers.headerfile")
    sw.Stop()
    printfn "Elapsed: %ims" sw.ElapsedMilliseconds
    printfn "Data: %A\n" result.Data

run()