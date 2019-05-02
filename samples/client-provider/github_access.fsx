// Uncomment those to use build script client assembly
// #r "../../bin/FSharp.Data.GraphQL.Client/net461/FSharp.Data.GraphQL.Client.dll"
// #r "../../bin/FSharp.Data.GraphQL.Client/net461/FSharp.Data.GraphQL.Shared.dll"

// Uncomment those to use build script client assembly using netstandard2.0
// #r "../../bin/FSharp.Data.GraphQL.Client/netstandard2.0/FSharp.Data.GraphQL.Shared.dll"
// #r "../../bin/FSharp.Data.GraphQL.Client/netstandard2.0/FSharp.Data.GraphQL.Client.dll"
// #r "../../bin/FSharp.Data.GraphQL.Client/typeproviders/fsharp41/net461/netstandard.dll"

// Uncomment those to use dotnet build command for the client assembly
// #r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/net461/FSharp.Data.GraphQL.Shared.dll"
// #r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/net461/FSharp.Data.GraphQL.Client.dll"

// Uncomment those to use dotnet build command for the client assembly using netstandard2.0
#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/netstandard2.0/FSharp.Data.GraphQL.Shared.dll"
#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/netstandard2.0/FSharp.Data.GraphQL.Client.dll"
#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/typeproviders/fsharp41/net461/netstandard.dll"

open FSharp.Data.GraphQL

// Some GraphQL API's gives access to their schema via GET method, whithout need to anthenticate via headers.
// The provider automatically tries to get the schema via GET method first. If it does not work,
// The classical way via POST is done.
type MyProvider = GraphQLProvider<"github_schema.json">

let operation = MyProvider.Operation<"""query q { viewer { login } }""">()

let headers = HttpHeaders.ofFile "github_authorization_headers.headerfile"

let runtimeContext = MyProvider.GetContext(serverUrl = "https://api.github.com/graphql", httpHeaders = headers)

// Dispose runtime context after using it.
try
    let result = operation.Run(runtimeContext)
    printfn "Data: %A\n" result.Data
finally
    runtimeContext.Dispose()