// Uncomment those to use build script client assembly
// #r "../../bin/FSharp.Data.GraphQL.Client/net461/FSharp.Data.GraphQL.Client.dll"
// #r "../../bin/FSharp.Data.GraphQL.Shared/net461/FSharp.Data.GraphQL.Shared.dll"
// #r "../../bin/FSharp.Data.GraphQL.Client/typeproviders/fsharp41/net461/FSharp.Data.GraphQL.Client.DesignTime.dll"
// #r "../../bin/FSharp.Data.GraphQL.Client/typeproviders/fsharp41/net461/netstandard.dll"

// Uncomment those to use build script client assembly using netstandard2.0
// #r "../../bin/FSharp.Data.GraphQL.Shared/netstandard2.0/FSharp.Data.GraphQL.Shared.dll"
// #r "../../bin/FSharp.Data.GraphQL.Client/netstandard2.0/FSharp.Data.GraphQL.Client.dll"
// #r "../../bin/FSharp.Data.GraphQL.Client/typeproviders/fsharp41/netstandard2.0/FSharp.Data.GraphQL.Client.DesignTime.dll"
// #r "../../bin/FSharp.Data.GraphQL.Client/typeproviders/fsharp41/net461/netstandard.dll"

// Uncomment those to use dotnet build command for the client assembly
// #r "../../src/FSharp.Data.GraphQL.Shared/bin/Debug/net461/FSharp.Data.GraphQL.Shared.dll"
// #r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/typeproviders/fsharp41/net461/FSharp.Data.GraphQL.Client.DesignTime.dll"
// #r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/typeproviders/fsharp41/net461/netstandard.dll"

// Uncomment those to use dotnet build command for the client assembly using netstandard2.0
#r "../../src/FSharp.Data.GraphQL.Shared/bin/Debug/netstandard2.0/FSharp.Data.GraphQL.Shared.dll"
#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/netstandard2.0/FSharp.Data.GraphQL.Client.dll"
#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/typeproviders/fsharp41/netstandard2.0/FSharp.Data.GraphQL.Client.DesignTime.dll"
#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/typeproviders/fsharp41/net461/netstandard.dll"

open FSharp.Data.GraphQL.Client
open System.Net

let connection = new GraphQLConnection()

let request : GraphQLRequest =
    { Query = """query q { viewer { login } }"""
      Variables = [||]
      ServerUrl = "https://api.github.com/graphql"
      HttpHeaders = 
            [| "Authorization", "bearer [your bearer token here]"
               "User-Agent", "[your github username here]" |]
      OperationName = Some "q" }

let response = GraphQLClient.sendRequest connection request

printfn "%s" response

connection.Dispose()