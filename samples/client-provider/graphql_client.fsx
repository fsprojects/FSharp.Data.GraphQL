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

open FSharp.Data.GraphQL.Client
open System.Diagnostics

let request : GraphQLRequest =
    { Query = """query q { viewer { login } }"""
      Variables = [||]
      ServerUrl = "https://api.github.com/graphql"
      CustomHeaders = 
            [| "Authorization", "bearer b473fe0ddc77cf55df7065ec9c0b51cdc4861bfa"
               "User-Agent", "ivelten" |]
      OperationName = Some "q" }

let sw = Stopwatch()
sw.Start()
let response = GraphQLClient.sendRequest request
sw.Stop()

printfn "Elapsed 1: %ims" sw.ElapsedMilliseconds
printfn "%s" response

sw.Restart()
let response2 = GraphQLClient.sendRequest request
sw.Stop()

printfn "Elapsed 2: %ims" sw.ElapsedMilliseconds
printfn "%s" response2