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

open FSharp.Data.GraphQL

type MyProvider = GraphQLProvider<"http://localhost:8084">

let operation = 
    MyProvider.Operation<"""query q($id: String!) {
      hero(id: $id) {
        name
        friends {
          ... on Human {
            id
            homePlanet
          }
          ... on Droid {
            id
            primaryFunction
          }
        }
      }
    }""">()

let result = operation.Run(id = "1000")

printfn "Result: %A" result.Data
printfn "Errors: %A" result.Errors