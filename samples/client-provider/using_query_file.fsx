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

type MyProvider = GraphQLProvider<"http://localhost:8084">

let ctx = MyProvider.GetContext()

// If you pass a query file, it will load the query from it.
let operation = 
    ctx.Operation<"""query q {
      hero(id: "1000") {
        name
        appearsIn
        homePlanet
        friends {
          ...FullFriends
        }
      }
    }

    fragment Friends on Character {
      ... on Human {
        name
        appearsIn
        homePlanet
      }
      ... on Droid {
        name
        appearsIn
        primaryFunction
      }
    }

    fragment FullFriends on Character {
      ... on Human {
        name
        appearsIn
        homePlanet
        friends {
          ...Friends
        }
      }
      ... on Droid {
        name
        appearsIn
        primaryFunction
        friends {
          ...Friends
        }
      }
    }""">()

let sw = Stopwatch()
sw.Start()
let result = operation.Run()
sw.Stop()
let ts = sw.Elapsed

printfn "Request time: %s" (System.String.Format("{0:00}:{1:00}:{2:00}.{3:00}", ts.Hours, ts.Minutes, ts.Seconds, ts.Milliseconds / 10))
printfn "Data: %A\n" result.Data
printfn "Errors: %A\n" result.Errors
printfn "Custom data: %A\n" result.CustomData

// Operations are disposable objects. Be sure to dispose their resources after using them.
operation.Dispose()