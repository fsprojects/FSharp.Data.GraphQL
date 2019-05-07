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

type MyProvider = GraphQLProvider<"http://localhost:8084">

let filter = MyProvider.Types.ThingFilter(format = "Cubic")

let operation = 
    MyProvider.Operation<"""query q($filter: ThingFilter) {
    things(filter: $filter) {
      id
      format
    }
  }""">()

let result = operation.Run()

printfn "Data: %A\n" result.Data
printfn "Errors: %A\n" result.Errors
printfn "Custom data: %A\n" result.CustomData