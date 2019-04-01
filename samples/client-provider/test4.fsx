// Uncomment those to use build script client assembly
//#r "../../bin/FSharp.Data.GraphQL.Client/ne47/FSharp.Data.GraphQL.Client.dll"
//#r "../../bin/FSharp.Data.GraphQL.Shared/ne47/FSharp.Data.GraphQL.Shared.dll"

// Uncomment those to use dotnet build command for the client assembly
#r "../../src/FSharp.Data.GraphQL.Shared/bin/Debug/net47/FSharp.Data.GraphQL.Shared.dll"
#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/net47/FSharp.Data.GraphQL.Client.dll"

open FSharp.Data.GraphQL

type MyProvider = GraphQLProvider<"http://localhost:8084">

let ctx = MyProvider.GetContext()

let operation = 
    ctx.Operation<"""query q($id: String!) {
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
    }""">(customHttpHeaders)

let result = operation.Run()
let data = result.Data

printfn "Result: %A" result.Data