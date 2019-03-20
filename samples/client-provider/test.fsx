// Uncomment those to use build script client assembly
//#r "../../bin/FSharp.Data.GraphQL.Client/ne47/FSharp.Data.GraphQL.Client.dll"
//#r "../../bin/FSharp.Data.GraphQL.Shared/ne47/FSharp.Data.GraphQL.Shared.dll"

// Uncomment those to use dotnet build command for the client assembly
#r "../../src/FSharp.Data.GraphQL.Shared/bin/Debug/net47/FSharp.Data.GraphQL.Shared.dll"
#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/net47/FSharp.Data.GraphQL.Client.dll"

open FSharp.Data.GraphQL

type MyProvider = GraphQLProvider<"http://localhost:8084">

let ctx = MyProvider.GetContext()

let res = 
    ctx.Query<"""query q {
      hero (id: "1000") {
        name
        appearsIn
        homePlanet
        appearsIn
        friends {
          ... on Human {
            name
            homePlanet
          }
          ... on Droid {
            name
            primaryFunction
          }
        }
      }
    }""">()
  
printfn "Operation name: %A" res.OperationName
printfn "Headers: %A" res.CustomHttpHeaders
printfn "Server: %s" res.ServerURl

let result = res.Run()

let data = result.Data

printfn "Data: %A" data

let hero = data.Hero.Value

printfn "Hero name: %s" hero.Name.Value

let friends = hero.Friends |> Array.choose id

printfn "Hero friends : %A" friends

printfn "Hero friends type: %A" (friends.GetType())