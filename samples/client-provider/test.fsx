// Uncomment those to use build script client assembly
//#r "../../bin/FSharp.Data.GraphQL.Client/ne47/FSharp.Data.GraphQL.Client.dll"
//#r "../../bin/FSharp.Data.GraphQL.Shared/ne47/FSharp.Data.GraphQL.Shared.dll"

// Uncomment those to use dotnet build command for the client assembly
#r "../../src/FSharp.Data.GraphQL.Shared/bin/Debug/net47/FSharp.Data.GraphQL.Shared.dll"
#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/net47/FSharp.Data.GraphQL.Client.dll"

open FSharp.Data.GraphQL

type MyProvider = GraphQLProvider<"introspection.json">

let ctx = MyProvider.GetContext("http://localhost:8084")

// let queryType =
//   ctx.Schema.Types
//   |> Array.find (fun t -> t.Name.ToLowerInvariant() = ctx.Schema.QueryType.Name.Value.ToLowerInvariant())

// printfn "Query type: %A" queryType

let x = MyEnum.Item1
let y = MyEnum.Item2

printfn "%O" x
printfn "%O" y

// let res = 
//     ctx.Query<"""query q {
//       hero (id: "1000") {
//         name
//         appearsIn
//         homePlanet
//         appearsIn
//         friends {
//           ... on Human {
//             name
//             homePlanet
//           }
//           ... on Droid {
//             name
//             primaryFunction
//           }
//         }
//       }
//     }""">()
//     |> Async.RunSynchronously

// printfn "Output: %s" res