// Uncomment those to use build script client assembly
//#r "../../bin/FSharp.Data.GraphQL.Client/ne47/FSharp.Data.GraphQL.Client.dll"
//#r "../../bin/FSharp.Data.GraphQL.Shared/ne47/FSharp.Data.GraphQL.Shared.dll"

// Uncomment those to use dotnet build command for the client assembly
#r "../../src/FSharp.Data.GraphQL.Shared/bin/Debug/net47/FSharp.Data.GraphQL.Shared.dll"
#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/net47/FSharp.Data.GraphQL.Client.dll"

open FSharp.Data.GraphQL

type MyProvider = GraphQLProvider<"http://localhost:8084">

let ctx = MyProvider.GetContext()




//// Variables are automatically parsed into the Run parameters.
//// The last parameter is optional and is always the custom HTTP Headers.
// let headers : (string * string) seq = upcast [||]
// let operation = 
//     ctx.Operation<"""query q($id: String!, $thing : Thing!) {
//       hero(id: $id) {
//         name
//         friends {
//           ... on Human {
//             id
//             homePlanet
//           }
//           ... on Droid {
//             id
//             primaryFunction
//           }
//         }
//       }
//     }""">()
//
//let ball = MyProvider.Types.Ball(form = "Spheric", format = "Spheric", id = "1")
//let result = operation.Run(customHttpHeaders = headers, id = "1000", thing = ball)




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
    }""">()

let result = operation.Run(id = "1000")

printfn "Result: %A" result.Data
printfn "Errors: %A" result.Errors