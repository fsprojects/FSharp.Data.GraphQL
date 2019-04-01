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
    ctx.Operation<"""mutation m {
      setMoon (id: "1", isMoon: true) {
        id
        name
        isMoon
      }
    }""">()

let result = operation.Run()

// Mutations are also supported.
// Subscriptions are not fully supported at the moment.
// Only direct responses will be received - we don't have a socket base connection implemented.
printfn "Mutation result: %A" result.Data