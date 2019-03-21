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
    ctx.Query<"""query testQuery {
      things {
        id
        format
      }
    }""">()

let result = res.Run()

let things = result.Data.Things

let balls = result.Data.Things |> Array.choose (fun x -> x.TryAsBall())

let boxes = result.Data.Things |> Array.choose (fun x -> x.TryAsBox())

printfn "Things: %A\n\n" (things |> Array.map (fun thing -> thing.Id, thing.Format))

printfn "Balls: %A\n\n" balls

printfn "Boxes: %A\n\n" boxes