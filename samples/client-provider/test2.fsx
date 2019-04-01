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
    ctx.Operation<"""query testQuery {
      things {
        id
        format
        ...on Ball {
          form
        }
        ...on Box {
          form
        }
      }
    }""">()

let result = operation.Run()
let things = result.Data.Value.Things

// Interfaces can be casted to the implementations the same way done with
// union types. Interfaces are returned as objects instead of CLR interfaces, though.
// That's because provided interfaces can't have fields at the moment because of a bug on the type provider SDK.
// See https://github.com/fsprojects/FSharp.TypeProviders.SDK/issues/211 for details.
let balls = things |> Array.choose (fun x -> x.TryAsBall())
let boxes = things |> Array.choose (fun x -> x.TryAsBox())

// Interface fields are mapped, as long as they are objects from the operation result.
// Input object interfaces can not have their fields mapped, because of a limitation
// on the type provider system itself.
// See https://github.com/fsprojects/FSharp.TypeProviders.SDK/issues/211 for details.
printfn "Things: %A\n\n" (things |> Array.map (fun thing -> thing.Id, thing.Format))

printfn "Balls: %A\n\n" balls

// Form is a deprecated field in the server schema. It is also marked as deprecated on the provided property.
printfn "Boxes: %A\n\n" (boxes |> Array.map (fun box -> box.Id, box.Form))