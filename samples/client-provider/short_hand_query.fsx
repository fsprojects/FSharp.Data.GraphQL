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
#r "../../src/FSharp.Data.GraphQL.Client/bin/Release/netstandard2.0/FSharp.Data.GraphQL.Shared.dll"
#r "../../src/FSharp.Data.GraphQL.Client/bin/Release/netstandard2.0/FSharp.Data.GraphQL.Client.dll"
#r "../../src/FSharp.Data.GraphQL.Client/bin/Release/typeproviders/fsharp41/net461/netstandard.dll"

open FSharp.Data.GraphQL

type MyProvider = GraphQLProvider<"sample_schema.json">

let o = 
    MyProvider.Operation<"""query Foo ($id2:Int) {
      hero {
        name
      }
    }""">()

let o = 
    MyProvider.Operation<"""query Foo ($id2:String) {
      hero {
        name
      }
    }""">()

let runtimeContext = MyProvider.GetContext(serverUrl = "http://localhost:8084")
o.Run()