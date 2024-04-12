// Uncomment those to use build script client assembly using netstandard2.0
//#r "../../../bin/FSharp.Data.GraphQL.Shared/netstandard2.0/FSharp.Data.GraphQL.Shared.dll"
//#r "../../../bin/FSharp.Data.GraphQL.Client/netstandard2.0/FSharp.Data.GraphQL.Client.dll"

//Uncomment those to use dotnet build command for the client assembly using netstandard2.0
#r "../../../src/FSharp.Data.GraphQL.Shared/bin/Debug/netstandard2.0/FSharp.Data.GraphQL.Shared.dll"
#r "../../../src/FSharp.Data.GraphQL.Client/bin/Debug/netstandard2.0/FSharp.Data.GraphQL.Client.dll"

open System.IO
open FSharp.Data.GraphQL

type MyProvider = GraphQLProvider<"http://localhost:3001/graphql", uploadInputTypeName = "Upload">

let mutation =
    MyProvider.Operation<"""mutation singleFileUpload($request : SingleUploadRequest!) {
      singleUpload(request : $request) {
        id
        path
        filename
        mimetype
      }
    }""">()

let upload() =
    use upload = new Upload(File.OpenRead("txt_file.txt"), "text.txt", ownsStream = true)
    let input = MyProvider.Types.SingleUploadRequest(upload)
    let result = mutation.Run(input)
    printfn "Data: %A" result.Data

upload()
