#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/typeproviders/fsharp41/netstandard2.0/Microsoft.Extensions.Http.dll"
#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/netstandard2.0/FSharp.Data.GraphQL.Shared.dll"
#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/netstandard2.0/FSharp.Data.GraphQL.Client.dll"

open FSharp.Data.GraphQL
open System.IO

let [<Literal>] ServerUrl = "http://localhost:8085"
let [<Literal>] EmptyGuidAsString = "00000000-0000-0000-0000-000000000000"

type Provider = GraphQLProvider<ServerUrl, uploadInputTypeName = "Upload", explicitOptionalParameters = false>

let context = Provider.GetContext(ServerUrl)

type Input = Provider.Types.Input
type InputField = Provider.Types.InputField

let operation =
    Provider.Operation<"""query Q($input: Input) {
        echo(input: $input) {
          single {
            ...Field
          }
          list {
            ...Field
          }
        }
      }

      fragment Field on OutputField {
        string
        stringOption
        int
        intOption
        uri
        deprecated
        guid
      }""">()
let fileOperation =
    Provider.Operation<"""mutation SingleUpload($file: Upload!) {
        singleUpload(file: $file) {
          name
          contentType
          contentAsText
        }
      }""">()

let filesOperation =
    Provider.Operation<"""mutation nullablesMultipleNullableUpload($files: [Upload]) {
        nullableMultipleNullableUpload(files: $files) {
          name
          contentType
          contentAsText
        }
      }""">()


let bytes = "Foo Bar"B
let upload = new Upload(bytes, "foo.txt")
let result = fileOperation.Run(upload)
result.Data




let uploads = [| Some(new Upload("Foo Bar"B, "foo.txt")); None|]
let results = filesOperation.Run(uploads)
