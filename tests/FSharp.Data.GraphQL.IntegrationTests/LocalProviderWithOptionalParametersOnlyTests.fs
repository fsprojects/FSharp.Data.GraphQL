module FSharp.Data.GraphQL.IntegrationTests.LocalProviderWithOptionalParametersOnlyTests

open Xunit
open System.Threading.Tasks
open FSharp.Data.GraphQL
open Helpers

[<Literal>]
let IntrospectionPath = "integration-introspection.json"
[<Literal>]
let EmptyGuidAsString = "00000000-0000-0000-0000-000000000000"

type Provider = GraphQLProvider<IntrospectionPath, uploadInputTypeName="File", explicitOptionalParameters=true>

let connection = TestHosts.createIntegrationConnection ()
let context =
    Provider.GetContext (serverUrl = TestHosts.integrationServerUrl, connectionFactory = (fun () -> connection))

type Input = Provider.Types.Input
type InputField = Provider.Types.InputField

module SimpleOperation =
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
          }"""> ()

    type Operation = Provider.Operations.Q

    let validateResult (input : Input option) (result : Operation.OperationResult) =
        result |> checkRequestTypeHeader "Classic"
        result.Data.IsSome |> equals true
        input
        |> Option.iter (fun input ->
            result.Data.Value.Echo.IsSome |> equals true
            input.List
            |> Option.iter (fun list ->
                result.Data.Value.Echo.Value.List.IsSome |> equals true
                let input =
                    list
                    |> Array.map (fun x -> x.Int, x.IntOption, x.String, x.StringOption, x.Uri, x.Guid.ToString ())
                let output =
                    result.Data.Value.Echo.Value.List.Value
                    |> Array.map (fun x -> x.Int, x.IntOption, x.String, x.StringOption, x.Uri, x.Guid)
                input |> equals output)
            input.Single
            |> Option.iter (fun single ->
                result.Data.Value.Echo.Value.Single.IsSome |> equals true
                let input =
                    single.Int, single.IntOption, single.String, single.StringOption, single.Uri, single.Guid.ToString ()
                let output =
                    result.Data.Value.Echo.Value.Single.Value
                    |> map (fun x -> x.Int, x.IntOption, x.String, x.StringOption, x.Uri, x.Guid)
                input |> equals output))

[<Fact; Trait("Execution", "Sync")>]
let ``Should be able to execute a query without sending input field`` () =
    SimpleOperation.operation.Run (context)
    |> SimpleOperation.validateResult None

[<Fact; Trait("Execution", "Sync")>]
let ``Should be able to execute a query using context, without sending input field`` () =
    SimpleOperation.operation.Run (context)
    |> SimpleOperation.validateResult None

[<Fact; Trait("Execution", "Async")>]
let ``Should be able to execute a query without sending input field asynchronously`` () =
    SimpleOperation.operation.AsyncRun (context)
    |> Async.RunSynchronously
    |> SimpleOperation.validateResult None

[<Fact; Trait("Execution", "Async")>]
let ``Should be able to execute a query using context, without sending input field, asynchronously`` () : Task = task {
    let! result = SimpleOperation.operation.AsyncRun (context)
    result |> SimpleOperation.validateResult None
}

[<Fact; Trait("Execution", "Sync")>]
let ``Should be able to execute a query sending an empty input field`` () =
    let input = Input ()
    SimpleOperation.operation.Run (context, Some input)
    |> SimpleOperation.validateResult (Some input)

[<Fact; Trait("Execution", "Sync")>]
let ``Should be able to execute a query using context, sending an empty input field`` () =
    let input = Input ()
    SimpleOperation.operation.Run (context, Some input)
    |> SimpleOperation.validateResult (Some input)

[<Fact; Trait("Execution", "Async")>]
let ``Should be able to execute a query without sending an empty input field asynchronously`` () : Task = task {
    let input = Input ()
    let! result = SimpleOperation.operation.AsyncRun (context, Some input)
    result |> SimpleOperation.validateResult (Some input)
}

[<Fact; Trait("Execution", "Async")>]
let ``Should be able to execute a query using context, sending an empty input field, asynchronously`` () : Task = task {
    let input = Input ()
    let! result = SimpleOperation.operation.AsyncRun (context, Some input)
    result |> SimpleOperation.validateResult (Some input)
}

[<Fact; Trait("Execution", "Sync")>]
let ``Should be able to execute a query sending an input field with single field`` () =
    let single = InputField ("A", 2, System.Uri ("http://localhost:1234"), EmptyGuidAsString)
    let input = Input (Some single)
    SimpleOperation.operation.Run (context, Some input)
    |> SimpleOperation.validateResult (Some input)

[<Fact; Trait("Execution", "Sync")>]
let ``Should be able to execute a query using context, sending an input field with single field`` () =
    let single = InputField ("A", 2, System.Uri ("http://localhost:1234"), EmptyGuidAsString)
    let input = Input (Some single)
    SimpleOperation.operation.Run (context, Some input)
    |> SimpleOperation.validateResult (Some input)

[<Fact; Trait("Execution", "Async")>]
let ``Should be able to execute a query without sending an input field with single field asynchronously`` () : Task = task {
    let single = InputField ("A", 2, System.Uri ("http://localhost:1234"), EmptyGuidAsString)
    let input = Input (Some single)
    let! result = SimpleOperation.operation.AsyncRun (context, Some input)
    result |> SimpleOperation.validateResult (Some input)
}

[<Fact; Trait("Execution", "Async")>]
let ``Should be able to execute a query using context, sending an input field with single field, asynchronously`` () : Task = task {
    let single = InputField ("A", 2, System.Uri ("http://localhost:1234"), EmptyGuidAsString)
    let input = Input (Some single)
    let! result = SimpleOperation.operation.AsyncRun (context, Some input)
    result |> SimpleOperation.validateResult (Some input)
}

[<Fact; Trait("Execution", "Sync")>]
let ``Should be able to execute a query sending an input field with list field`` () =
    let list = [| InputField ("A", 2, System.Uri ("http://localhost:4321"), EmptyGuidAsString) |]
    let input = Input (list = Some list)
    SimpleOperation.operation.Run (context, Some input)
    |> SimpleOperation.validateResult (Some input)

[<Fact; Trait("Execution", "Sync")>]
let ``Should be able to execute a query using context, sending an input field with list field`` () =
    let list = [| InputField ("A", 2, System.Uri ("http://localhost:4321"), EmptyGuidAsString) |]
    let input = Input (list = Some list)
    SimpleOperation.operation.Run (context, Some input)
    |> SimpleOperation.validateResult (Some input)

[<Fact; Trait("Execution", "Async")>]
let ``Should be able to execute a query without sending an input field with list field asynchronously`` () : Task = task {
    let list = [| InputField ("A", 2, System.Uri ("http://localhost:4321"), EmptyGuidAsString) |]
    let input = Input (list = Some list)
    let! result = SimpleOperation.operation.AsyncRun (context, Some input)
    result |> SimpleOperation.validateResult (Some input)
}

[<Fact; Trait("Execution", "Async")>]
let ``Should be able to execute a query using context, sending an input field with list field, asynchronously`` () : Task = task {
    let list = [| InputField ("A", 2, System.Uri ("http://localhost:4321"), EmptyGuidAsString) |]
    let input = Input (list = Some list)
    let! result = SimpleOperation.operation.AsyncRun (context, Some input)
    result |> SimpleOperation.validateResult (Some input)
}

[<Fact; Trait("Execution", "Sync")>]
let ``Should be able to execute a query sending an input field with single and list fields`` () =
    let single = InputField ("A", 2, System.Uri ("http://localhost:1234"), EmptyGuidAsString)
    let list = [| InputField ("A", 2, System.Uri ("http://localhost:4321"), EmptyGuidAsString) |]
    let input = Input (Some single, Some list)
    SimpleOperation.operation.Run (context, Some input)
    |> SimpleOperation.validateResult (Some input)

[<Fact; Trait("Execution", "Sync")>]
let ``Should be able to execute a query using context, sending an input field with single and list fields`` () =
    let single = InputField ("A", 2, System.Uri ("http://localhost:1234"), EmptyGuidAsString)
    let list = [| InputField ("A", 2, System.Uri ("http://localhost:4321"), EmptyGuidAsString) |]
    let input = Input (Some single, Some list)
    SimpleOperation.operation.Run (context, Some input)
    |> SimpleOperation.validateResult (Some input)

[<Fact; Trait("Execution", "Async")>]
let ``Should be able to execute a query without sending an input field with single and list fields asynchronously`` () : Task = task {
    let single = InputField ("A", 2, System.Uri ("http://localhost:1234"), EmptyGuidAsString)
    let list = [| InputField ("A", 2, System.Uri ("http://localhost:4321"), EmptyGuidAsString) |]
    let input = Input (Some single, Some list)
    let! result = SimpleOperation.operation.AsyncRun (context, Some input)
    result |> SimpleOperation.validateResult (Some input)
}

[<Fact; Trait("Execution", "Async")>]
let ``Should be able to execute a query using context, sending an input field with single and list fields, asynchronously`` () : Task = task {
    let single = InputField ("A", 2, System.Uri ("http://localhost:1234"), EmptyGuidAsString)
    let list = [| InputField ("A", 2, System.Uri ("http://localhost:4321"), EmptyGuidAsString) |]
    let input = Input (Some single, Some list)
    let! result = SimpleOperation.operation.AsyncRun (context, Some input)
    result |> SimpleOperation.validateResult (Some input)
}

module SingleRequiredUploadOperation =
    let operation =
        Provider.Operation<"""mutation SingleUpload($file: File!) {
            singleUpload(file: $file) {
              name
              contentType
              contentAsText
            }
          }"""> ()

    type Operation = Provider.Operations.SingleUpload

    let validateResult (file : File) (result : Operation.OperationResult) =
        result |> checkRequestTypeHeader "Multipart"
        result.Data.IsSome |> equals true
        result.Data.Value.SingleUpload.Name |> equals file.Name
        result.Data.Value.SingleUpload.ContentAsText
        |> equals file.Content
        result.Data.Value.SingleUpload.ContentType
        |> equals file.ContentType

[<Fact>]
let ``Should be able to execute a single required upload`` () =
    let file = {
        Name = "file.txt"
        ContentType = "text/plain"
        Content = "Sample text file contents"
    }
    SingleRequiredUploadOperation.operation.Run (context, file.MakeUpload (file.Name))
    |> SingleRequiredUploadOperation.validateResult file

[<Fact>]
let ``Should be able to execute a single required upload asynchronously`` () : Task = task {
    let file = {
        Name = "file.txt"
        ContentType = "text/plain"
        Content = "Sample text file contents"
    }
    let! result = SingleRequiredUploadOperation.operation.AsyncRun (context, file.MakeUpload ())
    result |> SingleRequiredUploadOperation.validateResult file
}

module SingleOptionalUploadOperation =
    let operation =
        Provider.Operation<"""mutation NullableSingleUpload($file: File) {
            nullableSingleUpload(file: $file) {
              name
              contentType
              contentAsText
            }
          }"""> ()

    type Operation = Provider.Operations.NullableSingleUpload

    let validateResult (file : File option) (result : Operation.OperationResult) =
        result |> checkRequestTypeHeader "Multipart"
        result.Data.IsSome |> equals true
        file
        |> Option.iter (fun file ->
            result.Data.Value.NullableSingleUpload.IsSome |> equals true
            result.Data.Value.NullableSingleUpload.Value.Name
            |> equals file.Name
            result.Data.Value.NullableSingleUpload.Value.ContentAsText
            |> equals file.Content
            result.Data.Value.NullableSingleUpload.Value.ContentType
            |> equals file.ContentType)

[<Fact>]
let ``Should be able to execute a single optional upload by passing a file`` () =
    let file = {
        Name = "file.txt"
        ContentType = "text/plain"
        Content = "Sample text file contents"
    }
    SingleOptionalUploadOperation.operation.Run (context, file.MakeUpload () |> Some)
    |> SingleOptionalUploadOperation.validateResult (Some file)

[<Fact>]
let ``Should be able to execute a single optional upload by passing a file, asynchronously`` () : Task = task {
    let file = {
        Name = "file.txt"
        ContentType = "text/plain"
        Content = "Sample text file contents"
    }
    let! result = SingleOptionalUploadOperation.operation.AsyncRun (context, file.MakeUpload ("test") |> Some)
    result
    |> SingleOptionalUploadOperation.validateResult (Some file)
}

[<Fact>]
let ``Should be able to execute a single optional upload by not passing a file`` () =
    SingleOptionalUploadOperation.operation.Run (context)
    |> SingleOptionalUploadOperation.validateResult None

[<Fact>]
let ``Should be able to execute a single optional upload by not passing a file asynchronously`` () : Task = task {
    let! result = SingleOptionalUploadOperation.operation.AsyncRun (context)
    result |> SingleOptionalUploadOperation.validateResult None
}

module RequiredMultipleUploadOperation =
    let operation =
        Provider.Operation<"""mutation MultipleUpload($files: [File!]!) {
            multipleUpload(files: $files) {
              name
              contentType
              contentAsText
            }
          }"""> ()

    type Operation = Provider.Operations.MultipleUpload

    let validateResult (files : File[]) (result : Operation.OperationResult) =
        result |> checkRequestTypeHeader "Multipart"
        result.Data.IsSome |> equals true
        let receivedFiles =
            result.Data.Value.MultipleUpload
            |> Array.map (fun file -> {
                Name = file.Name
                ContentType = file.ContentType
                Content = file.ContentAsText
            })
        receivedFiles |> equals files

[<Fact>]
let ``Should be able to execute a multiple required upload`` () =
    let files = [|
        {
            Name = "file1.txt"
            ContentType = "text/plain"
            Content = "Sample text file contents 1"
        }
        {
            Name = "file2.txt"
            ContentType = "text/plain"
            Content = "Sample text file contents 2"
        }
    |]
    RequiredMultipleUploadOperation.operation.Run (context, files |> Array.map (fun f -> f.MakeUpload ()))
    |> RequiredMultipleUploadOperation.validateResult files

[<Fact>]
let ``Should be able to execute a multiple required upload asynchronously`` () : Task = task {
    let files = [|
        {
            Name = "file1.txt"
            ContentType = "text/plain"
            Content = "Sample text file contents 1"
        }
        {
            Name = "file2.txt"
            ContentType = "text/plain"
            Content = "Sample text file contents 2"
        }
    |]
    let! result = RequiredMultipleUploadOperation.operation.AsyncRun (context, files |> Array.map (fun f -> f.MakeUpload ()))
    result
    |> RequiredMultipleUploadOperation.validateResult files
}

module OptionalMultipleUploadOperation =
    let operation =
        Provider.Operation<"""mutation NullableMultipleUpload($files: [File!]) {
            nullableMultipleUpload(files: $files) {
              name
              contentType
              contentAsText
            }
          }"""> ()

    type Operation = Provider.Operations.NullableMultipleUpload

    let validateResult (files : File[] option) (result : Operation.OperationResult) =
        result |> checkRequestTypeHeader "Multipart"
        result.Data.IsSome |> equals true
        let receivedFiles =
            result.Data.Value.NullableMultipleUpload
            |> Option.map (
                Array.map (fun file -> {
                    Name = file.Name
                    ContentType = file.ContentType
                    Content = file.ContentAsText
                })
            )
        receivedFiles |> equals files

[<Fact>]
let ``Should be able to execute a multiple upload`` () =
    let files = [|
        {
            Name = "file1.txt"
            ContentType = "text/plain"
            Content = "Sample text file contents 1"
        }
        {
            Name = "file2.txt"
            ContentType = "text/plain"
            Content = "Sample text file contents 2"
        }
    |]
    OptionalMultipleUploadOperation.operation.Run (context, files |> Array.map (fun f -> f.MakeUpload ()) |> Some)
    |> OptionalMultipleUploadOperation.validateResult (Some files)

[<Fact>]
let ``Should be able to execute a multiple upload asynchronously`` () : Task = task {
    let files = [|
        {
            Name = "file1.txt"
            ContentType = "text/plain"
            Content = "Sample text file contents 1"
        }
        {
            Name = "file2.txt"
            ContentType = "text/plain"
            Content = "Sample text file contents 2"
        }
    |]
    let! result = OptionalMultipleUploadOperation.operation.AsyncRun (context, (files |> Array.map _.MakeUpload()) |> Some)
    result
    |> OptionalMultipleUploadOperation.validateResult (Some files)
}

[<Fact>]
let ``Should be able to execute a multiple upload by sending no uploads`` () =
    OptionalMultipleUploadOperation.operation.Run (context)
    |> OptionalMultipleUploadOperation.validateResult None

[<Fact>]
let ``Should be able to execute a multiple upload asynchronously by sending no uploads`` () : Task = task {
    let! result = OptionalMultipleUploadOperation.operation.AsyncRun (context)
    result
    |> OptionalMultipleUploadOperation.validateResult None
}

module OptionalMultipleOptionalUploadOperation =
    let operation =
        Provider.Operation<"""mutation NullableMultipleNullableUpload($files: [File]) {
            nullableMultipleNullableUpload(files: $files) {
              name
              contentType
              contentAsText
            }
          }"""> ()

    type Operation = Provider.Operations.NullableMultipleNullableUpload

    let validateResult (files : File option[] option) (result : Operation.OperationResult) =
        result |> checkRequestTypeHeader "Multipart"
        result.Data.IsSome |> equals true
        let receivedFiles =
            result.Data.Value.NullableMultipleNullableUpload
            |> Option.map (
                Array.map (
                    Option.map (fun file -> {
                        Name = file.Name
                        ContentType = file.ContentType
                        Content = file.ContentAsText
                    })
                )
            )
        receivedFiles |> equals files

[<Fact>]
let ``Should be able to execute a multiple optional upload`` () =
    let files = [|
        Some {
            Name = "file1.txt"
            ContentType = "text/plain"
            Content = "Sample text file contents 1"
        }
        Some {
            Name = "file2.txt"
            ContentType = "text/plain"
            Content = "Sample text file contents 2"
        }
    |]
    OptionalMultipleOptionalUploadOperation.operation.Run (context, (files |> Array.map (Option.map _.MakeUpload())) |> Some)
    |> OptionalMultipleOptionalUploadOperation.validateResult (Some files)

[<Fact>]
let ``Should be able to execute a multiple optional upload asynchronously`` () : Task = task {
    let files = [|
        Some {
            Name = "file1.txt"
            ContentType = "text/plain"
            Content = "Sample text file contents 1"
        }
        Some {
            Name = "file2.txt"
            ContentType = "text/plain"
            Content = "Sample text file contents 2"
        }
    |]
    let! result = OptionalMultipleOptionalUploadOperation.operation.AsyncRun (context, (files |> Array.map (Option.map _.MakeUpload())) |> Some)
    result
    |> OptionalMultipleOptionalUploadOperation.validateResult (Some files)
}

[<Fact>]
let ``Should be able to execute a multiple optional upload by sending no uploads`` () =
    OptionalMultipleOptionalUploadOperation.operation.Run (context)
    |> OptionalMultipleOptionalUploadOperation.validateResult None

[<Fact>]
let ``Should be able to execute a multiple optional upload asynchronously by sending no uploads`` () : Task = task {
    let! result = OptionalMultipleOptionalUploadOperation.operation.AsyncRun (context)
    result
    |> OptionalMultipleOptionalUploadOperation.validateResult None
}

[<Fact>]
let ``Should be able to execute a multiple optional upload by sending some uploads`` () =
    let files = [|
        Some {
            Name = "file1.txt"
            ContentType = "text/plain"
            Content = "Sample text file contents 1"
        }
        None
        Some {
            Name = "file2.txt"
            ContentType = "text/plain"
            Content = "Sample text file contents 2"
        }
        None
    |]
    OptionalMultipleOptionalUploadOperation.operation.Run (context, files |> Array.map (Option.map _.MakeUpload()) |> Some)
    |> OptionalMultipleOptionalUploadOperation.validateResult (Some files)

[<Fact>]
let ``Should be able to execute a multiple optional upload asynchronously by sending some uploads`` () : Task = task {
    let files = [|
        Some {
            Name = "file1.txt"
            ContentType = "text/plain"
            Content = "Sample text file contents 1"
        }
        None
        Some {
            Name = "file2.txt"
            ContentType = "text/plain"
            Content = "Sample text file contents 2"
        }
        None
    |]
    let! result = OptionalMultipleOptionalUploadOperation.operation.AsyncRun (context, files |> Array.map (Option.map _.MakeUpload()) |> Some)
    result
    |> OptionalMultipleOptionalUploadOperation.validateResult (Some files)
}

module UploadRequestOperation =
    let operation =
        Provider.Operation<"""mutation UploadRequestOperation($request: UploadRequest!) {
            uploadRequest(request: $request) {
              single {
                ...File
              }
              multiple {
                ...File
              }
              nullableMultiple {
                ...File
              }
              nullableMultipleNullable {
                ...File
              }
            }
          }

          fragment File on UploadedFile {
            name
            contentType
            contentAsText
          }"""> ()

    type Operation = Provider.Operations.UploadRequestOperation

    type Request = Provider.Types.UploadRequest

    let validateResult (request : FilesRequest) (result : Operation.OperationResult) =
        result |> checkRequestTypeHeader "Multipart"
        result.Data.IsSome |> equals true
        result.Data.Value.UploadRequest.Single.ToDictionary ()
        |> File.FromDictionary
        |> equals request.Single
        result.Data.Value.UploadRequest.Multiple
        |> Array.map ((fun x -> x.ToDictionary ()) >> File.FromDictionary)
        |> equals request.Multiple
        result.Data.Value.UploadRequest.NullableMultiple
        |> Option.map (Array.map ((fun x -> x.ToDictionary ()) >> File.FromDictionary))
        |> equals request.NullableMultiple
        result.Data.Value.UploadRequest.NullableMultipleNullable
        |> Option.map (Array.map (Option.map ((fun x -> x.ToDictionary ()) >> File.FromDictionary)))
        |> equals request.NullableMultipleNullable

[<Fact>]
let ``Should be able to upload files inside another input type`` () =
    let request = {
        Single = {
            Name = "single.txt"
            ContentType = "text/plain"
            Content = "Single file content"
        }
        Multiple = [|
            {
                Name = "multiple1.txt"
                ContentType = "text/plain"
                Content = "Multiple files first file content"
            }
            {
                Name = "multiple2.txt"
                ContentType = "text/plain"
                Content = "Multiple files second file content"
            }
        |]
        NullableMultiple =
            Some [|
                {
                    Name = "multiple3.txt"
                    ContentType = "text/plain"
                    Content = "Multiple files third file content"
                }
            |]
        NullableMultipleNullable =
            Some [|
                Some {
                    Name = "multiple4.txt"
                    ContentType = "text/plain"
                    Content = "Multiple files fourth file content"
                }
                None
            |]
    }
    let input =
        let makeUpload (x : File) = x.MakeUpload ()
        UploadRequestOperation.Request (
            single = makeUpload request.Single,
            multiple = Array.map makeUpload request.Multiple,
            nullableMultiple = Some (Array.map makeUpload request.NullableMultiple.Value),
            nullableMultipleNullable = Some (Array.map (Option.map makeUpload) request.NullableMultipleNullable.Value)
        )
    UploadRequestOperation.operation.Run (context, input)
    |> UploadRequestOperation.validateResult request

module UploadComplexOperation =
    let operation =
        Provider.Operation<"""mutation UploadComplex($input: InputFile!) {
            uploadComplex(input: $input)
          }"""> ()

    type Operation = Provider.Operations.UploadComplex
    type InputFile = Provider.Types.InputFile

    let validateResult (file : File) (result : Operation.OperationResult) =
        result |> checkRequestTypeHeader "Multipart"
        result.Data.IsSome |> equals true
        result.Data.Value.UploadComplex |> equals file.Content

[<Fact>]
let ``Should be able to upload file using complex input object`` () =
    let file = {
        Name = "complex.txt"
        ContentType = "text/plain"
        Content = "Complex input object file content"
    }
    let input = UploadComplexOperation.InputFile (file = file.MakeUpload ())
    UploadComplexOperation.operation.Run (context, input)
    |> UploadComplexOperation.validateResult file

[<Fact>]
let ``Should be able to upload file using complex input object with context`` () =
    let file = {
        Name = "complex_context.txt"
        ContentType = "text/plain"
        Content = "Complex input with context file content"
    }
    let input = UploadComplexOperation.InputFile (file = file.MakeUpload ())
    UploadComplexOperation.operation.Run (context, input)
    |> UploadComplexOperation.validateResult file

[<Fact>]
let ``Should be able to upload file using complex input object asynchronously`` () : Task = task {
    let file = {
        Name = "complex_async.txt"
        ContentType = "text/plain"
        Content = "Complex input object async file content"
    }
    let input = UploadComplexOperation.InputFile (file = file.MakeUpload ())
    let! result = UploadComplexOperation.operation.AsyncRun (context, input)
    result |> UploadComplexOperation.validateResult file
}

[<Fact>]
let ``Should be able to upload file using complex input object with context asynchronously`` () : Task = task {
    let file = {
        Name = "complex_context_async.txt"
        ContentType = "text/plain"
        Content = "Complex input with context async file content"
    }
    let input = UploadComplexOperation.InputFile (file = file.MakeUpload ())
    let! result = UploadComplexOperation.operation.AsyncRun (context, input)
    result |> UploadComplexOperation.validateResult file
}
