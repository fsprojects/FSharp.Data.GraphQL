module FSharp.Data.GraphQL.Tests.FileTests

open System.Collections.Immutable
open System.IO
open System.Text
open System.Text.Json
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Types
open Xunit

type Root = { File : Stream }

let QueryType =
    Define.Object<unit> (
        name = "QueryType",
        fields =
            [ Define.Field ("dummy", StringType, fun _ _ -> "dummy")]
    )

type Input = {
    File : FileData
    File2 : FileData option
}

let private getFullInfo fileData =
    let reader = new StreamReader(fileData.Stream, Encoding.UTF8, true)
    let fileContent = reader.ReadToEnd()
    fileContent + fileData.ContentType

let InputObject = Define.InputObject<Input>(
    name = "Input",
    fields =
        [
            Define.Input("file", FileType)
            Define.Input("file2", Nullable FileType)
        ])

let MutationType =
    Define.Object<unit> (
        name = "MutationType",
        fields =
            [ Define.Field ("uploadFile", StringType, "", [ Define.Input ("input", FileType) ],
                (fun ctx () ->
                    let fileData = ctx.Arg<FileData> "input"
                    getFullInfo fileData
                ));
                Define.Field ("uploadFileComplex", StringType, "", [ Define.Input ("input", InputObject) ],
                (fun ctx () ->
                    let input = ctx.Arg<Input> "input"
                    let fileString = getFullInfo input.File
                    let file2String = match input.File2 with
                                        | Some file2 ->
                                            getFullInfo file2
                                        | None -> ""
                    fileString + file2String
                ))
            ]
    )
let schema = Schema (QueryType, MutationType)
let executor = Executor (schema, [])
let execute (query : string) = executor.AsyncExecute (query, getMockInputContext) |> sync
let executeWithVariables ( query : string, variables : ImmutableDictionary<string, JsonElement>) =
    executor.AsyncExecute (ast = parse query, getInputContext = getMockInputContext, variables = variables) |> sync

let mutationWithVariable = """mutation uploadFile ($file : FileType!) {
    uploadFile (input : $file)
}
"""

let mutationWithConstant = """mutation uploadFile () {
    uploadFile (input : "fileKey")
}"""

let mutationComplexObject = """mutation uploadFile () {
    uploadFileComplex (input : { file : "fileKey" })
}"""

let mutationComplexObjectWithTwoFiles = """mutation uploadFile () {
    uploadFileComplex (input : {file : "fileKey", file2: "fileKey2" })
}"""

[<Fact>]
let ``File type: Must upload file as input scalar using inline string as a file name`` () =
    let expected = NameValueLookup.ofList [ "uploadFile", MockInputContext.mockFileTextAndContentType ]
    let result = execute mutationWithConstant
    ensureDirect result <| fun data errors ->
            empty errors
            data |> equals (upcast expected)
    ()

[<Fact>]
let ``File type: Must upload a file as input scalar using a variable`` () =
    let expected = NameValueLookup.ofList [ "uploadFile", MockInputContext.mockFileTextAndContentType ]
    let jsonVariable = "\"fileKey\"" |> JsonDocument.Parse |> _.RootElement
    let variables = ImmutableDictionary<string, JsonElement>.Empty.Add ("file", jsonVariable)
    let result = executeWithVariables (mutationWithVariable, variables)
    ensureDirect result <| fun data errors ->
            empty errors
            data |> equals (upcast expected)
    ()

[<Fact>]
let ``File type: Must upload a file as input object field using inline string a file name`` () =
    let expected = NameValueLookup.ofList [ "uploadFileComplex", MockInputContext.mockFileTextAndContentType ]
    let result = execute mutationComplexObject
    ensureDirect result <| fun data errors ->
            empty errors
            data |> equals (upcast expected)
    ()

[<Fact>]
let ``File type: Must upload two files as input object field using inline string a file name`` () =
    let expectedContent = MockInputContext.mockFileTextAndContentType + MockInputContext.mockFileText2AndContentType
    let expected = NameValueLookup.ofList [
        "uploadFileComplex", expectedContent
    ]
    let result = execute mutationComplexObjectWithTwoFiles
    ensureDirect result <| fun data errors ->
            empty errors
            data |> equals (upcast expected)
    ()
