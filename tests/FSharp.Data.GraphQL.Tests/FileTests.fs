module FSharp.Data.GraphQL.Tests.FileTests

open System.Collections.Immutable
open System.IO
open System.Text
open System.Text.Json
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Shared
open Xunit
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.SchemaDefinitions

type Root = { File : Stream }

let QueryType =
    Define.Object<unit> (
        name = "QueryType",
        fields =
            [ Define.Field ("dummy", StringType, fun _ _ -> "dummy")]
    )

type Input = { File : Stream }

let InputObject = Define.InputObject<Input>(
    name = "Input",
    fields =
        [
            Define.Input("file", FileType)
        ])

let MutationType =
    Define.Object<unit> (
        name = "MutationType",
        fields =
            [ Define.Field ("uploadFile", StringType, "", [ Define.Input ("input", FileType) ],
                (fun ctx () ->
                    let stream = ctx.Arg<Stream> "input"
                    use reader = new StreamReader(stream, Encoding.UTF8, true)
                    reader.ReadToEnd()
                ))
                // Define.Field ("uploadFileWithinObject", StringType, "", [ Define.Input ("input", InputObject) ], stringifyInput)
            ]
    )
let schema = Schema (QueryType, MutationType)
let executor = Executor (schema, [])
let execute (query : string) = executor.AsyncExecute (query, mockInputContext) |> sync
let executeWithVariables ( query : string, variables : ImmutableDictionary<string, JsonElement>) =
    executor.AsyncExecute (ast = parse query, inputContext = mockInputContext, variables = variables) |> sync

let mutationWithVariable = """mutation uploadFile ($file : FileType!) {
    uploadFile (input : $file)
}
"""

let mutationWithConstant = """mutation uploadFile () {
    uploadFile (input : "fileKey")
}"""

[<Fact>]
let ``File type: Must return file text`` () =
    let expected = NameValueLookup.ofList [ "uploadFile", MockInputContext.mockFileText ]
    let result = execute mutationWithConstant
    ensureDirect result <| fun data errors ->
            empty errors
            data |> equals (upcast expected)
    ()

[<Fact>]
let ``File type: Must return file text when as using variable`` () =
    let expected = NameValueLookup.ofList [ "uploadFile", MockInputContext.mockFileText ]
    let jsonVariable = "\"fileKey\"" |> JsonDocument.Parse |> _.RootElement
    let variables = ImmutableDictionary<string, JsonElement>.Empty.Add ("file", jsonVariable)
    let result = executeWithVariables (mutationWithVariable, variables)
    ensureDirect result <| fun data errors ->
            empty errors
            data |> equals (upcast expected)
    ()
