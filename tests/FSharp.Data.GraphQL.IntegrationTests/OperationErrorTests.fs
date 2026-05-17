module FSharp.Data.GraphQL.IntegrationTests.OperationErrorTests

open System.Net.Http
open Xunit
open Helpers
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Client

let [<Literal>] ServerUrl = "http://localhost:8085"

type Provider = GraphQLProvider<ServerUrl, uploadInputTypeName = "File", explicitOptionalParameters = false>

module ErrorOperation =
    let operation =
        Provider.Operation<"""query ErrorQuery {
            alwaysError
        }""">()

    type Operation = Provider.Operations.ErrorQuery

[<Fact; Trait("OperationError", "Unit")>]
let ``Should parse operation error fields from raw response`` () =
    let result =
        OperationResultBase(
            rawResponse = new HttpResponseMessage(),
            responseJson =
                JsonValue.Parse
                    """{
                        "errors": [{
                            "message": "unit-test error",
                            "path": ["alwaysError", 0],
                            "locations": [{ "line": 2, "column": 13 }],
                            "extensions": { "code": "UNIT_TEST", "retryable": false, "severity": 7 }
                        }]
                    }""",
            operationFields = [||],
            operationTypeName = "Query"
        )

    result.Errors.Length |> equals 1

    let error : FSharp.Data.GraphQL.OperationError = result.Errors.[0]
    error.Message |> equals "unit-test error"
    error.Path |> equals [| box "alwaysError"; box 0 |]
    error.Locations |> equals [| { Line = 2; Column = 13 } |]
    error.Extensions.["code"] |> equals (box "UNIT_TEST")
    error.Extensions.["retryable"] |> equals (box false)
    error.Extensions.["severity"] |> equals (box 7)

[<Fact; Trait("OperationError", "Unit")>]
let ``Should parse all combinations of optional operation error fields`` () =
    let combinations =
        [ for includePath in [ false; true ] do
              for includeLocations in [ false; true ] do
                  for includeExtensions in [ false; true ] do
                      includePath, includeLocations, includeExtensions ]

    for includePath, includeLocations, includeExtensions in combinations do
        let optionalFields =
            [ if includePath then
                  "\"path\":[\"alwaysError\",0]"
              if includeLocations then
                  "\"locations\":[{\"line\":2,\"column\":13}]"
              if includeExtensions then
                  "\"extensions\":{\"code\":\"UNIT_TEST\",\"retryable\":false,\"severity\":7}" ]

        let errorObjectJson =
            "\"message\":\"unit-test combination error\"" :: optionalFields
            |> String.concat ","

        let responseJson = $"""{{"errors":[{{{errorObjectJson}}}]}}"""

        let result =
            OperationResultBase(
                rawResponse = new HttpResponseMessage(),
                responseJson = JsonValue.Parse responseJson,
                operationFields = [||],
                operationTypeName = "Query"
            )

        result.Errors.Length |> equals 1

        let error : FSharp.Data.GraphQL.OperationError = result.Errors.[0]
        error.Message |> equals "unit-test combination error"

        if includePath then
            error.Path |> equals [| box "alwaysError"; box 0 |]
        else
            error.Path |> equals [||]

        if includeLocations then
            error.Locations |> equals [| { Line = 2; Column = 13 } |]
        else
            error.Locations |> equals [||]

        if includeExtensions then
            error.Extensions.["code"] |> equals (box "UNIT_TEST")
            error.Extensions.["retryable"] |> equals (box false)
            error.Extensions.["severity"] |> equals (box 7)
        else
            error.Extensions |> equals Map.empty

[<Fact; Trait("OperationError", "Integration")>]
let ``Should map server error extensions and locations into operation result`` () =
    let result = ErrorOperation.operation.Run()

    result.Errors.Length |> equals 1

    let error : FSharp.Data.GraphQL.OperationError = result.Errors.[0]
    error.Message |> equals "Always fails for tests"
    error.Path |> equals [| box "alwaysError" |]

    error.Locations |> equals [||]

    error.Extensions.ContainsKey "code" |> equals true
    error.Extensions.["code"] |> equals (box "OPERATION_ERROR_TEST")
    error.Extensions.ContainsKey "severity" |> equals true
    error.Extensions.["severity"] |> equals (box 7)
    error.Extensions.ContainsKey "kind" |> equals true
    match error.Extensions.["kind"] with
    | :? string as kind -> kind |> equals "Execution"
    | :? int as kind -> kind |> equals 3
    | kind -> failwithf "Unexpected kind extension value: %A" kind
