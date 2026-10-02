module internal FSharp.Data.GraphQL.Server.Suave.RequestParsing

open System.Text
open System.Text.Json
open System.Text.Json.Serialization
open FsToolkit.ErrorHandling
open Suave.Http

open FSharp.Data.GraphQL.Shared

module ServerAst = FSharp.Data.GraphQL.Server.Ast

/// Reads the raw GraphQL request body, honoring the multipart request specification
/// (the "operations" field carries the GraphQL request when the request is multipart).
let private hasBody (request : HttpRequest) =
    request.rawForm.Length > 0
    || not request.multiPartFields.IsEmpty
    || not request.files.IsEmpty

let private tryBindGQLRequestContent (serializerOptions : JsonSerializerOptions) (request : HttpRequest) : Result<GQLRequestContent, string> =
    let json =
        if request.multiPartFields.IsEmpty then
            Encoding.UTF8.GetString request.rawForm
        else
            request.multiPartFields
            |> List.tryFind (fst >> (=) "operations")
            |> Option.map snd
            |> Option.defaultValue "{}"

    try
        Ok (JsonSerializer.Deserialize<GQLRequestContent>(json, serializerOptions))
    with :? JsonException as ex ->
        Error
            $"Expected JSON similar to value in '%s{GQLRequestContent.expectedJSON}', but could not parse the received request body. \
              Error: %s{ex.Message}"

let private parseOrError (query : string) =
    query
    |> FSharp.Data.GraphQL.Parser.tryParse
    |> Result.mapError (fun errorMessage -> $"Cannot parse GraphQL query: %s{errorMessage}")

let private checkAnonymousFieldsOnly (serializerOptions : JsonSerializerOptions) (request : HttpRequest) : Result<OperationType, string> = result {
    let! gqlRequest = tryBindGQLRequestContent serializerOptions request
    let! ast = parseOrError gqlRequest.Query
    let operationName = gqlRequest.OperationName |> Skippable.toValueOption

    let createParsedContent () : ParsedGQLQueryRequestContent = {
        Query = gqlRequest.Query
        Ast = ast
        OperationName = gqlRequest.OperationName
        Variables = gqlRequest.Variables
    }

    if ast.IsEmpty then
        return IntrospectionQuery ValueNone
    else
        match ServerAst.tryFindOperationByName operationName ast with
        | None -> return IntrospectionQuery ValueNone
        | Some op ->
            if
                op.OperationType
                <> FSharp.Data.GraphQL.Ast.OperationType.Query
            then
                return createParsedContent () |> OperationQuery
            else
                let hasNonMetaFields = ServerAst.containsFieldsBeyond ServerAst.metaTypeFields ignore ignore op

                if hasNonMetaFields then
                    return createParsedContent () |> OperationQuery
                else
                    return IntrospectionQuery (ValueSome ast)
}

/// <summary>
/// Checks if the request is an introspection query by first checking on such properties as
/// <c>GET</c> method or an empty request body, and lastly by parsing the document AST for an
/// introspection operation definition.
/// </summary>
let checkOperationType (serializerOptions : JsonSerializerOptions) (request : HttpRequest) : Result<OperationType, string> =
    if request.method = HttpMethod.GET then
        Ok (IntrospectionQuery ValueNone)
    elif not (hasBody request) then
        Ok (IntrospectionQuery ValueNone)
    else
        checkAnonymousFieldsOnly serializerOptions request
