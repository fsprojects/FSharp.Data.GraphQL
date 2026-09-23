module internal FSharp.Data.GraphQL.Server.Suave.GraphQLHttpHandler

open System.Text.Json
open System.Text.Json.Serialization
open Suave
open Suave.Http
open Suave.Operators
open Suave.Utils

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Shared

let private jsonMimeType = "application/json; charset=utf-8"

let private jsonResponse (serializerOptions : JsonSerializerOptions) (statusCode : HttpCode) (value : 'T) : WebPart =
    let json = JsonSerializer.Serialize (value, serializerOptions)
    Writers.setMimeType jsonMimeType
    >=> Response.response statusCode (UTF8.bytes json)

let private toResponse ({ DocumentId = documentId; Content = content } : GQLExecutionResult) =
    match content with
    | Direct (data, errs) -> GQLResponse.Direct (documentId, data, errs)
    | Deferred (data, errs, _deferred) -> GQLResponse.Direct (documentId, data, errs)
    | Stream _stream -> GQLResponse.Stream documentId
    | RequestError errs -> GQLResponse.RequestError (documentId, errs)

let private executeIntrospectionQuery (options : GraphQLOptions<'Root>) (ctx : HttpContext) (ast : Ast.Document voption) : Async<GQLResponse> = async {
    let getInputContext () = SuaveInputExecutionContext (ctx.request) :> IInputExecutionContext
    let executor = options.SchemaExecutor

    let! result =
        match ast with
        | ValueNone -> executor.AsyncExecute (IntrospectionQuery.Definition, getInputContext)
        | ValueSome ast -> executor.AsyncExecute (ast, getInputContext)

    return toResponse result
}

let private executeOperation (options : GraphQLOptions<'Root>) (ctx : HttpContext) (content : ParsedGQLQueryRequestContent) : Async<GQLResponse> =
    async {
        let getInputContext () = SuaveInputExecutionContext (ctx.request) :> IInputExecutionContext

        let operationName =
            content.OperationName
            |> Skippable.filter (not << isNull)
            |> Skippable.toValueOption

        let variables =
            content.Variables
            |> Skippable.filter (not << isNull)
            |> Skippable.toValueOption

        let root = options.RootFactory ctx

        let! result =
            options.SchemaExecutor.AsyncExecute (content.Ast, getInputContext, root, ?variables = variables, ?operationName = operationName)

        return toResponse result
    }

/// A <see cref="Suave.Http.WebPart"/> that parses and executes GraphQL requests over HTTP (both <c>GET</c> introspection
/// queries and <c>POST</c> queries/mutations, including the GraphQL multipart request specification for file uploads).
let handleGraphQL (options : GraphQLOptions<'Root>) : WebPart =
    fun (ctx : HttpContext) -> async {
        match RequestParsing.checkOperationType options.SerializerOptions ctx.request with
        | Ok (IntrospectionQuery ast) ->
            let! response = executeIntrospectionQuery options ctx ast
            return! jsonResponse options.SerializerOptions HTTP_200 response ctx
        | Ok (OperationQuery content) ->
            let! response = executeOperation options ctx content
            return! jsonResponse options.SerializerOptions HTTP_200 response ctx
        | Error errorMessage ->
            return! jsonResponse options.SerializerOptions HTTP_400 (GQLResponse.RequestError (0, [ GQLProblemDetails.Create errorMessage ])) ctx
    }
