namespace FSharp.Data.GraphQL.Server.Suave

open System
open System.IO
open System.Net.Mime
open System.Text
open System.Text.Json
open System.Text.Json.Serialization

open Suave
open Suave.Filters
open Suave.Http
open Suave.Operators
open Suave.RequestErrors
open Suave.Successful
open Suave.Writers

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Server
open FSharp.Data.GraphQL.Shared

type private SuaveRequestExecutionContext (httpContext : HttpContext) =

    interface IInputExecutionContext with

        member _.GetFile key =
            match httpContext.request.files |> Seq.tryFind (fun file -> file.fieldName = key) with
            | Some file ->
                use source = File.OpenRead file.tempFilePath
                let stream = new MemoryStream ()
                source.CopyTo stream
                stream.Seek (0L, SeekOrigin.Begin) |> ignore

                Ok {
                    FileName = file.fileName
                    Stream = stream
                    ContentType = file.mimeType
                }
            | None -> Result.Error $"File with key '{key}' not found"

module HttpHandlers =

    let private jsonMimeType = "application/json; charset=utf-8"
    let private problemJsonMimeType = "application/problem+json; charset=utf-8"
    let private serializerOptions = Shared.Json.getSerializerOptions Seq.empty

    let private toResponse { DocumentId = documentId; Content = content } =
        match content with
        | Direct (data, errs) -> GQLResponse.Direct (documentId, data, errs)
        | Deferred (data, errs, _) -> GQLResponse.Direct (documentId, data, errs)
        | Stream _ -> GQLResponse.Stream documentId
        | RequestError errs -> GQLResponse.RequestError (documentId, errs)

    let private okJson (payload : string) =
        setMimeType jsonMimeType
        >=> ok (Encoding.UTF8.GetBytes payload)

    let private badRequestJson (payload : string) =
        setMimeType problemJsonMimeType
        >=> bad_request (Encoding.UTF8.GetBytes payload)

    let private problemDetails title detail instance =
        JsonSerializer.Serialize (
            {| title = title
               detail = detail
               status = 400
               instance = instance |},
            serializerOptions
        )

    let private isMultipartRequest (request : HttpRequest) =
        request.headers
        |> Seq.exists (fun (key, value) ->
            String.Equals (key, "Content-Type", StringComparison.OrdinalIgnoreCase)
            && value.Contains (MediaTypeNames.Multipart.FormData, StringComparison.OrdinalIgnoreCase)
        )

    let private tryGetMultipartField name (request : HttpRequest) =
        request.multiPartFields
        |> Seq.tryPick (fun (fieldName, value) ->
            if String.Equals (fieldName, name, StringComparison.Ordinal) then
                Some value
            else
                None
        )

    let private requestBody (request : HttpRequest) =
        if isMultipartRequest request then
            tryGetMultipartField "operations" request
            |> Option.defaultValue ""
        else
            request.rawForm
            |> Encoding.UTF8.GetString

    let private operationNameAsValueOption (operationName : string Skippable) =
        match operationName with
        | Include value when not (isNull value) -> ValueSome value
        | _ -> ValueNone

    let private operationNameAsOption (operationName : string Skippable) =
        match operationName with
        | Include value when not (isNull value) -> Some value
        | _ -> None

    let private variablesAsOption (variables : _ Skippable) =
        match variables with
        | Include value when not (isNull value) -> Some value
        | _ -> None

    let private executeIntrospectionQuery (executor : Executor<'Root>) (optionalAstDocument : Ast.Document voption) = task {
        let inputContext () : IInputExecutionContext =
            SuaveRequestExecutionContext (HttpContext.empty) :> IInputExecutionContext

        let! result =
            match optionalAstDocument with
            | ValueSome ast -> executor.AsyncExecute (ast, inputContext) |> Async.StartAsTask
            | ValueNone -> executor.AsyncExecute (IntrospectionQuery.Definition, inputContext) |> Async.StartAsTask

        let payload = result |> toResponse |> fun response -> JsonSerializer.Serialize (response, serializerOptions)
        return okJson payload
    }

    let private executeOperation
        (executor : Executor<'Root>)
        (rootFactory : HttpContext -> 'Root)
        (httpContext : HttpContext)
        (content : ParsedGQLQueryRequestContent)
        =
        task {
            let root = rootFactory httpContext

            let inputContext () : IInputExecutionContext =
                SuaveRequestExecutionContext (httpContext) :> IInputExecutionContext

            let operationName = operationNameAsOption content.OperationName
            let variables = variablesAsOption content.Variables

            let! result =
                executor.AsyncExecute (content.Ast, inputContext, root, ?variables = variables, ?operationName = operationName)
                |> Async.StartAsTask

            let payload = result |> toResponse |> fun response -> JsonSerializer.Serialize (response, serializerOptions)
            return okJson payload
        }

    let private checkOperationType (httpContext : HttpContext) =
        let request = httpContext.request

        if request.method = HttpMethod.GET then
            Result.Ok (OperationType.IntrospectionQuery ValueNone)
        else
            let body = requestBody request

            if String.IsNullOrWhiteSpace body then
                Result.Ok (OperationType.IntrospectionQuery ValueNone)
            else
                try
                    let gqlRequest = JsonSerializer.Deserialize<GQLRequestContent> (body, serializerOptions)
                    let ast = gqlRequest.Query |> Parser.tryParse |> Result.mapError (fun message -> problemDetails "Cannot parse GraphQL query" message request.path)

                    ast
                    |> Result.map (fun ast ->
                        let parsedContent () = {
                            Query = gqlRequest.Query
                            Ast = ast
                            OperationName = gqlRequest.OperationName
                            Variables = gqlRequest.Variables
                        }

                        if ast.IsEmpty then
                            OperationType.IntrospectionQuery ValueNone
                        else
                            match Ast.tryFindOperationByName (operationNameAsValueOption gqlRequest.OperationName) ast with
                            | None -> OperationType.IntrospectionQuery ValueNone
                            | Some _ -> parsedContent () |> OperationType.OperationQuery
                    )
                with :? JsonException ->
                    Result.Error (
                        problemDetails
                            "Invalid JSON body"
                            $"Expected JSON similar to value in 'expected', but received value as in 'received': {body}"
                            request.path
                    )

    let private handleGraphQL<'Root> (executor : Executor<'Root>) (rootFactory : HttpContext -> 'Root) (httpContext : HttpContext) =
        async {
            let! webPart =
                task {
                    match checkOperationType httpContext with
                    | Result.Error payload -> return badRequestJson payload
                    | Result.Ok (OperationType.IntrospectionQuery optionalAstDocument) -> return! executeIntrospectionQuery executor optionalAstDocument
                    | Result.Ok (OperationType.OperationQuery content) -> return! executeOperation executor rootFactory httpContext content
                }
                |> Async.AwaitTask

            return! webPart httpContext
        }

    /// <summary>
    /// Sets the <c>Request-Type</c> response header to <c>Multipart</c> for multipart form requests and <c>Classic</c> otherwise.
    /// </summary>
    let setRequestType : WebPart =
        fun httpContext ->
            let requestType =
                if isMultipartRequest httpContext.request then
                    "Multipart"
                else
                    "Classic"

            setHeader "Request-Type" requestType httpContext

    /// <summary>
    /// Creates a Suave <see cref="T:Suave.WebPart.WebPart`1" /> that handles GraphQL GET and POST requests.
    /// </summary>
    /// <param name="executor">The GraphQL executor.</param>
    /// <param name="rootFactory">Creates the GraphQL root object from the current Suave HTTP context.</param>
    let graphQL<'Root> (executor : Executor<'Root>) (rootFactory : HttpContext -> 'Root) : WebPart =
        choose [ Filters.GET; Filters.POST ] >=> handleGraphQL executor rootFactory
