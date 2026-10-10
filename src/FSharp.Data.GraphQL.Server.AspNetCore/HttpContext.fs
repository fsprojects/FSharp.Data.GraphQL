[<AutoOpen>]
module FSharp.Data.GraphQL.Server.AspNetCore.HttpContextExtensions

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Runtime.CompilerServices
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Options

open FSharp.Core
open FsToolkit.ErrorHandling

/// Answers every problem with a request body with a problem details result of a fixed status code,
/// so that no body turns into an unhandled exception or repeats itself back to the client.
module internal RequestBody =

    /// <summary>
    /// The longest text that a problem response quotes from the reason a deserializer or a form reader gives.
    /// </summary>
    /// <remarks>
    /// Such a reason can quote the request body: System.Text.Json names the JSON path of the failure, which is built
    /// from property names of the body, and the multipart reader repeats a malformed header line. Cutting the reason
    /// keeps a response from reflecting more than a short excerpt of the body, whatever the size of the body.
    /// </remarks>
    [<Literal>]
    let MaxQuotedReasonLength = 256

    [<Literal>]
    let InvalidJsonTitle = "Invalid JSON body"

    [<Literal>]
    let InvalidMultipartTitle = "Invalid multipart request"

    [<Literal>]
    let UnreadableBodyTitle = "Unreadable request body"

    [<Literal>]
    let BodyTooLargeTitle = "Request body too large"

    /// The form field that carries the GraphQL request JSON, as the GraphQL multipart request specification names it
    [<Literal>]
    let OperationsField = "operations"

    /// The form field that maps files to the variables they stand for, as the GraphQL multipart request specification names it
    [<Literal>]
    let MapField = "map"

    [<Literal>]
    let private VariablesPathPrefix = "variables."

    /// <summary>Cuts a reason that may quote the request body to at most <see cref="MaxQuotedReasonLength"/> characters</summary>
    let quote (reason : string) =
        if reason.Length <= MaxQuotedReasonLength then
            reason
        else
            // Cutting between the halves of a surrogate pair would leave an invalid character at the end
            let length =
                if Char.IsHighSurrogate (reason[MaxQuotedReasonLength - 1]) then
                    MaxQuotedReasonLength - 1
                else
                    MaxQuotedReasonLength
            String.Concat (reason.AsSpan (0, length), "...".AsSpan ())

    /// A problem details response for the path of the request
    let problem (request : HttpRequest) (statusCode : int) (title : string) (detail : string) : IResult =
        Results.Problem (detail, request.Path.Value, statusCode, title)

    /// The response to a body that is not JSON of the expected shape: it shows the expected shape and the reason
    /// of the deserializer, cut to a short excerpt, but never the body itself.
    let invalidJson (request : HttpRequest) (expectedJson : string) (reason : string) : IResult =
        let extensions =
            seq { KeyValuePair ("expected", (expectedJson :> obj)) }
            |> ImmutableDictionary.CreateRange

        Results.Problem (
            $"Expected JSON similar to the value in 'expected': %s{quote reason}",
            request.Path.Value,
            StatusCodes.Status400BadRequest,
            InvalidJsonTitle,
            extensions = extensions
        )

    /// <summary>
    /// The response to the exception a server throws from the body stream with the status code to answer with:
    /// Kestrel answers a body over <see cref="Microsoft.AspNetCore.Server.Kestrel.Core.KestrelServerLimits.MaxRequestBodySize"/>
    /// with 413, and a body that ends before its Content-Length with 400
    /// </summary>
    let ofBadHttpRequest (request : HttpRequest) (ex : BadHttpRequestException) : IResult =
        let title =
            if ex.StatusCode = StatusCodes.Status413PayloadTooLarge then
                BodyTooLargeTitle
            else
                UnreadableBodyTitle
        problem request ex.StatusCode title (quote ex.Message)

    /// <summary>
    /// The beginnings of the messages with which the form reader reports a body over a size limit of
    /// <see cref="Microsoft.AspNetCore.Http.Features.FormOptions"/>; each goes on with the limit and ends with
    /// <c>" exceeded."</c>.
    /// </summary>
    /// <remarks>
    /// <para>
    /// The form reader reports a broken limit with <see cref="InvalidDataException"/>, the same type it reports a malformed
    /// body with, so only the message tells them apart. ASP.NET Core does not localize these messages.
    /// </para>
    /// <para>
    /// Only the limits on the size of the body are listed. The other limit messages are about a malformed request:
    /// <c>Multipart boundary length limit</c> is on a parameter of the <c>Content-Type</c> header, <c>Line length limit</c>
    /// is also reported for a delimiter line followed by other text, and <c>Multipart header length limit</c> for a body
    /// that does not start with its boundary. The messages that quote the body, such as the one of a malformed part
    /// header, never start with one of these.
    /// </para>
    /// </remarks>
    let private sizeLimitMessagePrefixes = [|
        "Multipart body length limit "
        "Multipart headers length limit "
        "Multipart headers count limit "
        "Form value count limit "
        "Form key length limit "
        "Form value length limit "
        "Form key or value length limit "
    |]

    /// Tells whether the form reader rejected the body because it breaks a size limit of the form options
    let private isFormSizeLimitExceeded (ex : InvalidDataException) =
        let message = ex.Message
        message.EndsWith (" exceeded.", StringComparison.Ordinal)
        && sizeLimitMessagePrefixes
           |> Array.exists (fun prefix -> message.StartsWith (prefix, StringComparison.Ordinal))

    /// <summary>
    /// The message of the exception the buffer of a request body throws once the body grows over
    /// <see cref="Microsoft.AspNetCore.Http.Features.FormOptions.BufferBodyLengthLimit"/>
    /// </summary>
    [<Literal>]
    let private BufferLimitExceededMessage = "Buffer limit exceeded."

    /// Reads the form of a multipart or URL-encoded request, answering a body the form reader cannot parse
    /// with 400 and a body over a server or form size limit with 413
    let readFormAsync (cancellationToken : CancellationToken) (request : HttpRequest) : Task<Result<IFormCollection, IResult>> = task {
        try
            let! form = request.ReadFormAsync cancellationToken
            return Ok form
        with
        // BadHttpRequestException derives from IOException, so it must be matched first
        | :? BadHttpRequestException as ex -> return Error (ofBadHttpRequest request ex)
        | :? InvalidDataException as ex when isFormSizeLimitExceeded ex ->
            return Error (problem request StatusCodes.Status413PayloadTooLarge BodyTooLargeTitle (quote ex.Message))
        | :? InvalidDataException as ex -> return Error (problem request StatusCodes.Status400BadRequest UnreadableBodyTitle (quote ex.Message))
        | :? IOException as ex when String.Equals (ex.Message, BufferLimitExceededMessage, StringComparison.Ordinal) ->
            return Error (problem request StatusCodes.Status413PayloadTooLarge BodyTooLargeTitle BufferLimitExceededMessage)
        | :? IOException ->
            // The multipart reader throws IOException when the body ends before the closing boundary,
            // which is also what a boundary the body does not use looks like
            return
                Error (
                    problem
                        request
                        StatusCodes.Status400BadRequest
                        UnreadableBodyTitle
                        "The multipart body ends before its closing boundary, or its parts are not delimited by the boundary its Content-Type declares."
                )
        | :? ArgumentOutOfRangeException ->
            // The multipart reader throws it for a boundary too long for its buffer, which only a
            // FormOptions.MultipartBoundaryLengthLimit raised above the buffer size lets through; its message repeats
            // the boundary, so it is not quoted
            return
                Error (
                    problem
                        request
                        StatusCodes.Status400BadRequest
                        UnreadableBodyTitle
                        "The multipart boundary its Content-Type declares is too long."
                )
    }

    /// A map path names a variable of the single operation a request carries, such as variables.file or variables.files.0
    let private isVariablePath (path : string) =
        path.Length > VariablesPathPrefix.Length
        && path.StartsWith (VariablesPathPrefix, StringComparison.Ordinal)
        && not (path.EndsWith (".", StringComparison.Ordinal))
        && not (path.Contains ("..", StringComparison.Ordinal))

    let private isVariablePathArray (value : JsonElement) =
        value.ValueKind = JsonValueKind.Array
        && value.GetArrayLength () > 0
        && value.EnumerateArray ()
           |> Seq.forall (fun path ->
               // GetString throws for any kind but String and Null, so the kind is checked first
               path.ValueKind = JsonValueKind.String
               && (match path.GetString () with
                   | null -> false
                   | path -> isVariablePath path))

    /// <summary>
    /// Checks that the <c>map</c> field of a GraphQL multipart request is a JSON object whose values are
    /// non-empty arrays of variable paths.
    /// </summary>
    /// <remarks>
    /// The keys are not checked against the files of the request: files are looked up by the name a variable gives
    /// (see <see cref="HttpContextRequestExecutionContext"/>), and the client of this library names its <c>map</c>
    /// keys by index while it names the file fields by upload name.
    /// </remarks>
    let validateMap (map : string) : Result<unit, string> =
        try
            use document = JsonDocument.Parse map
            let root = document.RootElement

            if
                root.ValueKind = JsonValueKind.Object
                && root.EnumerateObject () |> Seq.forall (fun property -> isVariablePathArray property.Value)
            then
                Ok ()
            else
                Error
                    """The 'map' field must be a JSON object whose values are non-empty arrays of variable paths, such as { "0": ["variables.file"] }."""
        with :? JsonException ->
            Error "The 'map' field is not valid JSON."

    /// Reads the GraphQL request JSON from the operations field of a form request, after checking its map field
    let readOperationsAsync (cancellationToken : CancellationToken) (request : HttpRequest) : Task<Result<string, IResult>> = taskResult {
        let! form = readFormAsync cancellationToken request

        let invalidMultipart detail = problem request StatusCodes.Status400BadRequest InvalidMultipartTitle detail

        let! operations =
            match form.TryGetValue OperationsField with
            | true, values when values.Count > 0 ->
                match values[0] with
                | null -> Error (invalidMultipart "The 'operations' field is empty.")
                | operations -> Ok operations
            | _ -> Error (invalidMultipart "A form request must carry the GraphQL request as JSON in its 'operations' field.")

        do!
            match form.TryGetValue MapField with
            | true, values when values.Count > 0 ->
                match values[0] with
                | null -> Error (invalidMultipart "The 'map' field is empty.")
                | map -> validateMap map |> Result.mapError invalidMultipart
            | _ -> Ok ()

        return operations
    }

type HttpContext with

    /// <summary>
    /// Uses the serializer options of <see cref="IGraphQLOptions"/> to deserialize the body of the
    /// <see cref="Microsoft.AspNetCore.Http.HttpRequest"/> asynchronously into an object of type 'T.
    /// A multipart or URL-encoded form request carries the JSON in its <c>operations</c> field,
    /// as the GraphQL multipart request specification defines.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Every problem with the body is answered with a problem details result rather than an exception:
    /// 400 for a body that is not JSON of the expected shape, for a form without an <c>operations</c> field or with
    /// a <c>map</c> field that is not an object of variable paths, and for a body the form reader cannot parse;
    /// 413 for a body over the server's request body size limit or over a
    /// <see cref="Microsoft.AspNetCore.Http.Features.FormOptions"/> limit.
    /// </para>
    /// <para>
    /// The result never repeats the body. It quotes at most the first 256 characters of the reason the deserializer
    /// or the form reader gives, because such a reason can name parts of the body.
    /// </para>
    /// </remarks>
    /// <typeparam name="'T">Type to deserialize to</typeparam>
    /// <param name="expectedJson">An example of the expected JSON, which a 400 result shows the client.</param>
    /// <returns>
    /// The deserialized object, or the problem details <see cref="IResult"/> that tells why the body could not be deserialized.
    /// </returns>
    [<Extension>]
    member ctx.TryBindJsonAsync<'T> (expectedJson : string) : Task<Result<'T, IResult>> = task {
        let serializerOptions = ctx.RequestServices.GetRequiredService<IOptions<IGraphQLOptions>>().Value.SerializerOptions
        let request = ctx.Request

        let ofDeserialized (value : 'T) =
            match box value with
            | null -> Error (RequestBody.invalidJson request expectedJson "The JSON value is null.")
            | _ -> Ok value

        try
            if request.HasFormContentType then
                match! RequestBody.readOperationsAsync ctx.RequestAborted request with
                | Error problem -> return Error problem
                | Ok operations -> return JsonSerializer.Deserialize<'T> (operations, serializerOptions) |> ofDeserialized
            else
                if not request.Body.CanSeek then
                    request.EnableBuffering ()

                let! value = JsonSerializer.DeserializeAsync<'T> (request.Body, serializerOptions, ctx.RequestAborted)
                return ofDeserialized value
        with
        | :? JsonException as ex -> return Error (RequestBody.invalidJson request expectedJson ex.Message)
        | :? BadHttpRequestException as ex -> return Error (RequestBody.ofBadHttpRequest request ex)
    }
