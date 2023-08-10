[<AutoOpen>]
module Microsoft.AspNetCore.Http.HttpContextExtensions

open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Runtime.CompilerServices
open System.Text.Json
open Microsoft.AspNetCore.Http

open FSharp.Core
open FsToolkit.ErrorHandling
open Giraffe

type HttpContext with

    /// <summary>
    /// Uses the <see cref="Json.ISerializer"/> to deserializes the entire body
    /// of the <see cref="Microsoft.AspNetCore.Http.HttpRequest"/> asynchronously into an object of type 'T.
    /// </summary>
    /// <typeparam name="'T">Type to deserialize to</typeparam>
    /// <returns>
    /// Retruns a <see cref="System.Threading.Tasks.Task{T}"/>Deserialized object or
    /// <see cref="ProblemDetails">ProblemDetails</see> as <see cref="IResult">IResult</see>
    /// if a body could not be deserialized.
    /// </returns>
    [<Extension>]
    member ctx.TryBindJsonAsync<'T>(expectedJson) = taskResult {
        let serializer = ctx.GetJsonSerializer()
        let request = ctx.Request

        try
            if not request.Body.CanSeek then
                request.EnableBuffering()

            return! serializer.DeserializeAsync<'T> request.Body
        with :? JsonException ->
            let body = request.Body
            body.Seek(0, SeekOrigin.Begin) |> ignore
            let reader = new StreamReader(body)
            let! body = reader.ReadToEndAsync()
            let message = "Expected JSON similar to value in 'expected', but received value as in 'received'"

            let extensions =
                seq {
                    KeyValuePair("expected", (expectedJson :> obj))
                    KeyValuePair("received", (body :> obj))
                }
                |> ImmutableDictionary.CreateRange

            let problem =
                Results.Problem(
                    message,
                    request.Path,
                    StatusCodes.Status400BadRequest,
                    "Invalid JSON body",
                    extensions = extensions
                )

            return! Error problem
    }
