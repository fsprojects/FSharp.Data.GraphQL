namespace FSharp.Data.GraphQL.Server.AspNetCore

open System
open System.Threading
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Options

/// <summary>
/// Accepts <c>graphql-transport-ws</c> WebSocket connections and runs each one as a
/// <see cref="GraphQLWebSocketConnection{Root}"/> for as long as the request and the application live.
/// </summary>
type GraphQLWebSocketMiddleware<'Root>
    (
        next : RequestDelegate, // must be kept for middleware signature compatibility
        applicationLifetime : IHostApplicationLifetime,
        serviceProvider : IServiceProvider,
        logger : ILogger<GraphQLWebSocketMiddleware<'Root>>,
        options : IOptions<GraphQLOptions<'Root>>
    ) =

    let options = options.Value

    /// Runs the WebSocket connection of the request, or rejects a request that is not a WebSocket one.
    member _.InvokeAsync (ctx : HttpContext) : Task =
        if ctx.WebSockets.IsWebSocketRequest then
            task {
                use! socket = ctx.WebSockets.AcceptWebSocketAsync ("graphql-transport-ws")
                use connectionLifetime =
                    CancellationTokenSource.CreateLinkedTokenSource (ctx.RequestAborted, applicationLifetime.ApplicationStopping)
                let connection = GraphQLWebSocketConnection<'Root> (ctx, socket, options, serviceProvider, logger, connectionLifetime.Token)
                try
                    do! connection.RunAsync ()
                with ex ->
                    logger.LogError (ex, "Cannot handle WebSocket message.")
            }
        else
            TypedResults.Problem (
                title = "WebSocket connection expected.",
                detail = $"'{options.WebsocketOptions.EndpointUrl}' endpoint only accepts WebSocket connections.",
                statusCode = StatusCodes.Status400BadRequest
            )
            :> IResult
            |> _.ExecuteAsync(ctx)
