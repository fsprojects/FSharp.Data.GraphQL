namespace FSharp.Data.GraphQL.Server.AspNetCore

open System
open System.Runtime.InteropServices
open System.Runtime.CompilerServices
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open FSharp.Data.GraphQL

[<AutoOpen; Extension>]
module ServiceCollectionExtensions =

    let createStandardOptions executor rootFactory endpointUrl = {
        SchemaExecutor = executor
        RootFactory = rootFactory
        SerializerOptions = Json.serializerOptions
        WebsocketOptions = {
            EndpointUrl = endpointUrl
            ConnectionInitTimeoutInMs = 3000
            CustomPingHandler = None
        }
    }

    // See https://learn.microsoft.com/en-us/dotnet/api/microsoft.aspnetcore.mvc.jsonoptions
    type MvcJsonOptions = Microsoft.AspNetCore.Mvc.JsonOptions
    // See https://learn.microsoft.com/en-us/dotnet/api/microsoft.aspnetcore.http.json.jsonoptions
    type HttpClientJsonOptions = Microsoft.AspNetCore.Http.Json.JsonOptions

    type IServiceCollection with

        [<Extension; CompiledName "AddGraphQLOptions">]
        member services.AddGraphQLOptions<'Root>
            (
                executor : Executor<'Root>,
                rootFactory : HttpContext -> 'Root,
                endpointUrl : string,
                [<Optional>] configure : Func<GraphQLOptions<'Root>, GraphQLOptions<'Root>>
            ) =
            let options =
                let options = createStandardOptions executor rootFactory endpointUrl
                match configure with
                | null -> options
                | _ -> configure.Invoke options
            services
                // We need this for output serialization purposes as we use <see href="IResult" />
                // Surprisingly minimal APIs use Microsoft.AspNetCore.Http.Json.JsonOptions
                // Use if you want to return HTTP responses using minmal APIs IResult interface
                .Configure<HttpClientJsonOptions>(
                    Action<HttpClientJsonOptions>(fun o ->
                        Json.configureDefaultSerializerOptions Seq.empty o.SerializerOptions
                    )
                )
                .AddSingleton<GraphQLOptions<'Root>>(options)
                .AddSingleton<IGraphQLOptions, GraphQLOptions<'Root>>(fun sp -> sp.GetRequiredService<GraphQLOptions<'Root>>())

[<AutoOpen; Extension>]
module ApplicationBuilderExtensions =

    type IApplicationBuilder with

        [<Extension; CompiledName "UseWebSocketsForGraphQL">]
        member builder.UseWebSocketsForGraphQL<'Root> () = builder.UseMiddleware<GraphQLWebSocketMiddleware<'Root>> ()
