namespace FSharp.Data.GraphQL.Server.AspNetCore

open System
open System.Runtime.InteropServices
open System.Runtime.CompilerServices
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Options
open FSharp.Data.GraphQL

[<AutoOpen; Extension>]
module ServiceCollectionExtensions =

    let createStandardOptions executor rootFactory endpointUrl = {
        SchemaExecutor = executor
        RootFactory = rootFactory
        ReadBufferSize = GraphQLOptionsDefaults.ReadBufferSize
        SerializerOptions = Json.serializerOptions
        WebsocketOptions = {
            EndpointUrl = endpointUrl
            ConnectionInitTimeout = TimeSpan.FromMilliseconds (GraphQLOptionsDefaults.WebSocketConnectionInitTimeoutInMs)
            CustomPingHandler = ValueNone
        }
    }

    // See https://learn.microsoft.com/en-us/dotnet/api/microsoft.aspnetcore.mvc.jsonoptions
    type MvcJsonOptions = Microsoft.AspNetCore.Mvc.JsonOptions
    // See https://learn.microsoft.com/en-us/dotnet/api/microsoft.aspnetcore.http.json.jsonoptions
    type HttpClientJsonOptions = Microsoft.AspNetCore.Http.Json.JsonOptions

    type IServiceCollection with

        /// <summary>
        /// Adds GraphQL options to the service collection.
        /// <para>
        /// It also adds converters to <see href="Microsoft.AspNetCore.Http.Json.JsonOptions" />
        /// to support serialization of GraphQL responses.
        /// </para>
        /// </summary>
        [<Extension; CompiledName "AddGraphQLOptions">]
        member services.AddGraphQLOptions<'Root>
            (
                executorFactory : Func<IServiceProvider, Executor<'Root>>,
                rootFactory : HttpContext -> 'Root,
                [<Optional; DefaultParameterValue (GraphQLOptionsDefaults.WebSocketEndpoint)>] webSocketEndpointUrl : string,
                [<Optional>] configure : Func<GraphQLOptions<'Root>, GraphQLOptions<'Root>>
            ) =
            let getOptions sp =
                let executor = executorFactory.Invoke sp
                let options = createStandardOptions executor rootFactory webSocketEndpointUrl
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
                .AddSingleton<IOptionsFactory<GraphQLOptions<'Root>>>(
                    fun sp ->
                        { new IOptionsFactory<GraphQLOptions<'Root>> with
                            member this.Create name = (getOptions sp)
                        }
                )
                .Configure<GraphQLOptions<'Root>>(Giraffe.HttpHandlers.IndentedOptionsName, (fun o -> o.SerializerOptions.WriteIndented <- true))
                .AddSingleton<IOptionsFactory<IGraphQLOptions>>(fun sp ->
                    { new IOptionsFactory<IGraphQLOptions> with
                        member this.Create name =
                            sp.GetRequiredService<IOptionsMonitor<GraphQLOptions<'Root>>>().Get(name)
                    }
                )

        /// <summary>
        /// Adds GraphQL options to the service collection. Requires an executor instance to be provided.
        /// <para>
        /// It also adds converters to <see href="Microsoft.AspNetCore.Http.Json.JsonOptions" />
        /// to support serialization of GraphQL responses.
        /// </para>
        /// </summary>
        [<Extension; CompiledName "AddGraphQLOptions">]
        member services.AddGraphQLOptions<'Root>
            (
                executor : Executor<'Root>,
                rootFactory : HttpContext -> 'Root,
                [<Optional; DefaultParameterValue (GraphQLOptionsDefaults.WebSocketEndpoint)>] webSocketEndpointUrl : string,
                [<Optional>] configure : Func<GraphQLOptions<'Root>, GraphQLOptions<'Root>>
            ) =
            services.AddGraphQLOptions ((fun _ -> executor), rootFactory, webSocketEndpointUrl, configure)

        /// <summary>
        /// Adds GraphQL options to the service collection. It gets the executor from the service provider.
        /// <para>
        /// It also adds converters to <see href="Microsoft.AspNetCore.Http.Json.JsonOptions" />
        /// to support serialization of GraphQL responses.
        /// </para>
        /// </summary>
        /// <remarks>
        /// The executor must be registered as a singleton service.
        /// </remarks>
        [<Extension; CompiledName "AddGraphQLOptions">]
        member services.AddGraphQLOptions<'Root>
            (
                rootFactory : HttpContext -> 'Root,
                [<Optional; DefaultParameterValue (GraphQLOptionsDefaults.WebSocketEndpoint)>] webSocketEndpointUrl : string,
                [<Optional>] configure : Func<GraphQLOptions<'Root>, GraphQLOptions<'Root>>
            ) =
            let getExecutorService (sp : IServiceProvider) = sp.GetRequiredService<Executor<'Root>>()
            services.AddGraphQLOptions (getExecutorService, rootFactory, webSocketEndpointUrl, configure)


[<AutoOpen; Extension>]
module ApplicationBuilderExtensions =

    type IApplicationBuilder with

        [<Extension; CompiledName "UseWebSocketsForGraphQL">]
        member builder.UseWebSocketsForGraphQL<'Root> () = builder.UseMiddleware<GraphQLWebSocketMiddleware<'Root>> ()
