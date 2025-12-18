namespace FSharp.Data.GraphQL.Server.AspNetCore

open System
open System.Runtime.InteropServices
open System.Runtime.CompilerServices
open System.Text.Json.Serialization
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Options
open FSharp.Data.GraphQL

[<AutoOpen; Extension>]
module ServiceCollectionExtensions =

    let createStandardOptions executor rootFactory additionalConverters endpointUrl = {
        SchemaExecutor = executor
        RootFactory = rootFactory
        ReadBufferSize = GraphQLOptionsDefaults.ReadBufferSize
        SerializerOptions = Shared.Json.getWSSerializerOptions additionalConverters
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
        /// Adds GraphQL options and services to the service collection.
        /// <para>
        /// It also adds converters to <see href="Microsoft.AspNetCore.Http.Json.JsonOptions" />
        /// to support serialization of GraphQL responses.
        /// </para>
        /// </summary>
        [<Extension; CompiledName "AddGraphQL">]
        member internal services.AddGraphQL<'Root, 'Handler when 'Handler :> GraphQLRequestHandler<'Root> and 'Handler : not struct>
            (
                executorFactory : Func<IServiceProvider, Executor<'Root>>,
                rootFactory : HttpContext -> 'Root,
                [<Optional>] additionalConverters : JsonConverter seq,
                [<Optional; DefaultParameterValue (GraphQLOptionsDefaults.WebSocketEndpoint)>] webSocketEndpointPath : string,
                [<Optional>] configure : Func<GraphQLOptions<'Root>, GraphQLOptions<'Root>>
            ) =

            let additionalConverters =
                additionalConverters
                |> ValueOption.ofObj
                |> ValueOption.defaultValue Seq.empty

            let getOptions sp =
                let executor = executorFactory.Invoke sp
                let options = createStandardOptions executor rootFactory additionalConverters webSocketEndpointPath
                match configure with
                | null -> options
                | _ -> configure.Invoke options

            services
                // We need this for output serialization purposes as we use <see href="IResult" />
                // Surprisingly minimal APIs use Microsoft.AspNetCore.Http.Json.JsonOptions
                // Use if you want to return HTTP responses using minmal APIs IResult interface
                .Configure<HttpClientJsonOptions>(
                    Action<HttpClientJsonOptions>(fun o ->
                        Shared.Json.configureDefaultSerializerOptions additionalConverters o.SerializerOptions
                    )
                )
                .AddSingleton<IOptionsFactory<GraphQLOptions<'Root>>>(
                    fun sp ->
                        { new IOptionsFactory<GraphQLOptions<'Root>> with
                            member this.Create name = (getOptions sp)
                        }
                )
                .Configure<GraphQLOptions<'Root>>(GraphQLOptions.IndentedOptionsName, (fun o -> o.SerializerOptions.WriteIndented <- true))
                .AddSingleton<IOptionsFactory<IGraphQLOptions>>(fun sp ->
                    { new IOptionsFactory<IGraphQLOptions> with
                        member this.Create name =
                            sp.GetRequiredService<IOptionsMonitor<GraphQLOptions<'Root>>>().Get(name)
                    }
                )
                .AddHttpContextAccessor()
                .AddScoped<IInputExecutionContext, HttpContextRequestExecutionContext>()
                .AddScoped<GraphQLRequestHandler<'Root>, 'Handler>()

        /// <summary>
        /// Adds GraphQL options and services to the service collection.
        /// <para>
        /// It also adds converters to <see href="Microsoft.AspNetCore.Http.Json.JsonOptions" />
        /// to support serialization of GraphQL responses.
        /// </para>
        /// </summary>
        [<Extension; CompiledName "AddGraphQL">]
        member internal services.AddGraphQL<'Root>
            (
                executorFactory : Func<IServiceProvider, Executor<'Root>>,
                rootFactory : HttpContext -> 'Root,
                [<Optional>] additionalConverters : JsonConverter seq,
                [<Optional; DefaultParameterValue (GraphQLOptionsDefaults.WebSocketEndpoint)>] webSocketEndpointPath : string,
                [<Optional>] configure : Func<GraphQLOptions<'Root>, GraphQLOptions<'Root>>
            ) =
            services.AddGraphQL<'Root, DefaultGraphQLRequestHandler<'Root>> (executorFactory, rootFactory, additionalConverters, webSocketEndpointPath, configure)

        /// <summary>
        /// Adds GraphQL options and services to the service collection. Requires an executor instance to be provided.
        /// <para>
        /// It also adds converters to <see href="Microsoft.AspNetCore.Http.Json.JsonOptions" />
        /// to support serialization of GraphQL responses.
        /// </para>
        /// </summary>
        [<Extension; CompiledName "AddGraphQL">]
        member services.AddGraphQL<'Root, 'Handler when 'Handler :> GraphQLRequestHandler<'Root> and 'Handler : not struct>
            (
                executor : Executor<'Root>,
                rootFactory : HttpContext -> 'Root,
                [<Optional>] additionalConverters : JsonConverter seq
            ) =
            services.AddGraphQL<'Root, 'Handler> ((fun _ -> executor), rootFactory, additionalConverters, configure = null)

        /// <summary>
        /// Adds GraphQL options and services to the service collection. Requires an executor instance to be provided.
        /// <para>
        /// It also adds converters to <see href="Microsoft.AspNetCore.Http.Json.JsonOptions" />
        /// to support serialization of GraphQL responses.
        /// </para>
        /// </summary>
        [<Extension; CompiledName "AddGraphQL">]
        member services.AddGraphQL<'Root>
            (
                executor : Executor<'Root>,
                rootFactory : HttpContext -> 'Root,
                [<Optional>] additionalConverters : JsonConverter seq
            ) =
            services.AddGraphQL<'Root> ((fun _ -> executor), rootFactory, additionalConverters, configure = null)

        /// <summary>
        /// Adds GraphQL options and services to the service collection. Requires an executor instance to be provided.
        /// <para>
        /// It also adds converters to <see href="Microsoft.AspNetCore.Http.Json.JsonOptions" />
        /// to support serialization of GraphQL responses.
        /// </para>
        /// </summary>
        [<Extension; CompiledName "AddGraphQL">]
        member services.AddGraphQL<'Root, 'Handler when 'Handler :> GraphQLRequestHandler<'Root> and 'Handler : not struct>
            (
                executor : Executor<'Root>,
                rootFactory : HttpContext -> 'Root,
                webSocketEndpointPath : string,
                [<Optional>] additionalConverters : JsonConverter seq
            ) =
            services.AddGraphQL<'Root, 'Handler> ((fun _ -> executor), rootFactory, additionalConverters, webSocketEndpointPath, null)

        /// <summary>
        /// Adds GraphQL options and services to the service collection. Requires an executor instance to be provided.
        /// <para>
        /// It also adds converters to <see href="Microsoft.AspNetCore.Http.Json.JsonOptions" />
        /// to support serialization of GraphQL responses.
        /// </para>
        /// </summary>
        [<Extension; CompiledName "AddGraphQL">]
        member services.AddGraphQL<'Root>
            (
                executor : Executor<'Root>,
                rootFactory : HttpContext -> 'Root,
                webSocketEndpointPath : string,
                [<Optional>] additionalConverters : JsonConverter seq
            ) =
            services.AddGraphQL<'Root> ((fun _ -> executor), rootFactory, additionalConverters, webSocketEndpointPath, null)

        /// <summary>
        /// Adds GraphQL options and services to the service collection. Requires an executor instance to be provided.
        /// <para>
        /// It also adds converters to <see href="Microsoft.AspNetCore.Http.Json.JsonOptions" />
        /// to support serialization of GraphQL responses.
        /// </para>
        /// </summary>
        [<Extension; CompiledName "AddGraphQL">]
        member services.AddGraphQL<'Root, 'Handler when 'Handler :> GraphQLRequestHandler<'Root> and 'Handler : not struct>
            (
                executor : Executor<'Root>,
                rootFactory : HttpContext -> 'Root,
                configure : Func<GraphQLOptions<'Root>, GraphQLOptions<'Root>>,
                [<Optional>] additionalConverters : JsonConverter seq
            ) =
            services.AddGraphQL<'Root, 'Handler> ((fun _ -> executor), rootFactory, additionalConverters, configure = configure)

        /// <summary>
        /// Adds GraphQL options and services to the service collection. Requires an executor instance to be provided.
        /// <para>
        /// It also adds converters to <see href="Microsoft.AspNetCore.Http.Json.JsonOptions" />
        /// to support serialization of GraphQL responses.
        /// </para>
        /// </summary>
        [<Extension; CompiledName "AddGraphQL">]
        member services.AddGraphQL<'Root>
            (
                executor : Executor<'Root>,
                rootFactory : HttpContext -> 'Root,
                configure : Func<GraphQLOptions<'Root>, GraphQLOptions<'Root>>,
                [<Optional>] additionalConverters : JsonConverter seq
            ) =
            services.AddGraphQL<'Root> ((fun _ -> executor), rootFactory, additionalConverters, configure = configure)

        /// <summary>
        /// Adds GraphQL options and services to the service collection. It gets the executor from the service provider.
        /// <para>
        /// It also adds converters to <see href="Microsoft.AspNetCore.Http.Json.JsonOptions" />
        /// to support serialization of GraphQL responses.
        /// </para>
        /// </summary>
        /// <remarks>
        /// The executor must be registered as a singleton service.
        /// </remarks>
        [<Extension; CompiledName "AddGraphQL">]
        member services.AddGraphQL<'Root, 'Handler when 'Handler :> GraphQLRequestHandler<'Root> and 'Handler : not struct>
            (
                rootFactory : HttpContext -> 'Root,
                [<Optional>] additionalConverters : JsonConverter seq
            ) =
            let getExecutorService (sp : IServiceProvider) = sp.GetRequiredService<Executor<'Root>>()
            services.AddGraphQL<'Root, 'Handler> (getExecutorService, rootFactory, additionalConverters, configure = null)

        /// <summary>
        /// Adds GraphQL options and services to the service collection. It gets the executor from the service provider.
        /// <para>
        /// It also adds converters to <see href="Microsoft.AspNetCore.Http.Json.JsonOptions" />
        /// to support serialization of GraphQL responses.
        /// </para>
        /// </summary>
        /// <remarks>
        /// The executor must be registered as a singleton service.
        /// </remarks>
        [<Extension; CompiledName "AddGraphQL">]
        member services.AddGraphQL<'Root>
            (
                rootFactory : HttpContext -> 'Root,
                [<Optional>] additionalConverters : JsonConverter seq
            ) =
            let getExecutorService (sp : IServiceProvider) = sp.GetRequiredService<Executor<'Root>>()
            services.AddGraphQL<'Root> (getExecutorService, rootFactory, additionalConverters, configure = null)

        /// <summary>
        /// Adds GraphQL options and services to the service collection. It gets the executor from the service provider.
        /// <para>
        /// It also adds converters to <see href="Microsoft.AspNetCore.Http.Json.JsonOptions" />
        /// to support serialization of GraphQL responses.
        /// </para>
        /// </summary>
        /// <remarks>
        /// The executor must be registered as a singleton service.
        /// </remarks>
        [<Extension; CompiledName "AddGraphQL">]
        member services.AddGraphQL<'Root, 'Handler when 'Handler :> GraphQLRequestHandler<'Root> and 'Handler : not struct>
            (
                rootFactory : HttpContext -> 'Root,
                [<Optional; DefaultParameterValue (GraphQLOptionsDefaults.WebSocketEndpoint)>] webSocketEndpointPath : string,
                [<Optional>] additionalConverters : JsonConverter seq
            ) =
            let getExecutorService (sp : IServiceProvider) = sp.GetRequiredService<Executor<'Root>>()
            services.AddGraphQL<'Root, 'Handler> (getExecutorService, rootFactory, additionalConverters, webSocketEndpointPath, null)

        /// <summary>
        /// Adds GraphQL options and services to the service collection. It gets the executor from the service provider.
        /// <para>
        /// It also adds converters to <see href="Microsoft.AspNetCore.Http.Json.JsonOptions" />
        /// to support serialization of GraphQL responses.
        /// </para>
        /// </summary>
        /// <remarks>
        /// The executor must be registered as a singleton service.
        /// </remarks>
        [<Extension; CompiledName "AddGraphQL">]
        member services.AddGraphQL<'Root>
            (
                rootFactory : HttpContext -> 'Root,
                [<Optional; DefaultParameterValue (GraphQLOptionsDefaults.WebSocketEndpoint)>] webSocketEndpointPath : string,
                [<Optional>] additionalConverters : JsonConverter seq
            ) =
            let getExecutorService (sp : IServiceProvider) = sp.GetRequiredService<Executor<'Root>>()
            services.AddGraphQL<'Root> (getExecutorService, rootFactory, additionalConverters, webSocketEndpointPath, null)

        /// <summary>
        /// Adds GraphQL options and services to the service collection. It gets the executor from the service provider.
        /// <para>
        /// It also adds converters to <see href="Microsoft.AspNetCore.Http.Json.JsonOptions" />
        /// to support serialization of GraphQL responses.
        /// </para>
        /// </summary>
        /// <remarks>
        /// The executor must be registered as a singleton service.
        /// </remarks>
        [<Extension; CompiledName "AddGraphQL">]
        member services.AddGraphQL<'Root, 'Handler when 'Handler :> GraphQLRequestHandler<'Root> and 'Handler : not struct>
            (
                rootFactory : HttpContext -> 'Root,
                configure : Func<GraphQLOptions<'Root>, GraphQLOptions<'Root>>,
                [<Optional>] additionalConverters : JsonConverter seq
            ) =
            let getExecutorService (sp : IServiceProvider) = sp.GetRequiredService<Executor<'Root>>()
            services.AddGraphQL<'Root, 'Handler> (getExecutorService, rootFactory, additionalConverters, configure = null)

        /// <summary>
        /// Adds GraphQL options and services to the service collection. It gets the executor from the service provider.
        /// <para>
        /// It also adds converters to <see href="Microsoft.AspNetCore.Http.Json.JsonOptions" />
        /// to support serialization of GraphQL responses.
        /// </para>
        /// </summary>
        /// <remarks>
        /// The executor must be registered as a singleton service.
        /// </remarks>
        [<Extension; CompiledName "AddGraphQL">]
        member services.AddGraphQL<'Root>
            (
                rootFactory : HttpContext -> 'Root,
                configure : Func<GraphQLOptions<'Root>, GraphQLOptions<'Root>>,
                [<Optional>] additionalConverters : JsonConverter seq
            ) =
            let getExecutorService (sp : IServiceProvider) = sp.GetRequiredService<Executor<'Root>>()
            services.AddGraphQL<'Root> (getExecutorService, rootFactory, additionalConverters, configure = configure)


[<AutoOpen; Extension>]
module ApplicationBuilderExtensions =

    type IApplicationBuilder with

        /// <summary>
        /// Registers the GraphQL WebSocket middleware to handle WebSocket connections at the configured endpoint.
        /// The middleware will only be applied to requests matching the endpoint path configured in <see cref="GraphQLOptions" />.
        /// </summary>
        [<Extension; CompiledName "UseWebSocketsForGraphQL">]
        member builder.UseWebSocketsForGraphQL<'Root> () =

            let options = builder.ApplicationServices.GetRequiredService<IOptions<GraphQLOptions<'Root>>>()
            let endpointPath = PathString options.Value.WebsocketOptions.EndpointUrl

            builder.UseWhen(
                (fun ctx -> ctx.Request.Path = endpointPath),
                fun appBuilder ->
                    appBuilder.UseMiddleware<GraphQLWebSocketMiddleware<'Root>>() |> ignore
            )
