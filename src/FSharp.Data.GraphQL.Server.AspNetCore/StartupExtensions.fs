namespace FSharp.Data.GraphQL.Server.AspNetCore

open FSharp.Data.GraphQL
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open System.Runtime.CompilerServices
open System.Text.Json

[<Extension>]
type ServiceCollectionExtensions() =

  static let createStandardOptions executor rootFactory endpointUrl =
    { SchemaExecutor = executor
      RootFactory = rootFactory
      SerializerOptions =
        JsonSerializerOptions()
        |> JsonConverterUtils.configureSerializer executor
      WebsocketOptions =
        { EndpointUrl = endpointUrl
          ConnectionInitTimeoutInMs = 3000
          CustomPingHandler = None }
    }

  [<Extension>]
  static member AddGraphQLOptions<'Root>(this : IServiceCollection, executor : Executor<'Root>, rootFactory : unit -> 'Root, endpointUrl : string) =
    this.AddSingleton<GraphQLOptions<'Root>>(createStandardOptions executor rootFactory endpointUrl)

  [<Extension>]
  static member AddGraphQLOptionsWith<'Root>
    ( this : IServiceCollection,
      executor : Executor<'Root>,
      rootFactory : unit -> 'Root,
      endpointUrl : string,
      extraConfiguration : GraphQLOptions<'Root> -> GraphQLOptions<'Root>
    ) =
    this.AddSingleton<GraphQLOptions<'Root>>(createStandardOptions executor rootFactory endpointUrl |> extraConfiguration)

  [<Extension>]
  static member UseWebSocketsForGraphQL<'Root>(this : IApplicationBuilder) =
    this.UseMiddleware<GraphQLWebSocketMiddleware<'Root>>()