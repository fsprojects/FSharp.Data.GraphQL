/// Compilation-only tests that verify all public AddGraphQL overloads resolve correctly.
/// These functions are never called at runtime.
module FSharp.Data.GraphQL.IntegrationTests.Server.ServiceCollectionExtensionsCompilationTests

open System
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection

open FSharp.Data.GraphQL.Server.AspNetCore
open FSharp.Data.GraphQL.Samples.StarWarsApi

let private rootFactory (ctx : HttpContext) : Root = Root(ctx)

let private probe_ExecutorInstance_Handler_NoOptionals () =
    let services = ServiceCollection() :> IServiceCollection
    services.AddGraphQL<Root, DefaultGraphQLRequestHandler<Root>>(Schema.executor, rootFactory) |> ignore

let private probe_ExecutorInstance_Handler_WithWsPath () =
    let services = ServiceCollection() :> IServiceCollection
    services.AddGraphQL<Root, DefaultGraphQLRequestHandler<Root>>(Schema.executor, rootFactory, "/ws") |> ignore

let private probe_ExecutorInstance_Handler_WithConfigure () =
    let services = ServiceCollection() :> IServiceCollection
    let configure = Func<GraphQLOptions<Root>, GraphQLOptions<Root>>(id)
    services.AddGraphQL<Root, DefaultGraphQLRequestHandler<Root>>(Schema.executor, rootFactory, configure) |> ignore

let private probe_ExecutorInstance_NoHandler_NoOptionals () =
    let services = ServiceCollection() :> IServiceCollection
    services.AddGraphQL<Root>(Schema.executor, rootFactory) |> ignore

let private probe_ExecutorInstance_NoHandler_WithWsPath () =
    let services = ServiceCollection() :> IServiceCollection
    services.AddGraphQL<Root>(Schema.executor, rootFactory, "/ws") |> ignore

let private probe_ExecutorInstance_NoHandler_WithConfigure () =
    let services = ServiceCollection() :> IServiceCollection
    let configure = Func<GraphQLOptions<Root>, GraphQLOptions<Root>>(id)
    services.AddGraphQL<Root>(Schema.executor, rootFactory, configure) |> ignore

let private probe_FromDI_Handler_WithWsPath () =
    let services = ServiceCollection() :> IServiceCollection
    services.AddGraphQL<Root, DefaultGraphQLRequestHandler<Root>>(rootFactory, "/ws") |> ignore

let private probe_FromDI_Handler_NoOptionals () =
    let services = ServiceCollection() :> IServiceCollection
    services.AddGraphQL<Root, DefaultGraphQLRequestHandler<Root>>(rootFactory) |> ignore

let private probe_FromDI_Handler_WithConfigure () =
    let services = ServiceCollection() :> IServiceCollection
    let configure = Func<GraphQLOptions<Root>, GraphQLOptions<Root>>(id)
    services.AddGraphQL<Root, DefaultGraphQLRequestHandler<Root>>(rootFactory, configure) |> ignore

let private probe_FromDI_NoHandler_WithWsPath () =
    let services = ServiceCollection() :> IServiceCollection
    services.AddGraphQL<Root>(rootFactory, "/ws") |> ignore

let private probe_FromDI_NoHandler_NoOptionals () =
    let services = ServiceCollection() :> IServiceCollection
    services.AddGraphQL<Root>(rootFactory) |> ignore

let private probe_FromDI_NoHandler_WithConfigure () =
    let services = ServiceCollection() :> IServiceCollection
    let configure = Func<GraphQLOptions<Root>, GraphQLOptions<Root>>(id)
    services.AddGraphQL<Root>(rootFactory, configure) |> ignore
