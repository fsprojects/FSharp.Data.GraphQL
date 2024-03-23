namespace FSharp.Data.GraphQL.Server.AspNetCore

open FSharp.Data.GraphQL
open System
open System.Text.Json
open System.Threading.Tasks
open Microsoft.AspNetCore.Http

type PingHandler = IServiceProvider -> JsonDocument voption -> Task<JsonDocument voption>

type GraphQLTransportWSOptions = {
    EndpointUrl : string
    ConnectionInitTimeoutInMs : int
    CustomPingHandler : PingHandler voption
}

type IGraphQLOptions =
    abstract member SerializerOptions : JsonSerializerOptions
    abstract member WebsocketOptions : GraphQLTransportWSOptions

type GraphQLOptions<'Root> = {
    SchemaExecutor : Executor<'Root>
    RootFactory : HttpContext -> 'Root
    SerializerOptions : JsonSerializerOptions
    WebsocketOptions : GraphQLTransportWSOptions
} with

    interface IGraphQLOptions with
        member this.SerializerOptions = this.SerializerOptions
        member this.WebsocketOptions = this.WebsocketOptions
