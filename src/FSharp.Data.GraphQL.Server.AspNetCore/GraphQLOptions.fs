namespace FSharp.Data.GraphQL.Server.AspNetCore

open FSharp.Data.GraphQL
open System
open System.Text.Json
open System.Threading.Tasks
open Microsoft.AspNetCore.Http

type PingHandler = IServiceProvider -> JsonDocument option -> Task<JsonDocument option>

type GraphQLTransportWSOptions = {
    EndpointUrl : string
    ConnectionInitTimeoutInMs : int
    CustomPingHandler : PingHandler option
}

type GraphQLOptions<'Root> = {
    SchemaExecutor : Executor<'Root>
    RootFactory : HttpContext -> 'Root
    SerializerOptions : JsonSerializerOptions
    WebsocketOptions : GraphQLTransportWSOptions
}
