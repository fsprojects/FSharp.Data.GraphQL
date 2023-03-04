namespace FSharp.Data.GraphQL.Server.AppInfrastructure

open FSharp.Data.GraphQL
open System
open System.Text.Json
open System.Threading.Tasks

type PingHandler =
  IServiceProvider -> JsonDocument option -> Task<JsonDocument option>

type GraphQLTransportWSOptions =
  { EndpointUrl: string
    ConnectionInitTimeoutInMs: int
    CustomPingHandler : PingHandler option }

type GraphQLOptions<'Root> =
 { SchemaExecutor: Executor<'Root>
   RootFactory: unit -> 'Root
   SerializerOptions: JsonSerializerOptions
   WebsocketOptions: GraphQLTransportWSOptions
 }