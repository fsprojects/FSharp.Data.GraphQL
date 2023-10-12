namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open System.Collections.Immutable
open System.Text.Json
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Types

type GraphQLQuery =
    { ExecutionPlan : ExecutionPlan
      Variables : ImmutableDictionary<string, JsonElement> }

type WebSocketClientMessage =
    | ConnectionInit
    | ConnectionTerminate
    | Start of id : string * payload : GraphQLQuery
    | Stop of id : string
    | ParseError of id : string option * err : string

type WebSocketServerMessage =
    | ConnectionAck
    | ConnectionError of err : string
    | Data of id : string * payload : Output
    | Error of id : string option * err : string
    | Complete of id : string
with
    static member OfResponseContent(id, subscription : GQLSubscriptionResponseContent) =
        match subscription with
        | SubscriptionResult data -> Data (id, data)
        | SubscriptionErrors (data, errors) -> Error (Some id, JsonSerializer.Serialize(errors, Json.serializerOptions))
    static member OfResponseContent(id, deferred : GQLDeferredResponseContent) =
        match deferred with
        | DeferredResult (data, path) -> Data (id, Map.ofList [ "data", data; "path", path ])
        | DeferredErrors (data, errors, path) -> Error (Some id, JsonSerializer.Serialize(errors, Json.serializerOptions))
