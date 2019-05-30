namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Types

type GraphQLQuery =
    { ExecutionPlan : ExecutionPlan
      Variables : Map<string, obj> }

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
