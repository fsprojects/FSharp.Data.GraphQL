namespace FSharp.Data.GraphQL.Samples.GiraffeServer

open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Types

type StartPayload =
    { ExecutionPlan : ExecutionPlan
      Variables : Map<string, obj> }

type SubscriptionSocketClientMessage =
    | ConnectionInit
    | ConnectionTerminate
    | Start of id : string * payload : StartPayload
    | Stop of id : string
    | ParseError of id : string option * err : string

type SubscriptionSocketServerMessage =
    | ConnectionAck
    | ConnectionError of err : string
    | Data of id : string * result : Output
    | Error of id : string option * err : string
    | Complete of id : string