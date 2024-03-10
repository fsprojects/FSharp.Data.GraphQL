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

type IGraphQLOptions =
    abstract member SerializerOptions : JsonSerializerOptions
    abstract member WebsocketOptions : GraphQLTransportWSOptions
    abstract member GetSerializerOptionsIdented : unit -> JsonSerializerOptions

type GraphQLOptions<'Root> = {
    SchemaExecutor : Executor<'Root>
    RootFactory : HttpContext -> 'Root
    SerializerOptions : JsonSerializerOptions
    WebsocketOptions : GraphQLTransportWSOptions
} with

    member options.GetSerializerOptionsIdented () =
        let options = JsonSerializerOptions (options.SerializerOptions)
        options.WriteIndented <- true
        options

    interface IGraphQLOptions with
        member this.SerializerOptions = this.SerializerOptions
        member this.WebsocketOptions = this.WebsocketOptions
        member this.GetSerializerOptionsIdented () = this.GetSerializerOptionsIdented ()
