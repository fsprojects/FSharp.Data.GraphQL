module FSharp.Data.GraphQL.Tests.AspNetCore.SerializationTests

open Xunit
open System.Text.Json
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Server.AspNetCore
open FSharp.Data.GraphQL.Server.AspNetCore.WebSockets
open System.Text.Json.Serialization

let serializerOptions = Json.serializerOptions

[<Fact>]
let ``Deserializes ConnectionInit correctly`` () =

    let input = "{\"type\":\"connection_init\"}"

    let result = JsonSerializer.Deserialize<ClientMessage> (input, serializerOptions)

    match result with
    | ConnectionInit ValueNone -> () // <-- expected
    | other -> Assert.Fail ($"unexpected actual value: '%A{other}'")

[<Fact>]
let ``Deserializes ConnectionInit with payload correctly`` () =

    let input = "{\"type\":\"connection_init\", \"payload\":\"hello\"}"

    let result = JsonSerializer.Deserialize<ClientMessage> (input, serializerOptions)

    match result with
    | ConnectionInit _ -> () // <-- expected
    | other -> Assert.Fail ($"unexpected actual value: '%A{other}'")

[<Fact>]
let ``Deserializes ClientPing correctly`` () =

    let input = "{\"type\":\"ping\"}"

    let result = JsonSerializer.Deserialize<ClientMessage> (input, serializerOptions)

    match result with
    | ClientPing ValueNone -> () // <-- expected
    | other -> Assert.Fail ($"unexpected actual value '%A{other}'")

[<Fact>]
let ``Deserializes ClientPing with payload correctly`` () =

    let input = "{\"type\":\"ping\", \"payload\":\"ping!\"}"

    let result = JsonSerializer.Deserialize<ClientMessage> (input, serializerOptions)

    match result with
    | ClientPing _ -> () // <-- expected
    | other -> Assert.Fail ($"unexpected actual value '%A{other}'")

[<Fact>]
let ``Deserializes ClientPong correctly`` () =

    let input = "{\"type\":\"pong\"}"

    let result = JsonSerializer.Deserialize<ClientMessage> (input, serializerOptions)

    match result with
    | ClientPong ValueNone -> () // <-- expected
    | other -> Assert.Fail ($"unexpected actual value: '%A{other}'")

[<Fact>]
let ``Deserializes ClientPong with payload correctly`` () =

    let input = "{\"type\":\"pong\", \"payload\": \"pong!\"}"

    let result = JsonSerializer.Deserialize<ClientMessage> (input, serializerOptions)

    match result with
    | ClientPong _ -> () // <-- expected
    | other -> Assert.Fail ($"unexpected actual value: '%A{other}'")

[<Fact>]
let ``Deserializes ClientComplete correctly`` () =

    let input = "{\"id\": \"65fca2b5-f149-4a70-a055-5123dea4628f\", \"type\":\"complete\"}"

    let result = JsonSerializer.Deserialize<ClientMessage> (input, serializerOptions)

    match result with
    | ClientComplete id -> Assert.Equal ("65fca2b5-f149-4a70-a055-5123dea4628f", id)
    | other -> Assert.Fail ($"unexpected actual value: '%A{other}'")

[<Fact>]
let ``Deserializes client subscription correctly`` () =

    let input =
        """{
            "id": "b5d4d2ff-d262-4882-a7b9-d6aec5e4faa6",
            "type": "subscribe",
            "payload" : {
                "query": "subscription { watchMoon(id: \"1\") { id name isMoon } }"
            }
           }
        """

    let result = JsonSerializer.Deserialize<ClientMessage> (input, serializerOptions)

    match result with
    | Subscribe (id, payload) ->
        Assert.Equal ("b5d4d2ff-d262-4882-a7b9-d6aec5e4faa6", id)
        Assert.Equal ("subscription { watchMoon(id: \"1\") { id name isMoon } }", payload.Query)
        Assert.Equal (Skip, payload.OperationName)
        Assert.Equal (Skip, payload.Variables)
    | other -> Assert.Fail ($"unexpected actual value: '%A{other}'")
