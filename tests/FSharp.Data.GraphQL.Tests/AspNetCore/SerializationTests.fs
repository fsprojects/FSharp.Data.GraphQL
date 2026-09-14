module FSharp.Data.GraphQL.Tests.AspNetCore.SerializationTests

open Xunit
open System.Text.Json
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Shared
open FSharp.Data.GraphQL.Shared.WebSockets
open System.Text.Json.Serialization

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

open FSharp.Data.GraphQL

let private serializePayload (payload : SubscriptionExecutionResult) =
    let message : RawServerMessage = { Id = ValueSome "1"; Type = "next"; Payload = ValueSome (ExecutionResult payload) }
    JsonSerializer.Serialize (message, serializerOptions)

let private hasProperty (name : string) (element : JsonElement) =
    let mutable ignored = Unchecked.defaultof<JsonElement>
    element.TryGetProperty (name, &ignored)

[<Fact>]
let ``Serializes incremental payload with path and hasNext`` () =
    let json = serializePayload (SubscriptionExecutionResult.CreateIncremental (box [| box 1 |], [], [ box "numbers"; box 0 ]))
    use document = JsonDocument.Parse json
    let payload = document.RootElement.GetProperty "payload"
    let data = payload.GetProperty "data"
    Assert.Equal (JsonValueKind.Array, data.ValueKind)
    Assert.Equal (1, data[0].GetInt32 ())
    let path = payload.GetProperty "path"
    Assert.Equal ("numbers", path[0].GetString ())
    Assert.Equal (0, path[1].GetInt32 ())
    Assert.True (payload.GetProperty("hasNext").GetBoolean (), $"Expected hasNext to be true in {json}")

[<Fact>]
let ``Serializes final incremental payload with hasNext only`` () =
    let json = serializePayload (SubscriptionExecutionResult.CreateCompleted ())
    use document = JsonDocument.Parse json
    let payload = document.RootElement.GetProperty "payload"
    Assert.False (payload.GetProperty("hasNext").GetBoolean (), $"Expected hasNext to be false in {json}")
    Assert.False (hasProperty "data" payload, $"Expected no data in {json}")
    Assert.False (hasProperty "path" payload, $"Expected no path in {json}")

[<Fact>]
let ``Serializes complete payload without path and hasNext`` () =
    let json = serializePayload (SubscriptionExecutionResult.Create (NameValueLookup.ofList [ "name", upcast "R2-D2" ], []))
    use document = JsonDocument.Parse json
    let payload = document.RootElement.GetProperty "payload"
    Assert.Equal ("R2-D2", payload.GetProperty("data").GetProperty("name").GetString ())
    Assert.False (hasProperty "path" payload, $"Expected no path in {json}")
    Assert.False (hasProperty "hasNext" payload, $"Expected no hasNext in {json}")

[<Fact>]
let ``Serializes errors payload with null data as before`` () =
    let json = serializePayload (SubscriptionExecutionResult.CreateErrors [ GQLProblemDetails.CreateWithKind ("Boom", Execution, [ box "numbers" ]) ])
    use document = JsonDocument.Parse json
    let payload = document.RootElement.GetProperty "payload"
    Assert.Equal (JsonValueKind.Null, payload.GetProperty("data").ValueKind)
    Assert.Equal ("Boom", (payload.GetProperty "errors").Item(0).GetProperty("message").GetString ())
