module FSharp.Data.GraphQL.Tests.AspNetCore.InvalidMessageTests

open FSharp.Data.GraphQL.Tests.AspNetCore
open FSharp.Data.GraphQL.Server.AspNetCore
open System.Text.Json
open Xunit

let toClientMessage (theInput : string) =
    let serializerOptions = new JsonSerializerOptions()
    serializerOptions.PropertyNameCaseInsensitive <- true
    serializerOptions.Converters.Add(new ClientMessageConverter<Root>(TestSchema.executor))
    serializerOptions.Converters.Add(new RawServerMessageConverter())
    JsonSerializer.Deserialize<ClientMessage>(theInput, serializerOptions)

let willResultInInvalidMessage expectedExplanation input =
    try
        let result =
            input
            |> toClientMessage
        Assert.Fail(sprintf "should have failed, but succeeded with result: '%A'" result)
    with
    | :? JsonException as ex ->
        Assert.Equal(expectedExplanation, ex.Message)
    | :? InvalidMessageException as ex ->
        Assert.Equal(expectedExplanation, ex.Message)

let willResultInJsonException input =
    try
        input
        |> toClientMessage
        |> ignore
        Assert.Fail("expected that a JsonException would have already been thrown at this point")
    with
    | :? JsonException as ex ->
        Assert.True(true)

[<Fact>]
let ``Unknown message type will result in invalid message`` () =
    """{
          "type": "connection_start"
       }
    """
    |> willResultInInvalidMessage "invalid type \"connection_start\" specified by client."

[<Fact>]
let ``Type not specified will result in invalid message`` () =
    """{
          "payload": "hello, let us connect"
       }
    """
    |> willResultInInvalidMessage "property \"type\" is missing"

[<Fact>]
let ``No payload in subscribe message will result in invalid message`` () =
    """{
          "type": "subscribe",
          "id": "b5d4d2ff-d262-4882-a7b9-d6aec5e4faa6"
       }
    """
    |> willResultInInvalidMessage "payload is required for this message, but none was present."

[<Fact>]
let ``Null payload json in subscribe message will result in invalid message`` () =
    """{
          "type": "subscribe",
          "id": "b5d4d2ff-d262-4882-a7b9-d6aec5e4faa6",
          "payload": null
       }
    """
    |> willResultInInvalidMessage "payload is required for this message, but none was present."

[<Fact>]
let ``Payload type of number in subscribe message will result in invalid message`` () =
    """{
        "type": "subscribe",
        "id": "b5d4d2ff-d262-4882-a7b9-d6aec5e4faa6",
        "payload": 42
    }
    """
    |> willResultInInvalidMessage "The JSON value could not be converted to FSharp.Data.GraphQL.Server.AspNetCore.GraphQLRequest. Path: $ | LineNumber: 0 | BytePositionInLine: 2."

[<Fact>]
let ``No id in subscribe message will result in invalid message`` () =
    """{
          "type": "subscribe",
          "payload": {
              "query": "subscription { watchMoon(id: \"1\") { id name isMoon } }"
          }
       }
    """
    |> willResultInInvalidMessage "property \"id\" is required for this message but was not present."

[<Fact>]
let ``String payload wrongly used in subscribe will result in invalid message`` () =
    """{
          "type": "subscribe",
          "id": "b5d4d2ff-d262-4882-a7b9-d6aec5e4faa6",
          "payload": "{\"query\": \"subscription { watchMoon(id: \\\"1\\\") { id name isMoon } }\"}"
       }
    """
    |> willResultInInvalidMessage "The JSON value could not be converted to FSharp.Data.GraphQL.Server.AspNetCore.GraphQLRequest. Path: $ | LineNumber: 0 | BytePositionInLine: 79."

[<Fact>]
let ``Id is incorrectly a number in a subscribe message will result in JsonException`` () =
    """{
          "type": "subscribe",
          "id": 42,
          "payload": {
            "query": "subscription { watchMoon(id: \"1\") { id name isMoon } }"
          }
       }
    """
    |> willResultInJsonException

[<Fact>]
let ``Typo in one of the messages root properties will result in invalid message`` () =
    """{
        "typo": "subscribe",
        "id": "b5d4d2ff-d262-4882-a7b9-d6aec5e4faa6",
        "payload": {
        "query": "subscription { watchMoon(id: \"1\") { id name isMoon } }"
        }
    }
    """
    |> willResultInInvalidMessage "unknown property \"typo\""

[<Fact>]
let ``Complete message without an id will result in invalid message`` () =
    """{
        "type": "complete"
       }
    """
    |> willResultInInvalidMessage "property \"id\" is required for this message but was not present."

[<Fact>]
let ``Complete message with a null id will result in invalid message`` () =
    """{
        "type": "complete",
        "id": null
       }
    """
    |> willResultInInvalidMessage "property \"id\" is required for this message but was not present."





