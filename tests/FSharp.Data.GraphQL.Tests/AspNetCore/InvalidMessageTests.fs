module FSharp.Data.GraphQL.Tests.AspNetCore.InvalidMessageTests

open System.Text.Json
open Xunit
open FSharp.Data.GraphQL.Server.AspNetCore
open FSharp.Data.GraphQL.Server.AspNetCore.WebSockets

let toClientMessage (theInput : string) =
    let serializerOptions = Json.serializerOptions
    JsonSerializer.Deserialize<ClientMessage> (theInput, serializerOptions)

let willResultInInvalidMessage expectedExplanation input =
    try
        let result = input |> toClientMessage
        Assert.Fail (sprintf "should have failed, but succeeded with result: '%A'" result)
    with
    | :? JsonException as ex -> Assert.Equal (expectedExplanation, ex.Message)
    | :? InvalidMessageException as ex -> Assert.Equal (expectedExplanation, ex.Message)

let willResultInJsonException input =
    try
        input |> toClientMessage |> ignore
        Assert.Fail ("Expected that a JsonException would have already been thrown at this point")
    with :? JsonException as ex ->
        Assert.True (true)

[<Fact>]
let ``Unknown message type will result in invalid message`` () =
    """{
          "type": "connection_start"
       }
    """
    |> willResultInInvalidMessage "Invalid type \"connection_start\" specified by client."

[<Fact>]
let ``Type not specified will result in invalid message`` () =
    """{
          "payload": "hello, let us connect"
       }
    """
    |> willResultInInvalidMessage "Property \"type\" is missing"

[<Fact>]
let ``No payload in subscribe message will result in invalid message`` () =
    """{
          "type": "subscribe",
          "id": "b5d4d2ff-d262-4882-a7b9-d6aec5e4faa6"
       }
    """
    |> willResultInInvalidMessage "Payload is required for this message, but none was present."

[<Fact>]
let ``Null payload json in subscribe message will result in invalid message`` () =
    """{
          "type": "subscribe",
          "id": "b5d4d2ff-d262-4882-a7b9-d6aec5e4faa6",
          "payload": null
       }
    """
    |> willResultInInvalidMessage "Invalid payload received: Failed to parse type FSharp.Data.GraphQL.Server.AspNetCore.GQLRequestContent: expected JSON object, found Null."

[<Fact>]
let ``Payload type of number in subscribe message will result in invalid message`` () =
    """{
        "type": "subscribe",
        "id": "b5d4d2ff-d262-4882-a7b9-d6aec5e4faa6",
        "payload": 42
    }
    """
    |> willResultInInvalidMessage
        "Invalid payload received: Failed to parse type FSharp.Data.GraphQL.Server.AspNetCore.GQLRequestContent: expected JSON object, found Number."

[<Fact>]
let ``No id in subscribe message will result in invalid message`` () =
    """{
          "type": "subscribe",
          "payload": {
              "query": "subscription { watchMoon(id: \"1\") { id name isMoon } }"
          }
       }
    """
    |> willResultInInvalidMessage "Property \"id\" is required for this message but was not present."

[<Fact>]
let ``String payload wrongly used in subscribe will result in invalid message`` () =
    """{
          "type": "subscribe",
          "id": "b5d4d2ff-d262-4882-a7b9-d6aec5e4faa6",
          "payload": "{\"query\": \"subscription { watchMoon(id: \\\"1\\\") { id name isMoon } }\"}"
       }
    """
    |> willResultInInvalidMessage
        "Invalid payload received: Failed to parse type FSharp.Data.GraphQL.Server.AspNetCore.GQLRequestContent: expected JSON object, found String."

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
    |> willResultInInvalidMessage "Unknown property \"typo\""

[<Fact>]
let ``Complete message without an id will result in invalid message`` () =
    """{
        "type": "complete"
       }
    """
    |> willResultInInvalidMessage "Property \"id\" is required for this message but was not present."

[<Fact>]
let ``Complete message with a null id will result in invalid message`` () =
    """{
        "type": "complete",
        "id": null
       }
    """
    |> willResultInInvalidMessage "Property \"id\" is required for this message but was not present."
