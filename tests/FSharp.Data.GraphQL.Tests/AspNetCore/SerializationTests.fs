module FSharp.Data.GraphQL.Tests.AspNetCore.SerializationTests

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Text.Json
open System.Text.Json.Serialization

open Xunit

open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Shared
open FSharp.Data.GraphQL.Server.AspNetCore.GraphQLSubscriptionsManagement
open FSharp.Data.GraphQL.Server.AspNetCore.ObservableErrorHandling
open FSharp.Data.GraphQL.Shared.WebSockets

[<Fact>]
let ``Deserializes ConnectionInit correctly`` () =

    let input = "{\"type\":\"connection_init\"}"

    let result = JsonSerializer.Deserialize<ClientMessage>(input, serializerOptions)

    match result with
    | ConnectionInit ValueNone -> () // <-- expected
    | other -> Assert.Fail ($"unexpected actual value: '%A{other}'")

[<Fact>]
let ``Deserializes ConnectionInit with payload correctly`` () =

    let input = "{\"type\":\"connection_init\", \"payload\":\"hello\"}"

    let result = JsonSerializer.Deserialize<ClientMessage>(input, serializerOptions)

    match result with
    | ConnectionInit _ -> () // <-- expected
    | other -> Assert.Fail ($"unexpected actual value: '%A{other}'")

[<Fact>]
let ``Deserializes ClientPing correctly`` () =

    let input = "{\"type\":\"ping\"}"

    let result = JsonSerializer.Deserialize<ClientMessage>(input, serializerOptions)

    match result with
    | ClientPing ValueNone -> () // <-- expected
    | other -> Assert.Fail ($"unexpected actual value '%A{other}'")

[<Fact>]
let ``Deserializes ClientPing with payload correctly`` () =

    let input = "{\"type\":\"ping\", \"payload\":\"ping!\"}"

    let result = JsonSerializer.Deserialize<ClientMessage>(input, serializerOptions)

    match result with
    | ClientPing _ -> () // <-- expected
    | other -> Assert.Fail ($"unexpected actual value '%A{other}'")

[<Fact>]
let ``Deserializes ClientPong correctly`` () =

    let input = "{\"type\":\"pong\"}"

    let result = JsonSerializer.Deserialize<ClientMessage>(input, serializerOptions)

    match result with
    | ClientPong ValueNone -> () // <-- expected
    | other -> Assert.Fail ($"unexpected actual value: '%A{other}'")

[<Fact>]
let ``Deserializes ClientPong with payload correctly`` () =

    let input = "{\"type\":\"pong\", \"payload\": \"pong!\"}"

    let result = JsonSerializer.Deserialize<ClientMessage>(input, serializerOptions)

    match result with
    | ClientPong _ -> () // <-- expected
    | other -> Assert.Fail ($"unexpected actual value: '%A{other}'")

[<Fact>]
let ``Deserializes ClientComplete correctly`` () =

    let input = "{\"id\": \"65fca2b5-f149-4a70-a055-5123dea4628f\", \"type\":\"complete\"}"

    let result = JsonSerializer.Deserialize<ClientMessage>(input, serializerOptions)

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

    let result = JsonSerializer.Deserialize<ClientMessage>(input, serializerOptions)

    match result with
    | Subscribe (id, payload) ->
        Assert.Equal ("b5d4d2ff-d262-4882-a7b9-d6aec5e4faa6", id)
        Assert.Equal ("subscription { watchMoon(id: \"1\") { id name isMoon } }", payload.Query)
        Assert.Equal (Skip, payload.OperationName)
        Assert.Equal (Skip, payload.Variables)
    | other -> Assert.Fail ($"unexpected actual value: '%A{other}'")

open FSharp.Data.GraphQL

let private serializePayload (payload : SubscriptionExecutionResult) =
    let message : RawServerMessage = {
        Id = ValueSome "1"
        Type = "next"
        Payload = ValueSome (ExecutionResult payload)
    }
    JsonSerializer.Serialize (message, serializerOptions)

let private hasProperty (name : string) (element : JsonElement) =
    let mutable ignored = Unchecked.defaultof<JsonElement>
    element.TryGetProperty (name, &ignored)

[<Fact>]
let ``Serializes incremental payload with path and hasNext`` () =
    let json =
        serializePayload (SubscriptionExecutionResult.CreateIncremental (box [| box 1 |], [], [ box "numbers"; box 0 ]))
    use document = JsonDocument.Parse json
    let payload = document.RootElement.GetProperty "payload"
    let data = payload.GetProperty "data"
    Assert.Equal (JsonValueKind.Array, data.ValueKind)
    Assert.Equal (1, data[0].GetInt32())
    let path = payload.GetProperty "path"
    Assert.Equal ("numbers", path[0].GetString())
    Assert.Equal (0, path[1].GetInt32())
    Assert.True (payload.GetProperty("hasNext").GetBoolean(), $"Expected hasNext to be true in {json}")

[<Fact>]
let ``Serializes final incremental payload with hasNext only`` () =
    let json = serializePayload (SubscriptionExecutionResult.CreateCompleted ())
    use document = JsonDocument.Parse json
    let payload = document.RootElement.GetProperty "payload"
    Assert.False (payload.GetProperty("hasNext").GetBoolean(), $"Expected hasNext to be false in {json}")
    Assert.False (hasProperty "data" payload, $"Expected no data in {json}")
    Assert.False (hasProperty "path" payload, $"Expected no path in {json}")

[<Fact>]
let ``Serializes complete payload without path and hasNext`` () =
    let json =
        serializePayload (SubscriptionExecutionResult.Create (NameValueLookup.ofList [ "name", upcast "R2-D2" ], []))
    use document = JsonDocument.Parse json
    let payload = document.RootElement.GetProperty "payload"
    Assert.Equal ("R2-D2", payload.GetProperty("data").GetProperty("name").GetString())
    Assert.False (hasProperty "path" payload, $"Expected no path in {json}")
    Assert.False (hasProperty "hasNext" payload, $"Expected no hasNext in {json}")

[<Fact>]
let ``Serializes errors payload with null data as before`` () =
    let json =
        serializePayload (SubscriptionExecutionResult.CreateErrors [ GQLProblemDetails.CreateWithKind ("Boom", Execution, [ box "numbers" ]) ])
    use document = JsonDocument.Parse json
    let payload = document.RootElement.GetProperty "payload"
    Assert.Equal (JsonValueKind.Null, payload.GetProperty("data").ValueKind)
    Assert.Equal ("Boom", (payload.GetProperty "errors").Item(0).GetProperty("message").GetString())

[<Fact>]
let ``Serializes an error message with its problem details as the payload`` () =
    // Regression test: RawServerMessageConverter used to write the ErrorMessages payload without a preceding
    // WritePropertyName ("payload"), which Utf8JsonWriter rejects, so every "error" message failed to serialize
    let message : RawServerMessage = {
        Id = ValueSome "1"
        Type = "error"
        Payload = ValueSome (ErrorMessages [ GQLProblemDetails.Create "Boom" ])
    }
    let json = JsonSerializer.Serialize (message, serializerOptions)
    use document = JsonDocument.Parse json
    let root = document.RootElement
    Assert.Equal ("error", root.GetProperty("type").GetString())
    Assert.Equal ("1", root.GetProperty("id").GetString())
    let payload = root.GetProperty "payload"
    Assert.Equal (JsonValueKind.Array, payload.ValueKind)
    Assert.Equal ("Boom", payload[0].GetProperty("message").GetString())

[<Fact>]
let ``Serializes a pong message with its payload`` () =
    // Regression test: the same missing WritePropertyName ("payload") affected a pong carrying a custom response
    use responseDocument = JsonDocument.Parse "\"pong!\""
    let message : RawServerMessage = {
        Id = ValueNone
        Type = "pong"
        Payload = ValueSome (CustomResponse responseDocument)
    }
    let json = JsonSerializer.Serialize (message, serializerOptions)
    use document = JsonDocument.Parse json
    let root = document.RootElement
    Assert.Equal ("pong", root.GetProperty("type").GetString())
    Assert.Equal ("pong!", root.GetProperty("payload").GetString())

[<Fact>]
let ``Observable error details sanitize non-GraphQL exception messages`` () =
    let actual = problemDetailsOfObservableError (Exception "sensitive backend failure")
    let error = Assert.Single actual
    Assert.Equal (UnexpectedObservableErrorMessage, error.Message)

[<Fact>]
let ``Observable error details preserve GraphQL-facing messages inside aggregates`` () =
    let actual =
        AggregateException [| Exception "sensitive backend failure"; GQLMessageException "Visible to client" |]
        |> problemDetailsOfObservableError
        |> List.map _.Message

    Assert.Contains (UnexpectedObservableErrorMessage, actual)
    Assert.Contains ("Visible to client", actual)
    Assert.DoesNotContain ("sensitive backend failure", actual)

[<Fact>]
let ``Observable error details fall back to the generic message for empty aggregates`` () =
    let actual = problemDetailsOfObservableError (AggregateException ())
    let error = Assert.Single actual
    Assert.Equal (UnexpectedObservableErrorMessage, error.Message)

[<Fact>]
let ``Observable error details do not duplicate repeated aggregate errors`` () =
    let actual =
        AggregateException [|
            GQLMessageException ("Visible to client", Dictionary<string, obj>(dict [ "a", box 1; "b", box 2 ])) :> exn
            GQLMessageException ("Visible to client", Dictionary<string, obj>(dict [ "b", box 2; "a", box 1 ])) :> exn
        |]
        |> problemDetailsOfObservableError

    let error = Assert.Single actual
    Assert.Equal ("Visible to client", error.Message)

[<Fact>]
let ``Request error sanitization replaces backend exception messages`` () =
    let actual =
        sanitizeRequestError (GQLProblemDetails.Create ("sensitive backend failure", Exception "sensitive backend failure"))
    Assert.Equal (UnexpectedObservableErrorMessage, actual.Message)

[<Fact>]
let ``Request error sanitization preserves GraphQL-facing errors`` () =
    let expected = GQLProblemDetails.OfError (GQLMessageException "Visible to client")
    let actual = sanitizeRequestError expected
    Assert.Equal (expected, actual)

type private TrackingSubscription (onDispose : unit -> unit) =
    interface IDisposable with
        member _.Dispose () = onDispose ()

[<Fact>]
let ``Removing all subscriptions attempts every disposal before raising aggregate failure`` () =
    let disposedIds = ConcurrentQueue ()
    let unsubscribedIds = ConcurrentQueue ()
    let subscriptions =
        Dictionary<SubscriptionId, SubscriptionUnsubscriber * OnUnsubscribeAction>() :> SubscriptionsDict

    let createSubscription id shouldThrow =
        let subscription =
            new TrackingSubscription (fun () ->
                disposedIds.Enqueue id

                if shouldThrow then
                    raise (InvalidOperationException $"Dispose failed for {id}"))

        let onUnsubscribe removedId =
            unsubscribedIds.Enqueue removedId

            if shouldThrow then
                raise (InvalidOperationException $"Unsubscribe failed for {removedId}")

        id, (subscription :> SubscriptionUnsubscriber), onUnsubscribe

    subscriptions
    |> addSubscription (createSubscription "first" true)
    subscriptions
    |> addSubscription (createSubscription "second" false)

    let error = Assert.Throws<AggregateException>(fun () -> subscriptions |> removeAllSubscriptions)

    Assert.False (subscriptions.ContainsKey "first")
    Assert.False (subscriptions.ContainsKey "second")
    Assert.Equal<string>(set [ "first"; "second" ], set disposedIds)
    Assert.Equal<string>(set [ "first"; "second" ], set unsubscribedIds)
    Assert.Single error.InnerExceptions
