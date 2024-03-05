module FSharp.Data.GraphQL.Tests.AspNetCore.SerializationTests

open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Server.AspNetCore
open System.Text.Json
open Xunit

let getStdSerializerOptions () =
    let serializerOptions = new JsonSerializerOptions()
    serializerOptions.PropertyNameCaseInsensitive <- true
    serializerOptions.Converters.Add(new ClientMessageConverter<Root>(TestSchema.executor))
    serializerOptions.Converters.Add(new RawServerMessageConverter())
    serializerOptions

[<Fact>]
let ``Deserializes ConnectionInit correctly`` () =
    let serializerOptions = getStdSerializerOptions()
    let input = "{\"type\":\"connection_init\"}"

    let result = JsonSerializer.Deserialize<ClientMessage>(input, serializerOptions)

    match result with
    | ConnectionInit None -> () // <-- expected
    | other ->
        Assert.Fail($"unexpected actual value: '%A{other}'")

[<Fact>]
let ``Deserializes ConnectionInit with payload correctly`` () =
    let serializerOptions = getStdSerializerOptions()

    let input = "{\"type\":\"connection_init\", \"payload\":\"hello\"}"

    let result = JsonSerializer.Deserialize<ClientMessage>(input, serializerOptions)

    match result with
    | ConnectionInit _ -> () // <-- expected
    | other ->
        Assert.Fail($"unexpected actual value: '%A{other}'")

[<Fact>]
let ``Deserializes ClientPing correctly`` () =
    let serializerOptions = getStdSerializerOptions()

    let input = "{\"type\":\"ping\"}"

    let result = JsonSerializer.Deserialize<ClientMessage>(input, serializerOptions)

    match result with
    | ClientPing None -> () // <-- expected
    | other ->
        Assert.Fail($"unexpected actual value '%A{other}'")

[<Fact>]
let ``Deserializes ClientPing with payload correctly`` () =
    let serializerOptions = getStdSerializerOptions()

    let input = "{\"type\":\"ping\", \"payload\":\"ping!\"}"

    let result = JsonSerializer.Deserialize<ClientMessage>(input, serializerOptions)

    match result with
    | ClientPing _ -> () // <-- expected
    | other ->
        Assert.Fail($"unexpected actual value '%A{other}'")

[<Fact>]
let ``Deserializes ClientPong correctly`` () =
    let serializerOptions = getStdSerializerOptions()

    let input = "{\"type\":\"pong\"}"

    let result = JsonSerializer.Deserialize<ClientMessage>(input, serializerOptions)

    match result with
    | ClientPong None -> () // <-- expected
    | other ->
        Assert.Fail($"unexpected actual value: '%A{other}'")

[<Fact>]
let ``Deserializes ClientPong with payload correctly`` () =
    let serializerOptions = getStdSerializerOptions()

    let input = "{\"type\":\"pong\", \"payload\": \"pong!\"}"
    
    let result = JsonSerializer.Deserialize<ClientMessage>(input, serializerOptions)

    match result with
    | ClientPong _ -> () // <-- expected
    | other ->
        Assert.Fail($"unexpected actual value: '%A{other}'")

[<Fact>]
let ``Deserializes ClientComplete correctly``() =
    let serializerOptions = getStdSerializerOptions()

    let input = "{\"id\": \"65fca2b5-f149-4a70-a055-5123dea4628f\", \"type\":\"complete\"}"

    let result = JsonSerializer.Deserialize<ClientMessage>(input, serializerOptions)

    match result with
    | ClientComplete id ->
        Assert.Equal("65fca2b5-f149-4a70-a055-5123dea4628f", id)
    | other ->
        Assert.Fail($"unexpected actual value: '%A{other}'")

[<Fact>]
let ``Deserializes client subscription correctly`` () =
    let serializerOptions = getStdSerializerOptions()

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
        Assert.Equal("b5d4d2ff-d262-4882-a7b9-d6aec5e4faa6", id)
        Assert.Equal(1, payload.ExecutionPlan.Operation.SelectionSet.Length)
        let watchMoonSelection = payload.ExecutionPlan.Operation.SelectionSet |> List.head
        match watchMoonSelection with
        | Field watchMoonField ->
            Assert.Equal("watchMoon", watchMoonField.Name)
            Assert.Equal(1, watchMoonField.Arguments.Length)
            let watchMoonFieldArg = watchMoonField.Arguments |> List.head
            Assert.Equal("id", watchMoonFieldArg.Name)
            match watchMoonFieldArg.Value with
            | StringValue theValue ->
                Assert.Equal("1", theValue)
            | other ->
                Assert.Fail($"expected arg to be a StringValue, but it was: %A{other}")
            Assert.Equal(3, watchMoonField.SelectionSet.Length)
            match watchMoonField.SelectionSet.[0] with
            | Field firstField ->
                Assert.Equal("id", firstField.Name)
            | other ->
                Assert.Fail($"expected field to be a Field, but it was: %A{other}")
            match watchMoonField.SelectionSet.[1] with
            | Field secondField ->
                Assert.Equal("name", secondField.Name)
            | other ->
                Assert.Fail($"expected field to be a Field, but it was: %A{other}")
            match watchMoonField.SelectionSet.[2] with
            | Field thirdField ->
                Assert.Equal("isMoon", thirdField.Name)
            | other ->
                Assert.Fail($"expected field to be a Field, but it was: %A{other}")
        | somethingElse ->
            Assert.Fail($"expected it to be a field, but it was: %A{somethingElse}")
    | other ->
        Assert.Fail($"unexpected actual value: '%A{other}'")