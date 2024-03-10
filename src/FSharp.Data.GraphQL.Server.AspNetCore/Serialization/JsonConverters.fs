namespace FSharp.Data.GraphQL.Server.AspNetCore

open System
open System.Text.Json
open System.Text.Json.Serialization

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Server.AspNetCore.WebSockets

[<Sealed>]
type ClientMessageConverter () =
    inherit JsonConverter<ClientMessage> ()

    let raiseInvalidMsg explanation = raise <| InvalidMessageException explanation

    /// From the spec: "Receiving a message of a type or format which is not specified in this document will result in an immediate socket closure with the event 4400: &lt;error-message&gt;.
    /// The &lt;error-message&gt; can be vaguely descriptive on why the received message is invalid."
    let invalidMsg (explanation : string) = InvalidMessage (4400, explanation) |> Result.Error

    let errMsgToStr (struct (docId : int, graphQLErrorMsgs : GQLProblemDetails list)) =
        String.Join ('\n', graphQLErrorMsgs |> Seq.map (fun err -> err.Message))

    let unpackRopResult ropResult =
        match ropResult with
        | Ok x -> x
        | Result.Error (InvalidMessage (_, explanation : string)) -> raiseInvalidMsg explanation

    let getOptionalString (reader : byref<Utf8JsonReader>) =
        if reader.TokenType.Equals (JsonTokenType.Null) then
            None
        else
            Some (reader.GetString ())

    let readPropertyValueAsAString (propertyName : string) (reader : byref<Utf8JsonReader>) =
        if reader.Read () then
            getOptionalString (&reader)
        else
            raiseInvalidMsg
            <| $"Was expecting a value for property \"%s{propertyName}\""

    let requireId (raw : RawMessage) : Result<string, ClientMessageProtocolFailure> =
        match raw.Id with
        | Some s -> Ok s
        | None ->
            invalidMsg
            <| "Property \"id\" is required for this message but was not present."

    let requireSubscribePayload
        (serializerOptions : JsonSerializerOptions)
        (payload : JsonDocument option)
        : Result<GQLRequestContent, ClientMessageProtocolFailure> =
        match payload with
        | None ->
            invalidMsg
            <| "Payload is required for this message, but none was present."
        | Some p ->
            try
                JsonSerializer.Deserialize<GQLRequestContent>(p, serializerOptions) |> Ok
            with
            | :? JsonException as ex ->
                invalidMsg
                <| $"Invalid payload received: {ex.Message}."

    let readRawMessage (reader : byref<Utf8JsonReader>, options : JsonSerializerOptions) : RawMessage =
        if not (reader.TokenType.Equals (JsonTokenType.StartObject)) then
            raise (new JsonException ($"reader's first token was not \"%A{JsonTokenType.StartObject}\", but \"%A{reader.TokenType}\""))
        else
            let mutable id : string option = None
            let mutable theType : string option = None
            let mutable payload : JsonDocument option = None
            while reader.Read ()
                  && (not (reader.TokenType.Equals (JsonTokenType.EndObject))) do
                match reader.GetString () with
                | "id" -> id <- readPropertyValueAsAString "id" &reader
                | "type" -> theType <- readPropertyValueAsAString "type" &reader
                | "payload" -> payload <- Some <| JsonDocument.ParseValue (&reader)
                | other -> raiseInvalidMsg <| $"Unknown property \"%s{other}\""

            match theType with
            | None -> raiseInvalidMsg "Property \"type\" is missing"
            | Some msgType -> { Id = id; Type = msgType; Payload = payload }

    override __.Read (reader : byref<Utf8JsonReader>, typeToConvert : Type, options : JsonSerializerOptions) : ClientMessage =
        let raw = readRawMessage (&reader, options)
        match raw.Type with
        | "connection_init" -> ConnectionInit raw.Payload
        | "ping" -> ClientPing raw.Payload
        | "pong" -> ClientPong raw.Payload
        | "complete" ->
            raw
            |> requireId
            |> Result.map ClientComplete
            |> unpackRopResult
        | "subscribe" ->
            raw
            |> requireId
            |> Result.bind (fun id ->
                raw.Payload
                |> requireSubscribePayload options
                |> Result.map (fun payload -> (id, payload)))
            |> Result.map Subscribe
            |> unpackRopResult
        | other ->
            raiseInvalidMsg
            <| $"Invalid type \"%s{other}\" specified by client."


    override __.Write (writer : Utf8JsonWriter, value : ClientMessage, options : JsonSerializerOptions) =
        raise (NotSupportedException "Serializing a WebSocketClientMessage is not supported (yet(?))")

[<Sealed>]
type RawServerMessageConverter () =
    inherit JsonConverter<RawServerMessage> ()

    override __.Read (reader : byref<Utf8JsonReader>, typeToConvert : Type, options : JsonSerializerOptions) : RawServerMessage =
        raise (NotSupportedException "deserializing a RawServerMessage is not supported (yet(?))")

    override __.Write (writer : Utf8JsonWriter, value : RawServerMessage, options : JsonSerializerOptions) =
        writer.WriteStartObject ()
        writer.WriteString ("type", value.Type)
        match value.Id with
        | None -> ()
        | Some id -> writer.WriteString ("id", id)

        match value.Payload with
        | None -> ()
        | Some serverRawPayload ->
            match serverRawPayload with
            | ExecutionResult output ->
                writer.WritePropertyName ("payload")
                JsonSerializer.Serialize (writer, output, options)
            | ErrorMessages msgs -> JsonSerializer.Serialize (writer, msgs, options)
            | CustomResponse jsonDocument -> jsonDocument.WriteTo (writer)

        writer.WriteEndObject ()
