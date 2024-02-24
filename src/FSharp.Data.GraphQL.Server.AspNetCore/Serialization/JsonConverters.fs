namespace FSharp.Data.GraphQL.Server.AspNetCore

open FSharp.Data.GraphQL
open System
open System.Text.Json
open System.Text.Json.Serialization

[<Sealed>]
type ClientMessageConverter<'Root>(executor : Executor<'Root>) =
  inherit JsonConverter<ClientMessage>()

  let raiseInvalidMsg explanation =
    raise <| InvalidMessageException explanation

  /// From the spec: "Receiving a message of a type or format which is not specified in this document will result in an immediate socket closure with the event 4400: &lt;error-message&gt;.
  /// The &lt;error-message&gt; can be vaguely descriptive on why the received message is invalid."
  let invalidMsg (explanation : string) =
    InvalidMessage (4400, explanation)
    |> Result.Error

  let errMsgToStr (struct (docId: int, graphQLErrorMsgs: GQLProblemDetails list)) =
   String.Join('\n', graphQLErrorMsgs |> Seq.map (fun err -> err.Message))

  let unpackRopResult ropResult =
    match ropResult with
    | Ok x -> x
    | Result.Error (InvalidMessage (_, explanation: string)) ->
      raiseInvalidMsg explanation

  let getOptionalString (reader : byref<Utf8JsonReader>) =
    if reader.TokenType.Equals(JsonTokenType.Null) then
      None
    else
      Some (reader.GetString())

  let readPropertyValueAsAString (propertyName : string) (reader : byref<Utf8JsonReader>) =
    if reader.Read() then
      getOptionalString(&reader)
    else
      raiseInvalidMsg <| sprintf "was expecting a value for property \"%s\"" propertyName

  let requireId (raw : RawMessage) : Result<string, ClientMessageProtocolFailure> =
    match raw.Id with
    | Some s -> Ok s
    | None -> invalidMsg <| "property \"id\" is required for this message but was not present."

  let requireSubscribePayload (serializerOptions : JsonSerializerOptions) (executor : Executor<'a>) (payload : JsonDocument option) : Result<GraphQLQuery, ClientMessageProtocolFailure> =
    match payload with
    | None ->
      invalidMsg <| "payload is required for this message, but none was present."
    | Some p ->
      let rawSubsPayload = JsonSerializer.Deserialize<GraphQLRequest option>(p, serializerOptions)
      match rawSubsPayload with
      | None ->
        invalidMsg <| "payload is required for this message, but none was present."
      | Some subscribePayload ->
        match subscribePayload.Query with
        | None ->
          invalidMsg <| sprintf "there was no query in the client's subscribe message!"
        | Some query ->
          query
          |> GraphQLQueryDecoding.decodeGraphQLQuery serializerOptions executor subscribePayload.OperationName subscribePayload.Variables
          |> Result.mapError (fun errMsg -> InvalidMessage (CustomWebSocketStatus.invalidMessage, errMsg |> errMsgToStr))


  let readRawMessage (reader : byref<Utf8JsonReader>, options: JsonSerializerOptions) : RawMessage =
    if not (reader.TokenType.Equals(JsonTokenType.StartObject))
      then raise (new JsonException((sprintf "reader's first token was not \"%A\", but \"%A\"" JsonTokenType.StartObject reader.TokenType)))
    else
      let mutable id : string option = None
      let mutable theType : string option = None
      let mutable payload : JsonDocument option = None
      while reader.Read() && (not (reader.TokenType.Equals(JsonTokenType.EndObject))) do
        match reader.GetString() with
        | "id" ->
          id <- readPropertyValueAsAString "id" &reader
        | "type" ->
          theType <- readPropertyValueAsAString "type" &reader
        | "payload" ->
          payload <- Some <| JsonDocument.ParseValue(&reader)
        | other ->
          raiseInvalidMsg <| sprintf "unknown property \"%s\"" other

      match theType with
      | None ->
        raiseInvalidMsg "property \"type\" is missing"
      | Some msgType ->
        { Id = id
          Type = msgType
          Payload = payload }

  override __.Read(reader : byref<Utf8JsonReader>, typeToConvert: Type, options: JsonSerializerOptions) : ClientMessage =
    let raw = readRawMessage(&reader, options)
    match raw.Type with
    | "connection_init" ->
      ConnectionInit raw.Payload
    | "ping" ->
      ClientPing raw.Payload
    | "pong" ->
      ClientPong raw.Payload
    | "complete" ->
      raw
      |> requireId
      |> Result.map ClientComplete
      |> unpackRopResult
    | "subscribe" ->
      raw
      |> requireId
      |> Result.bind
        (fun id ->
          raw.Payload
          |> requireSubscribePayload options executor
          |> Result.map (fun payload -> (id, payload))
        )
      |> Result.map Subscribe
      |> unpackRopResult
    | other ->
      raiseInvalidMsg <| sprintf "invalid type \"%s\" specified by client." other



  override __.Write(writer : Utf8JsonWriter, value : ClientMessage, options : JsonSerializerOptions) =
    failwith "serializing a WebSocketClientMessage is not supported (yet(?))"

[<Sealed>]
type RawServerMessageConverter() =
  inherit JsonConverter<RawServerMessage>()

  override __.Read(reader : byref<Utf8JsonReader>, typeToConvert: Type, options : JsonSerializerOptions) : RawServerMessage =
    failwith "deserializing a RawServerMessage is not supported (yet(?))"

  override __.Write(writer : Utf8JsonWriter, value : RawServerMessage, options : JsonSerializerOptions) =
    writer.WriteStartObject()
    writer.WriteString("type", value.Type)
    match value.Id with
    | None ->
      ()
    | Some id ->
      writer.WriteString("id", id)
    
    match value.Payload with
    | None ->
      ()
    | Some serverRawPayload ->
      match serverRawPayload with
      | ExecutionResult output ->
        writer.WritePropertyName("payload")
        JsonSerializer.Serialize(writer, output, options)
      | ErrorMessages msgs ->
        JsonSerializer.Serialize(writer, msgs, options)
      | CustomResponse jsonDocument ->
        jsonDocument.WriteTo(writer)

    writer.WriteEndObject()


module JsonConverterUtils =

  let [<Literal>] UnionTag = "kind"

  let private defaultJsonFSharpOptions =
    JsonFSharpOptions(
        JsonUnionEncoding.InternalTag
        ||| JsonUnionEncoding.AllowUnorderedTag
        ||| JsonUnionEncoding.NamedFields
        ||| JsonUnionEncoding.UnwrapSingleCaseUnions
        ||| JsonUnionEncoding.UnwrapRecordCases
        ||| JsonUnionEncoding.UnwrapOption
        ||| JsonUnionEncoding.UnwrapFieldlessTags,
        UnionTag,
        allowOverride = true)
  let configureSerializer (executor : Executor<'Root>) (jsonSerializerOptions : JsonSerializerOptions) =
    jsonSerializerOptions.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
    jsonSerializerOptions.PropertyNameCaseInsensitive <- true
    jsonSerializerOptions.Converters.Add(new JsonStringEnumConverter())
    jsonSerializerOptions.Converters.Add(new ClientMessageConverter<'Root>(executor))
    jsonSerializerOptions.Converters.Add(new RawServerMessageConverter())
    jsonSerializerOptions |> defaultJsonFSharpOptions.AddToJsonSerializerOptions
    jsonSerializerOptions