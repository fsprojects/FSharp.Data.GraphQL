namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Text.Json
open System.Text.Json.Nodes
open System.Text.Json.Serialization

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types

type private GQLEditableRequestContent =
    { Query : string
      OperationName : string Skippable
      Variables : JsonObject Skippable }

[<AutoOpen>]
module JsonNodeExtensions =

    open System.Buffers

    type JsonNode with

        static member Create (object: 'T) =
            let bufferWriter = new ArrayBufferWriter<byte>();
            use writer = new Utf8JsonWriter(bufferWriter)
            JsonSerializer.Serialize<'T>(writer, object, Json.serializerOptions)
            JsonSerializer.Deserialize<JsonNode>(bufferWriter.WrittenSpan, Json.serializerOptions)

        member node.AsJsonElement() =
            let bufferWriter = new ArrayBufferWriter<byte>()
            use writer = new Utf8JsonWriter(bufferWriter)
            node.WriteTo (writer, Json.serializerOptions)
            let bytes = bufferWriter.WrittenSpan
            let mutable reader = new Utf8JsonReader(bytes)
            JsonDocument.ParseValue(&reader).RootElement

[<Sealed>]
type GraphQLQueryConverter<'a>(executor : Executor<'a>, replacements: Map<string, obj>, ?meta : Metadata) =
    inherit JsonConverter<GraphQLQuery>()

    /// Active pattern to match GraphQL type defintion with nullable / optional types.
    let (|Nullable|_|) (tdef : TypeDef) =
        match tdef with
        | :? NullableDef as x -> Some x.OfType
        | _ -> None

    override __.CanConvert(t) = t = typeof<GraphQLQuery>

    override __.Write(_, _, _) = raise <| System.NotSupportedException()

    override __.Read(reader, _, options) =

        let request = JsonSerializer.Deserialize<GQLEditableRequestContent>(&reader, options)
        let result =
            let query = request.Query
            match meta with
            | Some meta -> executor.CreateExecutionPlan(query, meta = meta)
            | None -> executor.CreateExecutionPlan(query)
        match result with
        | Result.Error struct (_, errors) ->
            failwith (String.concat Environment.NewLine (errors |> Seq.map (fun error -> error.Message)))
        | Ok executionPlan when executionPlan.Variables = [] -> { ExecutionPlan = executionPlan; Variables = ImmutableDictionary.Empty }
        | Ok executionPlan ->
            match request.Variables with
            | Skip -> failwith "No variables provided"
            | Include vars ->
            // For multipart requests, we need to replace some variables
            // TODO: Implement JSON path
            Map.iter (fun path rep ->
                          vars.Remove path |> ignore
                          vars.Add(path, JsonNode.Create rep))
                     replacements
            //Map.iter(fun path rep -> vars.SelectToken(path).Replace(JObject.FromObject(rep))) replacements
            let variables =
                executionPlan.Variables
                |> List.fold (fun (acc: ImmutableDictionary<string, JsonElement>.Builder) (vdef: VarDef) ->
                    match vars.TryGetPropertyValue vdef.Name with
                    | true, jsonNode ->
                        let jsonElement = jsonNode.AsJsonElement()
                        acc.Add (vdef.Name, jsonElement)
                    | false, _  ->
                        match vdef.DefaultValue, vdef.TypeDef with
                        | Some _, _ -> ()
                        | _, Nullable _ -> ()
                        | None, _ -> failwithf "Variable %s has no default value and is missing!" vdef.Name
                    acc)
                    (ImmutableDictionary.CreateBuilder<string, JsonElement>())
            { ExecutionPlan = executionPlan; Variables = variables.ToImmutable() }

[<AutoOpen>]
module private GraphQLSubscriptionFields =
    let [<Literal>] FIELD_Type = "type"
    let [<Literal>] FIELD_Id = "id"
    let [<Literal>] FIELD_Payload = "payload"
    let [<Literal>] FIELD_Error = "error"

[<Sealed>]
type WebSocketClientMessageConverter<'a>(executor : Executor<'a>, replacements: Map<string, obj>, ?meta : Metadata) =
    inherit JsonConverter<WebSocketClientMessage>()

    override __.CanConvert(t) = t = typeof<WebSocketClientMessage>

    override __.Write(_, _, _) = raise <| NotSupportedException()

    override __.Read(reader, _, options) =
        let properties = JsonSerializer.Deserialize<Dictionary<string, JsonElement>>(&reader, options)
        let typ = properties.["type"]
        if typ.ValueKind = JsonValueKind.String then
            let value = typ.GetString()
            match value with
            | "connection_init" -> ConnectionInit
            | "connection_terminate" -> ConnectionTerminate
            | "start" ->
                let id =
                    match properties.TryGetValue FIELD_Id with
                    | true, value -> ValueSome <| value.GetString ()
                    | false, _ -> ValueNone
                let payload =
                    match properties.TryGetValue FIELD_Payload with
                    | true, value -> ValueSome <| value
                    | false, _ -> ValueNone
                match id, payload with
                | ValueSome id, ValueSome payload ->
                    try
                        let queryConverter =
                            match meta with
                            | Some meta -> GraphQLQueryConverter(executor, replacements, meta) :> JsonConverter
                            | None -> GraphQLQueryConverter(executor, replacements) :> JsonConverter
                        let options' = Json.getSerializerOptions (Seq.singleton queryConverter)
                        let req = payload.Deserialize<GraphQLQuery> options'
                        Start(id, req)
                    with e -> ParseError(Some id, "Parse Failed with Exception: " + e.Message)
                | ValueNone, _ -> ParseError(None, "Malformed GQL_START message, expected id field but found none")
                | _, ValueNone -> ParseError(None, "Malformed GQL_START message, expected payload field but found none")
            | "stop" ->
                match properties.TryGetValue FIELD_Id with
                | true, id -> Stop(id.GetString ())
                | false, _ -> ParseError(None, "Malformed GQL_STOP message, expected id field but found none")
            | _ ->
                ParseError(None, $"Message Type '%s{typ.GetRawText()}' is not supported!")
        else
            ParseError(None, $"Message Type must be string but got {Environment.NewLine}%s{typ.GetRawText()}")

[<Sealed>]
type WebSocketServerMessageConverter() =
    inherit JsonConverter<WebSocketServerMessage>()

    override __.CanConvert(t) = t = typedefof<WebSocketServerMessage> || t.DeclaringType = typedefof<WebSocketServerMessage>

    override __.Read(_, _, _) = raise <| NotSupportedException()

    override __.Write(writer, value, options) =
        writer.WriteStartObject()
        match value with
        | ConnectionAck ->
            writer.WriteString(FIELD_Type, "connection_ack")
        | ConnectionError(err) ->
            writer.WriteString(FIELD_Type, "connection_error")
            writer.WritePropertyName(FIELD_Payload)
            writer.WriteStartObject()
            writer.WriteString(FIELD_Error, err)
            writer.WriteEndObject()
        | Error(id, err) ->
            writer.WriteString(FIELD_Type, "error")
            writer.WritePropertyName(FIELD_Payload)
            writer.WriteStartObject()
            writer.WriteString(FIELD_Error, err)
            writer.WriteEndObject()
            match id with
            | Some id -> writer.WriteString (FIELD_Id, id)
            | None -> writer.WriteNull(FIELD_Id)
        | Data(id, result) ->
            writer.WriteString(FIELD_Type, "data")
            writer.WriteString(FIELD_Id, id)
            writer.WritePropertyName(FIELD_Payload)
            JsonSerializer.Serialize(writer, result, options)
        | Complete(id) ->
            writer.WriteString(FIELD_Type, "complete")
            writer.WriteString(FIELD_Id, id)
        writer.WriteEndObject()
