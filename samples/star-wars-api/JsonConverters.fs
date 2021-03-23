namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open System
open System.Collections.Generic
open System.Text.Json
open System.Text.Json.Serialization
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns
open Newtonsoft.Json.Linq

[<AutoOpen>]
module private JsonHelpers =

    let readObjectAsStreang (reader: byref<Utf8JsonReader>) =
        use document = JsonDocument.ParseValue (&reader)
        document.RootElement.GetRawText();


[<Sealed>]
type GraphQLQueryConverter<'a>(executor : Executor<'a>, replacements: Map<string, obj>, ?meta : Metadata) =
    inherit JsonConverter<GraphQLQuery>()

    override __.CanConvert(t) = t = typeof<GraphQLQuery>

    override __.Write(_, _, _) = failwith "Not supported"

    override __.Read(reader, _, options) =
        if reader.TokenType <> JsonTokenType.StartObject then raise <| JsonException ()
        let properties = Dictionary<string, string>(2)
        reader.Read () |> ignore
        while reader.TokenType <> JsonTokenType.EndObject do
            let propertyName = reader.GetString ()
            reader.Read () |> ignore
            match propertyName with
            | "query" -> properties.Add (propertyName, readObjectAsStreang(&reader))
            | "variables" -> properties.Add (propertyName, readObjectAsStreang(&reader))
            | _ -> ()
            reader.Read () |> ignore

        let query = properties.["query"]
        let plan =
            match meta with
            | Some meta -> executor.CreateExecutionPlan(query, meta = meta)
            | None -> executor.CreateExecutionPlan(query)
        let varDefs = plan.Variables
        match varDefs with
        | [] -> { ExecutionPlan = plan; Variables = Map.empty }
        | vs ->
            let query = JObject.Parse(query)
            // For multipart requests, we need to replace some variables
            Map.iter(fun path rep -> query.SelectToken(path).Replace(JObject.FromObject(rep))) replacements
            let vars = JObject.Parse(properties.["variables"])
            Map.iter(fun path rep -> vars.SelectToken(path).Replace(JObject.FromObject(rep))) replacements
            let variables =
                vs
                |> List.fold (fun (acc: Map<string, obj>)(vdef: VarDef) ->
                    match vars.TryGetValue(vdef.Name) with
                    | true, jval ->
                        let v =
                            match jval.Type with
                            | JTokenType.Null -> null
                            | JTokenType.String -> jval.ToString() :> obj
                            | _ -> JsonSerializer.Deserialize(jval.ToString(), vdef.TypeDef.Type, options)
                        Map.add (vdef.Name) v acc
                    | false, _  ->
                        match vdef.DefaultValue, vdef.TypeDef with
                        | Some _, _ -> acc
                        | _, Nullable _ -> acc
                        | None, _ -> failwithf "Variable %s has no default value and is missing!" vdef.Name) Map.empty
            { ExecutionPlan = plan; Variables = variables }

[<Sealed>]
type WebSocketClientMessageConverter<'a>(executor : Executor<'a>, replacements: Map<string, obj>, ?meta : Metadata) =
    inherit JsonConverter<WebSocketClientMessage>()

    override __.CanConvert(t) = t = typeof<WebSocketClientMessage>

    override __.Write(writer, query, options) = failwith "Not supported"

    override __.Read(reader, _, options) =
        if reader.TokenType <> JsonTokenType.StartObject then raise <| JsonException ()
        let properties = Dictionary<string, string>(2)
        reader.Read () |> ignore
        while reader.TokenType <> JsonTokenType.EndObject do
            let propertyName = reader.GetString ()
            reader.Read () |> ignore
            match propertyName with
            | "type"
            | "id" -> properties.Add (propertyName, reader.GetString ())
            | "payload" -> properties.Add (propertyName, readObjectAsStreang(&reader))
            | _ -> ()
            reader.Read () |> ignore

        let typ = properties.["type"]
        match typ with
        | "connection_init" -> ConnectionInit
        | "connection_terminate" -> ConnectionTerminate
        | "start" ->
            let id =
                match properties.TryGetValue "id" with
                | true, value -> ValueSome value
                | false, _ -> ValueNone
            let payload =
                match properties.TryGetValue "payload" with
                | true, value -> ValueSome value
                | false, _ -> ValueNone
            match id, payload with
            | ValueSome id, ValueSome payload ->
                try
                    let queryConverter =
                        match meta with
                        | Some meta -> GraphQLQueryConverter(executor, replacements, meta) :> JsonConverter
                        | None -> GraphQLQueryConverter(executor, replacements) :> JsonConverter
                    let options' = Json.getSerializerOptions ([|queryConverter|])
                    let req = JsonSerializer.Deserialize<GraphQLQuery>(payload, options')
                    Start(id, req)
                with e -> ParseError(Some id, "Parse Failed with Exception: " + e.Message)
            | ValueNone, _ -> ParseError(None, "Malformed GQL_START message, expected id field but found none")
            | _, ValueNone -> ParseError(None, "Malformed GQL_START message, expected payload field but found none")
        | "stop" ->
            match properties.TryGetValue "id" with
            | true, id -> Stop(id)
            | false, _ -> ParseError(None, "Malformed GQL_STOP message, expected id field but found none")
        | typ -> ParseError(None, "Message Type " + typ + " is not supported!")

[<Sealed>]
type WebSocketServerMessageConverter() =
    inherit JsonConverter<WebSocketServerMessage>()

    override __.CanConvert(t) = t = typedefof<WebSocketServerMessage> || t.DeclaringType = typedefof<WebSocketServerMessage>

    override __.Read(_, _, _) = raise <| NotSupportedException()

    override __.Write(writer, value, options) =
        writer.WriteStartObject()
        match value with
        | ConnectionAck ->
            writer.WriteString("type", "connection_ack")
        | ConnectionError(err) ->
            writer.WriteString("type", "connection_error")
            writer.WritePropertyName("payload")
            writer.WriteStartObject()
            writer.WriteString("error", err)
            writer.WriteEndObject()
        | Error(id, err) ->
            writer.WriteString("type", "error")
            writer.WritePropertyName("payload")
            writer.WriteStartObject()
            writer.WriteString("error", err)
            writer.WriteEndObject()
            match id with
            | Some id -> writer.WriteString ("id", id)
            | None -> writer.WriteNull("id")
        | Data(id, result) ->
            writer.WriteString("type", "data")
            writer.WriteString("id", id)
            writer.WritePropertyName("payload")
            JsonSerializer.Serialize(writer, result, options)
        | Complete(id) ->
            writer.WriteString("type", "complete")
            writer.WriteString("id", id)
        writer.WriteEndObject()
