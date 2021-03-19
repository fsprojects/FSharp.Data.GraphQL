namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open System.Buffers.Text
open System.Collections.Generic
open System.Text.Json
open System.Text.Json.Serialization
open Microsoft.FSharp.Reflection
open Dahomey.Json.Util
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
                            | _ -> jval.ToObject(vdef.TypeDef.Type, serializer)
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

    override __.CanWrite = false

    override __.CanConvert(t) = t = typeof<WebSocketClientMessage>

    override __.Write(_, _, _) = failwith "Not supported"

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
        | "connection_init" -> upcast ConnectionInit
        | "connection_terminate" -> upcast ConnectionTerminate
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
                    // TODO:
                    //let settings = JsonSerializerSettings()
                    //let queryConverter =
                    //    match meta with
                    //    | Some meta -> GraphQLQueryConverter(executor, replacements, meta) :> JsonConverter
                    //    | None -> GraphQLQueryConverter(executor, replacements) :> JsonConverter
                    //let optionConverter = OptionConverter() :> JsonConverter
                    //settings.Converters <- [| optionConverter; queryConverter |]
                    //settings.ContractResolver <- Newtonsoft.Json.Serialization.CamelCasePropertyNamesContractResolver()
                    //let req = JsonConvert.DeserializeObject<GraphQLQuery>(payload, settings)
                    //upcast Start(id, req)
                with e -> upcast ParseError(Some id, "Parse Failed with Exception: " + e.Message)
            | ValueNone, _ -> upcast ParseError(None, "Malformed GQL_START message, expected id field but found none")
            | _, ValueNone -> upcast ParseError(None, "Malformed GQL_START message, expected payload field but found none")
        | "stop" ->
            match properties.TryGetValue "id" with
            | true, value -> upcast Stop(id)
            | false, _ -> upcast ParseError(None, "Malformed GQL_STOP message, expected id field but found none")
        | typ -> upcast ParseError(None, "Message Type " + typ + " is not supported!")

[<Sealed>]
type WebSocketServerMessageConverter() =
    inherit JsonConverter<WebSocketServerMessage>()

    override __.CanRead = false

    override __.CanConvert(t) = t = typedefof<WebSocketServerMessage> || t.DeclaringType = typedefof<WebSocketServerMessage>

    override __.Write(writer, value, options) =
        let jobj = JObject()
        match value with
        | ConnectionAck ->
            jobj.Add(JProperty("type", "connection_ack"))
        | ConnectionError(err) ->
            let errObj = JObject()
            errObj.Add(JProperty("error", err))
            jobj.Add(JProperty("type", "connection_error"))
            jobj.Add(JProperty("payload", errObj))
        | Error(id, err) ->
            let errObj = JObject()
            errObj.Add(JProperty("error", err))
            jobj.Add(JProperty("type", "error"))
            jobj.Add(JProperty("payload", errObj))
            jobj.Add(JProperty("id", id))
        | Data(id, result) ->
            jobj.Add(JProperty("type", "data"))
            jobj.Add(JProperty("id", id))
            jobj.Add(JProperty("payload", JObject.FromObject(result)))
        | Complete(id) ->
            jobj.Add(JProperty("type", "complete"))
            jobj.Add(JProperty("id", id))
        jobj.WriteTo(writer)
