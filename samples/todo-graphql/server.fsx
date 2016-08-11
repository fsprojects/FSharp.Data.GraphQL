#r "../../packages/Suave/lib/net40/Suave.dll"
#r "../../packages/Newtonsoft.Json/lib/net40/Newtonsoft.Json.dll"
#r "../../packages/System.Runtime/lib/net462/System.Runtime.dll"
#r "../../packages/Hopac/lib/net45/Hopac.dll"
#r "../../packages/Hopac/lib/net45/Hopac.Core.dll"
#r "../../packages/Hopac/lib/net45/Hopac.Platform.dll"
#r "../../packages/FParsec/lib/net40-client/FParsec.dll"
#r "../../packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "../../packages/FSharp.Data.GraphQL.Server/lib/FSharp.Data.GraphQL.Shared.dll"
#r "../../packages/FSharp.Data.GraphQL.Server/lib/FSharp.Data.GraphQL.dll"

open System

type Task = 
    { Id : string
      Description : string
      Children : Task list
      Completed : bool }

let tasks = 
    [ { Id = "cfd81e81-18f4-45b9-bd69-74c84fb1eaa2"
        Description = "My first task"
        Children = []
        Completed = false } ]

let getTask id = tasks |> List.tryFind (fun h -> h.Id = id)        

// Schema definition
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Execution

let rec TaskType =
  Define.Object<Task>(
    name = "Task",
    description = "An individual or parent task",
    isTypeOf = (fun o -> o :? Task),
    fieldsFn = fun () ->
    [
        Define.Field("id", String, "The id of the task.", fun _ h -> h.Id)
        Define.Field("description", String, "The description of the task.", fun _ h -> h.Description)
        Define.Field("children", ListOf TaskType, "Subdivisions of the task, or an empty list if they have none.",
            fun _ h -> List.toSeq h.Children)
        Define.Field("completed", Boolean, "Ignored if Children is not empty.",
            fun _ h ->
                if List.isEmpty h.Children
                then h.Completed
                else (true, h.Children) ||> List.fold (fun b x -> b && x.Completed)
        )
    ])

let Query =
  Define.Object(
    name = "Query",
    fields = [
        Define.Field("task", Nullable TaskType, "Gets task", [ Define.Input("id", String) ],
                     fun ctx () -> getTask (ctx.Arg("id")))
    ])

let schema = Schema(Query)

// server initialization
open Suave
open Suave.Operators
open Newtonsoft.Json

type OptionConverter() =
    inherit JsonConverter()    
    override x.CanConvert(t) = 
        t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>
    override x.WriteJson(writer, value, serializer) =
        let value = 
            if value = null then null
            else 
                let _,fields = Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(value, value.GetType())
                fields.[0]  
        serializer.Serialize(writer, value)
    override x.ReadJson(reader, t, existingValue, serializer) = failwith "Not supported"
    
let settings = JsonSerializerSettings()
settings.Converters <- [| OptionConverter() :> JsonConverter |]
settings.ContractResolver <- Newtonsoft.Json.Serialization.CamelCasePropertyNamesContractResolver()
let json o = JsonConvert.SerializeObject(o, settings)
    
let tryParse fieldName data =
    let raw = Text.Encoding.UTF8.GetString data
    if raw <> null && raw <> ""
    then
        let map = JsonConvert.DeserializeObject<Map<string,string>>(raw)
        Map.tryFind fieldName map
    else None

let graphiql : WebPart =
    fun http ->
        async {
            match tryParse "query" http.request.rawForm with
            | Some query ->
                // at the moment parser is not parsing new lines correctly, so we need to get rid of them
                let q = query.Trim().Replace("\r\n", " ")
                let! result = schema.AsyncExecute(q)   
                return! http |> Successful.OK (json result)
            | None ->
                let! schemaResult = schema.AsyncExecute(Introspection.introspectionQuery)
                return! http |> Successful.OK (json schemaResult)
        }

let setCorsHeaders = 
    Writers.setHeader  "Access-Control-Allow-Origin" "*"
    >=> Writers.setHeader "Access-Control-Allow-Headers" "content-type"

startWebServer defaultConfig (setCorsHeaders >=> graphiql >=> Writers.setMimeType "application/json")
