#I "../../packages"
#r "Suave/lib/net40/Suave.dll"
#r "Newtonsoft.Json/lib/net40/Newtonsoft.Json.dll"
#r "System.Runtime/lib/net462/System.Runtime.dll"
#r "Hopac/lib/net45/Hopac.dll"
#r "Hopac/lib/net45/Hopac.Core.dll"
#r "Hopac/lib/net45/Hopac.Platform.dll"
#r "FParsec/lib/net40-client/FParsec.dll"
#r "FParsec/lib/net40-client/FParsecCS.dll"
#r "FSharp.Data.GraphQL.Server/lib/FSharp.Data.GraphQL.Shared.dll"
#r "FSharp.Data.GraphQL.Server/lib/FSharp.Data.GraphQL.dll"

open System
open System.IO
open Newtonsoft.Json

type Task = 
    { Id : string
      Description : string
      Children : Task list
      Completed : bool }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Task =
    let private lockObj = obj()

    let private filePath =
        Path.Combine(__SOURCE_DIRECTORY__, "data", "tasks.json")

    let mutable private rootTask =
        JsonConvert.DeserializeObject<Task>(File.ReadAllText(filePath))

    let tryFind id =
        let rec tryFind id (task: Task) =
            if task.Id = id then Some task else
            List.tryPick (tryFind id) task.Children 
        tryFind id rootTask

    type private UpdateStatus =
        | NotFound
        | FoundButNotUpdated
        | FoundAndUpdated of Task

    let tryUpdate id (f: Task->Task option) =
        let rec tryUpdate id (f: Task->Task option) (task: Task) =
            if task.Id = id
            then
                match f task with
                | Some t -> FoundAndUpdated t
                | None -> FoundButNotUpdated
            else
            (NotFound, task.Children)
            ||> List.fold (fun status child ->
                match status with
                | NotFound ->
                    match tryUpdate id f child with
                    | FoundAndUpdated child ->
                        let children =
                            task.Children
                            |> List.map (fun c -> if c.Id = child.Id then child else c)
                        FoundAndUpdated { task with Children = children }
                    | status -> status
                | status -> status)
        match tryUpdate id f rootTask with
        | FoundAndUpdated updatedRootTask ->
            lock lockObj (fun () ->
                // Persistence
                File.WriteAllText(filePath, JsonConvert.SerializeObject rootTask)
                rootTask <- updatedRootTask)
            true
        | _ -> false

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
      "Query", [
        Define.Field(
            "task", Nullable TaskType, "Gets task",
            [ Define.Input("id", String) ],
            fun ctx () -> ctx.Arg("id") |> Task.tryFind)
      ])

let Mutation =
  Define.Object(
    "Mutation", [
        Define.Field(
            "completed", Boolean, "Sets task as (not) completed. Returns true if successful",
            [ Define.Input("id", String); Define.Input("completed", Boolean) ],
            fun ctx () -> Task.tryUpdate (ctx.Arg "id") (fun t -> Some { t with Completed = ctx.Arg "completed" }))
        Define.Field(
            "addChild", Boolean, "Add child. Returns true if successful",
            [ Define.Input("parentId", String); Define.Input("id", String); Define.Input("description", String) ],
            fun ctx () ->
                let childId: string = ctx.Arg "id"
                let childDescription: string = ctx.Arg "description"
                Task.tryUpdate (ctx.Arg "parentId") (fun t ->
                    if t.Children |> List.exists (fun c -> c.Id = childId) |> not
                    then
                        let newChild = { Id=childId; Description=childDescription; Completed=false; Children=[]}
                        Some { t with Children = newChild::t.Children }
                    else None))
        Define.Field(
            "removeChild", Boolean, "Add child. Returns true if successful",
            [ Define.Input("parentId", String); Define.Input("id", String) ],
            fun ctx () ->
                let childId: string = ctx.Arg "id"
                Task.tryUpdate (ctx.Arg "parentId") (fun t ->
                    if t.Children |> List.exists (fun c -> c.Id = childId)
                    then Some { t with Children = t.Children |> List.filter (fun c -> c.Id = childId) }
                    else None))
    ])

let schema = Schema(Query, Mutation)

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
