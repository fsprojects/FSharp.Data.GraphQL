#r "../../packages/Suave/lib/net40/Suave.dll"
#r "../../packages/Newtonsoft.Json/lib/net40/Newtonsoft.Json.dll"
#r "../../src/FSharp.Data.GraphQL.Server/bin/Debug/FSharp.Data.GraphQL.Shared.dll"
#r "../../src/FSharp.Data.GraphQL.Server/bin/Debug/FSharp.Data.GraphQL.Server.dll"

open System

// Data

type Widget = 
    { Id: string;
      Name: string }

type User = 
    { Id: string;
      Name: string;
      Widgets: Widget list }

let viewer = {
    Id = "1"
    Name = "Anonymous"
    Widgets = [
        { Id = "1"; Name = "What's it"}
        { Id = "2"; Name = "Who's it"}
        { Id = "3"; Name = "How's it"} ]}

let getUser id = if viewer.Id = id then Some viewer else None
let getWidget id = viewer.Widgets |> List.tryFind (fun w -> w.Id = id)

// Schema definition
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Relay

let rec Widget = Define.Object<Widget>(
    name = "Widget",
    description = "A shiny widget",
    interfaces = [ Node ],
    fields = [
        Define.GlobalIdField(fun _ w -> w.Id)
        Define.Field("name", String, fun _ w -> w.Name)])

and User = Define.Object<User>(
    name = "User",
    description = "A person who uses our app",
    interfaces = [ Node ],
    fields = [
        Define.GlobalIdField(fun _ w -> w.Id)
        Define.Field("name", String, fun _ w -> w.Name)
        Define.Field("widgets", ConnectionOf Widget, "A person's collection of widgets", Connection.allArgs, fun ctx user -> 
            let widgets = user.Widgets |> List.toArray
            Connection.ofArray widgets )])

and Node = Define.Node<obj>(fun () -> [ User; Widget ])

let Query = Define.Object("Query", [
    Define.NodeField(Node, fun ctx () id -> 
        match id with
        | GlobalId("User", i) -> getUser i |> Option.map box
        | GlobalId("Widget", i) -> getWidget i |> Option.map box
        | _ -> None)
    Define.Field("viewer", User, fun _ () -> viewer)])

let schema = Schema(query = Query, config = { SchemaConfig.Default with Types = [ User; Widget ]})

// server initialization
open Suave
open Suave.Operators
open Newtonsoft.Json
open FSharp.Data.GraphQL.Execution

let settings = JsonSerializerSettings()
settings.ContractResolver <- Newtonsoft.Json.Serialization.CamelCasePropertyNamesContractResolver()
let json o = JsonConvert.SerializeObject(o, settings)

let tryParse data =
    let raw = Text.Encoding.UTF8.GetString data
    if raw <> null && raw <> ""
    then
        let map = JsonConvert.DeserializeObject<Map<string,obj>>(raw)
        Map.tryFind "query" map
    else None

let handle : WebPart =
    fun http ->
        async {
            match tryParse http.request.rawForm with
            | Some query ->
                // at the moment parser is not parsing new lines correctly, so we need to get rid of them
                let q = (query :?> string).Trim().Replace("\r\n", " ")
                let! result = schema.AsyncExecute(q)
                let serialized = json result
                return! http |> Successful.OK serialized
            | None ->
                let! schemaResult = schema.AsyncExecute(Introspection.introspectionQuery)
                return! http |> Successful.OK (json schemaResult)
        }

let setCorsHeaders =
    Writers.setHeader  "Access-Control-Allow-Origin" "*"
    >=> Writers.setHeader "Access-Control-Allow-Headers" "content-type"

startWebServer defaultConfig (setCorsHeaders >=> handle >=> Writers.setMimeType "application/json")
