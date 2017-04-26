
open System
open FSharp.Data

// Data
type Episode = 
    | NewHope = 1
    | Empire = 2
    | Jedi = 3
    
type Human = 
    { Id : string
      Name : string option
      Friends : string list
      AppearsIn : Episode list
      HomePlanet : string option }

type Droid = 
    { Id : string
      Name : string option
      Friends : string list
      AppearsIn : Episode list
      PrimaryFunction : string option }

type Planet =
    { Id : string
      Name : string option
      mutable IsMoon : bool option}
    member x.SetMoon b =
        x.IsMoon <- b
        x

type Root = 
    { Hero : Human option
      Droid : Droid option }

let humans = 
    [ { Id = "1000"
        Name = Some "Luke Skywalker"
        Friends = [ "1002"; "1003"; "2000"; "2001" ]
        AppearsIn = [ Episode.NewHope; Episode.Empire; Episode.Jedi ]
        HomePlanet = Some "Tatooine" }
      { Id = "1001"
        Name = Some "Darth Vader"
        Friends = [ "1004" ]
        AppearsIn = [ Episode.NewHope; Episode.Empire; Episode.Jedi ]
        HomePlanet = Some "Tatooine" }
      { Id = "1002"
        Name = Some "Han Solo"
        Friends = [ "1000"; "1003"; "2001" ]
        AppearsIn = [ Episode.NewHope; Episode.Empire; Episode.Jedi ]
        HomePlanet = None }
      { Id = "1003"
        Name = Some "Leia Organa"
        Friends = [ "1000"; "1002"; "2000"; "2001" ]
        AppearsIn = [ Episode.NewHope; Episode.Empire; Episode.Jedi ]
        HomePlanet = Some "Alderaan" }
      { Id = "1004"
        Name = Some "Wilhuff Tarkin"
        Friends = [ "1001" ]
        AppearsIn = [ Episode.NewHope ]
        HomePlanet = None } ]

let droids = 
    [ { Id = "2000"
        Name = Some "C-3PO"
        Friends = [ "1000"; "1002"; "1003"; "2001" ]
        AppearsIn = [ Episode.NewHope; Episode.Empire; Episode.Jedi ]
        PrimaryFunction = Some "Protocol" }
      { Id = "2001"
        Name = Some "R2-D2"
        Friends = [ "1000"; "1002"; "1003" ]
        AppearsIn = [ Episode.NewHope; Episode.Empire; Episode.Jedi ]
        PrimaryFunction = Some "Astromech" } ]

let planets =
    [ { Id = "1"
        Name = Some "Tatooine"
        IsMoon = Some false}
      { Id = "2"
        Name = Some "Endor"
        IsMoon = Some true}
      { Id = "3"
        Name = Some "Death Star"
        IsMoon = Some false}]

let getHuman id = humans |> List.tryFind (fun h -> h.Id = id)
let getDroid id = droids |> List.tryFind (fun d -> d.Id = id)
let getPlanet id = planets |> List.tryFind (fun p -> p.Id = id)

type Character =
    | Human of Human
    | Droid of Droid

let characters = (humans |> List.map Human) @ (droids |> List.map Droid)

let matchesId id = function
    | Human h -> h.Id = id
    | Droid d -> d.Id = id

let getCharacter id = characters |> List.tryFind (matchesId id)

// Schema definition
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Execution

let EpisodeType =
  Define.Enum(
    name = "Episode",
    description = "One of the films in the Star Wars Trilogy",
    options = [
        Define.EnumValue("NewHope", Episode.NewHope, "Released in 1977.")
        Define.EnumValue("Empire", Episode.Empire, "Released in 1980.")
        Define.EnumValue("Jedi", Episode.Jedi, "Released in 1983.") ])

let rec CharacterType =
  Define.Union(
    name = "Character",
    description = "A character in the Star Wars Trilogy",
    options = [ HumanType; DroidType ],
    resolveValue = (fun o ->
        match o with
        | Human h -> box h
        | Droid d -> upcast d),
    resolveType = (fun o ->
        match o with
        | Human _ -> upcast HumanType
        | Droid _ -> upcast DroidType))

and HumanType : ObjectDef<Human> =
  Define.Object<Human>(
    name = "Human",
    description = "A humanoid creature in the Star Wars universe.",
    isTypeOf = (fun o -> o :? Human),
    fieldsFn = fun () -> [
        Define.Field("id", String, "The id of the human.", fun _ h -> h.Id)
        Define.Field("name", Nullable String, "The name of the human.", fun _ h -> h.Name)
        Define.Field("friends", ListOf (Nullable CharacterType), "The friends of the human, or an empty list if they have none.",
            fun _ h -> 
                h.Friends
                |> List.map getCharacter 
                |> List.toSeq)
        Define.Field("appearsIn", ListOf EpisodeType, "Which movies they appear in.", fun _ h -> h.AppearsIn)
        Define.Field("homePlanet", Nullable String, "The home planet of the human, or null if unknown.", fun _ h -> h.HomePlanet) ])
        
and DroidType =
  Define.Object<Droid>(
    name = "Droid",
    description = "A mechanical creature in the Star Wars universe.",
    isTypeOf = (fun o -> o :? Droid),
    fieldsFn = fun () -> [
        Define.Field("id", String, "The id of the droid.", fun _ d -> d.Id)
        Define.Field("name", Nullable String, "The name of the Droid.", fun _ d -> d.Name)
        Define.Field("friends", ListOf (Nullable CharacterType), "The friends of the Droid, or an empty list if they have none.", 
            fun ctx d -> d.Friends |> List.map getCharacter |> List.toSeq)
        Define.Field("appearsIn", ListOf EpisodeType, "Which movies they appear in.", fun _ d -> d.AppearsIn)
        Define.Field("primaryFunction", Nullable String, "The primary function of the droid.", fun _ d -> d.PrimaryFunction) ])
and PlanetType =
    Define.Object<Planet>(
        name = "Planet",
        description = "A planet in the Star Wars universe.",
        isTypeOf = (fun o -> o :? Planet),
        fieldsFn = fun () -> [
            Define.Field("id", String, "The id of the planet", fun _ p -> p.Id)
            Define.Field("name", Nullable String, "The name of the planet.", fun _ p -> p.Name)
            Define.Field("ismoon", Nullable Boolean, "Is that a moon?", fun _ p -> p.IsMoon)])
// Define our root query type, used to expose possible queries to the client
let Query =
  Define.Object<Root>(
    name = "Query",
    fields = [
        Define.Field("hero", Nullable HumanType, "Gets human hero", [ Define.Input("id", String) ], fun ctx _ -> getHuman (ctx.Arg("id")))
        Define.Field("droid", Nullable DroidType, "Gets droid", [ Define.Input("id", String) ], fun ctx _ -> getDroid (ctx.Arg("id"))) 
        Define.Field("planet", Nullable PlanetType, "Gets planet", [ Define.Input("id", String) ], fun ctx _ -> getPlanet (ctx.Arg("id")))])

let Mutation =
    Define.Object<Root>(
        name = "Mutation",
        fields = [
            Define.Field(
                "setMoon", 
                Nullable PlanetType, 
                "Sets a moon status", 
                [ Define.Input("id", String); Define.Input("ismoon", Boolean) ], 
                fun ctx _ -> getPlanet(ctx.Arg("id")) |> Option.map (fun x -> x.SetMoon(Some(ctx.Arg("ismoon")))))])

let Subscription = 
    Define.Object<Root>(
        name = "Subscription",
        fields = [
            Define.Field(
                "watchMoon",
                Nullable PlanetType,
                "Watches to see if a planet is a moon",
                [ Define.Input("id", String) ],
                fun ctx _ -> getPlanet(ctx.Arg("id"))
            )])

let schema = Schema(Query, Mutation, Subscription) :> ISchema<Root>
let ex = Executor(schema)

// server initialization
open Suave
open Suave.Operators
open System.Reflection
open FSharp.Reflection
open Newtonsoft.Json

type OptionConverter() =
    inherit JsonConverter()
    
    override x.CanConvert(t) = 
        t.GetTypeInfo().IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>

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
        match Map.tryFind fieldName map with
        | Some "" -> None
        | s -> s
    else None


let graphiql : WebPart =
    fun http ->
        async {
            let tryQuery = tryParse "query" http.request.rawForm
            let tryVariables = tryParse "variables" http.request.rawForm |> Option.map (JsonConvert.DeserializeObject<Map<string, obj>>)
            match tryQuery, tryVariables  with
            | Some query, Some variables ->
                printfn "Received query: %s" query
                printfn "Recieved variables: %A" variables
                // at the moment parser is not parsing new lines correctly, so we need to get rid of them
                let q = query.Trim().Replace("\r\n", " ")
                let! result = ex.AsyncExecute(q, variables=variables)
                return! http |> Successful.OK (json result)
            | Some query, None ->
                printfn "Received query: %s" query
                let q = query.Trim().Replace("\r\n", " ")
                let! result = ex.AsyncExecute(q)
                return! http |> Successful.OK (json result)
            | None, _  ->
                let! schemaResult = ex.AsyncExecute(Introspection.introspectionQuery)
                return! http |> Successful.OK (json schemaResult)
        }

let setCorsHeaders = 
    Writers.setHeader  "Access-Control-Allow-Origin" "*"
    >=> Writers.setHeader "Access-Control-Allow-Headers" "content-type"

let serverConfig =
    { defaultConfig with
        bindings = [ HttpBinding.createSimple HTTP "127.0.0.1" 8083]

    }

[<EntryPoint>]
let main argv = 
    startWebServer serverConfig (setCorsHeaders >=> graphiql >=> Writers.setMimeType "application/json")
    0
// Example:
// curl --form 'query={ hero(id: "1000") { id, name, appearsIn, friends { id,name } } }' http://localhost:8083/