#r "../../packages/Suave/lib/net40/Suave.dll"
#r "../../packages/Newtonsoft.Json/lib/net40/Newtonsoft.Json.dll"
#r "../../src/FSharp.Data.GraphQL/bin/Debug/FSharp.Data.GraphQL.dll"

open System

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

let getHuman id = humans |> List.find (fun human -> human.Id = id)
let getDroid id = droids |> List.find (fun droid -> droid.Id = id)

// Schema definition
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Execution

let EpisodeType = Define.Enum(
    name = "Episode",
    description = "One of the films in the Star Wars Trilogy",
    options = [
        Define.EnumValue("NEWHOPE", Episode.NewHope, "Released in 1977.")
        Define.EnumValue("EMPIRE", Episode.Empire, "Released in 1980.")
        Define.EnumValue("JEDI", Episode.Jedi, "Released in 1983.")])

let rec CharacterType = Define.Interface(
    name = "Character",
    description = "A character in the Star Wars Trilogy",
    resolveType = (fun o ->
        match o with
        | :? Human -> HumanType
        | :? Droid -> DroidType
        | _ -> failwithf "Value of type '%s' is not a Character type" (o.GetType().FullName)),
    fields = fun () -> [
        Define.Field("id", NonNull String, "The id of the character.")
        Define.Field("name", String, "The name of the character.")
        Define.Field("friends", ListOf String, "The friends of the character, or an empty list if they have none.")
        Define.Field("appearsIn", ListOf EpisodeType, "Which movies they appear in.")])

and HumanType = Define.Object(
    name = "Human",
    description = "A humanoid creature in the Star Wars universe.",
    interfaces = [ CharacterType ],
    isTypeOf = (fun o -> o :? Human),
    fields = [
        Define.Field("id", NonNull String, "The id of the human.")
        Define.Field("name", String, "The name of the human.")
        Define.Field("friends", ListOf String, "The friends of the human, or an empty list if they have none.")
        Define.Field("appearsIn", ListOf EpisodeType, "Which movies they appear in.")
        Define.Field("homePlanet", String, "The home planet of the human, or null if unknown.")])
        
and DroidType = Define.Object(
    name = "Droid",
    description = "A mechanical creature in the Star Wars universe.",
    interfaces = [ CharacterType ],
    isTypeOf = (fun o -> o :? Droid),
    fields = [
        Define.Field("id", NonNull String, "The id of the droid.")
        Define.Field("name", String, "The name of the Droid.")
        Define.Field("friends", ListOf String, "The friends of the Droid, or an empty list if they have none.")
        Define.Field("appearsIn", ListOf EpisodeType, "Which movies they appear in.")
        Define.Field("primaryFunction", String, "The primary function of the droid.")])

let Query = Define.Object(
    name = "Query",
    fields = [
        Define.Field("hero", CharacterType, args = [ Define.Arg("id", String) ], resolve = fun ctx _ -> getHuman (ctx.Arg("id").Value))
        Define.Field("droid", CharacterType, args = [ Define.Arg("id", String) ], resolve = fun ctx _ -> getDroid (ctx.Arg("id").Value))])

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
    
let graphiql : WebPart =
    fun http ->
        async {
            match http.request.fieldData "query" with
            | Choice1Of2 query ->
                let! result = schema.AsyncExecute(query)   
                return! http |> Successful.OK (json result)
            | Choice2Of2 err ->
                return! http |> ServerErrors.INTERNAL_ERROR err
        }

startWebServer defaultConfig (graphiql >=> Writers.setMimeType "application/json")
// Example:
// curl --form 'query={ hero(id: "1000") { id, name, appearsIn } }' http://localhost:8083/