namespace FSharp.Data.GraphQL.Server.Middlewares.AspNetCore

open Microsoft.AspNetCore.WebUtilities
open Newtonsoft.Json
open System.Collections.Generic
open Newtonsoft.Json.Linq

type Operation =
    { Query : string
      Variables : Map<string, obj> }

type MultipartRequest =
    { Operations : Operation list }

module MultipartRequest =
    let private parseOperations (operations: string) (map : IDictionary<string, string list>) (files : IDictionary<string, File>) : Operation list =
        let operations =
            match JToken.Parse(operations) with
            | :? JArray as ops -> ops.ToObject<Operation list>()
            | :? JObject as op -> [ op.ToObject<Operation>() ]
            | _ -> failwith "Unexpected operations value."
        let map = 
            map 
            |> Seq.map (fun x -> x.Key, x.Value |> Seq.tryHead)
            |> Seq.filter (fun (_, v) -> Option.isSome v)
            |> Seq.map (fun (k, v) -> k, v.Value)
            |> Map.ofSeq
        let files =
            files
            |> Seq.map (fun x -> x.Key, x.Value)
            |> Map.ofSeq
        let mapOperation (operationIndex : int option) operation =
            let findFile (varName : string) (varValue : obj) =
                let pickFromMap varName =
                    map |> Map.tryPick (fun k v -> if v = varName then files |> Map.tryFind k else None)
                match varValue with
                | :? seq<obj> as values ->
                    values
                    |> Seq.mapi (fun valueIndex value ->
                        let varName =
                            match operationIndex with
                            | Some operationIndex -> sprintf "%i.variables.%s.%i" operationIndex varName valueIndex
                            | None -> sprintf "variables.%s.%i" varName valueIndex
                        match pickFromMap varName with
                        | Some file -> box file
                        | None -> value)
                    |> box
                | value ->
                    let varName =
                        match operationIndex with
                        | Some operationIndex -> sprintf "%i.variables.%s" operationIndex varName
                        | None -> sprintf "variables.%s" varName
                    match pickFromMap varName with
                    | Some file -> box file
                    | None -> value
            { operation with Variables = operation.Variables |> Map.map (fun k v -> findFile k v) }
        match operations with
        | [ operation ] -> [ mapOperation None operation ]
        | operations -> operations |> List.mapi (fun ix operation -> mapOperation (Some ix) operation)
    let read (reader : MultipartReader) = 
        async {
            let getName (section : MultipartSection) =
                match section.AsFormDataSection() with
                | null -> section.AsFileSection().Name
                | x -> x.Name
            let readText (section : MultipartSection) =
                section.AsFormDataSection().GetValueAsync()
                |> Async.AwaitTask
            let readFile (section : MultipartSection) =
                let section = section.AsFileSection()
                section.Name, { Name = section.FileName; ContentType = section.Section.ContentType; Content = section.FileStream }
            let mutable section : MultipartSection = null
            let readNextSection () = 
                async {
                    let! next = reader.ReadNextSectionAsync() |> Async.AwaitTask
                    section <- next
                }
            let mutable operations : string = null
            let mutable map : IDictionary<string, string list> = null
            let files = Dictionary<string, File>()
            do! readNextSection ()
            while not (isNull section) do
                match getName section with
                | "operations" -> 
                    let! text = readText section
                    operations <- text
                | "map" -> 
                    let! text = readText section
                    map <- JsonConvert.DeserializeObject<IDictionary<string, string list>>(text)
                | _ -> 
                    let key, value = readFile section
                    files.Add(key, value)
                do! readNextSection ()
            return { Operations = parseOperations operations map files }
        }