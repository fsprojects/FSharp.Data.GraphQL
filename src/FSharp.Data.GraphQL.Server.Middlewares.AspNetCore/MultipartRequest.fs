namespace FSharp.Data.GraphQL.Server.Middlewares.AspNetCore

open Microsoft.AspNetCore.WebUtilities
open Newtonsoft.Json
open System.Collections.Generic
open Newtonsoft.Json.Linq

/// A GraphQL operation request.
type Operation =
      /// Contains the query used in this operation.
    { Query : string
      /// Contains variables used by this operation.
      Variables : Map<string, obj> }

/// <summary> A GrahpQL request using multipart request specification. </summary>
/// <remarks> For more information, see https://github.com/jaydenseric/graphql-multipart-request-spec. </remarks>
type MultipartRequest =
      /// Contains the list of operations of this request.
      /// If the request is not batched, then the single operation will be inside this list as a singleton.
    { Operations : Operation list }

/// Contains tools for working with GraphQL multipart requests.
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

    /// Reads a GraphQL multipart request from a MultipartReader.
    let read (reader : MultipartReader) = 
        let getName (section : MultipartSection) =
            match section.AsFormDataSection() with
            | null -> section.AsFileSection().Name
            | x -> x.Name
        let readText (section : MultipartSection) =
            section.AsFormDataSection().GetValueAsync()
            |> Async.AwaitTask
            |> Async.RunSynchronously
        let readFile (section : MultipartSection) =
            let section = section.AsFileSection()
            section.Name, { Name = section.FileName; ContentType = section.Section.ContentType; Content = section.FileStream }
        let mutable section : MultipartSection = null
        let readNextSection () = 
            section <- reader.ReadNextSectionAsync() |> Async.AwaitTask |> Async.RunSynchronously
        let mutable operations : string = null
        let mutable map : IDictionary<string, string list> = null
        let files = Dictionary<string, File>()
        readNextSection ()
        while not (isNull section) do
            match getName section with
            | "operations" ->
                operations <- readText section
            | "map" ->
                map <- JsonConvert.DeserializeObject<IDictionary<string, string list>>(readText section)
            | _ ->
                let key, value = readFile section
                files.Add(key, value)
            readNextSection ()
        { Operations = parseOperations operations map files }