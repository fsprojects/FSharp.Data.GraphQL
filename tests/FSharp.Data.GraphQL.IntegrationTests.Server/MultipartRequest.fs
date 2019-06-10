namespace FSharp.Data.GraphQL.IntegrationTests.Server

open System
open Microsoft.AspNetCore.WebUtilities
open Newtonsoft.Json
open System.Collections
open System.Collections.Generic
open Newtonsoft.Json.Linq
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL

type GraphQLMultipartSection =
    | Form of FormMultipartSection
    | File of FileMultipartSection
    static member FromSection(section : MultipartSection) =
        match section with
        | null -> None
        | _ ->
            match section.AsFormDataSection() with
            | null ->
                match section.AsFileSection() with
                | null -> None
                | x -> Some (File x)
            | x -> Some (Form x)
    member x.Name =
        match x with
        | Form x -> x.Name
        | File x -> x.Name

/// A GraphQL operation request.
type Operation =
      /// Contains the query used in this operation.
    { Query : string
      /// Contains variables used by this operation.
      Variables : Map<string, obj> option }

/// <summary> A GrahpQL request using multipart request specification. </summary>
/// <remarks> For more information, see https://github.com/jaydenseric/graphql-multipart-request-spec. </remarks>
type MultipartRequest =
      /// Contains the list of operations of this request.
      /// If the request is not batched, then the single operation will be inside this list as a singleton.
    { Operations : Operation list }

/// Contains tools for working with GraphQL multipart requests.
module MultipartRequest =
    let private parseOperations (operations: Operation list) (map : IDictionary<string, string list>) (files : IDictionary<string, File>) =
        let map =
            map
            |> Seq.choose (fun x -> x.Value |> Seq.tryHead |> Option.map (fun v -> x.Key, v))
            |> Map.ofSeq
        let files =
            files
            |> Seq.map (|KeyValue|)
            |> Map.ofSeq
        let mapOperation (operationIndex : int option) (operation : Operation) =
            let findFile (varName : string) (varValue : obj) =
                let pickFromMap varName =
                    map |> Map.tryPick (fun k v -> if v = varName then files |> Map.tryFind k else None)
                let rec isUpload (t : InputType) =
                    match t with
                    | NamedType tname -> tname = "Upload"
                    | ListType t | NonNullType t -> isUpload t
                let ast = Parser.parse operation.Query
                let vardefs = 
                    ast.Definitions
                    |> List.choose (function OperationDefinition def -> Some def.VariableDefinitions | _ -> None)
                    |> List.collect id
                let vardef = vardefs |> List.find (fun x -> x.VariableName = varName)
                if not (isUpload vardef.Type)
                then varValue
                else
                    match varValue with
                    | :? IEnumerable as values ->
                        values
                        |> Seq.cast<obj>
                        |> Seq.mapi (fun valueIndex value ->
                            let varName =
                                match operationIndex with
                                | Some operationIndex -> String.Format("{0}.variables.{1}.{2}", operationIndex, varName, valueIndex)
                                | None -> String.Format("variables.{0}.{1}", varName, valueIndex)
                            match pickFromMap varName with
                            | Some file -> box file
                            | None -> value)
                        |> box
                    | value ->
                        let varName =
                            match operationIndex with
                            | Some operationIndex -> String.Format("{0}.variables.{1}", operationIndex, varName)
                            | None -> String.Format("variables.{0}", varName)
                        match pickFromMap varName with
                        | Some file -> box file
                        | None -> value
            { operation with Variables = operation.Variables |> Option.map (Map.map (fun k v -> findFile k v)) }
        match operations with
        | [ operation ] -> [ mapOperation None operation ]
        | operations -> operations |> List.mapi (fun ix operation -> mapOperation (Some ix) operation)

    /// Reads a GraphQL multipart request from a MultipartReader.
    let read (reader : MultipartReader) = 
        async {
            let mutable section : GraphQLMultipartSection option = None
            let readNextSection () =
                async {
                    let! next = reader.ReadNextSectionAsync() |> Async.AwaitTask
                    section <- GraphQLMultipartSection.FromSection(next)
                }
            let mutable operations : string = null
            let mutable map : IDictionary<string, string list> = null
            let files = Dictionary<string, File>()
            do! readNextSection ()
            while not section.IsNone do
                match section.Value with
                | Form section ->
                    let! value = section.GetValueAsync() |> Async.AwaitTask
                    match section.Name with
                    | "operations" -> operations <- value
                    | "map" -> map <- JsonConvert.DeserializeObject<IDictionary<string, string list>>(value)
                    | _ -> failwithf "Error reading multipart request. Unexpected section name \"%s\"." section.Name
                | File section ->
                    let stream = new System.IO.MemoryStream(4096)
                    do! section.FileStream.CopyToAsync(stream) |> Async.AwaitTask
                    stream.Position <- 0L
                    let value = { Name = section.FileName; ContentType = section.Section.ContentType; Content = stream }
                    files.Add(section.Name, value)
                do! readNextSection ()
            let operations =
                match JToken.Parse(operations) with
                | :? JArray as ops -> ops.ToObject<Operation list>(jsonSerializer)
                | :? JObject as op -> [ op.ToObject<Operation>(jsonSerializer) ]
                | _ -> failwith "Unexpected operations value."
            return { Operations = parseOperations operations map files }
        } |> Async.StartAsTask