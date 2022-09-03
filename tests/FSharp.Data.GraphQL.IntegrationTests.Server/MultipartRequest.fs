namespace FSharp.Data.GraphQL.IntegrationTests.Server

open System
open System.Collections
open System.Collections.Generic
open Microsoft.AspNetCore.WebUtilities
open Newtonsoft.Json
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
    let private parseOperations (operations: Operation list) (map : IDictionary<string, string>) (files : IDictionary<string, File>) =
        let mapOperation (operationIndex : int option) (operation : Operation) =
            let findFile (varName : string) (varValue : obj) =
                let tryPickMultipleFilesFromMap (length : int) (varName : string) =
                    Seq.init length (fun ix ->
                        match map.TryGetValue(sprintf "%s.%i" varName ix) with
                        | (true, v) -> Some v
                        | _ -> None)
                    |> Seq.map (fun key ->
                        key |> Option.map (fun key ->
                            match files.TryGetValue(key) with
                            | (true, v) -> Some v
                            | _ -> None)
                        |> Option.flatten)
                    |> List.ofSeq
                let pickMultipleFilesFromMap (length : int) (varName : string) =
                    Seq.init length (fun ix -> map.[sprintf "%s.%i" varName ix])
                    |> Seq.map (fun key -> files.[key])
                    |> List.ofSeq
                let tryPickSingleFileFromMap varName =
                    let found = map |> Seq.choose (fun kvp -> if kvp.Key = varName then Some files.[kvp.Value] else None) |> List.ofSeq
                    match found with
                    | [x] -> Some x
                    | _ -> None
                let pickSingleFileFromMap varName =
                    map
                    |> Seq.choose (fun kvp -> if kvp.Key = varName then Some files.[kvp.Value] else None)
                    |> Seq.exactlyOne
                let pickFileRequestFromMap (request : UploadRequest) varName =
                    { UploadRequest.Single = pickSingleFileFromMap (sprintf "%s.single" varName)
                      Multiple = pickMultipleFilesFromMap request.Multiple.Length (sprintf "%s.multiple" varName)
                      NullableMultiple = request.NullableMultiple |> Option.map (fun x -> pickMultipleFilesFromMap x.Length (sprintf "%s.nullableMultiple" varName))
                      NullableMultipleNullable = request.NullableMultipleNullable |> Option.map (fun x -> tryPickMultipleFilesFromMap x.Length (sprintf "%s.nullableMultipleNullable" varName)) }
                let rec isUpload (t : InputType) =
                    match t with
                    | NamedType tname -> tname = "Upload" || tname = "UploadRequest"
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
                    | :? JObject as jreq ->
                        let request = jreq.ToObject<UploadRequest>(jsonSerializer)
                        let varName =
                            match operationIndex with
                            | Some operationIndex -> sprintf "%i.variables.%s" operationIndex varName
                            | None -> sprintf "variables.%s" varName
                        pickFileRequestFromMap request varName |> box
                    | :? IEnumerable as values ->
                        values
                        |> Seq.cast<obj>
                        |> Seq.mapi (fun valueIndex _ ->
                            let varName =
                                match operationIndex with
                                | Some operationIndex -> sprintf "%i.variables.%s.%i" operationIndex varName valueIndex
                                | None -> sprintf "variables.%s.%i" varName valueIndex
                            tryPickSingleFileFromMap varName |> Option.map box |> Option.toObj)
                        |> box
                    | _ ->
                        let varName =
                            match operationIndex with
                            | Some operationIndex -> sprintf "%i.variables.%s" operationIndex varName
                            | None -> sprintf "variables.%s" varName
                        tryPickSingleFileFromMap varName |> Option.map box |> Option.toObj
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
            let mutable map : IDictionary<string, string> = null
            let files = Dictionary<string, File>()
            do! readNextSection ()
            while not section.IsNone do
                match section.Value with
                | Form section ->
                    let! value = section.GetValueAsync() |> Async.AwaitTask
                    match section.Name with
                    | "operations" ->
                        operations <- value
                    | "map" ->
                        map <- JsonConvert.DeserializeObject<Map<string, string list>>(value)
                               |> Seq.map (fun kvp -> kvp.Value.Head, kvp.Key)
                               |> Map.ofSeq
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