namespace FSharp.Data.GraphQL.Samples.StarWarsApi

open System
open System.Collections
open System.Collections.Generic
open System.Text.Json
open System.Text.Json.Serialization
open Microsoft.AspNetCore.WebUtilities

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Ast

[<Struct>]
type GraphQLMultipartSection =
    | Form of Form: FormMultipartSection
    | File of File: FileMultipartSection
    static member FromSection(section : MultipartSection) =
        match section with
        | null -> ValueNone
        | _ ->
            match section.AsFormDataSection() with
            | null ->
                match section.AsFileSection() with
                | null -> ValueNone
                | x -> ValueSome (File x)
            | x -> ValueSome (Form x)
    member x.Name =
        match x with
        | Form x -> x.Name
        | File x -> x.Name

/// <summary> A GrahpQL request using multipart request specification. </summary>
/// <remarks> For more information, see https://github.com/jaydenseric/graphql-multipart-request-spec. </remarks>
[<Struct>]
type MultipartRequest =
      /// Contains the list of operations of this request.
      /// If the request is not batched, then the single operation will be inside this list as a singleton.
    { Operations : GQLRequestContent list }

/// Contains tools for working with GraphQL multipart requests.
module MultipartRequest =

    let private parseOperations (jsonOptions: JsonSerializerOptions) (operations: GQLRequestContent list) (map : IDictionary<string, string>) (files : IDictionary<string, File>) =

        let mapOperation (operationIndex : int voption) (operation : GQLRequestContent) =
            let findFile (varName : string) (varValue : JsonElement) = seq {
                let tryPickMultipleFilesFromMap (length : int) (varName : string) =
                    Seq.init length (fun ix ->
                        match map.TryGetValue $"%s{varName}.%i{ix}" with
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
                    Seq.init length (fun ix -> map.[$"%s{varName}.%i{ix}"])
                    |> Seq.map (fun key -> files.[key])
                    |> List.ofSeq
                let tryPickSingleFileFromMap varName =
                    let found =
                        map
                        |> Seq.choose (fun kvp -> if kvp.Key = varName then Some files.[kvp.Value] else None)
                        |> List.ofSeq
                    match found with
                    | [x] -> Some x
                    | _ -> None
                let pickSingleFileFromMap varName =
                    map
                    |> Seq.choose (fun kvp -> if kvp.Key = varName then Some files.[kvp.Value] else None)
                    |> Seq.exactlyOne
                let pickFileRequestFromMap (request : UploadRequest) varName : UploadRequest =
                    { Single = pickSingleFileFromMap $"%s{varName}.single"
                      Multiple = pickMultipleFilesFromMap request.Multiple.Length $"%s{varName}.multiple"
                      NullableMultiple = request.NullableMultiple |> Option.map (fun x -> pickMultipleFilesFromMap x.Length $"%s{varName}.nullableMultiple")
                      NullableMultipleNullable = request.NullableMultipleNullable |> Option.map (fun x -> tryPickMultipleFilesFromMap x.Length $"%s{varName}.nullableMultipleNullable") }
                let rec isUpload (t : InputType) =
                    match t with
                    | NamedType tname -> tname = "Upload" || tname = "UploadRequest"
                    | ListType t | NonNullType t -> isUpload t
                let ast = Parser.parse operation.Query
                let varDefs =
                    ast.Definitions
                    |> List.choose (function OperationDefinition def -> Some def.VariableDefinitions | _ -> None)
                    |> List.collect id
                let varDef = varDefs |> List.find (fun x -> x.VariableName = varName)
                if isUpload varDef.Type
                then
                    match varValue.ValueKind with
                    | JsonValueKind.Object ->
                        let request = varValue.Deserialize<UploadRequest>(jsonOptions)
                        let varName =
                            match operationIndex with
                            | ValueSome operationIndex -> $"%i{operationIndex}.variables.%s{varName}"
                            | ValueNone -> $"variables.%s{varName}"
                        yield pickFileRequestFromMap request varName
                    | JsonValueKind.Array ->
                        let files =
                            varValue.EnumerateArray()
                            |> Seq.mapi (fun valueIndex _ ->
                                let varName =
                                    match operationIndex with
                                    | ValueSome operationIndex -> $"%i{operationIndex}.variables.%s{varName}.%i{valueIndex}"
                                    | ValueNone -> $"variables.%s{varName}.%i{valueIndex}"
                                tryPickSingleFileFromMap varName)
                            |> Seq.choose id
                        yield! files
                    | _ ->
                        let varName =
                            match operationIndex with
                            | ValueSome operationIndex -> $"%i{operationIndex}.variables.%s{varName}"
                            | ValueNone -> $"variables.%s{varName}"
                        match tryPickSingleFileFromMap varName with
                        | Some File -> yield file
                        | None -> ()
            }
            { operation with Variables = operation.Variables |> Skippable.map (Map.map (fun k v -> findFile k v)) }

        match operations with
        | [ operation ] -> [ mapOperation ValueNone operation ]
        | operations -> operations |> List.mapi (fun ix operation -> mapOperation (ValueSome ix) operation)

    /// Reads a GraphQL multipart request from a MultipartReader.
    let read (jsonOptions: JsonSerializerOptions) cancellationToken (reader : MultipartReader) =
        task {
            let mutable section : GraphQLMultipartSection voption = ValueNone
            let readNextSection () =
                task {
                    let! next = reader.ReadNextSectionAsync cancellationToken
                    section <- GraphQLMultipartSection.FromSection(next)
                }
            let mutable operations : string = null
            let mutable map : IDictionary<string, string> = null
            let files = Dictionary<string, File>()
            do! readNextSection ()
            while not section.IsNone do
                match section.Value with
                | Form section ->
                    let! value = section.GetValueAsync()
                    match section.Name with
                    | "operations" ->
                        operations <- value
                    | "map" ->
                        map <- JsonSerializer.Deserialize<Map<string, string list>>(value, jsonOptions)
                               |> Seq.map (fun kvp -> kvp.Value.Head, kvp.Key)
                               |> Map.ofSeq
                    | _ -> failwithf $"""Error reading multipart request. Unexpected section name "%s{section.Name}"."""
                | File section ->
                    let stream = new System.IO.MemoryStream(4096)
                    do! section.FileStream.CopyToAsync(stream, cancellationToken)
                    stream.Position <- 0L
                    let value = { Name = section.FileName; ContentType = section.Section.ContentType; Content = stream }
                    files.Add(section.Name, value)
                do! readNextSection ()
            let operations =
                let jsonElement = (JsonDocument.Parse operations).RootElement
                match jsonElement.ValueKind with
                | JsonValueKind.Array -> jsonElement.Deserialize<GQLRequestContent list>(jsonOptions)
                | JsonValueKind.Object -> [ jsonElement.Deserialize<GQLRequestContent>(jsonOptions) ]
                | _ -> failwith "Unexpected operations value."
            return { Operations = parseOperations jsonOptions operations map files }
        }
