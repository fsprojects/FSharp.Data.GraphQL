/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open System.IO
open System.Text.Json
open System.Collections
open System.Collections.Generic
open System.ComponentModel
open FSharp.Data.GraphQL.Client


/// The base type for all GraphQLProvider upload types.
/// Upload types are used in GraphQL multipart request spec, mostly for file uploading features.
type Upload (stream : Stream, fileName : string, ?contentType : string, ?ownsStream : bool) =
    new(bytes : byte [], fileName, ?contentType) =
        let stream = new MemoryStream(bytes)
        match contentType with
        | Some ct -> new Upload(stream, fileName, ct, true)
        | None -> new Upload(stream, fileName, ownsStream = true)

    /// Gets the stream associated to this Upload type.
    member __.Stream = stream

    /// Gets the content type of this Upload type.
    member __.ContentType =
        match contentType with
        | Some ct -> ct
        | None ->
            let ext = Path.GetExtension(fileName)
            match MimeTypes.dict.Force().TryGetValue(ext) with
            | (true, mime) -> mime
            | _ -> "application/octet-stream"

    /// Gets the name of the file which contained on the stream.
    member __.FileName = fileName

    /// Gets a boolean value indicating if this Upload type owns the stream associated with it.
    /// If true, it will dispose the stream when this Upload type is disposed.
    member __.OwnsStream = defaultArg ownsStream false

    interface IDisposable with
        member x.Dispose() = if x.OwnsStream then x.Stream.Dispose()


/// The base type for all GraphQLProvider provided enum types.
type EnumBase (name : string, value : string) =
    /// Gets the name of the provided enum type.
    member __.GetName() = name

    /// Gets the value of the provided enum type.
    member __.GetValue() = value

    override x.ToString() = x.GetValue()

    member x.Equals(other : EnumBase) =
        x.GetName() = other.GetName() && x.GetValue() = other.GetValue()

    override x.Equals(other : obj) =
        match other with
        | :? EnumBase as other -> x.Equals(other)
        | _ -> false

    override x.GetHashCode() = x.GetName().GetHashCode() ^^^ x.GetValue().GetHashCode()

    interface IEquatable<EnumBase> with
        member x.Equals(other) = x.Equals(other)


/// The base type for all GraphQLProvider provided record types.
type RecordBase (typeName: string, keyValuePairs: seq<KeyValuePair<string, obj>>) =
    let properties =
        keyValuePairs
        |> Seq.map (|KeyValue|)
        |> Map.ofSeq

    member _.GetName () = typeName

    member _.GetProperty(name: string) =
        match properties.TryGetValue(name) with
        | true, value -> value
        | false, _ -> failwithf "No property with '%s' found for type '%s'" name typeName

    member _.TryGetProperty(name: string) =
        Map.tryFind name properties

    member __.GetProperties () =
        properties

    member __.ToDictionary () =
        keyValuePairs
        |> Seq.map (|KeyValue|)
        |> readOnlyDict

    override this.ToString() =
        (properties :> seq<KeyValuePair<string, obj>>).ToString()

    override __.GetHashCode () =
        struct(typeName, properties).GetHashCode()

    override __.Equals(other) =
        match other with
        | :? RecordBase as r -> r.GetName() = typeName && r.GetProperties() = properties
        | _ -> false

    interface seq<KeyValuePair<string, obj>> with
        member _.GetEnumerator () =
            (properties :> IEnumerable<_>).GetEnumerator()
        member _.GetEnumerator () =
            (properties :> IEnumerable).GetEnumerator()

    interface IEquatable<RecordBase> with
        member x.Equals(other) = x.Equals(other)

type OperationError =
    {  Message: string
       Path: obj [] }

type OperationResult =
    { Data: RecordBase option
      Errors: OperationError []
      CustomData : Map<string, obj> }

/// The base type for all GraphQLProvider operation result provided types.
type OperationResultBase (responseStream: Stream, parser: JsonElement -> OperationResult, operationTypeName : string) =
    let response =
        use document = JsonDocument.Parse(responseStream)
        parser document.RootElement

    ///  <exclude />
    [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
    [<CompilerMessageAttribute("This property is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
    member __.RawData = response.Data

    /// Gets all the errors returned by the operation on the server.
    member __.Errors = response.Errors

    /// Gets all the custom data returned by the operation on server as a map of names and values.
    member __.CustomData = response.CustomData

    member x.Equals(other : OperationResultBase) =
        other.RawData <> response.Data

    override x.Equals(other : obj) =
        match other with
        | :? OperationResultBase as other -> x.Equals(other)
        | _ -> false

    override _.GetHashCode() = response.GetHashCode()

/// The base type for al GraphQLProvider operation provided types.
type OperationBase (query : string) =
    /// Gets the query string of the operation.
    member __.Query = query
