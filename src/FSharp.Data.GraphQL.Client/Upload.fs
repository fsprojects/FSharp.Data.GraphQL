// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL

open System
open System.IO
open System.Net.Mime
open System.Runtime.InteropServices
open FSharp.Data.GraphQL.Client

/// The base type for all GraphQLProvider upload types.
/// Upload types are used in GraphQL multipart request spec, mostly for file uploading features.
type Upload
    (stream : Stream, fileName : string, [<Optional>] name : string | null, [<Optional>] contentType : string | null, [<Optional>] ownsStream : bool)
    =

    new (bytes : byte[], fileName, [<Optional>] name, [<Optional>] contentType) =
        let stream = new MemoryStream (bytes)
        match contentType with
        | null -> new Upload (stream, fileName, name, ownsStream = true)
        | ct -> new Upload (stream, fileName, name, ct, true)

    new (bytes : byte[], fileName, contentType) = new Upload (bytes = bytes, fileName = fileName, name = null, contentType = contentType)

    new (stream, fileName, [<Optional>] name, [<Optional>] contentType) = new Upload (stream, fileName, name, contentType, ownsStream = false)

    /// Gets the stream associated to this Upload type.
    member _.Stream = stream

    /// Gets the content type of this Upload type.
    member _.ContentType =
        match contentType with
        | null ->
            let ext = Path.GetExtension (fileName)
            match MimeTypes.dict.Force().TryGetValue (ext) with
            | (true, mime) -> mime
            | _ -> MediaTypeNames.Application.Octet
        | ct -> ct

    /// Gets the name used to uniquily identify upload throuout multiple uploads
    /// and within a request.
    member val Name =
        name
        |> ValueOption.ofObj
        |> ValueOption.defaultWith (Guid.NewGuid >> string)

    /// Gets the name of the file which contained on the stream.
    member _.FileName = fileName

    /// Gets a boolean value indicating if this Upload type owns the stream associated with it.
    /// If true, it will dispose the stream when this Upload type is disposed.
    member _.OwnsStream = ownsStream

    interface IDisposable with
        member x.Dispose () =
            if x.OwnsStream then
                x.Stream.Dispose ()
