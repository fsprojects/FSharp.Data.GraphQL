module FSharp.Data.GraphQL.IntegrationTests.Helpers

open Xunit
open System.Text
open System.Collections.Generic
open FSharp.Data.GraphQL

let normalize (x : string) =
    x.Replace("\r\n", "\n").Split([|'\n'|])
    |> Array.map (fun x -> x.Trim())
    |> Array.reduce (fun x y -> x + "\n" + y)

let equals (expected : 'T) (actual : 'T) =
    Assert.Equal<'T>(expected, actual)

let hasItems (seq : seq<'T>) =
    Assert.True(Seq.length seq > 0)

let map fn x = fn x

type File =
    { Name : string
      ContentType : string
      Content : string }
    member x.MakeUpload() =
        let bytes = Encoding.UTF8.GetBytes(x.Content)
        new Upload(bytes, x.Name, x.ContentType)
    static member FromDictionary(dict : IReadOnlyDictionary<string, obj>) =
        { Name = downcast dict.["name"]
          ContentType = downcast dict.["contentType"]
          Content = downcast dict.["contentAsText"] }

type FilesRequest =
    { Single : File
      Multiple : File []
      NullableMultiple : File [] option
      NullableMultipleNullable : File option [] option }