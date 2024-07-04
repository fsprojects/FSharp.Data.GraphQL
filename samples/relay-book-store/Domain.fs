namespace FSharp.Data.GraphQL.Samples.RelayBookStore

type Book =
  {
    ID : string
    Title : string
    Year : int
  }

type BookCursor =
  {
    ID : string
    Title : string
  }

[<RequireQualifiedAccess>]
module BookCursor =

  open FsToolkit.ErrorHandling
  open Thoth.Json.Net

  let ofBook (x : Book) : BookCursor =
    {
      ID = x.ID
      Title = x.Title
    }

  let private encoder =
    fun x ->
      Encode.object
        [
          "i", Encode.string x.ID
          "t", Encode.string x.Title
        ]

  let private decoder =
    Decode.object
      (fun get ->
        {
          ID = get.Required.Field "i" Decode.string
          Title = get.Required.Field "t" Decode.string
        })

  let tryDecode (x : string) : BookCursor option =
    option {
      let! bytes = Base64.tryDecode x
      let! json = Utf8.tryDecode bytes

      return!
        Decode.fromString decoder json
        |> Result.toOption
    }

  let encode (x : BookCursor) : string =
    x
    |> encoder
    |> Encode.toString 0
    |> Utf8.encode
    |> Base64.encode
