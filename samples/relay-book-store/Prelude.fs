namespace FSharp.Data.GraphQL.Samples.RelayBookStore

[<AutoOpen>]
module internal Prelude =

  [<RequireQualifiedAccess>]
  module Base64 =

    open System

    let encode (x : byte array) : string =
      Convert.ToBase64String(x)

    let tryDecode (x : string) : byte array option =
      try
        Convert.FromBase64String(x)
        |> Some
      with
      | :? FormatException ->
        None

  [<RequireQualifiedAccess>]
  module Utf8 =

    open System
    open System.Text

    let tryDecode (xs : byte array) : string option =
      try
        Encoding.UTF8.GetString(xs)
        |> Some
      with
      | :? ArgumentException
      | :? DecoderFallbackException ->
        None

    let encode (x : string) : byte array =
      Encoding.UTF8.GetBytes(x)
