module FSharp.Data.GraphQL.DocumentId

open System.Globalization
open System.Security.Cryptography
open System.Text

let private formatByteAsLowerHex (value : byte) =
    value.ToString("x2", CultureInfo.InvariantCulture)

let fromCanonicalQuery (canonicalQuery : string) =
    let queryBytes = Encoding.UTF8.GetBytes canonicalQuery
    use sha256 = SHA256.Create()
    let hash = sha256.ComputeHash queryBytes
    hash
    |> Seq.map formatByteAsLowerHex
    |> String.concat ""
