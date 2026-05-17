module FSharp.Data.GraphQL.DocumentId

open System.Globalization
open System.Security.Cryptography
open System.Text
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Ast.Extensions

let private formatByteAsLowerHex (value : byte) =
    value.ToString("x2", CultureInfo.InvariantCulture)

let fromDocument (document : Document) =
    let canonicalQuery = document.ToQueryString()
    let queryBytes = Encoding.UTF8.GetBytes canonicalQuery
    use sha256 = SHA256.Create()
    let hash = sha256.ComputeHash queryBytes
    hash
    |> Array.map formatByteAsLowerHex
    |> String.concat ""
