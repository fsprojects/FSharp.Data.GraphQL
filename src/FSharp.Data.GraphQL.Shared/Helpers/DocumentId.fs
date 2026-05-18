module FSharp.Data.GraphQL.DocumentId

open System.Globalization
open System.Runtime.CompilerServices
open System.Security.Cryptography
open System.Text

let private formatByteAsLowerHex (value : byte) = value.ToString ("x2", CultureInfo.InvariantCulture)

/// <summary>
/// Computes a deterministic document identifier from a canonical GraphQL query string.
/// </summary>
/// <param name="canonicalQuery">The canonical GraphQL query string (must already be properly escaped according to GraphQL specification).</param>
/// <returns>A lowercase hexadecimal SHA-256 hash string that uniquely identifies the document content.</returns>
[<CompiledName("FromCanonicalQuery")>]
let fromCanonicalQuery (canonicalQuery : string) =
    let normalizedCanonicalQuery = canonicalQuery.Replace("\r\n", "\n").Replace ("\r", "\n")
    let queryBytes = Encoding.UTF8.GetBytes normalizedCanonicalQuery
    use sha256 = SHA256.Create ()
    let hash = sha256.ComputeHash queryBytes
    hash |> Seq.map formatByteAsLowerHex |> String.concat ""
