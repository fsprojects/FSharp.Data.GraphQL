module FSharp.Data.GraphQL.DocumentId

open System.Globalization
open System.Runtime.CompilerServices
open System.Security.Cryptography
open System.Text

let private formatByteAsLowerHex (value : byte) = value.ToString ("x2", CultureInfo.InvariantCulture)

let internal fromCanonicalQueryUnsafe (canonicalQuery : string) =
    let queryBytes = Encoding.UTF8.GetBytes canonicalQuery
    use sha256 = SHA256.Create ()
    let hash = sha256.ComputeHash queryBytes
    hash |> Seq.map formatByteAsLowerHex |> String.concat ""


/// <summary>
/// Computes a deterministic document identifier from a canonical GraphQL query string.
/// </summary>
/// <param name="canonicalQuery">The canonical GraphQL query string (must already be properly escaped according to GraphQL specification).</param>
/// <returns>A lowercase hexadecimal SHA-256 hash string that uniquely identifies the document content.</returns>
[<CompiledName("FromCanonicalQuery")>]
let fromCanonicalQuery (canonicalQuery : string) =
    let normalizedCanonicalQuery =
        let crIndex = canonicalQuery.IndexOf '\r'
        if crIndex < 0 then
            canonicalQuery
        else
            let sb = StringBuilder (canonicalQuery.Length)
            sb.Append (canonicalQuery, 0, crIndex) |> ignore
            let mutable i = crIndex
            while i < canonicalQuery.Length do
                let c = canonicalQuery[i]
                if c = '\r' then
                    sb.Append '\n' |> ignore
                    if i + 1 < canonicalQuery.Length && canonicalQuery[i + 1] = '\n' then
                        i <- i + 2
                    else
                        i <- i + 1
                else
                    sb.Append c |> ignore
                    i <- i + 1
            sb.ToString ()
    fromCanonicalQueryUnsafe normalizedCanonicalQuery
