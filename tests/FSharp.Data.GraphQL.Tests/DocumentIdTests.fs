// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.DocumentIdTests

open Xunit
open FSharp.Data.GraphQL

[<Fact>]
let ``DocumentId.fromCanonicalQuery produces deterministic hash`` () =
    let query = "query Example { a b }"
    let hash1 = DocumentId.fromCanonicalQuery query
    let hash2 = DocumentId.fromCanonicalQuery query
    equals hash1 hash2
    equals 64 hash1.Length  // SHA-256 hex string is 64 chars

[<Fact>]
let ``DocumentId.fromCanonicalQuery produces different hashes for different queries`` () =
    let query1 = "query Example1 { a }"
    let query2 = "query Example2 { b }"
    let hash1 = DocumentId.fromCanonicalQuery query1
    let hash2 = DocumentId.fromCanonicalQuery query2
    notEquals hash1 hash2

[<Fact>]
let ``DocumentId.fromCanonicalQuery handles empty string`` () =
    let query = ""
    let hash = DocumentId.fromCanonicalQuery query
    equals 64 hash.Length
    // SHA-256 of empty string
    equals "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" hash

[<Fact>]
let ``DocumentId.fromCanonicalQuery produces lowercase hex`` () =
    let query = "query Test { field }"
    let hash = DocumentId.fromCanonicalQuery query
    equals hash (hash.ToLowerInvariant())
    Assert.True(hash |> Seq.forall (fun c -> (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')))

[<Fact>]
let ``DocumentId.fromCanonicalQuery handles special characters in strings`` () =
    // Test with escaped characters that should be in the canonical form
    let query1 = """query Test { field(arg: "test\"quote") }"""
    let query2 = """query Test { field(arg: "test\nline") }"""
    let query3 = """query Test { field(arg: "test\ttab") }"""
    let hash1 = DocumentId.fromCanonicalQuery query1
    let hash2 = DocumentId.fromCanonicalQuery query2
    let hash3 = DocumentId.fromCanonicalQuery query3
    // All should produce valid hashes
    equals 64 hash1.Length
    equals 64 hash2.Length
    equals 64 hash3.Length
    // All should be different
    notEquals hash1 hash2
    notEquals hash2 hash3
    notEquals hash1 hash3

[<Fact>]
let ``DocumentId.fromCanonicalQuery is consistent with known SHA-256 values`` () =
    // Test a simple known case
    let query = "test"
    let hash = DocumentId.fromCanonicalQuery query
    // SHA-256 of "test"
    equals "9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08" hash

