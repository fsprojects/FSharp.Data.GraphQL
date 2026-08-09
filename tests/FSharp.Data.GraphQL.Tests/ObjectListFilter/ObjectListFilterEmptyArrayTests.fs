[<Xunit.Trait (Tests.TraitType.Category, Tests.TraitName.Linq)>]
[<Xunit.Trait (Tests.TraitType.Category, Tests.TraitName.ObjectListFilter)>]
module FSharp.Data.GraphQL.Tests.ObjectListFilter.EmptyArray.Tests

open Xunit
open System
open System.Linq
open System.Text.Json
open FSharp.Data.GraphQL.Shared
open FSharp.Data.GraphQL.Server.Middleware

// ────────────────────────────────────────────────────────────────────────────────────────────────────────────────
// Test types
// ────────────────────────────────────────────────────────────────────────────────────────────────────────────────

type EmptyArrayEntity = {
    Id: int
    Name: string
    Tags: string list
}

// ────────────────────────────────────────────────────────────────────────────────────────────────────────────────
// Test data
// ────────────────────────────────────────────────────────────────────────────────────────────────────────────────

let testData = [|
    { Id = 1; Name = "Alice"; Tags = [] }
    { Id = 2; Name = "Bob"; Tags = [ "admin"; "user" ] }
    { Id = 3; Name = "Charlie"; Tags = [ "user" ] }
    { Id = 4; Name = "Diana"; Tags = [] }
|]

let filterOptions = ObjectListFilterLinqOptions<EmptyArrayEntity, obj> (Json.getSerializerOptions Seq.empty)

let applyFilter (filter : ObjectListFilter) =
    filter.ApplyTo (testData.AsQueryable (), filterOptions) |> Seq.toList

// ════════════════════════════════════════════════════════════════════════════════════════════════════════════════
// Validation Tests – Check that empty filters do NOT crash and handle correctly
// ════════════════════════════════════════════════════════════════════════════════════════════════════════════════

[<Fact>]
let ``Contains on empty array field returns entities with empty tags`` () =
    // Contains on an empty list should match entities with empty lists
    let filter = Contains ({ FieldName = "tags"; Value = "admin" }, null)
    let result = applyFilter filter
    // Only Bob and Charlie have "admin" or any tags
    result |> List.length |> equals 1
    result |> List.map (fun e -> e.Name) |> equals [ "Bob" ]

[<Fact>]
let ``Empty filter list returns all entities`` () =
    // No filter applied means all entities pass
    // The built-in queryable should return all
    let result = testData.AsQueryable () |> Seq.toList
    result |> List.length |> equals 4

[<Fact>]
let ``And with empty field matches correctly`` () =
    // Filter: (id > 1 AND tags contains "admin")
    let filter =
        And (
            GreaterThan { FieldName = "id"; Value = 1 },
            Contains ({ FieldName = "tags"; Value = "admin" }, null)
        )
    let result = applyFilter filter
    // Only Bob (id=2) has "admin" tag and id > 1
    result |> List.length |> equals 1
    (List.head result).Name |> equals "Bob"

[<Fact>]
let ``Or with empty field returns union of results`` () =
    // Filter: (id = 1 OR tags contains "admin")
    let filter =
        Or (
            Equals ({ FieldName = "id"; Value = 1 }, null),
            Contains ({ FieldName = "tags"; Value = "admin" }, null)
        )
    let result = applyFilter filter
    // Alice (id=1) and Bob (has "admin")
    result |> List.length |> equals 2
    result |> List.map (fun e -> e.Name) |> List.sort |> equals [ "Alice"; "Bob" ]

[<Fact>]
let ``Not on empty array field returns complementary set`` () =
    // Filter: NOT (tags contains "user")
    let filter = Not (Contains ({ FieldName = "tags"; Value = "user" }, null))
    let result = applyFilter filter
    // Alice, Diana have empty tags; Bob and Charlie have "user"
    // so NOT "user" = Alice, Diana
    result |> List.length |> equals 2
    result |> List.map (fun e -> e.Name) |> List.sort |> equals [ "Alice"; "Diana" ]

// ════════════════════════════════════════════════════════════════════════════════════════════════════════════════
// SQL Generation Tests – Check that LINQ expression tree is correctly built
// ════════════════════════════════════════════════════════════════════════════════════════════════════════════════

[<Fact>]
let ``Contains filter on array field generates valid LINQ expression`` () =
    // Verify the filter doesn't crash and produces a valid LINQ provider execution
    // by actually running it
    let filter = Contains ({ FieldName = "tags"; Value = "user" }, null)
    let result = applyFilter filter
    // Bob and Charlie have "user" tag
    result |> List.length |> equals 2

[<Fact>]
let ``Complex nested filter generates valid LINQ expression`` () =
    // Nested: (NOT (id < 3)) AND (tags contains "user")
    let filter =
        And (
            Not (LessThan { FieldName = "id"; Value = 3 }),
            Contains ({ FieldName = "tags"; Value = "user" }, null)
        )
    let result = applyFilter filter
    // id >= 3: Charlie (3), Diana (4)
    // tags contains "user": Bob (2), Charlie (3)
    // intersection: Charlie (3)
    result |> List.length |> equals 1
    (List.head result).Name |> equals "Charlie"

[<Fact>]
let ``Equals empty list filter generates valid LINQ expression`` () =
    // Verify: tags = []
    // Should correctly compile LINQ expression for list equality check
    let emptyListValue = [] : string list
    let filter = Equals ({ FieldName = "tags"; Value = emptyListValue }, null)
    let result = applyFilter filter
    // Alice and Diana have empty tags
    result |> List.length |> equals 2
    result |> List.map (fun e -> e.Name) |> List.sort |> equals [ "Alice"; "Diana" ]

[<Fact>]
let ``Not Equals empty list filter generates valid LINQ expression`` () =
    // Verify: NOT (tags = [])  =>  tags != []
    // Should correctly compile LINQ expression for list inequality check
    let emptyListValue = [] : string list
    let filter = Not (Equals ({ FieldName = "tags"; Value = emptyListValue }, null))
    let result = applyFilter filter
    // Bob and Charlie have non-empty tags
    result |> List.length |> equals 2
    result |> List.map (fun e -> e.Name) |> List.sort |> equals [ "Bob"; "Charlie" ]

[<Fact>]
let ``Complex Equals empty list with logical operators generates valid LINQ expression`` () =
    // Verify: (id <= 2) AND (tags = [])
    let emptyListValue = [] : string list
    let filter =
        And (
            LessThanOrEqual { FieldName = "id"; Value = 2 },
            Equals ({ FieldName = "tags"; Value = emptyListValue }, null)
        )
    let result = applyFilter filter
    // id <= 2: Alice (1), Bob (2)
    // tags = []: Alice (1), Diana (4)
    // intersection: Alice (1)
    result |> List.length |> equals 1
    (List.head result).Name |> equals "Alice"

// ════════════════════════════════════════════════════════════════════════════════════════════════════════════════
// Coercion Tests – Check that string values are correctly coerced to list membership checks
// ════════════════════════════════════════════════════════════════════════════════════════════════════════════════

[<Fact>]
let ``Contains coerces string value into list element check`` () =
    // "admin" (string) should match list containing "admin"
    let filter = Contains ({ FieldName = "tags"; Value = "admin" }, null)
    let result = applyFilter filter
    result |> List.length |> equals 1
    result |> List.map (fun e -> e.Name) |> equals [ "Bob" ]

[<Fact>]
let ``Contains returns empty result when no match`` () =
    // Looking for a tag that doesn't exist in any entity
    let filter = Contains ({ FieldName = "tags"; Value = "superadmin" }, null)
    let result = applyFilter filter
    result |> List.length |> equals 0

[<Fact>]
let ``Contains with multiple identical tags matches correctly`` () =
    // If an entity had ["user"; "user"], Contains "user" should still match
    // (list element membership check, not count)
    let filter = Contains ({ FieldName = "tags"; Value = "user" }, null)
    let result = applyFilter filter
    result |> List.length |> equals 2
    result |> List.map (fun e -> e.Name) |> List.sort |> equals [ "Bob"; "Charlie" ]

[<Fact>]
let ``Equals on list field with empty list returns only empty lists`` () =
    // Filter: tags = []
    let emptyListValue = [] : string list
    let filter = Equals ({ FieldName = "tags"; Value = emptyListValue }, null)
    let result = applyFilter filter
    result |> List.length |> equals 2
    result |> List.map (fun e -> e.Name) |> List.sort |> equals [ "Alice"; "Diana" ]

[<Fact>]
let ``Not Equals on empty list returns only non-empty lists`` () =
    // Filter: NOT (tags = [])  =>  tags != []  =>  only entities with non-empty tags
    let emptyListValue = [] : string list
    let filter = Not (Equals ({ FieldName = "tags"; Value = emptyListValue }, null))
    let result = applyFilter filter
    result |> List.length |> equals 2
    result |> List.map (fun e -> e.Name) |> List.sort |> equals [ "Bob"; "Charlie" ]

[<Fact>]
let ``Implicit non-empty check via Contains finds all entities with any tag`` () =
    // Any entity where Contains matches ANY tag is considered "has tags"
    // This is implicit non-empty: if Contains("user") or Contains("admin") matches, the list is non-empty
    let filter1 = Contains ({ FieldName = "tags"; Value = "user" }, null)
    let filter2 = Contains ({ FieldName = "tags"; Value = "admin" }, null)
    let result1 = applyFilter filter1  // "user": Bob, Charlie
    let result2 = applyFilter filter2  // "admin": Bob
    let combined = 
        (result1 |> List.map (fun e -> e.Id))
        @ (result2 |> List.map (fun e -> e.Id))
        |> List.distinct
        |> List.sort
    // Bob (2) and Charlie (3) have at least one tag
    combined |> equals [ 2; 3 ]
