module FSharp.Data.GraphQL.Tests.TypeCoercionFilterTests

open Xunit
open FSharp.Data.GraphQL.Shared
open FSharp.Data.GraphQL.Server.Middleware
open FSharp.Data.GraphQL.Server.Middleware.ObjectListFilter
open FSharp.Data.GraphQL.Tests.TypeCoercionCommon

// ──────────────────────────────────────────────────────────────────────────────
// Equals operator
// ──────────────────────────────────────────────────────────────────────────────

[<Fact>]
let ``coerceFilter coerces string to Guid for Equals`` () =
    let filter = Equals ({ FieldName = "guidField"; Value = "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa" }, null)
    let result = applyFilter filter
    result |> List.length |> equals 1
    (List.head result).Name |> equals "Alice"

[<Fact>]
let ``coerceFilter coerces string to CLR enum for Equals`` () =
    let filter = Equals ({ FieldName = "color"; Value = "Green" }, null)
    let result = applyFilter filter
    result |> List.length |> equals 1
    (List.head result).Name |> equals "Bob"

[<Fact>]
let ``coerceFilter coerces int to CLR enum for Equals`` () =
    let filter = Equals ({ FieldName = "color"; Value = 2 }, null) // Blue = 2
    let result = applyFilter filter
    result |> List.length |> equals 1
    (List.head result).Name |> equals "Charlie"

[<Fact>]
let ``coerceFilter coerces string to DU-as-enum for Equals`` () =
    let filter = Equals ({ FieldName = "status"; Value = "Active" }, null)
    let result = applyFilter filter
    result |> List.length |> equals 1
    (List.head result).Name |> equals "Alice"

[<Fact>]
let ``coerceFilter coerces string to single-case DU wrapping string for Equals`` () =
    let filter = Equals ({ FieldName = "wrappedName"; Value = "Bob" }, null)
    let result = applyFilter filter
    result |> List.length |> equals 1
    (List.head result).Name |> equals "Bob"

[<Fact>]
let ``coerceFilter coerces int to single-case DU wrapping int for Equals`` () =
    let filter = Equals ({ FieldName = "wrappedScore"; Value = 30 }, null)
    let result = applyFilter filter
    result |> List.length |> equals 1
    (List.head result).Name |> equals "Charlie"

[<Fact>]
let ``coerceFilter coerces int64 to single-case DU wrapping int64 for Equals`` () =
    let filter = Equals ({ FieldName = "wrappedLong"; Value = 200L }, null)
    let result = applyFilter filter
    result |> List.length |> equals 1
    (List.head result).Name |> equals "Bob"

[<Fact>]
let ``coerceFilter coerces string to single-case DU wrapping Guid for Equals`` () =
    let filter = Equals ({ FieldName = "wrappedGuid"; Value = "cccccccc-cccc-cccc-cccc-cccccccccccc" }, null)
    let result = applyFilter filter
    result |> List.length |> equals 1
    (List.head result).Name |> equals "Charlie"

[<Fact>]
let ``coerceFilter passes through bool for Equals`` () =
    let filter = Equals ({ FieldName = "isActive"; Value = true }, null)
    let result = applyFilter filter
    result |> List.length |> equals 2
    result |> List.forall (fun e -> e.IsActive) |> equals true

// ──────────────────────────────────────────────────────────────────────────────
// comparison operators (GreaterThan / LessThan)
// ──────────────────────────────────────────────────────────────────────────────

[<Fact>]
let ``coerceFilter coerces int to decimal for GreaterThanOrEqual`` () =
    // Score: Alice=100, Bob=200, Charlie=300.  >= 200 -> Bob and Charlie
    let filter = GreaterThanOrEqual { FieldName = "score"; Value = 200 }
    let result = applyFilter filter
    result |> List.length |> equals 2
    result |> List.map (fun e -> e.Name) |> List.sort |> equals [ "Bob"; "Charlie" ]

[<Fact>]
let ``coerceFilter coerces string to DateTime for GreaterThan`` () =
    // CreatedAt: Alice=2024-01-01, Bob=2024-02-01, Charlie=2024-03-01.  > 2024-01-15
    let filter = GreaterThan { FieldName = "createdAt"; Value = "2024-01-15T00:00:00" }
    let result = applyFilter filter
    result |> List.length |> equals 2
    result |> List.map (fun e -> e.Name) |> List.sort |> equals [ "Bob"; "Charlie" ]

[<Fact>]
let ``coerceFilter coerces string to DateOnly for LessThan`` () =
    // BirthDate: Alice=1990-05-15, Bob=1985-08-20, Charlie=2000-12-31.  < 2000-01-01
    let filter = LessThan { FieldName = "birthDate"; Value = "2000-01-01" }
    let result = applyFilter filter
    result |> List.length |> equals 2
    result |> List.map (fun e -> e.Name) |> List.sort |> equals [ "Alice"; "Bob" ]

[<Fact>]
let ``coerceFilter coerces string to TimeOnly for GreaterThan`` () =
    // AlarmTime: Alice=08:00, Bob=09:00, Charlie=10:00.  > 08:30 -> Bob and Charlie
    let filter = GreaterThan { FieldName = "alarmTime"; Value = "08:30:00" }
    let result = applyFilter filter
    result |> List.length |> equals 2
    result |> List.map (fun e -> e.Name) |> List.sort |> equals [ "Bob"; "Charlie" ]

// ──────────────────────────────────────────────────────────────────────────────
// option / voption field unwrapping
// ──────────────────────────────────────────────────────────────────────────────

[<Fact>]
let ``coerceFilter coerces string through option wrapper for Equals`` () =
    let filter = Equals ({ FieldName = "optionName"; Value = "Alice" }, null)
    let result = applyFilter filter
    result |> List.length |> equals 1
    (List.head result).Name |> equals "Alice"

[<Fact>]
let ``coerceFilter coerces int through voption wrapper for Equals`` () =
    let filter = Equals ({ FieldName = "vOptionId"; Value = 3 }, null)
    let result = applyFilter filter
    result |> List.length |> equals 1
    (List.head result).Name |> equals "Charlie"

// ──────────────────────────────────────────────────────────────────────────────
// In operator
// ──────────────────────────────────────────────────────────────────────────────

[<Fact>]
let ``coerceFilter coerces In operator values to CLR enum`` () =
    let filter = In { FieldName = "color"; Value = [ box "Red"; box "Blue" ] }
    let result = applyFilter filter
    result |> List.length |> equals 2
    result |> List.map (fun e -> e.Name) |> List.sort |> equals [ "Alice"; "Charlie" ]

[<Fact>]
let ``coerceFilter coerces In operator values to DU-as-enum`` () =
    let filter = In { FieldName = "status"; Value = [ box "Active"; box "Pending" ] }
    let result = applyFilter filter
    result |> List.length |> equals 2
    result |> List.map (fun e -> e.Name) |> List.sort |> equals [ "Alice"; "Charlie" ]

[<Fact>]
let ``coerceFilter coerces In operator values to Guid`` () =
    let filter =
        In {
            FieldName = "guidField"
            Value = [
                box "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa"
                box "bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb"
            ]
        }
    let result = applyFilter filter
    result |> List.length |> equals 2
    result |> List.map (fun e -> e.Name) |> List.sort |> equals [ "Alice"; "Bob" ]

[<Fact>]
let ``coerceFilter coerces In operator values to single-case DU wrapping int`` () =
    let filter = In { FieldName = "wrappedScore"; Value = [ box 10; box 30 ] }
    let result = applyFilter filter
    result |> List.length |> equals 2
    result |> List.map (fun e -> e.Name) |> List.sort |> equals [ "Alice"; "Charlie" ]

// ──────────────────────────────────────────────────────────────────────────────
// string operators
// ──────────────────────────────────────────────────────────────────────────────

[<Fact>]
let ``coerceFilter handles StartsWith for plain string field`` () =
    let filter = StartsWith ({ FieldName = "name"; Value = "Al" }, null)
    let result = applyFilter filter
    result |> List.length |> equals 1
    (List.head result).Name |> equals "Alice"

[<Fact>]
let ``coerceFilter handles EndsWith for plain string field`` () =
    let filter = EndsWith ({ FieldName = "name"; Value = "ie" }, null)
    let result = applyFilter filter
    result |> List.length |> equals 1
    (List.head result).Name |> equals "Charlie"

[<Fact>]
let ``coerceFilter handles Contains for plain string field`` () =
    let filter = Contains ({ FieldName = "name"; Value = "ob" }, null)
    let result = applyFilter filter
    result |> List.length |> equals 1
    (List.head result).Name |> equals "Bob"

[<Fact>]
let ``coerceFilter handles Contains for list field (element membership)`` () =
    // Tags: Alice=["admin";"user"], Bob=["user"], Charlie=["moderator";"user"]
    let filter = Contains ({ FieldName = "tags"; Value = "admin" }, null)
    let result = applyFilter filter
    result |> List.length |> equals 1
    (List.head result).Name |> equals "Alice"

[<Fact>]
let ``coerceFilter handles Contains for list field matching multiple entities`` () =
    let filter = Contains ({ FieldName = "tags"; Value = "user" }, null)
    let result = applyFilter filter
    result |> List.length |> equals 3

// ──────────────────────────────────────────────────────────────────────────────
// AND / OR / NOT combinators
// ──────────────────────────────────────────────────────────────────────────────

[<Fact>]
let ``coerceFilter coerces values inside AND`` () =
    let filter =
        And (
            Equals ({ FieldName = "color"; Value = "Red" }, null),
            Equals ({ FieldName = "status"; Value = "Active" }, null)
        )
    let result = applyFilter filter
    result |> List.length |> equals 1
    (List.head result).Name |> equals "Alice"

[<Fact>]
let ``coerceFilter coerces values inside OR`` () =
    let filter =
        Or (
            Equals ({ FieldName = "color"; Value = "Red" }, null),
            Equals ({ FieldName = "color"; Value = "Blue" }, null)
        )
    let result = applyFilter filter
    result |> List.length |> equals 2
    result |> List.map (fun e -> e.Name) |> List.sort |> equals [ "Alice"; "Charlie" ]

[<Fact>]
let ``coerceFilter coerces values inside NOT`` () =
    let filter = Not (Equals ({ FieldName = "status"; Value = "Active" }, null))
    let result = applyFilter filter
    result |> List.length |> equals 2
    result |> List.map (fun e -> e.Name) |> List.sort |> equals [ "Bob"; "Charlie" ]
