module FSharp.Data.GraphQL.Tests.TypeCoercionCommon

open System
open System.Linq
open System.Text.Json
open FSharp.Data.GraphQL.Shared
open FSharp.Data.GraphQL.Server.Middleware

// ──────────────────────────────────────────────────────────────────────────────
// Test types
// ──────────────────────────────────────────────────────────────────────────────

/// CLR enum — coerced from string via JsonStringEnumConverter or from int via default STJ.
type Color =
    | Red = 0
    | Green = 1
    | Blue = 2

/// Multi-case fieldless DU (DU-as-enum) — coerced from string via
/// FSharp.SystemTextJson UnwrapFieldlessTags.
type Status =
    | Active
    | Inactive
    | Pending

/// Single-case DU wrapping a string — coerced via FSharp.SystemTextJson UnwrapSingleCaseUnions.
type WrappedString = WrappedString of string

/// Single-case DU wrapping an int.
type WrappedInt = WrappedInt of int

/// Single-case DU wrapping an int64.
type WrappedInt64 = WrappedInt64 of int64

/// Single-case DU wrapping a Guid.
type WrappedGuid = WrappedGuid of Guid

/// Entity used in coerceFilter integration tests.
type CoercionEntity = {
    Id: int
    Name: string
    Status: Status
    Color: Color
    GuidField: Guid
    WrappedName: WrappedString
    WrappedScore: WrappedInt
    WrappedLong: WrappedInt64
    WrappedGuid: WrappedGuid
    CreatedAt: DateTime
    BirthDate: DateOnly
    AlarmTime: TimeOnly
    Score: decimal
    IsActive: bool
    Tags: string list
    OptionName: string option
    VOptionId: int voption
}

// ──────────────────────────────────────────────────────────────────────────────
// Shared test infrastructure
// ──────────────────────────────────────────────────────────────────────────────

/// Full serializer options including FSharp.SystemTextJson (DU coercion) and
/// JsonStringEnumConverter (CLR enum coercion).
let jsonOptions = ValueSome (Json.getSerializerOptions Seq.empty)

/// No options — uses STJ defaults; sufficient for primitives, Guid, date/time.
let noOptions : JsonSerializerOptions voption = ValueNone

let filterOptions =
    ObjectListFilterLinqOptions<CoercionEntity, obj> (Json.getSerializerOptions Seq.empty)

let testData =
    [|
        {
            Id = 1
            Name = "Alice"
            Status = Active
            Color = Color.Red
            GuidField = Guid.Parse "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa"
            WrappedName = WrappedString "Alice"
            WrappedScore = WrappedInt 10
            WrappedLong = WrappedInt64 100L
            WrappedGuid = WrappedGuid (Guid.Parse "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa")
            CreatedAt = DateTime (2024, 1, 1)
            BirthDate = DateOnly (1990, 5, 15)
            AlarmTime = TimeOnly (8, 0)
            Score = 100m
            IsActive = true
            Tags = [ "admin"; "user" ]
            OptionName = Some "Alice"
            VOptionId = ValueSome 1
        }
        {
            Id = 2
            Name = "Bob"
            Status = Inactive
            Color = Color.Green
            GuidField = Guid.Parse "bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb"
            WrappedName = WrappedString "Bob"
            WrappedScore = WrappedInt 20
            WrappedLong = WrappedInt64 200L
            WrappedGuid = WrappedGuid (Guid.Parse "bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb")
            CreatedAt = DateTime (2024, 2, 1)
            BirthDate = DateOnly (1985, 8, 20)
            AlarmTime = TimeOnly (9, 0)
            Score = 200m
            IsActive = false
            Tags = [ "user" ]
            OptionName = None
            VOptionId = ValueNone
        }
        {
            Id = 3
            Name = "Charlie"
            Status = Pending
            Color = Color.Blue
            GuidField = Guid.Parse "cccccccc-cccc-cccc-cccc-cccccccccccc"
            WrappedName = WrappedString "Charlie"
            WrappedScore = WrappedInt 30
            WrappedLong = WrappedInt64 300L
            WrappedGuid = WrappedGuid (Guid.Parse "cccccccc-cccc-cccc-cccc-cccccccccccc")
            CreatedAt = DateTime (2024, 3, 1)
            BirthDate = DateOnly (2000, 12, 31)
            AlarmTime = TimeOnly (10, 0)
            Score = 300m
            IsActive = true
            Tags = [ "moderator"; "user" ]
            OptionName = Some "Charlie"
            VOptionId = ValueSome 3
        }
    |]

let applyFilter (filter : ObjectListFilter) =
    filter.ApplyTo (testData.AsQueryable (), filterOptions) |> Seq.toList
