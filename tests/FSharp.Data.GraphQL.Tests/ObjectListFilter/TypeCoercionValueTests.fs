module FSharp.Data.GraphQL.Tests.TypeCoercionValueTests

open Xunit
open System
open FSharp.Data.GraphQL.Server.Middleware
open FSharp.Data.GraphQL.Tests.TypeCoercionCommon

// ──────────────────────────────────────────────────────────────────────────────
// pass-through (value already has the target type)
// ──────────────────────────────────────────────────────────────────────────────

[<Fact>]
let ``tryCoerceValue passes through string`` () =
    TypeCoercion.tryCoerceValue noOptions typeof<string> (box "hello")
    |> wantValueSome |> equals (box "hello")

[<Fact>]
let ``tryCoerceValue passes through int`` () =
    TypeCoercion.tryCoerceValue noOptions typeof<int> (box 42)
    |> wantValueSome |> equals (box 42)

[<Fact>]
let ``tryCoerceValue passes through bool`` () =
    TypeCoercion.tryCoerceValue noOptions typeof<bool> (box true)
    |> wantValueSome |> equals (box true)

[<Fact>]
let ``tryCoerceValue passes through Guid`` () =
    let g = Guid.NewGuid ()
    TypeCoercion.tryCoerceValue noOptions typeof<Guid> (box g)
    |> wantValueSome |> equals (box g)

[<Fact>]
let ``tryCoerceValue passes through decimal`` () =
    TypeCoercion.tryCoerceValue noOptions typeof<decimal> (box 9.99m)
    |> wantValueSome |> equals (box 9.99m)

[<Fact>]
let ``tryCoerceValue passes through DateTime`` () =
    let dt = DateTime (2024, 6, 1)
    TypeCoercion.tryCoerceValue noOptions typeof<DateTime> (box dt)
    |> wantValueSome |> equals (box dt)

// ──────────────────────────────────────────────────────────────────────────────
// null → ValueNone
// ──────────────────────────────────────────────────────────────────────────────

[<Fact>]
let ``tryCoerceValue returns ValueNone for null`` () =
    TypeCoercion.tryCoerceValue noOptions typeof<string> null
    |> wantValueNone

// ──────────────────────────────────────────────────────────────────────────────
// numeric widening / narrowing
// ──────────────────────────────────────────────────────────────────────────────

[<Fact>]
let ``tryCoerceValue coerces int to int64`` () =
    TypeCoercion.tryCoerceValue noOptions typeof<int64> (box 42)
    |> wantValueSome |> equals (box 42L)

[<Fact>]
let ``tryCoerceValue returns ValueNone for string-to-int (STJ rejects quoted number)`` () =
    // STJ does not coerce quoted strings to numbers without JsonNumberHandling.AllowReadingFromString
    TypeCoercion.tryCoerceValue noOptions typeof<int> (box "99")
    |> wantValueNone

[<Fact>]
let ``tryCoerceValue returns ValueNone for string-to-int64 (STJ rejects quoted number)`` () =
    TypeCoercion.tryCoerceValue noOptions typeof<int64> (box "123456789")
    |> wantValueNone

[<Fact>]
let ``tryCoerceValue coerces double to decimal`` () =
    TypeCoercion.tryCoerceValue noOptions typeof<decimal> (box 3.14)
    |> wantValueSome |> equals (box 3.14m)

[<Fact>]
let ``tryCoerceValue coerces int to decimal`` () =
    TypeCoercion.tryCoerceValue noOptions typeof<decimal> (box 200)
    |> wantValueSome |> equals (box 200m)

[<Fact>]
let ``tryCoerceValue passes through bool (already correct type)`` () =
    TypeCoercion.tryCoerceValue noOptions typeof<bool> (box true)
    |> wantValueSome |> equals (box true)

// ──────────────────────────────────────────────────────────────────────────────
// string → Guid
// ──────────────────────────────────────────────────────────────────────────────

[<Fact>]
let ``tryCoerceValue coerces string to Guid`` () =
    let g = Guid.Parse "550e8400-e29b-41d4-a716-446655440000"
    TypeCoercion.tryCoerceValue noOptions typeof<Guid> (box "550e8400-e29b-41d4-a716-446655440000")
    |> wantValueSome |> equals (box g)

[<Fact>]
let ``tryCoerceValue returns ValueNone for invalid Guid string`` () =
    TypeCoercion.tryCoerceValue noOptions typeof<Guid> (box "not-a-guid")
    |> wantValueNone

// ──────────────────────────────────────────────────────────────────────────────
// string → date/time types (native STJ support)
// ──────────────────────────────────────────────────────────────────────────────

[<Fact>]
let ``tryCoerceValue coerces ISO string to DateTime`` () =
    TypeCoercion.tryCoerceValue noOptions typeof<DateTime> (box "2024-06-01T00:00:00")
    |> wantValueSome |> equals (box (DateTime (2024, 6, 1, 0, 0, 0)))

[<Fact>]
let ``tryCoerceValue coerces ISO string to DateTimeOffset`` () =
    TypeCoercion.tryCoerceValue noOptions typeof<DateTimeOffset> (box "2024-06-01T12:00:00+00:00")
    |> wantValueSome |> ignore

[<Fact>]
let ``tryCoerceValue coerces ISO string to DateOnly`` () =
    TypeCoercion.tryCoerceValue noOptions typeof<DateOnly> (box "2024-06-01")
    |> wantValueSome |> equals (box (DateOnly (2024, 6, 1)))

[<Fact>]
let ``tryCoerceValue coerces ISO string to TimeOnly`` () =
    TypeCoercion.tryCoerceValue noOptions typeof<TimeOnly> (box "14:30:00")
    |> wantValueSome |> equals (box (TimeOnly (14, 30, 0)))

// ──────────────────────────────────────────────────────────────────────────────
// CLR enum
// ──────────────────────────────────────────────────────────────────────────────

[<Fact>]
let ``tryCoerceValue coerces int to CLR enum without jsonOptions`` () =
    TypeCoercion.tryCoerceValue noOptions typeof<Color> (box 2)
    |> wantValueSome |> equals (box Color.Blue)

[<Fact>]
let ``tryCoerceValue coerces string to CLR enum with jsonOptions (JsonStringEnumConverter)`` () =
    TypeCoercion.tryCoerceValue jsonOptions typeof<Color> (box "Green")
    |> wantValueSome |> equals (box Color.Green)

[<Fact>]
let ``tryCoerceValue returns ValueNone for string CLR enum without jsonOptions`` () =
    // Without JsonStringEnumConverter, STJ rejects string enum tokens by default
    TypeCoercion.tryCoerceValue noOptions typeof<Color> (box "Red")
    |> wantValueNone

// ──────────────────────────────────────────────────────────────────────────────
// single-case DU (requires FSharp.SystemTextJson UnwrapSingleCaseUnions)
// ──────────────────────────────────────────────────────────────────────────────

[<Fact>]
let ``tryCoerceValue coerces string to single-case DU wrapping string`` () =
    TypeCoercion.tryCoerceValue jsonOptions typeof<WrappedString> (box "hello")
    |> wantValueSome |> equals (box (WrappedString "hello"))

[<Fact>]
let ``tryCoerceValue coerces int to single-case DU wrapping int`` () =
    TypeCoercion.tryCoerceValue jsonOptions typeof<WrappedInt> (box 42)
    |> wantValueSome |> equals (box (WrappedInt 42))

[<Fact>]
let ``tryCoerceValue coerces int64 to single-case DU wrapping int64`` () =
    TypeCoercion.tryCoerceValue jsonOptions typeof<WrappedInt64> (box 999L)
    |> wantValueSome |> equals (box (WrappedInt64 999L))

[<Fact>]
let ``tryCoerceValue coerces string to single-case DU wrapping Guid`` () =
    let g = Guid.Parse "550e8400-e29b-41d4-a716-446655440000"
    TypeCoercion.tryCoerceValue jsonOptions typeof<WrappedGuid> (box "550e8400-e29b-41d4-a716-446655440000")
    |> wantValueSome |> equals (box (WrappedGuid g))

[<Fact>]
let ``tryCoerceValue returns ValueNone for single-case DU without jsonOptions`` () =
    // Without FSharp.SystemTextJson, STJ doesn't know how to deserialize DUs
    TypeCoercion.tryCoerceValue noOptions typeof<WrappedString> (box "hello")
    |> wantValueNone

// ──────────────────────────────────────────────────────────────────────────────
// multi-case fieldless DU / DU-as-enum (requires FSharp.SystemTextJson UnwrapFieldlessTags)
// ──────────────────────────────────────────────────────────────────────────────

[<Fact>]
let ``tryCoerceValue coerces string to multi-case fieldless DU - Active`` () =
    TypeCoercion.tryCoerceValue jsonOptions typeof<Status> (box "Active")
    |> wantValueSome |> equals (box Active)

[<Fact>]
let ``tryCoerceValue coerces string to multi-case fieldless DU - Inactive`` () =
    TypeCoercion.tryCoerceValue jsonOptions typeof<Status> (box "Inactive")
    |> wantValueSome |> equals (box Inactive)

[<Fact>]
let ``tryCoerceValue coerces string to multi-case fieldless DU - Pending`` () =
    TypeCoercion.tryCoerceValue jsonOptions typeof<Status> (box "Pending")
    |> wantValueSome |> equals (box Pending)

[<Fact>]
let ``tryCoerceValue returns ValueNone for unknown multi-case DU case name`` () =
    TypeCoercion.tryCoerceValue jsonOptions typeof<Status> (box "Unknown")
    |> wantValueNone

// ──────────────────────────────────────────────────────────────────────────────
// unsupported conversion
// ──────────────────────────────────────────────────────────────────────────────

[<Fact>]
let ``tryCoerceValue returns ValueNone when source type has no JSON representation`` () =
    TypeCoercion.tryCoerceValue noOptions typeof<int> (box (obj ()))
    |> wantValueNone
