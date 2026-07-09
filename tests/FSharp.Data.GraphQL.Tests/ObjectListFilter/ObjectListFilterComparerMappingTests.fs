[<Xunit.Trait (Tests.TraitType.Category, Tests.TraitName.Linq)>]
[<Xunit.Trait (Tests.TraitType.Category, Tests.TraitName.ObjectListFilter)>]
module FSharp.Data.GraphQL.Tests.ObjectListFilter.ComparerMapping.Tests

open System
open System.Collections
open System.Globalization
open Xunit
open FSharp.Data.GraphQL.Server.Middleware

// ─────────────────────────────────────────────────────────────────────────────
// Singleton reference-equality branch
// ─────────────────────────────────────────────────────────────────────────────

[<Fact>]
let ``comparerToStringComparison maps well-known StringComparer instances`` () =
    let currentCultureIsInvariant =
        CultureInfo.CurrentCulture.CompareInfo.Equals CultureInfo.InvariantCulture.CompareInfo

    let testCases =
        [
            yield (StringComparer.OrdinalIgnoreCase :> IComparer, StringComparison.OrdinalIgnoreCase)
            yield (StringComparer.InvariantCultureIgnoreCase :> IComparer, StringComparison.InvariantCultureIgnoreCase)
            yield (StringComparer.Ordinal :> IComparer, StringComparison.Ordinal)
            yield (StringComparer.InvariantCulture :> IComparer, StringComparison.InvariantCulture)
            // On environments where CurrentCulture == InvariantCulture (e.g. Ubuntu CI with no locale),
            // StringComparer.Current* singletons ARE the same objects as StringComparer.Invariant*,
            // so they can only map to Invariant* values. Skip those cases in such environments.
            if not currentCultureIsInvariant then
                yield (StringComparer.CurrentCultureIgnoreCase :> IComparer, StringComparison.CurrentCultureIgnoreCase)
                yield (StringComparer.CurrentCulture :> IComparer, StringComparison.CurrentCulture)
        ]

    for comparer, expected in testCases do
        let actual = ObjectListFilter.comparerToStringComparison comparer |> wantValueSome
        actual |> equals expected

// ─────────────────────────────────────────────────────────────────────────────
// Each singleton must map to a distinct StringComparison value
// ─────────────────────────────────────────────────────────────────────────────

[<Fact>]
let ``comparerToStringComparison singleton mappings are all distinct`` () =
    let currentCultureIsInvariant =
        CultureInfo.CurrentCulture.CompareInfo.Equals CultureInfo.InvariantCulture.CompareInfo

    // On environments where CurrentCulture == InvariantCulture, Current* singletons are
    // the same objects as Invariant* ones, so distinctness can only be checked for the
    // remaining four singletons.
    let singletons : IComparer list =
        [
            yield StringComparer.OrdinalIgnoreCase
            yield StringComparer.InvariantCultureIgnoreCase
            yield StringComparer.Ordinal
            yield StringComparer.InvariantCulture
            if not currentCultureIsInvariant then
                yield StringComparer.CurrentCultureIgnoreCase
                yield StringComparer.CurrentCulture
        ]

    let results =
        singletons
        |> List.map (fun c -> ObjectListFilter.comparerToStringComparison c |> wantValueSome)

    let distinct = results |> List.distinct
    List.length distinct |> equals (List.length results)

// ─────────────────────────────────────────────────────────────────────────────
// IsWellKnownCultureAwareComparer fallback path
// StringComparer.Create produces a non-singleton comparer; the singleton
// ReferenceEquals fast path is skipped and IsWellKnownCultureAwareComparer
// is used instead.
// ─────────────────────────────────────────────────────────────────────────────

[<Fact>]
let ``comparerToStringComparison maps non-singleton InvariantCulture comparer`` () =
    let comparer = StringComparer.Create (CultureInfo.InvariantCulture, false) :> IComparer
    // must NOT be the same object as the singleton
    Assert.False (obj.ReferenceEquals (comparer, StringComparer.InvariantCulture :> obj))
    let result = ObjectListFilter.comparerToStringComparison comparer |> wantValueSome
    result |> equals StringComparison.InvariantCulture

[<Fact>]
let ``comparerToStringComparison maps non-singleton InvariantCultureIgnoreCase comparer`` () =
    let comparer = StringComparer.Create (CultureInfo.InvariantCulture, true) :> IComparer
    Assert.False (obj.ReferenceEquals (comparer, StringComparer.InvariantCultureIgnoreCase :> obj))
    let result = ObjectListFilter.comparerToStringComparison comparer |> wantValueSome
    result |> equals StringComparison.InvariantCultureIgnoreCase

[<Fact>]
let ``comparerToStringComparison maps non-singleton CurrentCulture comparer`` () =
    // When CurrentCulture == InvariantCulture (e.g. Ubuntu CI), a non-singleton comparer
    // created from CurrentCulture is indistinguishable from InvariantCulture and will
    // legitimately map to InvariantCulture.
    let comparer = StringComparer.Create (CultureInfo.CurrentCulture, false) :> IComparer
    Assert.False (obj.ReferenceEquals (comparer, StringComparer.CurrentCulture :> obj))
    let result = ObjectListFilter.comparerToStringComparison comparer |> wantValueSome
    let currentCultureIsInvariant =
        CultureInfo.CurrentCulture.CompareInfo.Equals CultureInfo.InvariantCulture.CompareInfo
    let expected =
        if currentCultureIsInvariant then StringComparison.InvariantCulture
        else StringComparison.CurrentCulture
    result |> equals expected

[<Fact>]
let ``comparerToStringComparison maps non-singleton CurrentCultureIgnoreCase comparer`` () =
    let comparer = StringComparer.Create (CultureInfo.CurrentCulture, true) :> IComparer
    Assert.False (obj.ReferenceEquals (comparer, StringComparer.CurrentCultureIgnoreCase :> obj))
    let result = ObjectListFilter.comparerToStringComparison comparer |> wantValueSome
    let currentCultureIsInvariant =
        CultureInfo.CurrentCulture.CompareInfo.Equals CultureInfo.InvariantCulture.CompareInfo
    let expected =
        if currentCultureIsInvariant then StringComparison.InvariantCultureIgnoreCase
        else StringComparison.CurrentCultureIgnoreCase
    result |> equals expected

// ─────────────────────────────────────────────────────────────────────────────
// Unknown / unsupported cases → ValueNone
// ─────────────────────────────────────────────────────────────────────────────

[<Fact>]
let ``comparerToStringComparison returns ValueNone for null`` () =
    ObjectListFilter.comparerToStringComparison null |> wantValueNone

[<Fact>]
let ``comparerToStringComparison returns ValueNone for non-StringComparer IComparer`` () =
    let customComparer =
        { new IComparer with
            member _.Compare (_, _) = 0
        }
    ObjectListFilter.comparerToStringComparison customComparer |> wantValueNone

[<Fact>]
let ``comparerToStringComparison returns ValueNone for non-standard culture comparer`` () =
    // A comparer for a specific non-current, non-invariant culture — the
    // IsWellKnownCultureAwareComparer fallback cannot map it to any of the six
    // StringComparison values, so it must return ValueNone.
    let trCulture = CultureInfo.GetCultureInfo "tr-TR"
    // Only run this test when the test host is not Turkish (otherwise CurrentCulture == tr-TR
    // and the result would legitimately be CurrentCulture).
    if not (CultureInfo.CurrentCulture.Name.StartsWith "tr") then
        let comparer = StringComparer.Create (trCulture, false) :> IComparer
        ObjectListFilter.comparerToStringComparison comparer |> wantValueNone

// ─────────────────────────────────────────────────────────────────────────────
// Determinism: calling comparerToStringComparison twice on the same instance
// must return the same result
// ─────────────────────────────────────────────────────────────────────────────

[<Fact>]
let ``comparerToStringComparison is deterministic for singletons`` () =
    let currentCultureIsInvariant =
        CultureInfo.CurrentCulture.CompareInfo.Equals CultureInfo.InvariantCulture.CompareInfo

    let singletons : IComparer list =
        [
            yield StringComparer.OrdinalIgnoreCase
            yield StringComparer.InvariantCultureIgnoreCase
            yield StringComparer.Ordinal
            yield StringComparer.InvariantCulture
            if not currentCultureIsInvariant then
                yield StringComparer.CurrentCultureIgnoreCase
                yield StringComparer.CurrentCulture
        ]

    for comparer in singletons do
        let first  = ObjectListFilter.comparerToStringComparison comparer
        let second = ObjectListFilter.comparerToStringComparison comparer
        first |> equals second
