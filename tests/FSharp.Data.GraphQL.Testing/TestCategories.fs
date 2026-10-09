// The category attributes of the test projects: one MSTest TestCategoryBaseAttribute descendant per category, so a test
// reads [<TestClass; EditionSep2025; Lexical>] instead of repeating [<TestCategory (Categories.Lexical)>] strings.
//
// Naming: an edition attribute starts with "Edition" (EditionOct2021, EditionSep2025, plus AllEditions for tests every
// edition shares) so that completion lists the editions together and the month-year name never stands alone; an area
// attribute is the bare area name (Lexical, Strings, Values, ...). The "Attribute" suffix keeps those types out of the way of
// ordinary identifiers: F# finds StringsAttribute for [<Strings>] only in attribute position.
namespace FSharp.Data.GraphQL.Testing

open System.Collections.Generic
open System.Collections.ObjectModel
open Microsoft.VisualStudio.TestTools.UnitTesting

/// <summary>
/// A <see cref="TestCategoryBaseAttribute"/> whose categories the derived attribute fixes, so that applying the derived
/// attribute is all a test needs; MSTest filters such attributes with <c>--filter TestCategory=...</c> exactly like
/// <see cref="TestCategoryAttribute"/>.
/// </summary>
[<AbstractClass>]
type PresetTestCategoryAttribute
    /// <param name="categories">The categories the derived attribute adds to every test it is applied to.</param>
    (categories : string array) =
    inherit TestCategoryBaseAttribute ()

    // MSTest receives a read-only view, so the categories of an attribute cannot be changed through it
    let testCategories = ReadOnlyCollection (Array.copy categories)

    /// <inheritdoc />
    override _.TestCategories = testCategories :> IList<string>

/// <summary>Marks a test as covering the October 2021 edition of the GraphQL specification: adds the <c>Oct2021</c> category.</summary>
[<Sealed>]
type EditionOct2021Attribute () =
    inherit PresetTestCategoryAttribute ([| Categories.Oct2021 |])

/// <summary>Marks a test as covering the September 2025 edition of the GraphQL specification: adds the <c>Sep2025</c> category.</summary>
[<Sealed>]
type EditionSep2025Attribute () =
    inherit PresetTestCategoryAttribute ([| Categories.Sep2025 |])

/// <summary>
/// Marks a test as covering behavior every edition of the GraphQL specification shares: adds the category of each edition,
/// <c>Oct2021</c> and <c>Sep2025</c>, so that a filter on any one edition selects the test.
/// </summary>
[<Sealed>]
type AllEditionsAttribute () =
    inherit PresetTestCategoryAttribute ([| Categories.Oct2021; Categories.Sep2025 |])

/// <summary>Marks a test of lexical analysis: adds the <c>Lexical</c> category.</summary>
[<Sealed>]
type LexicalAttribute () =
    inherit PresetTestCategoryAttribute ([| Categories.Lexical |])

/// <summary>Marks a test of quoted and block string values: adds the <c>Strings</c> category.</summary>
[<Sealed>]
type StringsAttribute () =
    inherit PresetTestCategoryAttribute ([| Categories.Strings |])

/// <summary>Marks a test of input values: adds the <c>Values</c> category.</summary>
[<Sealed>]
type ValuesAttribute () =
    inherit PresetTestCategoryAttribute ([| Categories.Values |])

/// <summary>Marks a test of executable definitions: adds the <c>Executable</c> category.</summary>
[<Sealed>]
type ExecutableAttribute () =
    inherit PresetTestCategoryAttribute ([| Categories.Executable |])

/// <summary>Marks a test of type system definitions and extensions: adds the <c>TypeSystem</c> category.</summary>
[<Sealed>]
type TypeSystemAttribute () =
    inherit PresetTestCategoryAttribute ([| Categories.TypeSystem |])

/// <summary>Marks a test with very large, very long or deeply nested input: adds the <c>Stress</c> category.</summary>
[<Sealed>]
type StressAttribute () =
    inherit PresetTestCategoryAttribute ([| Categories.Stress |])

/// <summary>Marks a test of printing a document back to GraphQL source: adds the <c>Printer</c> category.</summary>
[<Sealed>]
type PrinterAttribute () =
    inherit PresetTestCategoryAttribute ([| Categories.Printer |])

/// <summary>Marks a test over a corpus of real-world or third-party documents: adds the <c>Corpus</c> category.</summary>
[<Sealed>]
type CorpusAttribute () =
    inherit PresetTestCategoryAttribute ([| Categories.Corpus |])

/// <summary>Marks a test with malformed, truncated or hostile input: adds the <c>Robustness</c> category.</summary>
[<Sealed>]
type RobustnessAttribute () =
    inherit PresetTestCategoryAttribute ([| Categories.Robustness |])

/// <summary>Marks a test that takes noticeably longer than the rest: adds the <c>Slow</c> category.</summary>
[<Sealed>]
type SlowAttribute () =
    inherit PresetTestCategoryAttribute ([| Categories.Slow |])
