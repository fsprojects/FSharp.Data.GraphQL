namespace FSharp.Data.GraphQL.Testing.Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open FSharp.Data.GraphQL.Testing

// Each test carries the attribute it checks, so `dotnet test --filter TestCategory=<name>` selects it: the filter proves
// that MSTest reads the categories, the assertion proves which ones
[<TestClass>]
type TestCategoriesTests () =

    let assertCategories (expected : string array) (attribute : TestCategoryBaseAttribute) =
        CollectionAssert.AreEqual (
            expected,
            Seq.toArray attribute.TestCategories,
            $"%s{attribute.GetType().Name} must add exactly the categories %A{expected}"
        )

    [<TestMethod; EditionOct2021>]
    member _.``EditionOct2021 adds the Oct2021 category`` () = assertCategories [| "Oct2021" |] (EditionOct2021Attribute ())

    [<TestMethod; EditionSep2025>]
    member _.``EditionSep2025 adds the Sep2025 category`` () = assertCategories [| "Sep2025" |] (EditionSep2025Attribute ())

    [<TestMethod; AllEditions>]
    member _.``AllEditions adds the category of every edition`` () = assertCategories [| "Oct2021"; "Sep2025" |] (AllEditionsAttribute ())

    [<TestMethod; Lexical>]
    member _.``Lexical adds the Lexical category`` () = assertCategories [| "Lexical" |] (LexicalAttribute ())

    [<TestMethod; Strings>]
    member _.``Strings adds the Strings category`` () = assertCategories [| "Strings" |] (StringsAttribute ())

    [<TestMethod; Values>]
    member _.``Values adds the Values category`` () = assertCategories [| "Values" |] (ValuesAttribute ())

    [<TestMethod; Executable>]
    member _.``Executable adds the Executable category`` () = assertCategories [| "Executable" |] (ExecutableAttribute ())

    [<TestMethod; TypeSystem>]
    member _.``TypeSystem adds the TypeSystem category`` () = assertCategories [| "TypeSystem" |] (TypeSystemAttribute ())

    [<TestMethod; Stress>]
    member _.``Stress adds the Stress category`` () = assertCategories [| "Stress" |] (StressAttribute ())

    [<TestMethod; Printer>]
    member _.``Printer adds the Printer category`` () = assertCategories [| "Printer" |] (PrinterAttribute ())

    [<TestMethod; Corpus>]
    member _.``Corpus adds the Corpus category`` () = assertCategories [| "Corpus" |] (CorpusAttribute ())

    [<TestMethod; Robustness>]
    member _.``Robustness adds the Robustness category`` () = assertCategories [| "Robustness" |] (RobustnessAttribute ())

    [<TestMethod; Slow>]
    member _.``Slow adds the Slow category`` () = assertCategories [| "Slow" |] (SlowAttribute ())

    [<TestMethod>]
    member _.``The category constants are the names filters use`` () =
        CollectionAssert.AreEqual (
            [|
                "Oct2021"
                "Sep2025"
                "Lexical"
                "Strings"
                "Values"
                "Executable"
                "TypeSystem"
                "Stress"
                "Printer"
                "Corpus"
                "Robustness"
                "Slow"
            |],
            [|
                Categories.Oct2021
                Categories.Sep2025
                Categories.Lexical
                Categories.Strings
                Categories.Values
                Categories.Executable
                Categories.TypeSystem
                Categories.Stress
                Categories.Printer
                Categories.Corpus
                Categories.Robustness
                Categories.Slow
            |],
            "The category constants must keep the names test filters are written with"
        )

    [<TestMethod>]
    member _.``The categories of an attribute cannot be changed`` () =
        let attribute = LexicalAttribute ()
        Assert.ThrowsExactly<NotSupportedException>(Action (fun () -> attribute.TestCategories.Add "Other"), "The categories must be read-only")
        |> ignore
        assertCategories [| "Lexical" |] attribute
