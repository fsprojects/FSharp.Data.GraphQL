// Samples of tests MSTest runs, skips or rejects, which the TestDiscoveryGuard tests check the guard against. Nothing runs
// them: this assembly is not a test application.
namespace FSharp.Data.GraphQL.Testing.Tests.DiscoverySamples

open System.Threading.Tasks
open Microsoft.VisualStudio.TestTools.UnitTesting

// Tests MSTest runs: the guard must report none of them

/// Test methods of every shape MSTest runs.
[<TestClass>]
type ValidTests () =

    /// Returns unit.
    [<TestMethod>]
    member _.``returns unit`` () = ()

    /// Returns Task.
    [<TestMethod>]
    member _.``returns Task`` () : Task = Task.CompletedTask

    /// Returns ValueTask.
    [<TestMethod>]
    member _.``returns ValueTask`` () : ValueTask = ValueTask.CompletedTask

    /// Takes data rows.
    [<TestMethod; DataRow 1; DataRow 2>]
    member _.``takes data rows`` (value : int) = ignore value

    /// A plain member: no test, whatever it returns.
    member _.``not a test`` () = 42

/// An abstract base without the TestClass attribute whose test runs through the derived test class.
[<AbstractClass>]
type InheritedTestsBase () =

    /// Runs as a test of InheritedTests.
    [<TestMethod>]
    member _.``inherited test`` () = ()

/// The test class the inherited test runs in.
[<TestClass>]
type InheritedTests () =
    inherit InheritedTestsBase ()

/// A generic abstract base whose test runs through the test class closing it.
[<AbstractClass>]
type GenericTestsBase<'T> () =

    /// Runs as a test of ClosedGenericTests.
    [<TestMethod>]
    member _.``generic inherited test`` () = ignore typeof<'T>

/// The test class closing the generic base.
[<TestClass>]
type ClosedGenericTests () =
    inherit GenericTestsBase<int> ()

/// A public module holding a test class: MSTest discovers nested public classes.
module NestedTests =

    /// A test class nested in a module.
    [<TestClass>]
    type Nested () =

        /// Runs.
        [<TestMethod>]
        member _.``nested test`` () = ()

// Tests MSTest skips without a word

/// Module-level tests: static methods of a static class.
module ModuleLevelTests =

    /// Skipped.
    [<TestMethod>]
    let ``module-level test`` () = ()

    /// Skipped, although marked by an attribute derived from TestMethodAttribute.
    [<STATestMethod>]
    let ``module-level test with a derived attribute`` () = ()

/// A module marked with the TestClass attribute: still a static class.
[<TestClass>]
module ModuleMarkedTestClass =

    /// Skipped.
    [<TestMethod>]
    let ``module-level test in a module marked TestClass`` () = ()

/// A class without the TestClass attribute.
type MissingTestClassTests () =

    /// Skipped.
    [<TestMethod>]
    member _.``test without TestClass`` () = ()

/// An abstract test class nothing derives from.
[<TestClass; AbstractClass>]
type AbstractTests () =

    /// Skipped.
    [<TestMethod>]
    member _.``test in an abstract class nothing derives from`` () = ()

// F# rejects [<TestClass>] on a struct with FS0842, an error in Release builds; MSTest would skip the struct
#nowarn "842"

/// A struct marked with the TestClass attribute.
[<TestClass; Struct>]
type StructTests =

    /// Skipped.
    [<TestMethod>]
    member _.``test in a struct`` () = ()

#warnon "842"

// Tests that make MSTest fail discovery of the whole assembly

/// Test methods returning what MSTest does not await or ignore (UTA007).
[<TestClass>]
type UnsupportedReturnTypeTests () =

    /// Returns an Async of unit.
    [<TestMethod>]
    member _.``returns Async`` () = async { () }

    /// Returns a Task of unit: a task CE without the Task annotation.
    [<TestMethod>]
    member _.``returns Task of unit`` () = task { () }

    /// Returns int.
    [<TestMethod>]
    member _.``returns int`` () = 42

/// A static test method (UTA007).
[<TestClass>]
type StaticTestMethodTests () =

    /// Static.
    [<TestMethod>]
    static member ``static test`` () = ()

/// A test method that is not public (UTA007).
[<TestClass>]
type NonPublicTestMethodTests () =

    /// Internal.
    [<TestMethod>]
    member internal _.``internal test`` () = ()

/// A test class that is not public (UTA001).
[<TestClass>]
type internal NonPublicTests () =

    /// In an internal class.
    [<TestMethod>]
    member _.``test in an internal class`` () = ()

/// A generic test class MSTest cannot instantiate.
[<TestClass>]
type GenericTests<'T> () =

    /// In a generic class.
    [<TestMethod>]
    member _.``test in a generic class`` () = ignore typeof<'T>
