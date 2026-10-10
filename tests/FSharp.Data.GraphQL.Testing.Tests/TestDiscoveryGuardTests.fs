namespace FSharp.Data.GraphQL.Testing.Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open FSharp.Data.GraphQL.Testing
open FSharp.Data.GraphQL.Testing.Tests.DiscoverySamples

[<TestClass>]
type TestDiscoveryGuardTests () =

    static let samplesAssembly = typeof<ValidTests>.Assembly

    static let samplesNamespace = "FSharp.Data.GraphQL.Testing.Tests.DiscoverySamples."

    static let problemOf (problems : DiscoveryProblem list) (memberName : string) =
        Assert.ContainsSingle (
            (fun (problem : DiscoveryProblem) -> String.Equals (problem.MemberName, samplesNamespace + memberName, StringComparison.Ordinal)),
            problems,
            $"The guard must report exactly one problem of %s{memberName}"
        )

    [<TestMethod>]
    member _.``Every test in this assembly is discoverable`` () =
        TestDiscoveryGuard.AssertNoProblems (typeof<TestDiscoveryGuardTests>.Assembly, "MSTest must run every test of the self-test project")

    [<TestMethod>]
    member _.``The guard reports exactly the broken samples`` () =
        let expected = [
            struct ("AbstractTests.test in an abstract class nothing derives from", DiscoveryProblemKind.AbstractTestClass)
            struct ("GenericTests`1", DiscoveryProblemKind.GenericTestClass)
            struct ("MissingTestClassTests.test without TestClass", DiscoveryProblemKind.MissingTestClass)
            struct ("ModuleLevelTests.module-level test", DiscoveryProblemKind.ModuleLevelTestMethod)
            struct ("ModuleLevelTests.module-level test with a derived attribute", DiscoveryProblemKind.ModuleLevelTestMethod)
            struct ("ModuleMarkedTestClass.module-level test in a module marked TestClass", DiscoveryProblemKind.ModuleLevelTestMethod)
            struct ("NonPublicTestMethodTests.internal test", DiscoveryProblemKind.NonPublicTestMethod)
            struct ("NonPublicTests", DiscoveryProblemKind.NonPublicTestClass)
            struct ("StaticTestMethodTests.static test", DiscoveryProblemKind.StaticTestMethod)
            struct ("StructTests", DiscoveryProblemKind.ValueTypeTestClass)
            struct ("UnsupportedReturnTypeTests.returns Async", DiscoveryProblemKind.UnsupportedReturnType)
            struct ("UnsupportedReturnTypeTests.returns Task of unit", DiscoveryProblemKind.UnsupportedReturnType)
            struct ("UnsupportedReturnTypeTests.returns int", DiscoveryProblemKind.UnsupportedReturnType)
        ]
        let actual =
            TestDiscoveryGuard.FindProblems samplesAssembly
            |> List.map (fun problem -> struct (problem.MemberName.Substring samplesNamespace.Length, problem.Kind))
        // Sorted the same way on both sides: the guard orders by member name, the expectation is written alphabetically
        Assert.StructurallyEquals (
            expected
            |> List.sortWith (fun struct (left, _) struct (right, _) -> String.CompareOrdinal (left, right)),
            actual,
            "The guard must report each broken sample, and nothing else"
        )

    [<TestMethod>]
    member _.``A module-level test is reported with the module and the fix`` () =
        let problem =
            problemOf (TestDiscoveryGuard.FindProblems samplesAssembly) "ModuleLevelTests.module-level test"
        Assert.AreEqual (DiscoveryProblemKind.ModuleLevelTestMethod, problem.Kind, "A module-level test must be reported as such")
        Assert.AreEqual ("module-level test", problem.Member.Name, "The problem must point at the test method")
        Assert.StartsWith (problem.MemberName, problem.Message, StringComparison.Ordinal, "The message must start with the member name")
        Assert.Contains ("is a function of the F# module", problem.Message, StringComparison.Ordinal, "The message must name the cause")
        Assert.Contains ("Move it into a [<TestClass>] type", problem.Message, StringComparison.Ordinal, "The message must name the fix")

    [<TestMethod>]
    member _.``An Async test method is reported with its return type and the fix`` () =
        let problem =
            problemOf (TestDiscoveryGuard.FindProblems samplesAssembly) "UnsupportedReturnTypeTests.returns Async"
        Assert.Contains ("returns FSharpAsync<Unit>", problem.Message, StringComparison.Ordinal, "The message must name the return type")
        Assert.Contains (": Task = task {", problem.Message, StringComparison.Ordinal, "The message must show the Task annotation")

    [<TestMethod>]
    member _.``A test method returning a value is told to return unit`` () =
        let problem =
            problemOf (TestDiscoveryGuard.FindProblems samplesAssembly) "UnsupportedReturnTypeTests.returns int"
        Assert.Contains ("returns Int32", problem.Message, StringComparison.Ordinal, "The message must name the return type")
        Assert.Contains ("Make it return unit", problem.Message, StringComparison.Ordinal, "The message must suggest returning unit")
        Assert.DoesNotContain (": Task = task {", problem.Message, StringComparison.Ordinal, "A synchronous test needs no Task annotation")

    [<TestMethod>]
    member _.``A test method in a class without TestClass is reported with the class`` () =
        let problem =
            problemOf (TestDiscoveryGuard.FindProblems samplesAssembly) "MissingTestClassTests.test without TestClass"
        Assert.Contains (
            $"Mark %s{samplesNamespace}MissingTestClassTests with [<TestClass>]",
            problem.Message,
            StringComparison.Ordinal,
            "The message must name the class to mark"
        )

    [<TestMethod>]
    member _.``An abstract base runs its tests through the test classes the given types include`` () =
        Assert.IsEmpty (
            TestDiscoveryGuard.FindProblems [ typeof<InheritedTestsBase>; typeof<InheritedTests> ],
            "The tests of an abstract base must count as run when a given test class derives from it"
        )
        let problem =
            Assert.ContainsSingle (
                TestDiscoveryGuard.FindProblems [ typeof<InheritedTestsBase> ],
                "Without the derived test class among the given types, the base's test must be reported"
            )
        Assert.AreEqual (DiscoveryProblemKind.MissingTestClass, problem.Kind, "The base itself has no TestClass attribute")

    [<TestMethod>]
    member _.``Valid tests produce no problems`` () =
        Assert.IsEmpty (
            TestDiscoveryGuard.FindProblems [
                typeof<ValidTests>
                typeof<InheritedTestsBase>
                typeof<InheritedTests>
                typedefof<GenericTestsBase<_>>
                typeof<ClosedGenericTests>
                typeof<NestedTests.Nested>
            ],
            "The guard must accept every test MSTest runs"
        )

    [<TestMethod>]
    member _.``AssertNoProblems fails with every problem listed`` () =
        let message =
            failureOf (fun () -> TestDiscoveryGuard.AssertNoProblems (samplesAssembly, "The samples are broken on purpose"))
        Assert.Contains (
            "TestDiscoveryGuard found 13 test(s) in FSharp.Data.GraphQL.Testing.Tests.DiscoverySamples that MSTest skips or rejects.",
            message,
            StringComparison.Ordinal,
            "The failure must count the problems and name the assembly"
        )
        Assert.Contains ("The samples are broken on purpose", message, StringComparison.Ordinal, "The failure must include the caller's message")
        Assert.Contains ($"- %s{samplesNamespace}StructTests is marked", message, StringComparison.Ordinal, "The failure must list each problem")
