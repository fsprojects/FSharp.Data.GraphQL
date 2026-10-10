namespace FSharp.Data.GraphQL.Testing

open System
open System.Reflection
open System.Runtime.InteropServices
open System.Threading.Tasks
open Microsoft.VisualStudio.TestTools.UnitTesting

/// <summary>What keeps MSTest from running a test, as found by <see cref="TestDiscoveryGuard"/>.</summary>
[<RequireQualifiedAccess>]
type DiscoveryProblemKind =
    /// <summary>
    /// A <c>[&lt;TestMethod&gt;]</c> function of an F# module. A module compiles to a static class, so the test is a static
    /// method no <c>[&lt;TestClass&gt;]</c> instance has, and MSTest skips it without a word.
    /// </summary>
    | ModuleLevelTestMethod
    /// <summary>
    /// A static test method of a type. MSTest fails discovery with UTA007 when the type is a test class and skips the
    /// method otherwise.
    /// </summary>
    | StaticTestMethod
    /// <summary>
    /// A test method that is not public in a public type. MSTest fails discovery with UTA007 when the type is a test class
    /// and skips the method otherwise. The members of a type that is not public are not reported separately: F# compiles
    /// them as non-public, and making the type public fixes them along with it.
    /// </summary>
    | NonPublicTestMethod
    /// <summary>
    /// A test method returning anything but <c>unit</c>, <see cref="T:System.Threading.Tasks.Task"/> or
    /// <see cref="T:System.Threading.Tasks.ValueTask"/>, such as <c>Async&lt;unit&gt;</c> or <c>Task&lt;unit&gt;</c>,
    /// which <c>task { }</c> builds unless the method is annotated <c>: Task</c>. MSTest fails discovery with UTA007 when
    /// its type is a test class and skips it otherwise.
    /// </summary>
    | UnsupportedReturnType
    /// <summary>
    /// A test method of a type without <c>[&lt;TestClass&gt;]</c> that no test class derives from. MSTest skips it without
    /// a word.
    /// </summary>
    | MissingTestClass
    /// <summary>
    /// A test method of an abstract test class that no concrete test class derives from. MSTest skips it without a word.
    /// </summary>
    | AbstractTestClass
    /// <summary>
    /// A test class that is not public, or is nested in a type or module that is not. MSTest fails discovery with UTA001.
    /// </summary>
    | NonPublicTestClass
    /// <summary>A test class that is a generic type definition and not abstract. MSTest fails discovery.</summary>
    | GenericTestClass
    /// <summary>
    /// A test class that is a struct. F# warns about <c>[&lt;TestClass&gt;]</c> on a struct (FS0842), and MSTest skips it
    /// without a word.
    /// </summary>
    | ValueTypeTestClass

/// A test MSTest skips or rejects, as found by TestDiscoveryGuard.
[<NoComparison>]
type DiscoveryProblem = {
    /// What keeps MSTest from running the test.
    Kind : DiscoveryProblemKind
    /// The test method, or the test class when the problem is the class itself.
    Member : MemberInfo
    /// The full name of the member: its namespace, enclosing modules and types, and its own name, joined by dots.
    MemberName : string
    /// What is wrong and how to fix it, starting with the member name.
    Message : string
}

/// <summary>
/// Finds the tests of an assembly that MSTest skips without a word or that make it fail discovery. F# gets no MSTest
/// analyzers (MSTEST0002, MSTEST0003 and the others are Roslyn analyzers for C#), so a test project calls
/// <see cref="M:FSharp.Data.GraphQL.Testing.TestDiscoveryGuard.AssertNoProblems(System.Reflection.Assembly,System.String)"/>
/// from one of its own tests:
/// <code>
/// [&lt;TestMethod&gt;]
/// member _.``Every test in this assembly is discoverable`` () =
///     TestDiscoveryGuard.AssertNoProblems (typeof&lt;MyTests&gt;.Assembly)
/// </code>
/// <para>
/// The silent cases are the reason for the guard: a <c>[&lt;TestMethod&gt;]</c> on a function of an F# module, a test
/// method in a type without <c>[&lt;TestClass&gt;]</c>, and a test method in an abstract test class nothing derives from
/// all pass a run as if they did not exist. The cases MSTest rejects itself (UTA001, UTA007, a generic test class) make it
/// report one problem and run no test at all; the guard reports them all at once, with the F# fix.
/// </para>
/// <para>
/// A test method belongs to the type that declares it; it runs when that type, or a type deriving from it, is a public,
/// concrete, non-generic class marked <c>[&lt;TestClass&gt;]</c> itself. An abstract base class holding test methods for its
/// derived test classes is therefore fine.
/// </para>
/// </summary>
[<AbstractClass; Sealed>]
type TestDiscoveryGuard private () =

    static let supportedReturnTypes = [| typeof<Void>; typeof<Task>; typeof<ValueTask> |]

    // What an asynchronous F# test returns when it lacks the `: Task` annotation
    static let asynchronousReturnTypes = [| typedefof<Task<_>>; typedefof<ValueTask<_>>; typedefof<Async<_>> |]

    static let isAsynchronous (returnType : Type) =
        returnType.IsGenericType
        && asynchronousReturnTypes
           |> Array.contains (returnType.GetGenericTypeDefinition ())

    static let allDeclaredMethods =
        BindingFlags.Public
        ||| BindingFlags.NonPublic
        ||| BindingFlags.Instance
        ||| BindingFlags.Static
        ||| BindingFlags.DeclaredOnly

    // F# puts several CompilationMapping attributes on some types, so all of them are looked at
    static let isModule (type' : Type) =
        type'.GetCustomAttributes<CompilationMappingAttribute>()
        |> Seq.exists (fun mapping ->
            mapping.SourceConstructFlags
            &&& SourceConstructFlags.KindMask
                =
                SourceConstructFlags.Module)

    // TestClassAttribute is not inherited: MSTest wants it on the class it instantiates
    static let hasTestClass (type' : Type) = type'.IsDefined (typeof<TestClassAttribute>, false)

    // Derived attributes such as DataTestMethod and STATestMethod mark test methods too
    static let isTestMethod (method : MethodInfo) = method.IsDefined (typeof<TestMethodAttribute>, true)

    static let isRunnableTestClass (type' : Type) =
        type'.IsClass
        && not type'.IsAbstract
        && type'.IsVisible
        && not type'.ContainsGenericParameters
        && hasTestClass type'

    static let definitionOf (type' : Type) =
        if type'.IsGenericType then
            type'.GetGenericTypeDefinition ()
        else
            type'

    // Whether the test methods `declaring` declares run as part of `runner`: runner is `declaring` or derives from it,
    // possibly through a closed generic base whose definition declares them
    static let rec runsTestsOf (declaring : Type) (runner : Type) =
        definitionOf runner = declaring
        || (match runner.BaseType with
            | null -> false
            | baseType -> runsTestsOf declaring baseType)

    static let nameOf (type' : Type) =
        // Reflection joins nested types and F# modules with '+'; dots read the way the source does
        match type'.FullName with
        | null -> type'.Name
        | fullName -> fullName.Replace ('+', '.')

    static let typeProblem (type' : Type) (kind : DiscoveryProblemKind) =
        let name = nameOf type'
        let description =
            match kind with
            | DiscoveryProblemKind.NonPublicTestClass ->
                "is marked [<TestClass>] but is not public, or is nested in a type or module that is not, "
                + "so MSTest fails discovery of the whole assembly with UTA001."
            | DiscoveryProblemKind.GenericTestClass ->
                "is marked [<TestClass>] but is a generic type definition MSTest cannot instantiate, so MSTest fails "
                + "discovery of the whole assembly. Make it an abstract base of a [<TestClass>] that closes it."
            | _ -> "is marked [<TestClass>] but is a struct, so MSTest skips it without a word. Make it a class."
        {
            Kind = kind
            Member = type'
            MemberName = name
            Message = $"%s{name} %s{description}"
        }

    static let methodProblem (declaring : Type) (method : MethodInfo) (kind : DiscoveryProblemKind) =
        let declaringName = nameOf declaring
        let name = $"%s{declaringName}.%s{method.Name}"
        let description =
            match kind with
            | DiscoveryProblemKind.ModuleLevelTestMethod ->
                $"is a function of the F# module %s{declaringName}, which compiles to a static method of a static class, "
                + "so MSTest skips it without a word. Move it into a [<TestClass>] type as an instance member."
            | DiscoveryProblemKind.StaticTestMethod ->
                "is static, but MSTest runs instance test methods only: UTA007 in a [<TestClass>], skipped elsewhere. "
                + $"Declare it as member _.``%s{method.Name}`` () = ..."
            | DiscoveryProblemKind.NonPublicTestMethod ->
                "is not public, but MSTest runs public test methods only: UTA007 in a [<TestClass>], skipped elsewhere."
            | DiscoveryProblemKind.UnsupportedReturnType ->
                let fix =
                    if isAsynchronous method.ReturnType then
                        $"Annotate it: member _.``%s{method.Name}`` () : Task = task {{ ... }}"
                    else
                        "Make it return unit: end it with an assertion, or pipe the last value into ignore."
                $"returns %s{Formatting.typeName method.ReturnType}, but MSTest runs test methods returning unit, Task or "
                + $"ValueTask only: UTA007 in a [<TestClass>], skipped elsewhere. %s{fix}"
            | DiscoveryProblemKind.AbstractTestClass ->
                $"is declared in the abstract %s{declaringName}, and no concrete [<TestClass>] derives from it, "
                + "so MSTest skips it without a word."
            | _ ->
                $"is declared in %s{declaringName}, which is not marked [<TestClass>], and no [<TestClass>] derives from it, "
                + $"so MSTest skips it without a word. Mark %s{declaringName} with [<TestClass>]."
        {
            Kind = kind
            Member = method
            MemberName = name
            Message = $"%s{name} %s{description}"
        }

    static let sortProblems (problems : DiscoveryProblem seq) =
        problems
        |> Seq.toList
        |> List.sortWith (fun left right ->
            match String.CompareOrdinal (left.MemberName, right.MemberName) with
            | 0 -> compare left.Kind right.Kind
            | order -> order)

    static let assertNoProblems (problems : DiscoveryProblem list) (scope : string) (message : string | null) =
        match problems with
        | [] -> ()
        | problems ->
            Assert.RaiseFailure (
                $"TestDiscoveryGuard found %i{problems.Length} test(s) in %s{scope} that MSTest skips or rejects.",
                message,
                problems
                |> Seq.map (fun problem -> $"- %s{problem.Message}")
                |> String.concat Environment.NewLine
            )

    /// <summary>Finds the tests among the given types that MSTest skips or rejects.</summary>
    /// <param name="types">
    /// The types to check. Only these are searched for the test classes that derive from a type declaring test methods.
    /// </param>
    /// <returns>The problems found, ordered by member name; empty when MSTest runs every test.</returns>
    static member FindProblems (types : Type seq) : DiscoveryProblem list =
        let types = types |> Seq.distinct |> Seq.toArray
        let runners = types |> Array.filter isRunnableTestClass

        let classProblems =
            types
            |> Seq.filter (fun type' -> hasTestClass type' && not (isModule type'))
            |> Seq.choose (fun type' ->
                if type'.IsValueType then
                    Some (typeProblem type' DiscoveryProblemKind.ValueTypeTestClass)
                elif not type'.IsVisible then
                    Some (typeProblem type' DiscoveryProblemKind.NonPublicTestClass)
                elif type'.IsGenericTypeDefinition && not type'.IsAbstract then
                    Some (typeProblem type' DiscoveryProblemKind.GenericTestClass)
                else
                    None)

        let methodProblemsOf (declaring : Type) (method : MethodInfo) =
            let inModule = isModule declaring
            seq {
                if inModule then
                    DiscoveryProblemKind.ModuleLevelTestMethod
                elif method.IsStatic then
                    DiscoveryProblemKind.StaticTestMethod
                // F# compiles the members of a non-public type as non-public; making the type public fixes both, and the
                // type has a problem of its own
                if not method.IsPublic && declaring.IsVisible then
                    DiscoveryProblemKind.NonPublicTestMethod
                if not (supportedReturnTypes |> Array.contains method.ReturnType) then
                    DiscoveryProblemKind.UnsupportedReturnType
                // A test class that is not public, generic or a struct has a problem of its own, reported above
                if
                    not inModule
                    && not (runners |> Array.exists (runsTestsOf declaring))
                then
                    if not (hasTestClass declaring) then
                        DiscoveryProblemKind.MissingTestClass
                    elif declaring.IsAbstract then
                        DiscoveryProblemKind.AbstractTestClass
            }
            |> Seq.map (methodProblem declaring method)

        let methodProblems =
            types
            |> Seq.collect (fun declaring ->
                declaring.GetMethods allDeclaredMethods
                |> Seq.filter isTestMethod
                |> Seq.collect (methodProblemsOf declaring))

        seq {
            yield! classProblems
            yield! methodProblems
        }
        |> sortProblems

    /// <summary>Finds the tests of an assembly that MSTest skips or rejects.</summary>
    /// <param name="assembly">The test assembly to check, usually <c>typeof&lt;SomeTests&gt;.Assembly</c>.</param>
    /// <returns>The problems found, ordered by member name; empty when MSTest runs every test.</returns>
    static member FindProblems (assembly : Assembly) : DiscoveryProblem list = TestDiscoveryGuard.FindProblems (assembly.GetTypes ())

    /// <summary>Fails the test, listing every problem, when MSTest skips or rejects a test among the given types.</summary>
    /// <param name="types">The types to check, as for <c>FindProblems</c>.</param>
    /// <param name="message">Context shown below the description of the failure.</param>
    static member AssertNoProblems (types : Type seq, [<Optional>] message : string | null) : unit =
        assertNoProblems (TestDiscoveryGuard.FindProblems types) "the given types" message

    /// <summary>Fails the test, listing every problem, when MSTest skips or rejects a test of an assembly.</summary>
    /// <param name="assembly">The test assembly to check, usually <c>typeof&lt;SomeTests&gt;.Assembly</c>.</param>
    /// <param name="message">Context shown below the description of the failure.</param>
    static member AssertNoProblems (assembly : Assembly, [<Optional>] message : string | null) : unit =
        assertNoProblems (TestDiscoveryGuard.FindProblems assembly) (nonNull (assembly.GetName ()).Name) message
