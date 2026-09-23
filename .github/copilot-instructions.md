# Copilot Instructions

## Project Details

* F# 10, C# 14, .NET 10, nullability checks enabled
* .NET SDK roll-forward policy in #file:'global.json'; the exact SDK version is pinned in the GitHub workflows and #file:'build/Program.fs'
* Common parameters specified in #file:'Directory.Build.props'
* Central NuGet package version management – versions go in #file:'Packages.props', not in `.fsproj` files
* Build: `dotnet build FSharp.Data.GraphQL.slnx`
* Integration tests and their server live in `FSharp.Data.GraphQL.Integration.slnx`
* Full build pipeline (FAKE): `build.cmd` / `build.sh`, which run `dotnet run --project build\Build.fsproj`

## Solution Structure

```text
/
├── src/
│   ├── FSharp.Data.GraphQL.Shared/             – AST, parser, type system, validation, introspection
│   ├── FSharp.Data.GraphQL.Server/             – schema definition and execution engine
│   ├── FSharp.Data.GraphQL.Server.Middleware/  – built-in middlewares (ObjectListFilter, query weight, …)
│   ├── FSharp.Data.GraphQL.Server.Relay/       – Relay connections and node support
│   ├── FSharp.Data.GraphQL.Server.AspNetCore/  – ASP.NET Core integration (HTTP and WebSocket)
│   ├── FSharp.Data.GraphQL.Server.Giraffe/     – Giraffe integration
│   ├── FSharp.Data.GraphQL.Server.Oxpecker/    – Oxpecker integration
│   ├── FSharp.Data.GraphQL.Server.Suave/       – Suave integration
│   ├── FSharp.Data.GraphQL.Client/             – client type provider runtime
│   └── FSharp.Data.GraphQL.Client.DesignTime/  – client type provider design-time component
├── tests/
│   ├── FSharp.Data.GraphQL.Tests/              – xUnit unit tests
│   ├── FSharp.Data.GraphQL.Tests.Sql/          – LINQ to SQL tests
│   ├── FSharp.Data.GraphQL.IntegrationTests/   – xUnit integration tests (in-process hosts via WebApplicationFactory)
│   ├── FSharp.Data.GraphQL.IntegrationTests.Server/
│   └── FSharp.Data.GraphQL.Benchmarks/         – BenchmarkDotNet benchmarks
├── samples/                                    – sample servers and client provider scripts
├── build/                                      – FAKE build scripts
└── docs/                                       – FSharp.Formatting documentation source
```

## Libraries in Use

If you need any source code you can find it in the following repositories:

* [`FParsec`](https://github.com/stephan-tolksdorf/fparsec) `master` branch
* [`XParsec`](https://github.com/roboz0r/XParsec/) `main` branch
* [`FSharp.Control.Reactive`](https://github.com/fsprojects/FSharp.Control.Reactive) `master` branch
* [`FSharp.Data.TypeProviders`](https://github.com/fsprojects/FSharp.Data.TypeProviders) `main` branch
* [`FSharp.SystemTextJson`](https://github.com/Tarmil/FSharp.SystemTextJson.git) `master` branch
* [`FsToolkit.ErrorHandling`](https://github.com/demystifyfp/FsToolkit.ErrorHandling) `master` branch
* [`Giraffe`](https://github.com/giraffe-fsharp/Giraffe) `master` branch
* [`Oxpecker`](https://github.com/Lanayx/Oxpecker.git) `main` branch
* [`xUnit`](https://github.com/xunit/xunit) – test framework

Use GitHub MCP tools for code search in these repositories when needed.

## Shell Operations

* Always prefer PowerShell for all shell operations.
* Always prefer installing PowerShell modules over installing CLI tools.

## GitHub Operations

* First of all use GitHub MCP tools.
* If no tool is available, use the `PowerShellForGitHub` PowerShell module for all GitHub operations. Never use GitHub CLI (`gh`) or any other CLI tool for GitHub.

## MCP Servers

MCP server configuration lives in #file:'.mcp.json':

* `servers` – read by VS Code / GitHub Copilot.
* `mcpServers` – read by Claude Code. Mirrors the same servers; keep both sections in sync when adding or changing a server.

Local tool packages are pinned in #file:'.config/dotnet-tools.json' – run `dotnet tool restore` before first use.

| Server | Tool package | Command | Notes |
|---|---|---|---|
| `F#` | `fslangmcp` | `dotnet tool run fslangmcp` | Semantic F# MCP backed by the compiler and FSAC. Use it for cross-project symbol search, project/file outlines, diagnostics, rename previews, dead-code checks, and other F#-aware analysis that plain text search misses. See <https://github.com/Neftedollar/FsLangMCP>. |
| `GitHub` | – | HTTP | Code search in dependency repositories. |
| `Microsoft Docs` | – | HTTP | Official Microsoft and .NET documentation. |

For F# work, prefer FsLangMCP over `rg`/plain text search whenever the task depends on symbol meaning, compile context, cross-project usage, diagnostics, or safe refactoring preview.

## F# Coding Guidelines

### Language and Tooling

* Always use the latest F# 10 features over old syntax.
* If you are running outside of an IDE, or the IDE does not provide F# semantic tools, use FsLangMCP as the primary tool for F# code navigation, symbol discovery, diagnostics, usage search and refactoring preview. Prefer its semantic tools over plain text search.
* The compiler generates `IsCaseName` instance properties (for example `IsOk`, `IsNone`) for each DU case – use them when a single-case check is needed.
* Code is formatted with Fantomas using the settings in #file:'.editorconfig'. After making code changes, run `dotnet fantomas <changed files>` (or #file:'format-changed-files.ps1') before committing.

### Asynchrony and Cancellation

* Prefer `task` CE over `async` CE.
* When a method must return non-generic `Task`, annotate the return type explicitly: `member _.MyMethod (...) : Task = task { ... }`. Never cast through pipelines.
* `task` CE can await `ValueTask` APIs directly.
* Curried functions take the `CancellationToken` as their **first** parameter, so that it can be partially applied.
* Never hand-roll wrappers such as `ValueTask (task { ... })`, `.AsTask ()` round-trips or manual `unit -> Task` thunks. Use the matching [`IcedTasks`](https://github.com/TheAngryByrd/IcedTasks) CE instead (`valueTask`, `valueTaskUnit`, `taskUnit`, `coldTask`, `cancellableTask`, `cancellableValueTask`, `backgroundTask`, …) and always use the full CE names, not the short aliases (`vTask`, `pvTask`, …). This library does not reference `IcedTasks` yet – adding it introduces a transitive dependency for every consumer, so confirm with the maintainers first.

### Values and Collections

* Prefer `voption` over `option`.
* Prefer `struct ('T1 * 'T2)` over reference tuples, and anonymous struct records (`struct {| ... |}`) over tuples for return types of public functions and methods.
* Never group with `Seq.groupBy` – use `ToLookup` from `System.Linq`. It groups once into an `ILookup<'Key, 'T>` instead of re-grouping on every enumeration and does not allocate a tuple per group. Pass a lambda (`xs.ToLookup (fun x -> keyOf x)`), not a bare function value.
* When casting sequence items use `Seq.cast<TargetType>` instead of `Seq.map (fun item -> item :> TargetType)`.
* When concatenating two sequences or lists, prefer `seq { yield! xs; yield! ys }` (or `[ yield! xs; yield! ys ]` for lists) over the `@` operator or `Seq.append`.
* When pipe operators are used on a materializable collection multiple times in a row, prefer `Seq` module for the chain and materialize at the end.

### Functions, Lambdas and Strings

* Prefer underscore lambda syntax like `Seq.map _.Name` over `Seq.map (fun x -> x.Name)`, but only when the expression is a simple member access. Complex expressions like `Seq.where (fun x -> x.Name = name)` or `Seq.map (fun x -> x.Field1, x.Field2)` cannot be simplified. Never write a space in `_.MethodCall()` – it breaks parsing.
* Simplify `Seq.map (fun x -> someFunction x)` to `Seq.map someFunction`.
* Prefer interpolated strings over `printf` functions for string formatting. Format specifiers like `$"%s{value}"` are valid in interpolated strings and help type inference.
* Use descriptive function names that indicate transformation direction.

### Nullable Reference Types

* Declare variables non-nullable; check for `null` at entry points only.
* Trust the SDK null annotations – do not add null checks when the type system says a value cannot be null.
* Use `withNull` for null checks instead of boxing delegates/functions (avoid `isNull (box value)`).
* Prefer `match` on `null` over `if isNull` – it narrows the type and suppresses nullness warnings:

  ```fsharp
  // Preferred
  match someObject with
  | null -> ()
  | someObject -> someObject.SomeProperty
  ```

* Before suppressing a nullness warning (3261, 3262, …), exhaust these alternatives in order:
  1. `nonNull value` – asserts non-null at runtime and fails fast.
  2. `Unchecked.nonNull value` – skips the runtime check; only when non-null was already verified upstream.
  3. An inline `#nowarn` / `#warnon` pair around the smallest possible scope – last resort for interop boundaries, centralised in a single helper rather than scattered across call sites.
* Never suppress warnings file-wide; `#nowarn` and `#warnon` are valid anywhere in a file.

### XML Documentation Comments

* A doc comment is either plain text with no tags at all, or fully explicit XML starting with `<summary>` – never a mixture. The compiler adds `<summary>` by itself only when the comment has no tags, and silently escapes tags otherwise:
  * No tags anywhere → bare `///` lines, no `<summary>`.
  * Any tag at all (`<see/>`, `<c/>`, `<para>`, `<param>`, `<returns>`, …) → the comment must start with an explicit `<summary>`, and every `<para>` must be inside it.

  ```fsharp
  // ✅ plain text — the compiler supplies <summary>
  /// Represents the result of a GraphQL execution.

  // ❌ a sibling tag without <summary> — the <param> is escaped into the summary and lost
  /// Executes a query.
  /// <param name="query">Query text</param>

  // ✅ any tag present, so the comment is explicit XML throughout
  /// <summary>Executes a query.</summary>
  /// <param name="query">Query text</param>
  ```

* Every public API must have XML documentation.
* On an explicit interface implementation (`interface X with member _.M (...) = ...`), write `/// <inheritdoc />` alone instead of restating the interface member's documentation, unless this implementation has behavior worth calling out beyond what the interface already documents – write a normal `<summary>`/`<remarks>` there instead.
* Refer to types and members through `<see cref="Type.Member"/>`, never through `<c>` or plain text. `<c>` is for literal values only (JSON, GraphQL, setting names). Refer to language keywords through `<see langword="null"/>`.
* Split multi-paragraph documentation into `<para>` elements inside `<summary>` – bare line breaks are collapsed by documentation renderers.
* Write enumerations as `<list type="bullet">` (or `type="number"`) with `<item><description>…</description></item>`, never as Markdown-style bullets.

### Opens Sorting

Sort `open` statements alphabetically within groups: `System` first, `Microsoft` second, `FSharp` third; then other external namespaces; then this solution's namespaces. `open type` goes last in each group. Type and module aliases form a separate final group.

### Class Constructors

This is how to define a non-default F# class constructor:

```fsharp
type DerivedClass =
    inherit BaseClass

    new (``arguments here``) as ``created object``
        =
        // create any objects used in the base class constructor
        let fieldValue = ""
        {
            inherit
                BaseClass (``arguments here``)
        }
        then
            ``created object``.otherField <- fieldValue

    [<DefaultValue>]
    val mutable otherField : FieldType
```

### Class Instantiation

Always prefer F# class initializers over property assignment! **You absolutely must use F# class initializers instead of property assignment**!

Class declaration:

```fsharp
type MyClass (someConstructorParam : string) =
    member ReadOnlyProperty = someConstructorParam

    member val MutableProperty1 = "" with get, set
    member val MutableProperty2 = "" with get, set
```

Wrong:

```fsharp
let myClass = MyClass("some value")
myClass.MutableProperty1 <- "new value"
myClass.MutableProperty2 <- "new value"
```

Right:

```fsharp
let myClass =
    MyClass(
        // constructor parameters go first without names
        "some value",
        // then mutable properties go next with names
        MutableProperty1 = "new value",
        MutableProperty2 =
            // operations must be placed into parentheses
            (5 |> string)
    )
```

### C#-Consumable Extension Members

```fsharp
// AutoOpen makes the module automatically available without an explicit open statement
// Extension makes the members visible to C#
[<AutoOpen; Extension>]
module MyTypeExtensions =

    type MyType with

        // Extension is visible to C#
        // CompiledName makes the method name friendly to C#
        [<Extension; CompiledName "ExtensionMethod">]
        member this.ExtensionMethod (param1 : string) : ReturnType =
            ()
```

## Naming Conventions

* Use PascalCase for modules, types, and public members.
* Use camelCase for `let` bindings, functions, private fields, and local variables.
* Prefix interface names with `I` (e.g., `ISchema`).
* Do not prefix type parameters with `T` in new code (e.g., use `'Result` instead of `'TResult`).
* Name tests using spaces (e.g., ``let ``Test name with spaces`` () = ...``).

## Coding Instructions

**IMPORTANT!!!** When you create a code file, **ALWAYS** put it **at the end** of fsproj project in order to avoid missing types that must come above.

During the implementation, if you need some types or members defined in the other project but that project is not referenced, then:

1. stop implementing the solution;
2. respond with summary about what is needed and which project it is defined in;
3. ask if you can add the reference before continuing coding.

## Testing

* Tests use xUnit.
* If you work with tests, then do not build the whole solution as it is large and the build happens very slow. Run the tests individually or the whole test project instead.
* Prefer running tests through the IDE's MCP test tools; fall back to the `--no-build` switch of `dotnet test` first to speed up execution when those tools are unavailable or fail to run the tests, and use the trx format for results so failures can be consumed and fixed.
* Use `Assert.Equal`, `Assert.Collection`, `Assert.Contains` / `Assert.DoesNotContain`, `Assert.Empty` / `Assert.NotEmpty` and `Assert.Single` for collection assertions – they work directly with F# lists, arrays and sequences.
* Every assertion should produce a self-explanatory failure output.
* Async tests must return `Task`, not `Async` or `Task<unit>` – always declare `) : Task = task {`.

## Build and Test Steps

We use `dotnet test` CLI to test the project.

* If the build fails with errors or non-zero exit code, fix it based on the error messages given and repeat the build step.
* If build or tests step fails, fix the errors and repeat from build. After that, report all relevant build errors, error messages and specific details about failing tests and their failure details.
* If any of the tests fail, check if the test, the test expectation (either inline in the test or a reference file configured for the test) or the implementation needs updating, and fix it.

### Acceptance Criteria

* Builds without errors.
* Runs tests without errors. If some tests needed adjustments, those test expectations/baseline adjustments were done.
* If the acceptance criteria was not met, collect the error messages (build failures or failing tests) and report them.

## General

* Make only high-confidence suggestions when reviewing code changes.
* Write code with good maintainability practices, including comments on why certain design decisions were made.
* Handle edge cases and write clear exception handling.
* Never duplicate code unless you got explicit confirmation that you are allowed to do so.
* All comments, documentation, README files, and markdown files must be written in **English only**.
