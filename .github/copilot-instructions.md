# Project details

F# 10
C# 14
.NET 10
Nullability checks enabled

## Shell operations

Always prefer PowerShell for all shell operations.
Always prefer installing PowerShell modules over installing CLI tools.

## GitHub operations

First of all use GitHub MCP tools.
If no tool avaialble, use `PowerShellForGitHub` PowerShell module for all GitHub operations. Never use GitHub CLI (`gh`) or any other CLI tool for GitHub.

## Libraries we use

If you need any source code you can find it in the following repositories:
[FParsec](https://github.com/stephan-tolksdorf/fparsec) `master` branch
[XParsec](https://github.com/roboz0r/XParsec/) `main` branch
[FSharp.Control.Reactive](https://github.com/fsprojects/FSharp.Control.Reactive) `master` branch
[FSharp.Data.TypeProviders](https://github.com/fsprojects/FSharp.Data.TypeProviders) `main` branch
[FSharp.SystemTextJson](https://github.com/Tarmil/FSharp.SystemTextJson.git) `master` branch
[Oxpecker](https://github.com/Lanayx/Oxpecker.git) `main` branch

# How to work with tests

If you work with tests, then do not build the whole solution as it is large and the build happens very slow.
Always try running tests with `--no-build` switch first to speedup execution.
Instead run the tests individually or the whole test project.
`CollectionAssert` cannot work with F# lists, only with F# array syntax

# Code Style and Standards

We prefer the latest F# 9 features over the old syntax

Prefer `voption` over `option`

Prefer `task` CE over `async` CE

This is how you define a non-default F# class constructor:

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

Always prefer F# class initializers over property assignment! **You absolutely must use F# class initializers instead of property assignment**!

Having a class declaration:

``` F#
type MyClass (someConstructorParam : string) =
    member ReadOnlyProperty = someConstructorParam

    member val MutableProperty1 = "" with get, set
    member val MutableProperty2 = "" with get, set
```

The following excerpt of class creation is wrong:

``` F#
let myClass = MyClass("some value")
myClass.MutableProperty1 <- "new value"
myClass.MutableProperty2 <- "new value"
```

The following excerpt (uses initializer syntax) of class creation is right:

``` F#
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

## Coding instructions

During the implementation, if you need some types or members defined in the other project but that project is not referenced, then:

1. stop implementing the solution;
2. respond with summary about what is needed and which project it is defined in;
3. ask if you can add the reference before continuing coding.

Never ever duplicate the code unless you got explicit confirmation that you are allowed to do so.

## Documentation and Naming

* Document public APIs with XML comments
* Use descriptive function names that indicate transformation direction

# Build and test steps

We use `dotnet test` CLI to test the project.
If the build fails with errors or non-zero exit code, fix it based on the error messages given and repeat the build step.
If build or tests step fails, fix the errors and repeat from build. After that, report all relevant build errors, error messages and specific details about failing tests and their test test failure details.

## Fixing tests

* If any of the tests fail: Check if the test, test expectation (either inline in the test or a reference file configured for the test) or implementation needs updating, and fix it

## Acceptance criteria

* Builds without errors.
* Runs tests without errors. If some tests needed adjustments, those test expectations/baseline adjustments were done.
* If the acceptance criteria was not met, collect the error messages (build failures or failing tests) and report them.
