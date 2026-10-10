namespace FSharp.Data.GraphQL.Testing

open System.Collections.Generic
open System.Diagnostics
open System.Runtime.InteropServices

/// MSTest's own Assert under a name of its own: the Assert below shares its name, so inside it Assert means the type
/// being defined and MSTest is reached through this alias.
type private MSAssert = Microsoft.VisualStudio.TestTools.UnitTesting.Assert

/// <summary>
/// The F# assertions MSTest's <see cref="T:Microsoft.VisualStudio.TestTools.UnitTesting.Assert"/> lacks: options, value
/// options, results and F# structural equality.
/// <para>
/// A test file opens both <c>Microsoft.VisualStudio.TestTools.UnitTesting</c> and <c>FSharp.Data.GraphQL.Testing</c>, in
/// either order, and calls <c>Assert.AreEqual</c>, <c>Assert.HasCount</c> and <c>Assert.WantSome</c> alike: F# looks a
/// static member up on every type named <c>Assert</c> in scope, so each call finds the type that declares the member.
/// That holds only while no member name exists on both types, so this type never declares a member MSTest's
/// <c>Assert</c> has.
/// </para>
/// <para>
/// Every member takes an optional message. The failure message always describes what was expected and shows the actual
/// value; the caller's message, when given, follows on the next line, as in MSTest's own failures.
/// </para>
/// <para>
/// A failure goes through MSTest's <c>Assert.Fail</c>, which throws
/// <see cref="T:Microsoft.VisualStudio.TestTools.UnitTesting.AssertFailedException"/> at once, even inside
/// <c>Assert.Scope ()</c>: the <c>Want</c> members return the value they unwrap and have none to return after a failure.
/// </para>
/// </summary>
[<AbstractClass; Sealed; DebuggerNonUserCode; StackTraceHidden>]
type Assert private () =

    /// <summary>Fails the current test with a message laid out like MSTest's own failures.</summary>
    /// <param name="summary">What was expected and what was found, naming the assertion.</param>
    /// <param name="message">The caller's message, if any.</param>
    /// <param name="details">The labelled values to show below a blank line, or an empty string.</param>
    /// <returns>Never returns: the result type only lets a failure stand where a value is expected.</returns>
    static member internal RaiseFailure<'T> (summary : string, message : string | null, details : string) : 'T =
        MSAssert.Fail (Formatting.failureMessage summary message details)
        // Assert.Fail always throws; the value only satisfies the type checker
        Unchecked.defaultof<'T>

    /// <summary>Returns the value inside an option that must be <c>Some</c> and fails the test when it is <c>None</c>.</summary>
    /// <param name="value">The option that must be <c>Some</c>.</param>
    /// <param name="message">Context shown below the description of the failure.</param>
    /// <returns>The value inside <c>Some</c>.</returns>
    static member WantSome (value : 'T option, [<Optional>] message : string | null) : 'T =
        match value with
        | Some some -> some
        | None -> Assert.RaiseFailure ("Assert.WantSome expected Some, but got None.", message, "")

    /// <summary>Fails the test when an option is <c>None</c>.</summary>
    /// <param name="value">The option that must be <c>Some</c>.</param>
    /// <param name="message">Context shown below the description of the failure.</param>
    static member IsSome (value : 'T option, [<Optional>] message : string | null) : unit =
        match value with
        | Some _ -> ()
        | None -> Assert.RaiseFailure ("Assert.IsSome expected Some, but got None.", message, "")

    /// <summary>Fails the test when an option is <c>Some</c>, showing the value it holds.</summary>
    /// <param name="value">The option that must be <c>None</c>.</param>
    /// <param name="message">Context shown below the description of the failure.</param>
    static member IsNone (value : 'T option, [<Optional>] message : string | null) : unit =
        match value with
        | None -> ()
        | Some _ ->
            Assert.RaiseFailure (
                "Assert.IsNone expected None, but got Some.",
                message,
                Formatting.details [ struct ("actual", Formatting.render value) ]
            )

    /// <summary>
    /// Returns the value inside a value option that must be <c>ValueSome</c> and fails the test when it is <c>ValueNone</c>.
    /// </summary>
    /// <param name="value">The value option that must be <c>ValueSome</c>.</param>
    /// <param name="message">Context shown below the description of the failure.</param>
    /// <returns>The value inside <c>ValueSome</c>.</returns>
    static member WantValueSome (value : 'T voption, [<Optional>] message : string | null) : 'T =
        match value with
        | ValueSome some -> some
        | ValueNone -> Assert.RaiseFailure ("Assert.WantValueSome expected ValueSome, but got ValueNone.", message, "")

    /// <summary>Fails the test when a value option is <c>ValueNone</c>.</summary>
    /// <param name="value">The value option that must be <c>ValueSome</c>.</param>
    /// <param name="message">Context shown below the description of the failure.</param>
    static member IsValueSome (value : 'T voption, [<Optional>] message : string | null) : unit =
        match value with
        | ValueSome _ -> ()
        | ValueNone -> Assert.RaiseFailure ("Assert.IsValueSome expected ValueSome, but got ValueNone.", message, "")

    /// <summary>Fails the test when a value option is <c>ValueSome</c>, showing the value it holds.</summary>
    /// <param name="value">The value option that must be <c>ValueNone</c>.</param>
    /// <param name="message">Context shown below the description of the failure.</param>
    static member IsValueNone (value : 'T voption, [<Optional>] message : string | null) : unit =
        match value with
        | ValueNone -> ()
        | ValueSome _ ->
            Assert.RaiseFailure (
                "Assert.IsValueNone expected ValueNone, but got ValueSome.",
                message,
                Formatting.details [ struct ("actual", Formatting.render value) ]
            )

    /// <summary>Returns the value inside a result that must be <c>Ok</c> and fails the test with the error when it is <c>Error</c>.</summary>
    /// <param name="value">The result that must be <c>Ok</c>.</param>
    /// <param name="message">Context shown below the description of the failure.</param>
    /// <returns>The value inside <c>Ok</c>.</returns>
    static member WantOk (value : Result<'T, 'Error>, [<Optional>] message : string | null) : 'T =
        match value with
        | Ok ok -> ok
        | Error _ ->
            Assert.RaiseFailure (
                "Assert.WantOk expected Ok, but got Error.",
                message,
                Formatting.details [ struct ("actual", Formatting.render value) ]
            )

    /// <summary>Fails the test with the error when a result is <c>Error</c>.</summary>
    /// <param name="value">The result that must be <c>Ok</c>.</param>
    /// <param name="message">Context shown below the description of the failure.</param>
    static member IsOk (value : Result<'T, 'Error>, [<Optional>] message : string | null) : unit =
        match value with
        | Ok _ -> ()
        | Error _ ->
            Assert.RaiseFailure (
                "Assert.IsOk expected Ok, but got Error.",
                message,
                Formatting.details [ struct ("actual", Formatting.render value) ]
            )

    /// <summary>
    /// Returns the error inside a result that must be <c>Error</c> and fails the test with the value when it is <c>Ok</c>.
    /// </summary>
    /// <param name="value">The result that must be <c>Error</c>.</param>
    /// <param name="message">Context shown below the description of the failure.</param>
    /// <returns>The error inside <c>Error</c>.</returns>
    static member WantError (value : Result<'T, 'Error>, [<Optional>] message : string | null) : 'Error =
        match value with
        | Error error -> error
        | Ok _ ->
            Assert.RaiseFailure (
                "Assert.WantError expected Error, but got Ok.",
                message,
                Formatting.details [ struct ("actual", Formatting.render value) ]
            )

    /// <summary>Fails the test with the value when a result is <c>Ok</c>.</summary>
    /// <param name="value">The result that must be <c>Error</c>.</param>
    /// <param name="message">Context shown below the description of the failure.</param>
    static member IsError (value : Result<'T, 'Error>, [<Optional>] message : string | null) : unit =
        match value with
        | Error _ -> ()
        | Ok _ ->
            Assert.RaiseFailure (
                "Assert.IsError expected Error, but got Ok.",
                message,
                Formatting.details [ struct ("actual", Formatting.render value) ]
            )

    /// <summary>
    /// Fails the test unless an option is <c>Some</c> with a value structurally equal (F# <c>=</c>) to the expected one.
    /// </summary>
    /// <param name="expected">The value the option must hold.</param>
    /// <param name="actual">The option under test.</param>
    /// <param name="message">Context shown below the description of the failure.</param>
    static member SomeEquals (expected : 'T, actual : 'T option, [<Optional>] message : string | null) : unit =
        match actual with
        | Some value when value = expected -> ()
        | Some _ ->
            Assert.RaiseFailure (
                "Assert.SomeEquals expected the value inside Some to be structurally equal to the expected value.",
                message,
                Formatting.comparison (Formatting.render (Some expected)) (Formatting.render actual)
            )
        | None ->
            Assert.RaiseFailure (
                "Assert.SomeEquals expected Some, but got None.",
                message,
                Formatting.comparison (Formatting.render (Some expected)) (Formatting.render actual)
            )

    /// <summary>
    /// Fails the test unless a value option is <c>ValueSome</c> with a value structurally equal (F# <c>=</c>) to the
    /// expected one.
    /// </summary>
    /// <param name="expected">The value the value option must hold.</param>
    /// <param name="actual">The value option under test.</param>
    /// <param name="message">Context shown below the description of the failure.</param>
    static member ValueSomeEquals (expected : 'T, actual : 'T voption, [<Optional>] message : string | null) : unit =
        match actual with
        | ValueSome value when value = expected -> ()
        | ValueSome _ ->
            Assert.RaiseFailure (
                "Assert.ValueSomeEquals expected the value inside ValueSome to be structurally equal to the expected value.",
                message,
                Formatting.comparison (Formatting.render (ValueSome expected)) (Formatting.render actual)
            )
        | ValueNone ->
            Assert.RaiseFailure (
                "Assert.ValueSomeEquals expected ValueSome, but got ValueNone.",
                message,
                Formatting.comparison (Formatting.render (ValueSome expected)) (Formatting.render actual)
            )

    /// <summary>
    /// Fails the test unless a result is <c>Ok</c> with a value structurally equal (F# <c>=</c>) to the expected one.
    /// </summary>
    /// <param name="expected">The value the result must hold.</param>
    /// <param name="actual">The result under test.</param>
    /// <param name="message">Context shown below the description of the failure.</param>
    static member OkEquals (expected : 'T, actual : Result<'T, 'Error>, [<Optional>] message : string | null) : unit =
        let expectedResult : Result<'T, 'Error> = Ok expected
        match actual with
        | Ok value when value = expected -> ()
        | Ok _ ->
            Assert.RaiseFailure (
                "Assert.OkEquals expected the value inside Ok to be structurally equal to the expected value.",
                message,
                Formatting.comparison (Formatting.render expectedResult) (Formatting.render actual)
            )
        | Error _ ->
            Assert.RaiseFailure (
                "Assert.OkEquals expected Ok, but got Error.",
                message,
                Formatting.comparison (Formatting.render expectedResult) (Formatting.render actual)
            )

    /// <summary>
    /// Fails the test unless a result is <c>Error</c> with an error structurally equal (F# <c>=</c>) to the expected one.
    /// </summary>
    /// <param name="expected">The error the result must hold.</param>
    /// <param name="actual">The result under test.</param>
    /// <param name="message">Context shown below the description of the failure.</param>
    static member ErrorEquals (expected : 'Error, actual : Result<'T, 'Error>, [<Optional>] message : string | null) : unit =
        let expectedResult : Result<'T, 'Error> = Error expected
        match actual with
        | Error error when error = expected -> ()
        | Error _ ->
            Assert.RaiseFailure (
                "Assert.ErrorEquals expected the error inside Error to be structurally equal to the expected error.",
                message,
                Formatting.comparison (Formatting.render expectedResult) (Formatting.render actual)
            )
        | Ok _ ->
            Assert.RaiseFailure (
                "Assert.ErrorEquals expected Error, but got Ok.",
                message,
                Formatting.comparison (Formatting.render expectedResult) (Formatting.render actual)
            )

    /// <summary>
    /// Fails the test unless two values are structurally equal by F# <c>=</c>, showing both rendered by <c>%A</c>.
    /// <para>
    /// Unlike MSTest's <c>Assert.AreEqual</c>, which calls <see cref="M:System.Object.Equals(System.Object)"/>, F#
    /// equality compares arrays element by element and goes into nested records, unions, options, lists and tuples.
    /// </para>
    /// </summary>
    /// <param name="expected">The expected value.</param>
    /// <param name="actual">The value under test.</param>
    /// <param name="message">Context shown below the description of the failure.</param>
    static member StructurallyEquals (expected : 'T, actual : 'T, [<Optional>] message : string | null) : unit =
        if expected <> actual then
            Assert.RaiseFailure (
                "Assert.StructurallyEquals expected the values to be structurally equal.",
                message,
                Formatting.comparison (Formatting.render expected) (Formatting.render actual)
            )

    /// <summary>Fails the test when two values are structurally equal by F# <c>=</c>, showing the value rendered by <c>%A</c>.</summary>
    /// <param name="notExpected">The value the value under test must differ from.</param>
    /// <param name="actual">The value under test.</param>
    /// <param name="message">Context shown below the description of the failure.</param>
    static member NotStructurallyEquals (notExpected : 'T, actual : 'T, [<Optional>] message : string | null) : unit =
        if notExpected = actual then
            Assert.RaiseFailure (
                "Assert.NotStructurallyEquals expected the values to differ, but they are structurally equal.",
                message,
                Formatting.details [
                    struct ("not expected", Formatting.render notExpected)
                    struct ("actual", Formatting.render actual)
                ]
            )

    /// <summary>
    /// Fails the test unless a value equals the default value of its type: <see langword="null"/> for a reference type
    /// (and so <c>None</c> for an option), zero for a number, all fields default for a struct.
    /// </summary>
    /// <param name="value">The value that must be the default of its type.</param>
    /// <param name="message">Context shown below the description of the failure.</param>
    static member IsDefaultOf (value : 'T, [<Optional>] message : string | null) : unit =
        let defaultValue = Unchecked.defaultof<'T>
        // EqualityComparer instead of F# equality: it puts no equality constraint on 'T and handles null
        if not (EqualityComparer<'T>.Default.Equals(value, defaultValue)) then
            Assert.RaiseFailure (
                $"Assert.IsDefaultOf expected the default value of %s{Formatting.typeName typeof<'T>}.",
                message,
                Formatting.comparison (Formatting.render defaultValue) (Formatting.render value)
            )

    /// <summary>
    /// Fails the test unconditionally where an expression needs a value, such as a match case the test must not reach.
    /// </summary>
    /// <param name="message">Context shown below the description of the failure, typically what the test got instead.</param>
    /// <returns>Never returns: the result type lets the call stand where a value is expected.</returns>
    static member FailWithData<'T> ([<Optional>] message : string | null) : 'T =
        Assert.RaiseFailure (
            $"Assert.FailWithData was reached: the test took a path that must not produce a %s{Formatting.typeName typeof<'T>}.",
            message,
            ""
        )
