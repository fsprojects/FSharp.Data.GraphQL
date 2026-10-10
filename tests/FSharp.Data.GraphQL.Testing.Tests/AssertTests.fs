namespace FSharp.Data.GraphQL.Testing.Tests

open System
open System.Threading.Tasks
open Microsoft.VisualStudio.TestTools.UnitTesting
open FSharp.Data.GraphQL.Testing

/// A record whose structural equality differs from reference equality.
type Point = { X : int; Y : int }

/// A struct whose default value has all fields zero.
[<Struct>]
type Size = { Width : int; Height : int }

[<TestClass>]
type AssertTests () =

    // Options

    [<TestMethod>]
    member _.``WantSome returns the value inside Some`` () =
        Assert.AreEqual (42, Assert.WantSome (Some 42), "WantSome must return the value inside Some")

    [<TestMethod>]
    member _.``WantSome fails on None`` () =
        assertFailureContains [ "Assert.WantSome expected Some, but got None." ] (fun () -> Assert.WantSome (None : int option) |> ignore)

    [<TestMethod>]
    member _.``WantSome failure includes the caller's message`` () =
        assertFailureContains [ "Assert.WantSome expected Some, but got None."; "The parser must return a document" ] (fun () ->
            Assert.WantSome ((None : int option), "The parser must return a document")
            |> ignore)

    [<TestMethod>]
    member _.``IsSome passes on Some`` () = Assert.IsSome (Some "value", "IsSome must accept Some")

    [<TestMethod>]
    member _.``IsSome fails on None`` () =
        assertFailureContains [ "Assert.IsSome expected Some, but got None." ] (fun () -> Assert.IsSome (None : string option))

    [<TestMethod>]
    member _.``IsNone passes on None`` () = Assert.IsNone ((None : int option), "IsNone must accept None")

    [<TestMethod>]
    member _.``IsNone fails on Some and shows the value`` () =
        assertFailureContains [ "Assert.IsNone expected None, but got Some."; "actual: Some 42" ] (fun () -> Assert.IsNone (Some 42))

    // Value options

    [<TestMethod>]
    member _.``WantValueSome returns the value inside ValueSome`` () =
        Assert.AreEqual ("value", Assert.WantValueSome (ValueSome "value"), "WantValueSome must return the value inside ValueSome")

    [<TestMethod>]
    member _.``WantValueSome fails on ValueNone`` () =
        assertFailureContains [ "Assert.WantValueSome expected ValueSome, but got ValueNone." ] (fun () ->
            Assert.WantValueSome (ValueNone : int voption) |> ignore)

    [<TestMethod>]
    member _.``IsValueSome passes on ValueSome`` () = Assert.IsValueSome (ValueSome 1, "IsValueSome must accept ValueSome")

    [<TestMethod>]
    member _.``IsValueSome fails on ValueNone`` () =
        assertFailureContains [ "Assert.IsValueSome expected ValueSome, but got ValueNone." ] (fun () -> Assert.IsValueSome (ValueNone : int voption))

    [<TestMethod>]
    member _.``IsValueNone passes on ValueNone`` () =
        Assert.IsValueNone ((ValueNone : int voption), "IsValueNone must accept ValueNone")

    [<TestMethod>]
    member _.``IsValueNone fails on ValueSome and shows the value`` () =
        assertFailureContains [ "Assert.IsValueNone expected ValueNone, but got ValueSome."; "actual: ValueSome 42" ] (fun () ->
            Assert.IsValueNone (ValueSome 42))

    // Results

    [<TestMethod>]
    member _.``WantOk returns the value inside Ok`` () =
        Assert.AreEqual (42, Assert.WantOk (Ok 42 : Result<int, string>), "WantOk must return the value inside Ok")

    [<TestMethod>]
    member _.``WantOk fails on Error and shows the error`` () =
        assertFailureContains [ "Assert.WantOk expected Ok, but got Error."; "actual: Error \"boom\"" ] (fun () ->
            Assert.WantOk (Error "boom" : Result<int, string>) |> ignore)

    [<TestMethod>]
    member _.``IsOk passes on Ok`` () = Assert.IsOk ((Ok () : Result<unit, string>), "IsOk must accept Ok")

    [<TestMethod>]
    member _.``IsOk fails on Error and shows the error`` () =
        assertFailureContains [ "Assert.IsOk expected Ok, but got Error."; "actual: Error \"boom\"" ] (fun () ->
            Assert.IsOk (Error "boom" : Result<int, string>))

    [<TestMethod>]
    member _.``WantError returns the error inside Error`` () =
        Assert.AreEqual ("boom", Assert.WantError (Error "boom" : Result<int, string>), "WantError must return the error inside Error")

    [<TestMethod>]
    member _.``WantError fails on Ok and shows the value`` () =
        assertFailureContains [ "Assert.WantError expected Error, but got Ok."; "actual: Ok 42" ] (fun () ->
            Assert.WantError (Ok 42 : Result<int, string>) |> ignore)

    [<TestMethod>]
    member _.``IsError passes on Error`` () =
        Assert.IsError ((Error "boom" : Result<int, string>), "IsError must accept Error")

    [<TestMethod>]
    member _.``IsError fails on Ok and shows the value`` () =
        assertFailureContains [ "Assert.IsError expected Error, but got Ok."; "actual: Ok 42" ] (fun () ->
            Assert.IsError (Ok 42 : Result<int, string>))

    // Equality of wrapped values

    [<TestMethod>]
    member _.``SomeEquals passes on Some with a structurally equal value`` () =
        Assert.SomeEquals ([| 1; 2 |], Some [| 1; 2 |], "SomeEquals must compare arrays element by element")

    [<TestMethod>]
    member _.``SomeEquals fails on Some with another value`` () =
        assertFailureContains
            [
                "Assert.SomeEquals expected the value inside Some to be structurally equal to the expected value."
                "expected: Some 1"
                "actual:   Some 2"
            ]
            (fun () -> Assert.SomeEquals (1, Some 2))

    [<TestMethod>]
    member _.``SomeEquals fails on None`` () =
        assertFailureContains [ "Assert.SomeEquals expected Some, but got None."; "expected: Some 1"; "actual:   None" ] (fun () ->
            Assert.SomeEquals (1, None))

    [<TestMethod>]
    member _.``ValueSomeEquals passes on ValueSome with a structurally equal value`` () =
        Assert.ValueSomeEquals ({ X = 1; Y = 2 }, ValueSome { X = 1; Y = 2 }, "ValueSomeEquals must compare records structurally")

    [<TestMethod>]
    member _.``ValueSomeEquals fails on ValueSome with another value`` () =
        assertFailureContains
            [
                "Assert.ValueSomeEquals expected the value inside ValueSome to be structurally equal to the expected value."
                "expected: ValueSome 1"
                "actual:   ValueSome 2"
            ]
            (fun () -> Assert.ValueSomeEquals (1, ValueSome 2))

    [<TestMethod>]
    member _.``ValueSomeEquals fails on ValueNone`` () =
        assertFailureContains [ "Assert.ValueSomeEquals expected ValueSome, but got ValueNone."; "actual:   ValueNone" ] (fun () ->
            Assert.ValueSomeEquals (1, ValueNone))

    [<TestMethod>]
    member _.``OkEquals passes on Ok with a structurally equal value`` () =
        Assert.OkEquals ([ 1; 2 ], (Ok [ 1; 2 ] : Result<int list, string>), "OkEquals must compare lists structurally")

    [<TestMethod>]
    member _.``OkEquals fails on Ok with another value`` () =
        assertFailureContains
            [
                "Assert.OkEquals expected the value inside Ok to be structurally equal to the expected value."
                "expected: Ok 1"
                "actual:   Ok 2"
            ]
            (fun () -> Assert.OkEquals (1, (Ok 2 : Result<int, string>)))

    [<TestMethod>]
    member _.``OkEquals fails on Error and shows the error`` () =
        assertFailureContains [ "Assert.OkEquals expected Ok, but got Error."; "actual:   Error \"boom\"" ] (fun () ->
            Assert.OkEquals (1, (Error "boom" : Result<int, string>)))

    [<TestMethod>]
    member _.``ErrorEquals passes on Error with a structurally equal error`` () =
        Assert.ErrorEquals ("boom", (Error "boom" : Result<int, string>), "ErrorEquals must accept an equal error")

    [<TestMethod>]
    member _.``ErrorEquals fails on Error with another error`` () =
        assertFailureContains
            [
                "Assert.ErrorEquals expected the error inside Error to be structurally equal to the expected error."
                "expected: Error \"boom\""
                "actual:   Error \"bang\""
            ]
            (fun () -> Assert.ErrorEquals ("boom", (Error "bang" : Result<int, string>)))

    [<TestMethod>]
    member _.``ErrorEquals fails on Ok and shows the value`` () =
        assertFailureContains [ "Assert.ErrorEquals expected Error, but got Ok."; "actual:   Ok 42" ] (fun () ->
            Assert.ErrorEquals ("boom", (Ok 42 : Result<int, string>)))

    // Structural equality

    [<TestMethod>]
    member _.``StructurallyEquals compares arrays element by element where AreEqual compares references`` () =
        let expected = [| { X = 1; Y = 2 } |]
        let actual = [| { X = 1; Y = 2 } |]
        Assert.AreNotEqual<Point array>(expected, actual, "MSTest must compare the two arrays by reference")
        Assert.StructurallyEquals (expected, actual, "StructurallyEquals must compare the arrays element by element")

    [<TestMethod>]
    member _.``StructurallyEquals fails on different values and shows both`` () =
        assertFailureContains
            [
                "Assert.StructurallyEquals expected the values to be structurally equal."
                "expected: { X = 1"
                "actual:   { X = 3"
            ]
            (fun () -> Assert.StructurallyEquals ({ X = 1; Y = 2 }, { X = 3; Y = 2 }))

    [<TestMethod>]
    member _.``StructurallyEquals notes values that render the same but differ`` () =
        assertFailureContains [ "expected: 0.3"; "actual:   0.3"; "Both values render the same" ] (fun () ->
            Assert.StructurallyEquals (0.3, 0.1 + 0.2))

    [<TestMethod>]
    member _.``StructurallyEquals indents the continuation lines of a multi-line value`` () =
        let continuation =
            Environment.NewLine
            + String (' ', "expected: ".Length)
            + " "
        assertFailureContains [ continuation ] (fun () -> Assert.StructurallyEquals ([ 1..40 ], [ 1..41 ]))

    [<TestMethod>]
    member _.``NotStructurallyEquals passes on different values`` () =
        Assert.NotStructurallyEquals ([ 1 ], [ 2 ], "NotStructurallyEquals must accept different lists")

    [<TestMethod>]
    member _.``NotStructurallyEquals fails on equal values and shows them`` () =
        assertFailureContains
            [
                "Assert.NotStructurallyEquals expected the values to differ, but they are structurally equal."
                "not expected: [1; 2]"
                "actual:       [1; 2]"
            ]
            (fun () -> Assert.NotStructurallyEquals ([ 1; 2 ], [ 1; 2 ]))

    // Default values

    [<TestMethod>]
    member _.``IsDefaultOf passes on default values`` () =
        Assert.IsDefaultOf (0, "IsDefaultOf must accept zero")
        Assert.IsDefaultOf ((null : string | null), "IsDefaultOf must accept a null string")
        Assert.IsDefaultOf ((None : int option), "IsDefaultOf must accept None, whose representation is null")
        Assert.IsDefaultOf ({ Width = 0; Height = 0 }, "IsDefaultOf must accept a struct with default fields")

    [<TestMethod>]
    member _.``IsDefaultOf fails on another value and names the type`` () =
        assertFailureContains
            [
                "Assert.IsDefaultOf expected the default value of Size."
                "expected: { Width = 0"
                "actual:   { Width = 1"
            ]
            (fun () -> Assert.IsDefaultOf { Width = 1; Height = 0 })

    [<TestMethod>]
    member _.``IsDefaultOf names generic types the way they read in source`` () =
        assertFailureContains [ "the default value of FSharpOption<Int32>."; "actual:   Some 1" ] (fun () -> Assert.IsDefaultOf (Some 1))

    // Failing where a value is expected

    [<TestMethod>]
    member _.``FailWithData stands where a value is expected and fails`` () =
        let pick (value : int option) : int =
            match value with
            | Some value -> value
            | None -> Assert.FailWithData "The sample has no value"
        Assert.AreEqual (1, pick (Some 1), "FailWithData must not affect the other match cases")
        assertFailureContains [ "Assert.FailWithData was reached"; "must not produce a Int32."; "The sample has no value" ] (fun () ->
            pick None |> ignore)

    [<TestMethod>]
    member _.``FailWithData fails without a message`` () =
        assertFailureContains [ "Assert.FailWithData was reached: the test took a path that must not produce a String." ] (fun () ->
            Assert.FailWithData<string>() |> ignore)

    // Message layout

    [<TestMethod>]
    member _.``A failure lays out the description, the caller's message and the details like MSTest`` () =
        let message = failureOf (fun () -> Assert.IsNone (Some 42, "The cache must be empty"))
        let expected =
            lines [
                "Assertion failed."
                "Assert.IsNone expected None, but got Some."
                "The cache must be empty"
                ""
                "actual: Some 42"
            ]
        Assert.AreEqual (expected.ReplaceLineEndings (), message.ReplaceLineEndings (), "The failure message must be laid out like MSTest's own")

    [<TestMethod>]
    member _.``A blank caller's message is left out`` () =
        let message = failureOf (fun () -> Assert.IsSome ((None : int option), "   "))
        Assert.EndsWith ("Assert.IsSome expected Some, but got None.", message, StringComparison.Ordinal, "A blank message must add no line")

    [<TestMethod>]
    member _.``A failure throws even inside an assertion scope, where MSTest's own assertions defer`` () =
        // Want members have no value to return after a failure, so they must not join MSTest's soft assertions
        #nowarn "57"
        let scope = Assert.Scope ()
        #warnon "57"
        try
            assertFailureContains [ "Assert.WantSome expected Some, but got None." ] (fun () -> Assert.WantSome (None : int option) |> ignore)
        finally
            scope.Dispose ()

    [<TestMethod>]
    member _.``Assertions work in asynchronous tests`` () : Task = task {
        let! value = Task.FromResult (Some 42)
        Assert.SomeEquals (42, value, "The awaited option must hold 42")
    }
