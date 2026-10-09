// Both MSTest and this repository's Testing library declare a type named Assert. F# looks a static member such as
// Assert.HasCount up on every type of that name in scope, so a test file can call the members of both, whichever namespace
// it opens last. These two namespace groups open them in both orders; the project would not compile if either order hid
// one of the types.
namespace FSharp.Data.GraphQL.Testing.Tests.MSTestOpenedLast

open System
// Deliberately out of the usual order: MSTest's namespace is opened after the Testing library's
open FSharp.Data.GraphQL.Testing
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type AssertResolutionTests () =

    [<TestMethod>]
    member _.``Both Assert types resolve when MSTest is opened last`` () =
        let values = Assert.WantSome (Some [ 1; 2; 3 ], "The Testing library's WantSome must resolve")
        Assert.HasCount (3, values, "MSTest's HasCount must resolve")
        Assert.StructurallyEquals ([ 1; 2; 3 ], values, "The Testing library's StructurallyEquals must resolve")
        Assert.IsTrue (List.contains 2 values, "MSTest's IsTrue must resolve")
        Assert.Contains ("b", "abc", StringComparison.Ordinal, "MSTest's string Contains must resolve")

namespace FSharp.Data.GraphQL.Testing.Tests.TestingLibraryOpenedLast

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open FSharp.Data.GraphQL.Testing

[<TestClass>]
type AssertResolutionTests () =

    [<TestMethod>]
    member _.``Both Assert types resolve when the Testing library is opened last`` () =
        let values =
            Assert.WantOk ((Ok [| "a"; "b" |] : Result<string array, string>), "The Testing library's WantOk must resolve")
        Assert.HasCount (2, values, "MSTest's HasCount must resolve")
        CollectionAssert.AreEqual ([| "a"; "b" |], values, "MSTest's CollectionAssert must compare the arrays")
        Assert.IsValueNone ((ValueNone : int voption), "The Testing library's IsValueNone must resolve")
        Assert.AreEqual (
            "a",
            Assert.ContainsSingle (
                Array.filter (fun value -> String.Equals (value, "a", StringComparison.Ordinal)) values,
                "MSTest's ContainsSingle must resolve"
            ),
            "The single element must be 'a'"
        )
