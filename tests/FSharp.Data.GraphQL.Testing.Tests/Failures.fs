/// Runs assertions that must fail and checks their failure messages.
[<AutoOpen>]
module internal FSharp.Data.GraphQL.Testing.Tests.Failures

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

/// Runs an assertion that must fail and returns its failure message.
let failureOf (assertion : unit -> unit) : string =
    let failure = Assert.ThrowsExactly<AssertFailedException>(Action assertion, "The assertion must fail")
    failure.Message

/// Runs an assertion that must fail and checks that its failure message contains every fragment.
let assertFailureContains (fragments : string list) (assertion : unit -> unit) : unit =
    let message = failureOf assertion
    for fragment in fragments do
        Assert.Contains (fragment, message, StringComparison.Ordinal, $"The failure message must contain '%s{fragment}'")

/// Joins lines the way the failure messages do.
let lines (lines : string list) = String.concat Environment.NewLine lines
