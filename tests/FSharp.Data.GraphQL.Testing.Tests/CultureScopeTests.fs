namespace FSharp.Data.GraphQL.Testing.Tests

open System
open System.Globalization
open System.Threading.Tasks
open Microsoft.VisualStudio.TestTools.UnitTesting
open FSharp.Data.GraphQL.Testing

// Every test first switches to cultures of its own through an outer scope, so that what the inner scope restores is known
// whatever culture the machine runs with
[<TestClass>]
type CultureScopeTests () =

    let ukrainian = CultureInfo "uk-UA"
    let french = CultureInfo "fr-FR"
    let german = CultureInfo "de-DE"

    let assertCultures (culture : CultureInfo) (uiCulture : CultureInfo) (moment : string) =
        Assert.AreEqual (culture.Name, CultureInfo.CurrentCulture.Name, $"CurrentCulture must be '%s{culture.Name}' %s{moment}")
        Assert.AreEqual (uiCulture.Name, CultureInfo.CurrentUICulture.Name, $"CurrentUICulture must be '%s{uiCulture.Name}' %s{moment}")

    [<TestMethod>]
    member _.``The default scope switches both cultures to the invariant culture and restores them`` () =
        use _ = new CultureScope (ukrainian, french)
        do
            use _ = new CultureScope ()
            assertCultures CultureInfo.InvariantCulture CultureInfo.InvariantCulture "inside the scope"
        assertCultures ukrainian french "after the scope"

    [<TestMethod>]
    member _.``A scope with one culture switches both cultures to it`` () =
        use _ = new CultureScope (ukrainian, french)
        do
            use _ = new CultureScope (german)
            assertCultures german german "inside the scope"
        assertCultures ukrainian french "after the scope"

    [<TestMethod>]
    member _.``A scope with two cultures switches each culture and exposes the previous ones`` () =
        use _ = new CultureScope (ukrainian, french)
        do
            use scope = new CultureScope (german, CultureInfo.InvariantCulture)
            assertCultures german CultureInfo.InvariantCulture "inside the scope"
            Assert.AreEqual (ukrainian, scope.PreviousCulture, "PreviousCulture must be the culture the scope restores")
            Assert.AreEqual (french, scope.PreviousUICulture, "PreviousUICulture must be the UI culture the scope restores")
        assertCultures ukrainian french "after the scope"

    [<TestMethod>]
    member _.``A scope restores both cultures when its body throws`` () =
        use _ = new CultureScope (ukrainian, french)
        Assert.ThrowsExactly<InvalidOperationException>(
            Action (fun () ->
                use _ = new CultureScope ()
                assertCultures CultureInfo.InvariantCulture CultureInfo.InvariantCulture "before the body throws"
                raise (InvalidOperationException "The body failed")),
            "The exception of the body must propagate through the scope"
        )
        |> ignore
        assertCultures ukrainian french "after the body threw"

    [<TestMethod>]
    member _.``Disposing a scope twice does not undo a later switch`` () =
        use _ = new CultureScope (ukrainian, french)
        let scope = new CultureScope (german)
        (scope :> IDisposable).Dispose()
        use _ = new CultureScope (CultureInfo.InvariantCulture)
        (scope :> IDisposable).Dispose()
        assertCultures CultureInfo.InvariantCulture CultureInfo.InvariantCulture "after the second Dispose"

    [<TestMethod>]
    member _.``A scope in an asynchronous test holds across awaits and is restored after them`` () : Task = task {
        use _ = new CultureScope (ukrainian, french)
        do! task {
            use _ = new CultureScope (german)
            do! Task.Yield ()
            assertCultures german german "after an await inside the scope"
            let! culture = Task.Run (fun () -> CultureInfo.CurrentCulture.Name)
            Assert.AreEqual (german.Name, culture, "A task started inside the scope must see its culture")
        }
        assertCultures ukrainian french "after the asynchronous scope"
    }

    [<TestMethod>]
    member _.``A culture switched inside a task does not flow back to its caller`` () : Task = task {
        use _ = new CultureScope (ukrainian, french)
        // The caveat of CultureScope: a scope created inside a task and handed out switches nothing for the caller
        let! scope = task {
            do! Task.Yield ()
            return new CultureScope (german)
        }
        assertCultures ukrainian french "after the task that created a scope returned"
        (scope :> IDisposable).Dispose()
        assertCultures ukrainian french "after disposing the scope outside the task"
    }
