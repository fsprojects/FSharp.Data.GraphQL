namespace FSharp.Data.GraphQL.Testing

open System
open System.Globalization

/// <summary>
/// Switches <see cref="P:System.Globalization.CultureInfo.CurrentCulture"/> and
/// <see cref="P:System.Globalization.CultureInfo.CurrentUICulture"/> for the lifetime of a <see langword="use"/> binding
/// and restores the previous cultures on <see cref="M:System.IDisposable.Dispose"/>, also when the body throws. It replaces
/// the <c>UseInvariantCulture</c> attribute of the xUnit test projects:
/// <code>use _ = new CultureScope ()</code>
/// <para>
/// Both cultures live in the execution context, like an <see cref="T:System.Threading.AsyncLocal`1"/>: a switch flows
/// into the awaits of the same <c>task { }</c> and the tasks started inside the scope, and never reaches tests running in
/// parallel. The one caveat is where the scope is created: create and dispose it in the same method body (a test
/// method, synchronous or a <c>task { }</c>). A scope created inside a <c>task { }</c> and handed to its caller switches
/// nothing for the caller, because what a task changes in the execution context never flows back out of the task.
/// </para>
/// </summary>
[<Sealed>]
type CultureScope
    /// <param name="culture">The culture to make <see cref="P:System.Globalization.CultureInfo.CurrentCulture"/>.</param>
    /// <param name="uiCulture">The culture to make <see cref="P:System.Globalization.CultureInfo.CurrentUICulture"/>.</param>
    (culture : CultureInfo, uiCulture : CultureInfo) =

    let previousCulture = CultureInfo.CurrentCulture
    let previousUICulture = CultureInfo.CurrentUICulture
    let mutable disposed = false

    do
        CultureInfo.CurrentCulture <- culture
        CultureInfo.CurrentUICulture <- uiCulture

    /// <summary>
    /// Switches both cultures to <see cref="P:System.Globalization.CultureInfo.InvariantCulture"/>, so that formatting and
    /// parsing in a test do not depend on the culture of the machine running it.
    /// </summary>
    new () = new CultureScope (CultureInfo.InvariantCulture, CultureInfo.InvariantCulture)

    /// <summary>Switches both cultures to the same culture.</summary>
    /// <param name="culture">
    /// The culture to make both <see cref="P:System.Globalization.CultureInfo.CurrentCulture"/> and
    /// <see cref="P:System.Globalization.CultureInfo.CurrentUICulture"/>.
    /// </param>
    new (culture : CultureInfo) = new CultureScope (culture, culture)

    /// <summary>The <see cref="P:System.Globalization.CultureInfo.CurrentCulture"/> this scope restores.</summary>
    member _.PreviousCulture = previousCulture

    /// <summary>The <see cref="P:System.Globalization.CultureInfo.CurrentUICulture"/> this scope restores.</summary>
    member _.PreviousUICulture = previousUICulture

    interface IDisposable with

        /// <summary>
        /// Restores the cultures that were current when the scope was created. Disposing the scope again does nothing,
        /// so a second call cannot undo a culture switch made after the first one.
        /// </summary>
        member _.Dispose () =
            if not disposed then
                disposed <- true
                CultureInfo.CurrentCulture <- previousCulture
                CultureInfo.CurrentUICulture <- previousUICulture
