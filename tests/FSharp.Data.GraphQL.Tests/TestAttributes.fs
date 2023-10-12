namespace FSharp.Data.GraphQL.Tests

open System
open System.Globalization
open Xunit.Sdk

type UseInvariantCultureAttribute() =
    inherit BeforeAfterTestAttribute()

    let mutable _originalUICulture: CultureInfo = null
    let mutable _originalCulture: CultureInfo = null

    override _.Before (methodUnderTest) =
        _originalUICulture <- CultureInfo.CurrentUICulture
        _originalCulture <- CultureInfo.CurrentCulture

        CultureInfo.CurrentUICulture <- CultureInfo.InvariantCulture
        CultureInfo.CurrentCulture <- CultureInfo.InvariantCulture

    override _.After (methodUnderTest) =
        CultureInfo.CurrentUICulture <- _originalUICulture
        CultureInfo.CurrentCulture <- _originalCulture
