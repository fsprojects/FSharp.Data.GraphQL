/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

[<AutoOpen>]
module FSharp.Data.GraphQL.Tests.Helpers

open System
open Xunit

let equals (expected : 'x) (actual : 'x) = Assert.True ((actual = expected), sprintf "expected %+A\nbut got %+A" expected actual)
let throws<'e when 'e :> exn> (action: unit -> unit) = Assert.Throws<'e>(action)

