/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

[<AutoOpen>]
module Helpers

open System
open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types

let equals (expected : 'x) (actual : 'x) = Assert.True ((actual = expected), sprintf "expected %+A\nbut got %+A" expected actual)
let throws<'e when 'e :> exn> (action: unit -> unit) = Assert.Throws<'e>(action)

let sync = Async.RunSynchronously
let field name typedef (resolve: 'a -> 'b) = Schema.Field(name = name, schema = typedef, resolve = resolve)
let fieldA name typedef args (resolve: 'a * Args -> 'b) = Schema.Field<'a,'b>(name = name, schema = typedef, arguments = args, resolve = resolve)
let arg name typedef = Schema.Argument(name, typedef)
let objdef name fields = Schema.ObjectType(name, fields)
let (<??) opt other =
    match opt with
    | None -> Some other
    | _ -> opt
