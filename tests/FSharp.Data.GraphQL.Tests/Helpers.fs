/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
[<AutoOpen>]
module Helpers

open System
open System.Collections.Generic
open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Introspection
open FSharp.Data.GraphQL.Execution

let equals (expected : 'x) (actual : 'x) = 
    Assert.True((actual = expected), sprintf "expected %+A\nbut got %+A" expected actual)
let noErrors (result: IDictionary<string, obj>) =
    match result.TryGetValue("errors") with
    | true, errors -> failwithf "expected ExecutionResult to have no errors but got %+A" errors
    | false, _ -> ()
let throws<'e when 'e :> exn> (action : unit -> unit) = Assert.Throws<'e>(action)
let sync = Async.RunSynchronously
let is<'t> (o: obj) = o :? 't
let hasError errMsg (errors: string seq) =
    let containsMessage = 
        errors
        |> Seq.exists (fun e -> e.Contains(errMsg))
    Assert.True (containsMessage, sprintf "expected to contain message '%s', but no such message was found. Messages found: %A" errMsg errors)

let (<??) opt other = 
    match opt with
    | None -> Some other
    | _ -> opt

