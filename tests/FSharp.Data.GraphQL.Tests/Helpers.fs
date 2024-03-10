// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc
[<AutoOpen>]
module internal Helpers

open System
open System.Collections.Generic
open System.Linq
open System.Text.Json.Serialization
open System.Threading
open System.Threading.Tasks
open Xunit
open FSharp.Data.GraphQL

let isType<'a> actual = Assert.IsAssignableFrom<'a>(actual)
let isSeq<'a> actual = isType<'a seq> actual
let isDict<'k, 'v> actual = isSeq<KeyValuePair<'k, 'v>> actual
let isNameValueDict actual = isDict<string, obj> actual
let fail (message: string) = Assert.Fail message
let equals (expected : 'x) (actual : 'x) =
    if not (actual = expected) then fail <| $"expected %A{expected}{Environment.NewLine}but got %A{actual}"
let notEquals (expected : 'x) (actual : 'x) =
    if actual = expected then fail <| $"unexpected %+A{expected}"
let noErrors (result: IDictionary<string, obj>) =
    match result.TryGetValue("errors") with
    | true, errors -> fail <| sprintf "expected ExecutionResult to have no errors but got %+A" errors
    | false, _ -> ()
let nonEmpty (xs : 'a seq) =
    Assert.False(Seq.isEmpty xs, sprintf "expected non-empty sequence, but got %A" xs)
let empty (xs: 'a seq) =
    Assert.True(Seq.isEmpty xs, sprintf "expected empty sequence, but got %A" xs)
let single (xs : 'a seq) =
    let length = Seq.length xs
    if length <> 1
    then fail <| sprintf "Expected single item in sequence, but found %i items.\n%A" length xs
    Seq.head xs
let throws<'e when 'e :> exn> (action : unit -> unit) = Assert.Throws<'e>(action)
let throwsAsync<'e when 'e :> exn> (action : unit Async) = Assert.ThrowsAsync<'e>(fun () -> Async.StartAsTask(action))
let throwsAsyncVal<'e when 'e :> exn> (action : unit AsyncVal) = Assert.ThrowsAsync<'e>(fun () -> Async.StartAsTask(action |> AsyncVal.toAsync))
let sync = Async.RunSynchronously
let is<'t> (o: obj) = o :? 't

let hasError (errMsg : string) (errors: GQLProblemDetails seq) =
    let containsMessage = errors |> Seq.exists (fun pd -> pd.Message.Contains(errMsg))
    Assert.True (containsMessage, sprintf "Expected to contain message '%s', but no such message was found. Messages found: %A" errMsg errors)

let hasErrorAtPath path (errMsg : string) (errors: GQLProblemDetails seq) =
    match errors |> Seq.where (fun pd -> pd.Message.Contains(errMsg)) |> Seq.tryHead with
    | Some error ->
        error.Path
        |> Skippable.filter (fun pathValue -> Assert.True((pathValue = path), $"Expected that message '%s{errMsg}' has path {path}, but path {pathValue} found."); true)
        |> Skippable.defaultWith (fun () -> Assert.Fail($"Expected that message '%s{errMsg}' has path {path}, but no path found."); []) |> ignore
    | None ->
        Assert.Fail ($"Expected to contain message '%s{errMsg}', but no such message was found. Messages found: %A{errors}")

let (<??) opt other =
    match opt with
    | None -> Some other
    | _ -> opt
let undefined (value: 't) =
    Assert.True((value = Unchecked.defaultof<'t>), sprintf "Expected value to be undefined, but was: %A" value)
let contains (expected : 'a) (xs : 'a seq) =
    Assert.Contains(expected, xs); xs
let itemEquals (index : int) (expected : 'a) (xs : 'a seq) =
    match xs |> Seq.tryItem index with
    | Some item -> item |> equals expected
    | None -> fail <| sprintf "Expected sequence to contain item at index %i, but sequence does not contain enough elements" index
    xs
let seqEquals (expected : 'a seq) (actual : 'a seq) =
    Assert.Equal<'a>(expected, actual)

let greaterThanOrEqual expected actual =
    Assert.True(actual >= expected, sprintf "Expected value to be greather than or equal to %A, but was: %A" expected actual)

open System.Text.Json
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Server.AspNetCore

let stringifyArg name (ctx : ResolveFieldContext) () =
    let arg = ctx.TryArg name |> Option.toObj
    JsonSerializer.Serialize (arg, Json.serializerOptions)

let stringifyInput = stringifyArg "input"


open FSharp.Data.GraphQL.Parser

let asts query =
    ["defer"; "stream"]
    |> Seq.map (query >> parse)

let setEvent (mre : ManualResetEvent) =
    mre.Set() |> ignore

let resetEvent (mre : ManualResetEvent) =
    mre.Reset() |> ignore

let waitEvent (mre : ManualResetEvent) errorMsg =
    if TimeSpan.FromSeconds(float 30) |> mre.WaitOne |> not
    then fail errorMsg

let rec waitFor (condition : unit -> bool) (times : int) errorMsg =
    Thread.Sleep(100) // Wait a bit before checking condition
    if not (condition ())
    then
        if times = 0
        then fail errorMsg
        else waitFor condition (times - 1) errorMsg

let rec ensureThat (condition : unit -> bool) (times : int) errorMsg =
    Thread.Sleep(100) // Wait a bit before checking condition
    if not (condition ())
    then fail errorMsg
    elif times > 0
    then ensureThat condition (times - 1) errorMsg

let ms x =
    let factor =
        match Environment.ProcessorCount with
        | x when x >= 8 -> 1
        | x when x >= 4 -> 5
        | _ -> 20
    x * factor

type TestObserver<'T>(obs : IObservable<'T>, ?onReceived : TestObserver<'T> -> 'T -> unit) as this =
    let received = List<'T>()
    let mutable isCompleted = false
    let mre = new ManualResetEvent(false)
    let mutable subscription = Unchecked.defaultof<IDisposable>
    do subscription <- obs.Subscribe(this)
    member _.Received
        with get() = received.AsEnumerable()
    member _.WaitCompleted(?expectedItemCount, ?timeout) =
        let ms = defaultArg timeout 30
        if TimeSpan.FromSeconds(float ms) |> mre.WaitOne |> not
        then fail "Timeout waiting for OnCompleted"
        match expectedItemCount with
        | Some x ->
            if received.Count < x
            then failwithf "Expected to receive %i items, but received %i\nItems: %A" x received.Count received
        | None -> ()
    member _.WaitForItems(expectedItemCount) =
        let errorMsg = sprintf "Expected to receive least %i items, but received %i\nItems: %A" expectedItemCount received.Count received
        waitFor (fun () -> received.Count = expectedItemCount) (expectedItemCount * 100) errorMsg
    member x.WaitForItem() = x.WaitForItems(1)
    member _.IsCompleted
        with get() = isCompleted
    interface IObserver<'T> with
        member _.OnCompleted() =
            isCompleted <- true
            mre.Set() |> ignore
        member _.OnError(error) = error.Reraise()
        member _.OnNext(value) =
            received.Add(value)
            onReceived |> Option.iter (fun evt -> evt this value)
    interface IDisposable with
        member _.Dispose() =
            subscription.Dispose()
            mre.Dispose()

[<RequireQualifiedAccess>]
module Observer =
    let create (sub : IObservable<'T>) =
        new TestObserver<'T>(sub)

    let createWithCallback (onReceive : TestObserver<'T> -> 'T -> unit) (sub : IObservable<'T>) =
        new TestObserver<'T>(sub, onReceive)

open System.Runtime.CompilerServices

[<Extension>]
type ExecutorExtensions =

    [<Extension>]
    static member CreateExecutionPlanOrFail (executor: Executor<'Root>, queryOrMutation: string, ?operationName: string, ?meta : Metadata) =
        match executor.CreateExecutionPlan(queryOrMutation, ?operationName = operationName, ?meta = meta) with
        | Ok executionPlan -> executionPlan
        | Error _ -> fail "invalid query"; Unchecked.defaultof<_>
