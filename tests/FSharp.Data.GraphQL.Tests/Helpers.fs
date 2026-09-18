// The MIT License (MIT)
// Copyright (c) 2016 Bazinga Technologies Inc
[<AutoOpen>]
module internal Helpers

open System
open System.Collections.Generic
open System.IO
open System.Linq
open System.Text
open System.Text.Json.Serialization
open System.Threading
open FSharp.Data.GraphQL
open Xunit

let serializerOptions = Shared.Json.getWSSerializerOptions Seq.empty

let isType<'a> actual = Assert.IsAssignableFrom<'a>(actual)
let isSeq<'a> actual = isType<'a seq> actual
let isDict<'k, 'v> actual = isSeq<KeyValuePair<'k, 'v>> actual
let isNameValueDict actual = isDict<string, obj> actual
let fail (message: string) = Assert.Fail message
let wantSome opt = match opt with | Some value -> value | None -> fail "Expected Some but got None"; Unchecked.defaultof<_>
let wantNone opt = match opt with | None -> () | Some _ -> fail "Expected None but got Some"
let wantValueSome opt = match opt with | ValueSome value -> value | _ -> fail "Expected ValueSome but got ValueNone"; Unchecked.defaultof<_>
let wantValueNone opt = match opt with | ValueNone -> () | _ -> fail "Expected ValueNone but got ValueSome"
let equals (expected : 'x) (actual : 'x) =
    if not (actual = expected) then fail <| $"expected %A{expected}\nbut got %A{actual}"
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
let throwsAsync<'e when 'e :> exn> (action : unit Async) = Assert.ThrowsAsync<'e>(fun () -> Async.StartImmediateAsTask action)
let throwsAsyncVal<'e when 'e :> exn> (action : unit AsyncVal) = Assert.ThrowsAsync<'e>(fun () -> Async.StartImmediateAsTask (action |> AsyncVal.toAsync))
let sync = Async.RunSynchronously
let is<'t> (o: obj) = o :? 't

let hasError (errMsg : string) (errors : GQLProblemDetails seq) =
    let containsMessage = errors |> Seq.exists (fun pd -> pd.Message.Contains(errMsg))
    Assert.True (containsMessage, sprintf "Expected to contain message '%s', but no such message was found. Messages found: %A" errMsg errors)

let hasErrorAtPath path (errMsg : string) (errors: GQLProblemDetails seq) =
    match errors |> Seq.where (fun pd -> pd.Message.Contains errMsg) |> Seq.tryHead with
    | Some error ->
        error.Path
        |> Skippable.filter (fun pathValue -> Assert.True ((pathValue = path), $"Expected that message '%s{errMsg}' has path {path}, but path {pathValue} found."); true)
        |> Skippable.defaultWith (fun () -> Assert.Fail ($"Expected that message '%s{errMsg}' has path {path}, but no path found."); []) |> ignore
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
    Assert.True (actual >= expected, sprintf "Expected value to be greather than or equal to %A, but was: %A" expected actual)

open System.Text.Json
open FSharp.Data.GraphQL.Types

let stringifyArg name (ctx : ResolveFieldContext) () =
    let arg = ctx.TryArg name |> ValueOption.toObj
    JsonSerializer.Serialize (arg, serializerOptions)

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
    Thread.Sleep 100 // Wait a bit before checking condition
    if not (condition ())
    then
        if times = 0
        then fail errorMsg
        else waitFor condition (times - 1) errorMsg

let rec ensureThat (condition : unit -> bool) (times : int) errorMsg =
    Thread.Sleep 100 // Wait a bit before checking condition
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

type TestObserver<'T>(obs : IObservable<'T>, [<Struct>] ?onReceived : TestObserver<'T> -> 'T -> unit) as this =
    let received = List<'T>()
    let mutable isCompleted = false
    let mre = new ManualResetEvent(false)
    let mutable subscription = Unchecked.defaultof<IDisposable>
    do subscription <- obs.Subscribe(this)
    member _.Received = received.AsEnumerable()
    member _.WaitCompleted (?expectedItemCount, ?timeout) =
        let ms = defaultArg timeout 30
        if TimeSpan.FromSeconds (float ms) |> mre.WaitOne |> not
        then fail "Timeout waiting for OnCompleted"
        match expectedItemCount with
        | Some x ->
            if received.Count < x
            then failwithf "Expected to receive %i items, but received %i\nItems: %A" x received.Count received
        | None -> ()
    member _.WaitForItems (expectedItemCount) =
        let errorMsg = sprintf "Expected to receive least %i items, but received %i\nItems: %A" expectedItemCount received.Count received
        waitFor (fun () -> received.Count = expectedItemCount) (expectedItemCount * 100) errorMsg
    member x.WaitForItem () = x.WaitForItems(1)
    member _.IsCompleted
        with get() = isCompleted
    interface IObserver<'T> with
        member _.OnCompleted () =
            isCompleted <- true
            mre.Set() |> ignore
        member _.OnError (error) = error.Reraise()
        member _.OnNext (value) =
            received.Add (value)
            onReceived |> ValueOption.iter (fun evt -> evt this value)
    interface IDisposable with
        member _.Dispose () =
            subscription.Dispose ()
            mre.Dispose ()

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
    static member CreateExecutionPlanOrFail (executor: Executor<'Root>, queryOrMutation: string, [<Struct>] ?operationName: string, [<Struct>] ?meta : Metadata) =
        match executor.CreateExecutionPlan(queryOrMutation, ?operationName = operationName, ?meta = meta) with
        | Ok executionPlan -> executionPlan
        | Error _ -> fail "invalid query"; Unchecked.defaultof<_>


module MockInputContext =

    let mockContentType = System.Net.Mime.MediaTypeNames.Text.Plain
    let mockFileKey = "fileKey"
    let mockFileKey2 = "fileKey2"
    let mockFileText = "fileText"
    let mockFileText2 = "fileText2"

    let mockFileTextAndContentType = mockFileText + mockContentType
    let mockFileText2AndContentType = mockFileText2 + mockContentType

    type MockInputExecutionContext () =

        member _.FileKey = mockFileKey
        member _.FileKey2 = mockFileKey2
        member _.FileText = mockFileText
        member _.FileText2 = mockFileText2
        member context.Stream =
            let bytes = Encoding.UTF8.GetBytes context.FileText
            new MemoryStream (bytes) :> Stream

        member context.Stream2 =
            let bytes = Encoding.UTF8.GetBytes context.FileText2
            new MemoryStream (bytes) :> Stream

        interface IInputExecutionContext with
            member context.GetFile key =
                if (key = context.FileKey) then
                    Ok { FileName = key; Stream = context.Stream; ContentType = mockContentType }
                else if (key = context.FileKey2) then
                    Ok { FileName = key; Stream = context.Stream2; ContentType = mockContentType }
                else
                    failwith $"only file {context.FileKey} and file {context.FileKey2} exist"

    let mockInputContextInstance = MockInputExecutionContext()

let getMockInputContext = fun () -> MockInputContext.mockInputContextInstance :> IInputExecutionContext

open System.Threading.Tasks
open IcedTasks

/// <summary>
/// An asynchronous sequence that produces each item through a task created on demand.
/// </summary>
/// <remarks>
/// Tests use it instead of a <c>taskSeq</c> block for sequences that really suspend, because <c>taskSeq</c> code compiled
/// without optimizations, as in Debug builds of this project, does not resume correctly after an await.
/// </remarks>
type SuspendingAsyncEnumerable<'T> (produceItem : CancellationToken -> int -> Task<'T voption>, [<Struct>] ?onDisposed : unit -> unit) =
    interface IAsyncEnumerable<'T> with
        member _.GetAsyncEnumerator cancellationToken =
            let index = ref 0
            let current = ref Unchecked.defaultof<'T>
            { new IAsyncEnumerator<'T> with
                member _.Current = current.Value
                member _.MoveNextAsync () =
                    valueTask {
                        match! produceItem cancellationToken index.Value with
                        | ValueSome item ->
                            current.Value <- item
                            index.Value <- index.Value + 1
                            return true
                        | ValueNone -> return false
                    }
              interface IAsyncDisposable with
                member _.DisposeAsync () =
                    onDisposed |> ValueOption.iter (fun onDisposed -> onDisposed ())
                    ValueTask.CompletedTask
            }

/// Awaits the task without blocking the test thread and fails the test with the message when the task does not complete in time
let waitForTask (timeout : TimeSpan) (message : string) (awaited : Task) : Task = task {
    let! completed = Task.WhenAny (awaited, Task.Delay timeout)
    if not (obj.ReferenceEquals (completed, awaited)) then
        fail message
}

open FSharp.Control

/// Returns the value after the scaled delay
let delay time x = async {
    do! Async.Sleep (ms time)
    return x
}

/// An asynchronous sequence of the items, safe to use as a taskSeq in Debug builds because it never awaits
let asyncItems (items : 'T list) = taskSeq {
    for item in items do
        yield item
}

/// A source whose GetAsyncEnumerator throws instead of returning an enumerator
type ThrowingAsyncEnumerable<'T> (message : string) =
    interface IAsyncEnumerable<'T> with
        member _.GetAsyncEnumerator _ = failwith message

/// Produces the item, then fails while pulling the next one
let itemThenFailure (item : 'T) =
    SuspendingAsyncEnumerable<'T> (fun _ index ->
        task {
            match index with
            | 0 -> return ValueSome item
            | _ -> return failwith "Boom during enumeration"
        })
    :> IAsyncEnumerable<'T>

/// Produces the item, then completes, and throws from DisposeAsync
let itemThenDisposalFailure (item : 'T) =
    SuspendingAsyncEnumerable<'T> (
        (fun _ index -> task { return if index = 0 then ValueSome item else ValueNone }),
        fun () -> failwith "Boom disposing"
    )
    :> IAsyncEnumerable<'T>

/// Produces numbers forever with a small delay, recording how many were pulled and signalling disposal
let endlessNumbers (pulled : int ref) (disposed : TaskCompletionSource) =
    SuspendingAsyncEnumerable<int> (
        (fun _ index -> task {
            pulled.Value <- index + 1
            do! Task.Delay 20
            return ValueSome (index + 1)
        }),
        fun () -> disposed.TrySetResult () |> ignore
    )
    :> IAsyncEnumerable<int>
