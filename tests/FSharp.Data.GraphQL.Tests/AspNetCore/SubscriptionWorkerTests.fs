module FSharp.Data.GraphQL.Tests.AspNetCore.SubscriptionWorkerTests

open System
open System.Reactive.Disposables
open System.Reactive.Linq
open System.Reactive.Subjects
open System.Text.Json.Serialization
open System.Threading
open System.Threading.Channels
open System.Threading.Tasks
open Microsoft.Extensions.Logging.Abstractions
open Xunit
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Server.AspNetCore
open FSharp.Data.GraphQL.Server.AspNetCore.ObservableErrorHandling
open FSharp.Data.GraphQL.Shared.WebSockets

// Drives SubscriptionWorker over in-memory channels, standing in for the connection's sender and control loop.

let private subscriptionId = "1"
let private generation = 7
let private timeout = TimeSpan.FromSeconds 10.0

type private Harness<'T> (source : IObservable<'T>, payloads : ISubscriptionPayloads<'T>) =
    let outbound = Channel.CreateUnbounded<OutboundMessage> ()
    let inbox = Channel.CreateUnbounded<ConnectionEvent> ()
    let cancellation = new CancellationTokenSource ()
    let worker = SubscriptionWorker<'T> (subscriptionId, generation, source, payloads, outbound.Writer, inbox.Writer, NullLogger.Instance)

    let drain (reader : ChannelReader<'Event>) =
        let events = ResizeArray<'Event> ()
        let mutable more = true
        while more do
            match reader.TryRead () with
            | true, event -> events.Add event
            | false, _ -> more <- false
        List.ofSeq events

    member _.Run () = worker.RunAsync cancellation.Token
    member _.Cancel () = cancellation.Cancel ()
    /// Waits until the worker has queued at least one message
    member _.WaitForMessage () : Task = task {
        use waitCancellation = new CancellationTokenSource (timeout)
        let! _ = outbound.Reader.WaitToReadAsync waitCancellation.Token
        ()
    }
    member _.SentMessages () = drain outbound.Reader
    member _.InboxEvents () = drain inbox.Reader

let private runToEnd (harness : Harness<'T>) : Task = task {
    let run = harness.Run ()
    do! waitForTask timeout "The worker did not end in time" run
    do! run
}

let private kindOf message =
    match message with
    | Send (Next _) -> "next"
    | Send (Complete _) -> "complete"
    | Send (ServerError _) -> "error"
    | Send other -> $"%A{other}"
    | Close _ -> "close"

let private nextPayloads messages =
    messages
    |> List.choose (function
        | Send (Next (_, payload)) -> Some payload
        | _ -> None)

let private deferredPayloads () : ISubscriptionPayloads<GQLDeferredResponseContent> =
    DeferredPayloads (NullLogger.Instance, NameValueLookup.ofList [ "items", upcast [||] ], [])

let private streamPayloads () : ISubscriptionPayloads<GQLSubscriptionResponseContent> = StreamPayloads NullLogger.Instance

let private itemsPath = [ box "items" ]

[<Fact>]
let ``A pending announced before the initial payload is emitted in the initial payload`` () : Task = task {
    let source = [ DeferredPending (itemsPath, ValueNone, true); DeferredCompleted itemsPath ].ToObservable ()
    let harness = Harness (source, deferredPayloads ())
    do! runToEnd harness
    let messages = harness.SentMessages ()
    messages |> List.map kindOf |> equals [ "next"; "next"; "next"; "complete" ]
    match nextPayloads messages with
    | [ initial; completed; final ] ->
        initial.HasNext |> equals (Include true)
        (initial.Pending |> Skippable.toValueOption |> wantValueSome |> single).Path |> equals itemsPath
        (completed.Completed |> Skippable.toValueOption |> wantValueSome |> single).Errors |> equals Skip
        final.HasNext |> equals (Include false)
    | payloads -> fail $"Unexpected payloads %A{payloads}"
    harness.InboxEvents () |> equals [ SubscriptionEnded (subscriptionId, generation) ]
}

[<Fact>]
let ``A source completing before any event still sends the initial payload first, then hasNext false and complete`` () : Task = task {
    let harness = Harness (Observable.Empty<GQLDeferredResponseContent> (), deferredPayloads ())
    do! runToEnd harness
    let messages = harness.SentMessages ()
    messages |> List.map kindOf |> equals [ "next"; "next"; "complete" ]
    match nextPayloads messages with
    | [ initial; final ] ->
        initial.HasNext |> equals (Include true)
        initial.Pending |> equals Skip
        final.HasNext |> equals (Include false)
    | payloads -> fail $"Unexpected payloads %A{payloads}"
    harness.InboxEvents () |> equals [ SubscriptionEnded (subscriptionId, generation) ]
}

[<Fact>]
let ``A source failing before any event still sends the initial payload first, then error`` () : Task = task {
    let harness = Harness (Observable.Throw<GQLDeferredResponseContent> (InvalidOperationException "sensitive backend failure"), deferredPayloads ())
    do! runToEnd harness
    let messages = harness.SentMessages ()
    messages |> List.map kindOf |> equals [ "next"; "error" ]
    match messages with
    | [ Send (Next (_, initial)); Send (ServerError (id, errors)) ] ->
        initial.HasNext |> equals (Include true)
        id |> equals subscriptionId
        (errors |> single).Message |> equals UnexpectedObservableErrorMessage
    | messages -> fail $"Unexpected messages %A{messages}"
    harness.InboxEvents () |> equals [ SubscriptionEnded (subscriptionId, generation) ]
}

[<Fact>]
let ``A subscription stream sends next per item and complete when the source completes`` () : Task = task {
    let source =
        [
            SubscriptionResult (NameValueLookup.ofList [ "value", upcast 1 ])
            SubscriptionResult (NameValueLookup.ofList [ "value", upcast 2 ])
        ]
            .ToObservable ()
    let harness = Harness (source, streamPayloads ())
    do! runToEnd harness
    let messages = harness.SentMessages ()
    messages |> List.map kindOf |> equals [ "next"; "next"; "complete" ]
    nextPayloads messages
    |> List.map _.Data
    |> equals [
        Include (ValueSome (box (NameValueLookup.ofList [ "value", upcast 1 ])))
        Include (ValueSome (box (NameValueLookup.ofList [ "value", upcast 2 ])))
    ]
    harness.InboxEvents () |> equals [ SubscriptionEnded (subscriptionId, generation) ]
}

[<Fact>]
let ``A source whose Subscribe throws sends error, sends no next, and ends the subscription`` () : Task = task {
    let source =
        { new IObservable<GQLSubscriptionResponseContent> with
            member _.Subscribe _ = raise (InvalidOperationException "sensitive backend failure")
        }
    let harness = Harness (source, streamPayloads ())
    do! runToEnd harness
    match harness.SentMessages () with
    | [ Send (ServerError (id, errors)) ] ->
        id |> equals subscriptionId
        (errors |> single).Message |> equals UnexpectedObservableErrorMessage
    | messages -> fail $"Unexpected messages %A{messages}"
    harness.InboxEvents () |> equals [ SubscriptionEnded (subscriptionId, generation) ]
}

[<Fact>]
let ``Cancelling the worker disposes the source, sends nothing further, and posts SubscriptionEnded`` () : Task = task {
    use subject = new Subject<GQLSubscriptionResponseContent> ()
    let harness = Harness (subject, streamPayloads ())
    let run = harness.Run ()
    // The worker subscribes on its own thread; an item published before that would reach nobody
    waitFor (fun () -> subject.HasObservers) 100 "The worker did not subscribe to the source in time"
    subject.OnNext (SubscriptionResult (NameValueLookup.ofList [ "value", upcast 1 ]))
    do! harness.WaitForMessage ()
    harness.Cancel ()
    do! waitForTask timeout "The worker did not end after cancellation" run
    do! run
    Assert.False (subject.HasObservers, "The source must be unsubscribed when the worker is cancelled")
    // An item published after the cancellation reaches nobody
    subject.OnNext (SubscriptionResult (NameValueLookup.ofList [ "value", upcast 2 ]))
    harness.SentMessages () |> List.map kindOf |> equals [ "next" ]
    harness.InboxEvents () |> equals [ SubscriptionEnded (subscriptionId, generation) ]
}

[<Fact>]
let ``A source whose disposal throws still posts SubscriptionEnded`` () : Task = task {
    let source =
        { new IObservable<GQLSubscriptionResponseContent> with
            member _.Subscribe _ = Disposable.Create (fun () -> raise (InvalidOperationException "Boom disposing"))
        }
    let harness = Harness (source, streamPayloads ())
    let run = harness.Run ()
    harness.Cancel ()
    do! waitForTask timeout "The worker did not end after cancellation" run
    do! run
    harness.SentMessages () |> empty
    harness.InboxEvents () |> equals [ SubscriptionEnded (subscriptionId, generation) ]
}
