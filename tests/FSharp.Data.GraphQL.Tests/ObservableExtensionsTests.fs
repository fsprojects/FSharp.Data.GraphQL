/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Tests.ObservableExtensionsTests

open System
open System.Linq
open System.Collections.Generic
open Xunit
open FSharp.Data.GraphQL
open Helpers
open System.Threading

type TestObserver<'T>(obs : IObservable<'T>) as this =
    let received = List<'T>()
    let mutable isCompleted = false
    let mre = new ManualResetEvent(false)
    let mutable subscription = Unchecked.defaultof<IDisposable>
    do subscription <- obs.Subscribe(this)
    member __.Received with get() = received.AsEnumerable()
    member __.WaitCompleted() =
        wait mre "Timeout waiting for OnCompleted"
    member __.IsCompleted with get() = isCompleted
    interface IObserver<'T> with
        member __.OnCompleted() = 
            isCompleted <- true
            mre.Set() |> ignore
        member __.OnError(error) = raise error
        member __.OnNext(value) = received.Add(value)
    interface IDisposable with
        member __.Dispose() = 
            subscription.Dispose()
            mre.Dispose()

[<RequireQualifiedAccess>]
module Observer =
    let create (sub : IObservable<'T>) = new TestObserver<'T>(sub)

[<Fact>]
let ``ofSeq should call OnComplete`` () =
    let source = seq { for x in 1 .. 5 do yield x }
    let obs = Observable.ofSeq source
    use sub = Observer.create obs
    sub.WaitCompleted()
    sub.Received |> seqEquals source