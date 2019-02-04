/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc
[<AutoOpen>]
module Helpers

open System
open System.Linq
open System.Collections.Generic
open Xunit
open FSharp.Data.GraphQL.Execution
open System.Threading

let isType<'a> actual = Assert.IsAssignableFrom<'a>(actual)
let isSeq<'a> actual = isType<'a seq> actual
let isDict<'k, 'v> actual = isSeq<KeyValuePair<'k, 'v>> actual
let isNameValueDict actual = isDict<string, obj> actual
let equals (expected : 'x) (actual : 'x) =
    Assert.True((actual = expected), sprintf "expected %+A\nbut got %+A" expected actual)
let notEquals (expected : 'x) (actual : 'x) =
    Assert.True((actual <> expected), sprintf "unexpected %+A" expected)
let noErrors (result: IDictionary<string, obj>) =
    match result.TryGetValue("errors") with
    | true, errors -> failwithf "expected ExecutionResult to have no errors but got %+A" errors
    | false, _ -> ()
let empty (xs: 'a seq) =
    Assert.True(Seq.isEmpty xs, sprintf "expected empty sequence, but got %A" xs)
let fail (message: string) =
    Assert.True(false, message)
let single (xs : 'a seq) =
    let length = Seq.length xs
    if length <> 1
    then fail <| sprintf "Expected single item in sequence, but found %i items.\n%A" length xs
    Seq.head xs
let throws<'e when 'e :> exn> (action : unit -> unit) = Assert.Throws<'e>(action)
let sync = Async.RunSynchronously
let is<'t> (o: obj) = o :? 't
let hasError (errMsg : string) (errors: Error seq) =
    let containsMessage = errors |> Seq.exists (fun (message, _) -> message.Contains(errMsg))
    Assert.True (containsMessage, sprintf "Expected to contain message '%s', but no such message was found. Messages found: %A" errMsg errors)
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

open Newtonsoft.Json
open Newtonsoft.Json.Serialization
open Microsoft.FSharp.Reflection
open FSharp.Data.GraphQL.Parser

type internal OptionConverter() =
    inherit JsonConverter()

    override __.CanConvert(t) =
        t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>
    override __.WriteJson(writer, value, serializer) =
        let value =
            if isNull value then null
            else
                let _,fields = Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(value, value.GetType())
                fields.[0]
        serializer.Serialize(writer, value)

    override __.ReadJson(reader, t, _, serializer) =
        let innerType = t.GetGenericArguments().[0]
        let innerType =
            if innerType.IsValueType then (typedefof<Nullable<_>>).MakeGenericType([|innerType|])
            else innerType
        let value = serializer.Deserialize(reader, innerType)
        let cases = FSharpType.GetUnionCases(t)
        if isNull value then FSharpValue.MakeUnion(cases.[0], [||])
        else FSharpValue.MakeUnion(cases.[1], [|value|])

let internal settings = JsonSerializerSettings()
settings.Converters <- [| OptionConverter() |]
settings.ContractResolver <- CamelCasePropertyNamesContractResolver()

let toJson (o: 't) : string = JsonConvert.SerializeObject(o, settings)

let fromJson<'t> (json: string) : 't = JsonConvert.DeserializeObject<'t>(json, settings)

let asts query =
    ["defer"; "stream"]
    |> Seq.map (query >> parse)

let set (mre : ManualResetEvent) =
    mre.Set() |> ignore

let reset (mre : ManualResetEvent) =
    mre.Reset() |> ignore

let wait (mre : ManualResetEvent) errorMsg =
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

type TestObserver<'T>(obs : IObservable<'T>, ?onReceived : TestObserver<'T> -> 'T -> unit) as this =
    let received = List<'T>()
    let mutable isCompleted = false
    let mre = new ManualResetEvent(false)
    let mutable subscription = Unchecked.defaultof<IDisposable>
    do subscription <- obs.Subscribe(this)
    member __.Received 
        with get() = received.AsEnumerable()
    member __.WaitCompleted() =
        wait mre "Timeout waiting for OnCompleted"
    member x.WaitCompleted(expectedItemCount) =
        x.WaitCompleted()
        if received.Count < expectedItemCount
        then failwithf "Expected to receive %i items, but received %i" expectedItemCount received.Count
    member __.WaitForItems(expectedItemCount) =
        let errorMsg = sprintf "Expected to receive least %i items, but received %i" expectedItemCount received.Count
        waitFor (fun () -> received.Count = expectedItemCount) (expectedItemCount * 100) errorMsg
    member x.WaitForItem() = x.WaitForItems(1)
    member __.IsCompleted 
        with get() = isCompleted
    interface IObserver<'T> with
        member __.OnCompleted() = 
            isCompleted <- true
            mre.Set() |> ignore
        member __.OnError(error) = raise error
        member __.OnNext(value) = 
            received.Add(value)
            onReceived |> Option.iter (fun evt -> evt this value)
    interface IDisposable with
        member __.Dispose() = 
            subscription.Dispose()
            mre.Dispose()

[<RequireQualifiedAccess>]
module Observer =
    let create (sub : IObservable<'T>) = 
        new TestObserver<'T>(sub)

    let createWithCallback (onReceive : TestObserver<'T> -> 'T -> unit) (sub : IObservable<'T>) =
        new TestObserver<'T>(sub, onReceive)