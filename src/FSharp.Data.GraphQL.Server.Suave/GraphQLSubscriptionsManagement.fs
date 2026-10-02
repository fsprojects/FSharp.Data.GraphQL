module internal FSharp.Data.GraphQL.Server.Suave.GraphQLSubscriptionsManagement

open System
open System.Collections.Generic

open FSharp.Data.GraphQL.Shared.WebSockets

/// The active subscriptions of one connection, keyed by the id the client gave each of them, with the handle that
/// unsubscribes from its source.
type SubscriptionsDict = Dictionary<SubscriptionId, IDisposable>

// `subscriptions` is mutated both from the WebSocket's sequential message loop and from the `IObserver` callbacks of
// each active subscription's stream, which can fire on an arbitrary thread. `Dictionary` is not safe for concurrent
// access, so every operation here locks on the dictionary instance itself - the same instance is shared by every
// caller for a given connection, so this serializes all of them against each other.
let createSubscriptions () = SubscriptionsDict (StringComparer.Ordinal)

let addSubscription (id : SubscriptionId, unsubscriber : IDisposable) (subscriptions : SubscriptionsDict) =
    lock subscriptions (fun () -> subscriptions.Add (id, unsubscriber))

let isIdTaken (id : SubscriptionId) (subscriptions : SubscriptionsDict) = lock subscriptions (fun () -> subscriptions.ContainsKey id)

let removeSubscription (id : SubscriptionId) (subscriptions : SubscriptionsDict) =
    lock subscriptions (fun () ->
        match subscriptions.TryGetValue id with
        | true, unsubscriber ->
            subscriptions.Remove id |> ignore
            unsubscriber.Dispose ()
        | false, _ -> ())

let removeAllSubscriptions (subscriptions : SubscriptionsDict) =
    lock subscriptions (fun () ->
        try
            for unsubscriber in subscriptions.Values do
                unsubscriber.Dispose ()
        finally
            subscriptions.Clear ())
