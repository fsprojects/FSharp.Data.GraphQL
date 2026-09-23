module internal FSharp.Data.GraphQL.Server.Suave.GraphQLSubscriptionsManagement

open FSharp.Data.GraphQL.Shared.WebSockets

// `subscriptions` is mutated both from the WebSocket's sequential message loop and from the `IObserver` callbacks of
// each active subscription's stream, which can fire on an arbitrary thread. `IDictionary`'s backing `Dictionary` is
// not safe for concurrent access, so every operation here locks on the dictionary instance itself - the same
// instance is shared by every caller for a given connection, so this serializes all of them against each other.
let addSubscription
    (id : SubscriptionId, unsubscriber : SubscriptionUnsubscriber, onUnsubscribe : OnUnsubscribeAction)
    (subscriptions : SubscriptionsDict)
    =
    lock subscriptions (fun () -> subscriptions.Add (id, (unsubscriber, onUnsubscribe)))

let isIdTaken (id : SubscriptionId) (subscriptions : SubscriptionsDict) = lock subscriptions (fun () -> subscriptions.ContainsKey (id))

let executeOnUnsubscribeAndDispose (id : SubscriptionId) (subscription : SubscriptionUnsubscriber * OnUnsubscribeAction) =
    match subscription with
    | unsubscriber, onUnsubscribe ->
        try
            id |> onUnsubscribe
        finally
            unsubscriber.Dispose ()

let removeSubscription (id : SubscriptionId) (subscriptions : SubscriptionsDict) =
    lock subscriptions (fun () ->
        match subscriptions.TryGetValue id with
        | true, sub ->
            sub |> executeOnUnsubscribeAndDispose id
            subscriptions.Remove (id) |> ignore
        | false, _ -> ())

let removeAllSubscriptions (subscriptions : SubscriptionsDict) =
    lock subscriptions (fun () ->
        subscriptions
        |> Seq.iter (fun subscription ->
            subscription.Value
            |> executeOnUnsubscribeAndDispose subscription.Key)
        subscriptions.Clear ())
