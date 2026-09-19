module internal FSharp.Data.GraphQL.Server.AspNetCore.GraphQLSubscriptionsManagement

open System

open FSharp.Data.GraphQL.Shared.WebSockets

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
    let subscription =
        lock subscriptions (fun () ->
            match subscriptions.TryGetValue id with
            | true, sub ->
                subscriptions.Remove (id) |> ignore
                ValueSome sub
            | false, _ -> ValueNone)

    match subscription with
    | ValueSome sub -> sub |> executeOnUnsubscribeAndDispose id
    | ValueNone -> ()

let removeAllSubscriptions (subscriptions : SubscriptionsDict) =
    let subscriptionsToDispose =
        lock subscriptions (fun () ->
            let snapshot =
                subscriptions
                |> Seq.map (fun subscription -> struct (subscription.Key, subscription.Value))
                |> Seq.toArray

            subscriptions.Clear ()
            snapshot)

    let exceptions = ResizeArray ()

    subscriptionsToDispose
    |> Array.iter (fun struct (id, subscription) ->
        try
            subscription |> executeOnUnsubscribeAndDispose id
        with ex ->
            exceptions.Add ex)

    if exceptions.Count > 0 then
        raise (AggregateException ("One or more subscriptions failed to unsubscribe.", exceptions))
