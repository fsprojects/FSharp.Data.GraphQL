module internal FSharp.Data.GraphQL.Server.AspNetCore.GraphQLSubscriptionsManagement

open FSharp.Data.GraphQL.Server.AspNetCore.WebSockets

let addSubscription
    (id : SubscriptionId, unsubscriber : SubscriptionUnsubscriber, onUnsubscribe : OnUnsubscribeAction)
    (subscriptions : SubscriptionsDict)
    =
    subscriptions.Add (id, (unsubscriber, onUnsubscribe))

let isIdTaken (id : SubscriptionId) (subscriptions : SubscriptionsDict) = subscriptions.ContainsKey (id)

let executeOnUnsubscribeAndDispose (id : SubscriptionId) (subscription : SubscriptionUnsubscriber * OnUnsubscribeAction) =
    match subscription with
    | unsubscriber, onUnsubscribe ->
        try
            id |> onUnsubscribe
        finally
            unsubscriber.Dispose ()

let removeSubscription (id : SubscriptionId) (subscriptions : SubscriptionsDict) =
    match subscriptions.TryGetValue id with
    | true, sub ->
        sub |> executeOnUnsubscribeAndDispose id
        subscriptions.Remove (id) |> ignore
    | false, _ -> ()

let removeAllSubscriptions (subscriptions : SubscriptionsDict) =
    subscriptions
    |> Seq.iter (fun subscription ->
        subscription.Value
        |> executeOnUnsubscribeAndDispose subscription.Key)
    subscriptions.Clear ()
