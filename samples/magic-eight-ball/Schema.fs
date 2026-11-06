module FSharp.Data.GraphQL.Samples.MagicEightBall.Schema

open System
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types

let private answers = [|
    "It is certain."
    "It is decidedly so."
    "Without a doubt."
    "Yes, definitely."
    "You may rely on it."
    "As I see it, yes."
    "Most likely."
    "Outlook good."
    "Yes."
    "Signs point to yes."
    "Reply hazy, try again."
    "Ask again later."
    "Better not tell you now."
    "Cannot predict now."
    "Concentrate and ask again."
    "Don't count on it."
    "My reply is no."
    "My sources say no."
    "Outlook not so good."
    "Very doubtful."
|]

type Root = { Random : Random }

let private shake (random : Random) = answers[random.Next answers.Length]

let Query =
    Define.Object<Root>(
        name = "Query",
        fields = [
            Define.Field (
                "ask",
                StringType,
                "Shakes the magic eight ball and returns its answer to the given question.",
                [ Define.Input ("question", StringType) ],
                fun _ root -> shake (root.Random)
            )
        ]
    )

let Mutation =
    Define.Object<Root>(
        name = "Mutation",
        fields = [
            Define.Field (
                "shake",
                StringType,
                "Shakes the magic eight ball and notifies everyone subscribed to `onShake` with the new answer.",
                fun ctx root ->
                    let answer = shake (root.Random)
                    ctx.Schema.SubscriptionProvider.Publish<string> "onShake" answer
                    answer
            )
        ]
    )

let Subscription =
    Define.SubscriptionObject<Root>(
        name = "Subscription",
        fields = [
            Define.SubscriptionField (
                "onShake",
                Query,
                StringType,
                "Notified with a new answer whenever anyone shakes the ball via the `shake` mutation.",
                fun _ _ (answer : string) -> Some answer
            )
        ]
    )

let schema : ISchema<Root> = upcast Schema (Query, Mutation, Subscription)
