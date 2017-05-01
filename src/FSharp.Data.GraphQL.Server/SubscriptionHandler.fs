namespace FSharp.Data.GraphQL.Subscription

open System
open System.Collections.Generic
open System.Collections.Concurrent
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns

type Subscription = {
    Callback: (ResolveFieldContext -> IDictionary<string, obj> -> unit)
}

type ActiveSubscription = {
    Callback: (IDictionary<string, obj> -> unit)
    Args: Map<string, obj>
}

type SubscriptionHandler() =
    // The reason we use outputdef keys is that it allows us to use the type com
    let activeSubscriptions = new ConcurrentDictionary<string, ActiveSubscription list>()
    let registeredSubscriptions = new ConcurrentDictionary<string, Subscription>()

    let getBaseTypeName (t: TypeDef) =
        match t with
        | Nullable n -> n.ToString()
        | List l -> l.ToString()
        | t' -> t'.ToString()



    member this.RegisterSubscription (fieldName: string) (callback: ResolveFieldContext -> obj -> unit) =
        // Adds the callback if it does not already exist, we need the ignore because the function returns a boolean
        registeredSubscriptions.TryAdd(fieldName, {Callback = callback}) |> ignore

    member this.ActivateSubscription (fieldName: string) (ctx: ResolveFieldContext) =
        let triggerType = ctx.ReturnType
        // We need to know the type we are going to be subscribing to
        match registeredSubscriptions.TryGetValue fieldName with
        | true, subscription -> 
            let active = {
                Callback = (subscription.Callback ctx)
                Args = ctx.Args
            }
            activeSubscriptions.AddOrUpdate(getBaseTypeName triggerType, [active], fun key xs -> active::xs)
            |> ignore
        | _ -> raise <| GraphQLException (sprintf "Attempted to activate a non-existent subscription %s" fieldName)

    member this.FireEvent (triggerType: #OutputDef) (args: Map<string, obj>) (eventValue: IDictionary<string, obj>) =
        match activeSubscriptions.TryGetValue (getBaseTypeName triggerType) with
        | true, subs ->
            subs
            |> List.iter (fun s ->
                // Check to make sure each of the args in the active sub is in args
                let isMatch = 
                    s.Args
                    |> Map.forall (fun k v ->
                        match args.TryFind k with
                        | Some v' -> 
                            v = v'
                        | None -> 
                            false)
                if isMatch then s.Callback eventValue else ())
        | _ -> ()