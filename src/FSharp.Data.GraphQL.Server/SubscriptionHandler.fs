namespace FSharp.Data.GraphQL.Subscription

open System
open System.Collections.Generic
open FSharp.Data.GraphQL.Types


type SubscriptionRegistration = {
    //InputType : OutputDef
    Callback : (ResolveFieldContext -> IDictionary<string, obj> -> unit)
}

type ActiveSubscription = {
    Args : Map<string, obj>
}

type SubscriptionHandler() =
    let mutable activeSubscriptions = Map.empty : Map<string, (obj -> unit) list>
    let mutable registeredSubscriptions = Map.empty : Map<string, SubscriptionRegistration>
    member this.RegisterSubscription (fieldName: string) (callback: ResolveFieldContext -> obj -> unit) =
        printfn "Subscription for field name %s registered" fieldName
        if not (registeredSubscriptions.ContainsKey(fieldName)) then 
            registeredSubscriptions <- registeredSubscriptions.Add(fieldName, {Callback = callback})
    
    member this.FireEvent (eventName:string) (ctx: ResolveFieldContext) (eventValue: IDictionary<string, obj>) =
        printfn "Firing event for field name %s with object %A" eventName eventValue
        registeredSubscriptions.TryFind eventName |> Option.map (fun x -> x.Callback ctx eventValue) |> ignore
