namespace FSharp.Data.GraphQL.Execution

open System
open System.Collections.Generic
open System.Collections.Concurrent
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Types.Patterns

type FieldExecuteMap () = 
    let fieldExecuteMap = new Dictionary<string * string, ExecuteField>()

    interface IFieldExecuteMap with
        member this.SetExecute(typeName: string, fieldName: string, executeField: ExecuteField) = 
            let key = typeName, fieldName
            if not (fieldExecuteMap.ContainsKey(key)) then fieldExecuteMap.Add(key, executeField)

        member this.GetExecute(typeName: string, fieldName: string) = 
            let key = 
                if List.exists ((=) fieldName) ["__schema"; "__type"; "__typename" ]
                then "", fieldName
                else typeName, fieldName
            if fieldExecuteMap.ContainsKey(key) then fieldExecuteMap.[key] else Unchecked.defaultof<ExecuteField>


type Subscription = {
    Callback: (ResolveFieldContext -> obj -> IDictionary<string, obj> -> unit)
    Filter: (ResolveFieldContext -> obj -> obj -> bool)
}

type ActiveSubscription = {
    Callback: (IDictionary<string, obj> -> unit)
    Filter: (obj -> bool)
    Context: ResolveFieldContext
    Name: string
}

type IActiveSubscriptionProvider =
    interface
        /// Add an active subscription to the collection, returns a unique identifier of type 'T
        abstract AddSubscription : string -> ActiveSubscription -> string
        /// Removes a subscription with a key string and a unique identifier 'T
        abstract RemoveSubscription : string -> string -> bool
        /// Retrieves a sequence of subscriptions for a given key
        abstract GetSubscriptions : string -> ActiveSubscription seq
    end

/// Default implementation for IActiveSubscriptionProvider
/// It implements the subscription provider using dictionaries
type DefaultSubscriptionProvider () =
    let activeSubscriptions = new ConcurrentDictionary<string, Dictionary<System.Guid, ActiveSubscription>>()
    interface IActiveSubscriptionProvider with
        member this.AddSubscription (key: string) (active: ActiveSubscription) =
            let g = System.Guid.NewGuid()
            let addNew = 
                let dict = new Dictionary<System.Guid, ActiveSubscription>()
                dict.Add(g, active)
                dict
            activeSubscriptions.AddOrUpdate(key, addNew, (fun k d -> d.Add(g, active); d)) |> ignore
            g.ToString()
        member this.RemoveSubscription (key: string) (g: string) =
            let guid = System.Guid.Parse(g)
            match activeSubscriptions.TryGetValue key with
            | true, subs ->
                subs.Remove guid
            | _ -> false
        
        member this.GetSubscriptions (key:string) =
            match activeSubscriptions.TryGetValue key with
            | true, subs ->
                subs
                |> Seq.map(fun (KeyValue(_, v)) -> v)
            | _ -> Seq.empty
                

type SubscriptionHandler (fieldExecuteMap: IFieldExecuteMap, activeSubscriptions: IActiveSubscriptionProvider) =


    let registeredSubscriptions = new Dictionary<string, Subscription>()

    let getBaseTypeName (t: TypeDef) =
        match t with
        | Nullable n -> n.ToString()
        | List l -> l.ToString()
        | t' -> t'.ToString()

    let resolveCallback (active: ActiveSubscription) value =
        let ctx = active.Context
        let execute = fieldExecuteMap.GetExecute(ctx.Context.ExecutionPlan.RootDef.Name, ctx.ExecutionInfo.Definition.Name)
        let res = 
            execute ctx value
            |> AsyncVal.map (fun r -> KeyValuePair<_,_>(active.Name, r))
            |> AsyncVal.rescue (fun e -> ctx.AddError e; KeyValuePair<_,_>(active.Name, null))
        let kv = 
            match ctx.Context.ExecutionPlan.Strategy with
            | ExecutionStrategy.Parallel -> AsyncVal.collectParallel [|res|]
            | ExecutionStrategy.Sequential -> AsyncVal.collectSequential [|res|]
        let asyncResult = async {
            let! kv' = kv
            let dict = NameValueLookup(kv') :> IDictionary<string, obj>
            return active.Callback dict
        }
        Async.Start asyncResult
    interface ISubscriptionHandler with 
        member this.RegisterSubscription (fieldName: string) (callback: ResolveFieldContext -> obj -> obj -> unit) (filter: ResolveFieldContext -> obj -> obj -> bool) =
            // Adds the callback if it does not already exist, we need the ignore because the function returns a boolean
            let sub = {
                Callback = callback
                Filter = filter
            }
            registeredSubscriptions.Add(fieldName, sub) |> ignore

        member this.ActivateSubscription (fieldName: string) (ctx: ResolveFieldContext) (root: obj) =
            let triggerType = ctx.ReturnType
            // We need to know the type we are going to be subscribing to
            match registeredSubscriptions.TryGetValue fieldName with
            | true, subscription -> 
                let active = {
                    Callback = (subscription.Callback ctx root)
                    Filter = (subscription.Filter ctx root)
                    Context = ctx
                    Name = fieldName
                }
                activeSubscriptions.AddSubscription (getBaseTypeName triggerType) active
            | _ -> raise <| GraphQLException (sprintf "Attempted to activate a non-existent subscription %s" fieldName)

        member this.DeactivateSubscription (key: string) (identifier: string) =
            activeSubscriptions.RemoveSubscription key identifier

        member this.FireEvent (triggerType: #OutputDef) value =
            activeSubscriptions.GetSubscriptions (getBaseTypeName triggerType)
            |> Seq.filter (fun s -> s.Filter value)
            |> Seq.iter (fun s -> resolveCallback s value)


/// Used to keep both our FieldExecuteMap and SubscriptionHandler in the same context
type ExecutionHandler(activeSubscriptionProvider: IActiveSubscriptionProvider) =
    let f = FieldExecuteMap():> IFieldExecuteMap
    let s = SubscriptionHandler(f, activeSubscriptionProvider) :> ISubscriptionHandler
    member this.FieldExecuteMap = f 
    member this.SubscriptionHandler = s