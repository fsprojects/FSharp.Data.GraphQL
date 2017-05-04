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


type internal Subscription = {
    Callback: (ResolveFieldContext -> obj -> IDictionary<string, obj> -> unit)
    Filter: (ResolveFieldContext -> obj -> obj -> bool)
}

type internal ActiveSubscription = {
    Callback: (IDictionary<string, obj> -> unit)
    Filter: (obj -> bool)
    Context: ResolveFieldContext
    Name: string
}

type SubscriptionHandler (fieldExecuteMap: IFieldExecuteMap) =

    // The reason we use outputdef keys is that it allows us to use the type com
    let activeSubscriptions = new ConcurrentDictionary<string, ActiveSubscription list>()
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
                activeSubscriptions.AddOrUpdate(getBaseTypeName triggerType, [active], fun key xs -> active::xs)
                |> ignore
            | _ -> raise <| GraphQLException (sprintf "Attempted to activate a non-existent subscription %s" fieldName)

        member this.FireEvent (triggerType: #OutputDef) value =
            // TODO: Enable parallelism
            match activeSubscriptions.TryGetValue (getBaseTypeName triggerType) with
            | true, subs ->
                subs
                |> List.filter (fun s -> s.Filter value)
                |> List.iter (fun s -> resolveCallback s value)
            | _ -> ()


/// Used to keep both our FieldExecuteMap and SubscriptionHandler in the same context
type ExecutionHandler() =
    let f = FieldExecuteMap():> IFieldExecuteMap
    let s = SubscriptionHandler(f) :> ISubscriptionHandler
    member this.FieldExecuteMap = f 
    member this.SubscriptionHandler = s