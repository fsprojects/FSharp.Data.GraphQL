namespace FSharp.Data.GraphQL

open System

[<Struct>]
type AsyncVal<'T> =
    val Value : 'T
    val Async : Async<'T>
    new (value: 'T) = { Value = value; Async = Unchecked.defaultof<Async<'T>> }
    new (async: Async<'T>) = { Value = Unchecked.defaultof<'T>; Async = async }
    member x.IsAsync = not (System.Object.ReferenceEquals(x.Async, null))
    member x.IsSync = System.Object.ReferenceEquals(x.Async, null)
    static member Zero = AsyncVal<'T>(Unchecked.defaultof<'T>)
    
[<RequireQualifiedAccess>]
module AsyncVal =
    
    /// Returns true if AsyncVal wraps an Async computation, otherwise false.
    let inline isAsync (x: AsyncVal<'T>) = x.IsAsync

    /// Returns true if AsyncVal contains immediate result, otherwise false.
    let inline isSync (x: AsyncVal<'T>) = x.IsSync

    /// Returns value wrapped by current AsyncVal. If it's part of Async computation,
    /// it's executed synchronously and then value is returned.
    let get (x: AsyncVal<'T>) = 
        if x.IsSync 
        then x.Value
        else x.Async |> Async.RunSynchronously

    /// Create new AsyncVal from Async computation.
    let inline ofAsync (a: Async<'T>) = AsyncVal<'T>(a)

    /// Returns an AsyncVal wrapper around provided Async computation.
    let inline wrap (v: 'T) = AsyncVal<'T>(v)
    
    /// Converts AsyncVal to Async computation.
    let toAsync (x: AsyncVal<'T>) =
        if x.IsSync
        then async.Return x.Value
        else x.Async

    /// Returns an empty AsyncVal with immediatelly executed value.
    let inline empty<'T> : AsyncVal<'T> = AsyncVal<'T>.Zero

    /// Maps content of AsyncVal using provided mapping function, returning new 
    /// AsyncVal as the result.
    let map (fn: 'T -> 'Res) (x: AsyncVal<'T>) =
        if x.IsSync
        then AsyncVal<'Res>(fn x.Value)
        else AsyncVal<'Res> (async {
            let! result = x.Async
            return fn result })

    /// Folds content of AsyncVal over provided initial state zero using provided fn.
    /// Returns new AsyncVal as a result.
    let fold (fn: 'State -> 'T -> 'State) (zero: 'State) (x: AsyncVal<'T>) : AsyncVal<'State> =
        if x.IsSync
        then AsyncVal<_> (fn zero x.Value)
        else ofAsync <| async {
            let! res = x.Async 
            return fn zero res }

    /// Binds AsyncVal using binder function to produce new AsyncVal.
    let bind (binder: 'T -> AsyncVal<'U>) (x: AsyncVal<'T>) : AsyncVal<'U> =
        if x.IsSync
        then binder x.Value
        else ofAsync <| async {
            let! value = x.Async
            let bound = binder value
            if bound.IsSync
            then return bound.Value
            else return! bound.Async }
            
    /// Converts array of AsyncVals into AsyncVal with array results.
    /// In case when are non-immediate values in provided array, they are 
    /// executed asynchronously, one by one with regard to their order in array.
    let collectSequential (values: AsyncVal<'T> []) : AsyncVal<'T []> =
        let i, a = values |> Array.partition isSync
        match i, a with
        | [||], [||] -> AsyncVal<_> [||]
        | immediates, [||] -> 
            let x = immediates |> Array.map (fun v -> v.Value)
            AsyncVal<_> x
        | [||], awaitings -> 
            let asyncs = awaitings |> Array.map (fun v -> v.Async)
            let x = async {
                let results = Array.zeroCreate asyncs.Length
                let mutable i = 0
                for a in asyncs do
                    let! res = a
                    results.[i] <- res
                    i <- i + 1
                return results
            }
            ofAsync x
        | immediates, awaitings ->
            //TODO: optimize
            let ready = immediates |> Array.map (fun v -> v.Value)
            let asyncs = awaitings |> Array.map (fun v -> v.Async)
            let x = async {
                let results = Array.zeroCreate (ready.Length + asyncs.Length)
                Array.Copy(ready, results, ready.Length)
                let mutable i = ready.Length
                for a in asyncs do
                    let! res = a
                    results.[i] <- res
                    i <- i + 1
                return results
            }
            ofAsync x

    /// Converts array of AsyncVals into AsyncVal with array results.
    /// In case when are non-immediate values in provided array, they are 
    /// executed all in parallel, in unordered fashion.
    let collectParallel (values: AsyncVal<'T> []) : AsyncVal<'T []> =
        let i, a = values |> Array.partition isSync
        match i, a with
        | [||], [||] -> AsyncVal<_> [||]
        | immediates, [||] -> 
            let x = immediates |> Array.map (fun v -> v.Value)
            AsyncVal<_> x
        | [||], awaitings -> 
            let x = awaitings |> Array.map (fun v -> v.Async) |> Async.Parallel
            ofAsync x
        | immediates, awaitings ->
            //TODO: optimize
            let len =  immediates.Length
            let asyncs = awaitings |> Array.map (fun v -> v.Async)
            let results = Array.zeroCreate (len + asyncs.Length)
            for i = 0 to len - 1 do
                results.[i] <- immediates.[i].Value
            let x = async {
                let! asyncResults = asyncs |> Async.Parallel
                Array.Copy(asyncResults, 0, results, len, asyncResults.Length)
                return results
            }
            ofAsync x

type AsyncValBuilder () =
    member x.Zero () = AsyncVal.empty
    member x.Return v = AsyncVal.wrap v
    member x.ReturnFrom (v: AsyncVal<_>) = v
    member x.ReturnFrom (a: Async<_>) = AsyncVal.ofAsync a
    member x.Bind (v: AsyncVal<'T>, binder: 'T -> AsyncVal<'U>) = 
        AsyncVal.bind binder v
    member x.Bind (a: Async<'T>, binder: 'T -> AsyncVal<'U>) = 
        AsyncVal.ofAsync <| async {
            let! value = a
            let bound = binder value
            if bound.IsSync
            then return bound.Value
            else return! bound.Async }
            
[<AutoOpen>]
module AsyncExtensions =
    
    /// Computation expression for working on AsyncVals.
    let asyncVal = AsyncValBuilder ()
    
    /// Active pattern used for checking if AsyncVal contains immediate value.
    let (|Immediate|_|) (x: AsyncVal<'T>) = if x.IsSync then Some x.Value else None

    /// Active patter used for checking if AsyncVal wraps an Async computation.
    let (|Async|_|) (x: AsyncVal<'T>) = if x.IsAsync then Some x.Async else None

    type Microsoft.FSharp.Control.AsyncBuilder with

        member x.ReturnFrom (v: AsyncVal<'T>) =
            if v.IsSync 
            then async.Return v.Value
            else async.ReturnFrom v.Async

        member x.Bind (v: AsyncVal<'T>, binder) =
            if v.IsSync
            then async.Bind (async.Return v.Value, binder)
            else async.Bind (v.Async, binder)