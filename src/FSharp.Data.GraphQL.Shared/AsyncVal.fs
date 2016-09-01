namespace FSharp.Data.GraphQL

open System
open System.Collections.Generic

/// A struct used to operate on both synchronous values and Async computations using an uniform API.
[<Struct>]
type AsyncVal<'T> =
    val Value : 'T
    val Async : Async<'T>
    new (value: 'T) = { Value = value; Async = Unchecked.defaultof<Async<'T>> }
    new (async: Async<'T>) = { Value = Unchecked.defaultof<'T>; Async = async }
    member x.IsAsync = not (System.Object.ReferenceEquals(x.Async, null))
    member x.IsSync = System.Object.ReferenceEquals(x.Async, null)
    override x.ToString () = 
        if x.IsSync 
        then "AsyncVal(" + x.Value.ToString() + ")"
        else "AsyncVal(Async<>)"
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
    let map (fn: 'T -> 'U) (x: AsyncVal<'T>) =
        if x.IsSync
        then AsyncVal<'U>(fn x.Value)
        else AsyncVal<'U> (async {
            let! result = x.Async
            return fn result })

    /// Applies rescue fn in case when contained Async value throws an exception.
    let rescue (fn: exn -> 'T) (x: AsyncVal<'T>) =
        if x.IsSync // sync vals will never throw, as they contain ready value
        then x
        else ofAsync(async {
            try return! x.Async
            with e -> return fn e })

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
    /// Returned array maintain order of values.
    let collectSequential (values: AsyncVal<'T> []) : AsyncVal<'T []> =
        if values.Length = 0 then AsyncVal<_> [||]
        elif values |> Array.exists isAsync then
            ofAsync <| async {
                let results = Array.zeroCreate values.Length
                for i = 0 to values.Length - 1 do
                    let v = values.[i]
                    if v.IsSync
                    then results.[i] <- v.Value
                    else
                        let! r = v.Async
                        results.[i] <- r
                return results }
        else AsyncVal<_> (values |> Array.map (fun x -> x.Value)) 
            

    /// Converts array of AsyncVals into AsyncVal with array results.
    /// In case when are non-immediate values in provided array, they are 
    /// executed all in parallel, in unordered fashion. Order of values
    /// inside returned array is maintained.
    let collectParallel (values: AsyncVal<'T> []) : AsyncVal<'T []> =
        if values.Length = 0 then AsyncVal<_> [||]
        else 
            let indexes = List<_>(0)
            let continuations = List<_>(0)
            let results = Array.zeroCreate values.Length
            for i = 0 to values.Length - 1 do
                let v = values.[i]
                if v.IsSync
                then results.[i] <- v.Value
                else 
                    indexes.Add i
                    continuations.Add v.Async
            if indexes.Count = 0
            then AsyncVal<_> results
            else ofAsync (async {
                let! vals = continuations |> Async.Parallel
                for i = 0 to indexes.Count - 1 do
                    results.[indexes.[i]] <- vals.[i]
                return results })

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