namespace FSharp.Data.GraphQL

open System
open System.Collections.Generic
open System.Linq
open System.Threading.Tasks

#nowarn "25"

/// <summary>
/// A struct used to operate on both synchronous values and Async computations
/// using the same, uniform API.
/// </summary>
[<Struct>]
type AsyncVal<'T> =
    | Value of value : 'T
    | Async of asynchronous : Async<'T>
    | Failure of exn : Exception

    static member Zero = Value (Unchecked.defaultof<'T>)
    override x.ToString () =
        match x with
        | Value v -> "AsyncVal(" + v.ToString () + ")"
        | Async _ -> "AsyncVal(Async<>)"
        | Failure f -> "AsyncVal(Failure:" + f.Message + ")"

[<RequireQualifiedAccess>]
module AsyncVal =

    /// Returns true if AsyncVal wraps an Async computation, otherwise false.
    let inline isAsync (x : AsyncVal<'T>) = match x with | Async _ -> true | _ -> false

    /// Returns true if AsyncVal contains immediate result, otherwise false.
    let inline isSync (x : AsyncVal<'T>) = match x with | Value _ -> true | _ -> false

    /// Returns true if the AsyncVal failed, otherwise false
    let inline isFailure (x : AsyncVal<'T>) = match x with | Failure _ -> true | _ -> false

    /// Returns value wrapped by current AsyncVal. If it's part of Async computation,
    /// it's executed synchronously and then value is returned.
    /// If the asyncVal failed, then the exception that caused the failure is raised
    let get (x : AsyncVal<'T>) =
        match x with
        | Value v -> v
        | Async a -> a |> Async.RunSynchronously
        | Failure f -> f.Reraise ()

    /// Create new AsyncVal from Async computation.
    let inline ofAsync (a : Async<'T>) = Async (a)

    /// Returns an AsyncVal wrapper around provided Async computation.
    let inline wrap (v : 'T) = Value (v)

    /// Converts AsyncVal to Async computation.
    let toAsync (x : AsyncVal<'T>) =
        match x with
        | Value v -> async.Return v
        | Async a -> a
        | Failure f -> async.Return (f.Reraise ())

    /// Converts AsyncVal to Async computation.
    let toTask (x : AsyncVal<'T>) =
        match x with
        | Value v -> Task.FromResult (v)
        | Async a -> Async.StartAsTask (a)
        | Failure f -> Task.FromException<'T> (f)

    /// Returns an empty AsyncVal with immediatelly executed value.
    let inline empty<'T> : AsyncVal<'T> = AsyncVal<'T>.Zero

    /// Maps content of AsyncVal using provided mapping function, returning new
    /// AsyncVal as the result.
    let map (fn : 'T -> 'U) (x : AsyncVal<'T>) =
        match x with
        | Value v -> Value (fn v)
        | Async a ->
            Async ( async {
                let! result = a
                return fn result
            })
        | Failure f -> Failure (f)


    /// Applies rescue fn in case when contained Async value throws an exception.
    let rescue path (fn : FieldPath -> exn -> IGQLError list) (x : AsyncVal<'t>) =
        match x with
        | Value v -> Value (Ok v)
        | Async a ->
            Async (async {
                try
                    let! v = a
                    return Ok v
                with e ->
                    return fn path e |> Error
            })
        | Failure f -> Value (fn path f |> Error)
        |> map (Result.mapError (List.map (GQLProblemDetails.OfFieldExecutionError (path |> List.rev))))


    /// Folds content of AsyncVal over provided initial state zero using provided fn.
    /// Returns new AsyncVal as a result.
    let fold (fn : 'State -> 'T -> 'State) (zero : 'State) (x : AsyncVal<'T>) : AsyncVal<'State> =
        match x with
        | Value v -> Value (fn zero v)
        | Async a ->
            Async (async {
                let! res = a
                return fn zero res
            })
        | Failure f -> Failure (f)


    /// Binds AsyncVal using binder function to produce new AsyncVal.
    let bind (binder : 'T -> AsyncVal<'U>) (x : AsyncVal<'T>) : AsyncVal<'U> =
        match x with
        | Value v -> binder v
        | Async a ->
            Async (async {
                let! value = a
                let bound = binder value
                match bound with
                | Value v -> return v
                | Async a -> return! a
                | Failure f -> return f.Reraise ()
            })
        | Failure f -> Failure (f)

    /// Converts array of AsyncVals into AsyncVal with array results.
    /// In case when are non-immediate values in provided array, they are
    /// executed asynchronously, one by one with regard to their order in array.
    /// Returned array maintain order of values.
    /// If the array contains a Failure, then the entire array will not resolve
    let collectSequential (values : AsyncVal<'T>[]) : AsyncVal<'T[]> =
        if values.Length = 0 then Value [||]
        elif values |> Array.exists isAsync then
            Async (async {
                let results = Array.zeroCreate values.Length
                let exceptions = ResizeArray values.Length
                for i = 0 to values.Length - 1 do
                    let v = values.[i]
                    match v with
                    | Value v -> results.[i] <- v
                    | Async a ->
                        let! r = a
                        results.[i] <- r
                    | Failure f -> exceptions.Add f
                match exceptions.Count with
                | 0 -> return results
                | 1 -> return exceptions.First().Reraise ()
                | _ -> return AggregateException exceptions |> raise
            })
        else
            let exceptions =
                values
                |> Array.choose (function
                    | Failure f -> Some f
                    | _ -> None)
            match exceptions.Length with
            | 0 -> Value (values |> Array.map (fun (Value v) -> v))
            | 1 -> Failure (exceptions.First ())
            | _ -> Failure (AggregateException exceptions)

    /// Converts array of AsyncVals into AsyncVal with array results.
    /// In case when are non-immediate values in provided array, they are
    /// executed all in parallel, in unordered fashion. Order of values
    /// inside returned array is maintained.
    /// If the array contains a Failure, then the entire array will not resolve
    let collectParallel (values : AsyncVal<'T>[]) : AsyncVal<'T[]> =
        if values.Length = 0 then Value [||]
        else
            let indexes = List<_> (0)
            let continuations = List<_> (0)
            let results = Array.zeroCreate values.Length
            let exceptions = ResizeArray values.Length
            for i = 0 to values.Length - 1 do
                let value = values.[i]
                match value with
                | Value v -> results.[i] <- v
                | Async a ->
                    indexes.Add i
                    continuations.Add a
                | Failure f -> exceptions.Add f
            match exceptions.Count with
            | 1 -> AsyncVal.Failure (exceptions.First ())
            | count when count > 1 -> AsyncVal.Failure (AggregateException exceptions)
            | _ ->
                if indexes.Count = 0 then Value (results)
                else Async (async {
                    let! vals = continuations |> Async.Parallel
                    for i = 0 to indexes.Count - 1 do
                        results.[indexes.[i]] <- vals.[i]
                    return results
                })

    /// Converts array of AsyncVals of arrays into AsyncVal with array results
    /// by calling collectParallel and then appending the results.
    let appendParallel (values : AsyncVal<'T[]>[]) : AsyncVal<'T[]> =
        values
        |> collectParallel
        |> map (Array.fold Array.append Array.empty)

    /// Converts array of AsyncVals of arrays into AsyncVal with array results
    /// by calling collectSequential and then appending the results.
    let appendSequential (values : AsyncVal<'T[]>[]) : AsyncVal<'T[]> =
        values
        |> collectSequential
        |> map (Array.fold Array.append Array.empty)

type AsyncValBuilder () =
    member _.Zero () = AsyncVal.empty
    member _.Return v = AsyncVal.wrap v
    member _.ReturnFrom (v : AsyncVal<_>) = v
    member _.ReturnFrom (a : Async<_>) = AsyncVal.ofAsync a
    member _.Bind (v : AsyncVal<'T>, binder : 'T -> AsyncVal<'U>) = AsyncVal.bind binder v
    member _.Bind (a : Async<'T>, binder : 'T -> AsyncVal<'U>) =
        Async (async {
            let! value = a
            let bound = binder value
            match bound with
            | Value v -> return v
            | Async a -> return! a
            | Failure f -> return f.Reraise ()
        })


[<AutoOpen>]
module AsyncExtensions =

    /// Computation expression for working on AsyncVals.
    let asyncVal = AsyncValBuilder ()

    /// Active pattern used for checking if AsyncVal contains immediate value.
    let (|Immediate|_|) (x : AsyncVal<'T>) = match x with | Value v -> Some v | _ -> None

    /// Active patter used for checking if AsyncVal wraps an Async computation.
    let (|Async|_|) (x : AsyncVal<'T>) = match x with | Async a -> Some a | _ -> None

    type Microsoft.FSharp.Control.AsyncBuilder with

        member _.ReturnFrom (v : AsyncVal<'T>) =
            match v with
            | Value v -> async.Return v
            | Async a -> async.ReturnFrom a
            | Failure f -> async.Return (raise f)

        member _.Bind (v : AsyncVal<'T>, binder) =
            match v with
            | Value v -> async.Bind (async.Return v, binder)
            | Async a -> async.Bind (a, binder)
            | Failure f -> async.Bind (async.Return (raise f), binder)
