namespace FSharp.Data.GraphQL

/// Contains tracing operations to use when DEBUG constant is defined.
module Tracer =
    let private print (debugMessage : string) (elapsed : int64) =
        if elapsed > 0L then
            printfn "GraphQLProvider debug: %s (time to execute: %ims)" debugMessage elapsed

    /// Executes fn and, if DEBUG constant is defined, measures its execution time.
    /// Prints a debug message followed by the execution time in milliseconds.
    /// After that, returns fn result.
    /// If DEBUG constant is not defined, only executes fn and return its result.
    let runAndMeasureExecutionTime (debugMessage : string) (fn : unit -> 'T) =
        #if DEBUG
        let sw = System.Diagnostics.Stopwatch()
        sw.Start()
        #endif
        let result = fn ()
        #if DEBUG
        sw.Stop()
        print debugMessage sw.ElapsedMilliseconds
        #endif
        result

    /// Executes fn asynchronously and, if DEBUG constant is defined, measures its execution time.
    /// Prints a debug message followed by the execution time in milliseconds.
    /// After that, returns fn result.
    /// If DEBUG constant is not defined, only executes fn and return its result.
    let asyncRunAndMeasureExecutionTime (debugMessage : string) (fn : unit -> Async<'T>) =
        #if DEBUG
        async {
            let sw = System.Diagnostics.Stopwatch()
            sw.Start()
            let! result = fn ()
            sw.Stop()
            print debugMessage sw.ElapsedMilliseconds
            return result
        }
        #else
        fn ()
        #endif