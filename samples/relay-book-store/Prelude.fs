namespace FSharp.Data.GraphQL.Samples.RelayBookStore

[<AutoOpen>]
module internal Prelude =

    [<RequireQualifiedAccess>]
    module Base64 =

        open System

        let encode (x : byte array) : string = Convert.ToBase64String (x)

        let tryDecode (x : string) : byte array option =
            try
                Convert.FromBase64String (x) |> Some
            with :? FormatException ->
                None

    [<RequireQualifiedAccess>]
    module Utf8 =

        open System
        open System.Text

        let tryDecode (xs : byte array) : string option =
            try
                Encoding.UTF8.GetString (xs) |> Some
            with
            | :? ArgumentException
            | :? DecoderFallbackException -> None

        let encode (x : string) : byte array = Encoding.UTF8.GetBytes (x)

    [<RequireQualifiedAccess>]
    module Async =

        open System.Threading
        open System.Threading.Tasks

        /// Takes an async workflow and returns a new workflow that only executes once
        /// Subsequent and concurrent executions will reuse the result from the first execution
        let memoize (workflow : Async<'t>) : Async<'t> =
            let mutable count = 0
            let tcs = TaskCompletionSource<_> ()
            async {
                if Interlocked.Increment (&count) = 1 then
                    try
                        let! t = workflow
                        tcs.SetResult (t)
                    with exn ->
                        tcs.SetException (exn)

                return! Async.AwaitTask tcs.Task
            }
