[<AutoOpen>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module System.Exception

open System
open System.Diagnostics
open System.Runtime.ExceptionServices

// Useful for reraising exceptions under an async {...} and task {...} contexts
// See this for more details: https://github.com/fsharp/fslang-suggestions/issues/660
type internal Exception with

    [<DebuggerHidden>]
    member __.Reraise () =
        (ExceptionDispatchInfo.Capture __).Throw ()
        Unchecked.defaultof<_>
