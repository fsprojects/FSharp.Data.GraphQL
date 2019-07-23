namespace StarWars

open Fabulous.Core
open System.Collections.Generic
open System.Threading

module Cmd =
    let ofAsyncMsgOption (p: Async<'msg option>) : Cmd<'msg> =
        [ fun dispatch -> async { let! msg = p in match msg with None -> () | Some msg -> dispatch msg } |> Async.StartImmediate ]

module Extensions =
    let debounce<'T> =
        let memoizations = Dictionary<obj, CancellationTokenSource>(HashIdentity.Structural)
        fun (timeout: int) (fn: 'T -> unit) value ->
            let key = fn.GetType()
            match memoizations.TryGetValue(key) with
            | true, cts -> cts.Cancel()
            | _ -> ()
            let cts = new CancellationTokenSource()
            memoizations.[key] <- cts
            (async {
                try
                    do! Async.Sleep timeout
                    memoizations.Remove(key) |> ignore
                    fn value
                with
                | _ -> ()
            })
            |> (fun task -> Async.StartImmediate(task, cts.Token))
    let debounce250<'T> = debounce<'T> 250

[<AutoOpen>]
module ColorExt =
    open Xamarin.Forms

    type ColorPair =
        { ForegroundColor : Color
          BackgroundColor : Color}

    type Color with
        static member BlueZodiac =
            Color.FromHex("#3E4359")

        static member ParisDaisy =
            Color.FromHex("#FCE84C")

        static member Manatee =
            Color.FromHex("#858CA6")

        static member BlueViolet =
            Color.FromHex("#B351E8")

        static member MountainMeadow =
            Color.FromHex("#25E6B6")

        static member SafetyOrange =
            Color.FromHex("#F56302")
