// Copyright 2018 Fabulous contributors. See LICENSE.md for license.
namespace StarWars.iOS

open System
open UIKit
open Foundation
open Xamarin.Forms
open Xamarin.Forms.Platform.iOS
open StarWars

[<Register ("AppDelegate")>]
type AppDelegate () =
    inherit FormsApplicationDelegate ()

    override this.FinishedLaunching (app, options) =
        Forms.Init()
        let graphqlInfo =
                 { new IGraphQLInfo with
                    member x.GetSchemeUrl () = "http://127.0.0.1:8084" }
        let appcore = new App(graphqlInfo)
        this.LoadApplication (appcore)
        base.FinishedLaunching(app, options)

module Main =
    [<EntryPoint>]
    let main args =
        UIApplication.Main(args, null, "AppDelegate")
        0

