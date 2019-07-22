// Copyright 2018 Fabulous contributors. See LICENSE.md for license.
namespace StarWars

open Fabulous.Core
open Fabulous.DynamicViews
open Xamarin.Forms
open System
open Commands

module App =
    type Msg =
        | MainPageMsg of MainPage.Msg
        | CharacterDetailPageMsg of CharacterDetailPage.Msg
        | GoToDetail of Character
        | NavigationPopped

    type Model = 
        { MainPageModel: MainPage.Model
          CharacterDetailPageModel: CharacterDetailPage.Model option }

    let init ctx () = 
        let mainModel, mainMsg, _ = MainPage.init ctx
        let initModel =
            { MainPageModel = mainModel
              CharacterDetailPageModel = None }
        initModel, (Cmd.map MainPageMsg mainMsg)

    let handleMainExternalMsg externalMsg model =
        match externalMsg with
        | MainPage.ExternalMsg.NoOp ->
            Cmd.none
        | MainPage.ExternalMsg.NavigateToCharacterDetail character ->
            Cmd.ofMsg (GoToDetail character)

    let handleCharacterDetailPageMsg externalMsg model =
        Cmd.none

    let update msg model =
        match msg with
        | MainPageMsg msg ->
            let m, cmd, externalMsg = MainPage.update msg model.MainPageModel
            let cmd2 = handleMainExternalMsg externalMsg model
            let selfCmd = Cmd.batch [(Cmd.map MainPageMsg cmd); cmd2 ]
            { model with MainPageModel = m }, selfCmd
        | CharacterDetailPageMsg msg ->
            match model.CharacterDetailPageModel with
            | Some pageModel -> 
                let m, cmd, externalMsg = CharacterDetailPage.update msg pageModel
                let cmd2 = handleCharacterDetailPageMsg externalMsg model
                let selfCmd = Cmd.batch [ (Cmd.map CharacterDetailPageMsg cmd); cmd2 ]
                { model with CharacterDetailPageModel = Some m }, selfCmd
            | _ ->
                model, Cmd.none
        | GoToDetail character ->
            let m, cmd = CharacterDetailPage.init character
            let selfCmd = (Cmd.map CharacterDetailPageMsg cmd)
            { model with CharacterDetailPageModel = Some m }, selfCmd
        | NavigationPopped ->
            match model.CharacterDetailPageModel with
            | Some _ ->
                { model with CharacterDetailPageModel = None}, Cmd.none
            | _ ->
                model, Cmd.none
                
    let view (model: Model) dispatch =
        let mainPage = MainPage.view model.MainPageModel (MainPageMsg >> dispatch)

        let detailPage =
            model.CharacterDetailPageModel
            |> 
            Option.map (fun dModel -> CharacterDetailPage.view dModel (CharacterDetailPageMsg >> dispatch))

        View.NavigationPage(
            barTextColor = Style.accentTextColor,
            barBackgroundColor = Style.accentColor,
            popped = (fun x -> dispatch NavigationPopped),
            pages =
                match detailPage with
                | None ->
                    [ mainPage ]
                | Some detail ->
                    [ mainPage; detail ]
        )

    let program ctx =
        Program.mkProgram (init ctx) update view

type App (ctx : IGraphQLInfo) as app = 
    inherit Application ()

    let runner =
        GraphQLApi.GetContext(serverUrl=(ctx.GetSchemeUrl()))
        |> (fun runtimeCtx -> App.program runtimeCtx)
        |> Program.runWithDynamicView app
