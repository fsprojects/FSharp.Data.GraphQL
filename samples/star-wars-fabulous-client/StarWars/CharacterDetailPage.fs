namespace StarWars

open System
open System.Net
open System.Diagnostics
open FSharp.Data
open FSharp.Data.GraphQL
open Xamarin.Forms
open Fabulous.Core
open Fabulous.DynamicViews
open Commands

module CharacterDetailPage =

    type Msg =
        | Error of message : string

    type ExternalMsg =
        | NoOp

    type Model =
        { Character: Character
          ErrorMessage: string option }

    let update msg (model: Model) =
        match msg with
        | Error message ->
            let m = { model with ErrorMessage = Some message }
            m, Cmd.none, ExternalMsg.NoOp

    let init (character:Character) = { Character = character; ErrorMessage = None}, Cmd.none

    let getValue optValue =
        Option.defaultValue "Unknown" <| optValue

    let getCharacterName (character:Character) =
        match character.IsHuman() with
        | true ->
            getValue (character.AsHuman().Name)
        | _ ->
            getValue (character.AsDroid().Name)

    let getCharacterHomePlanetOrPrimaryFunction (character:Character) =
        match character.IsHuman() with
        | true ->
            let homePlanet = getValue (character.AsHuman().HomePlanet)
            sprintf "Home Planet: %s" homePlanet
        | _ ->
            let fn = getValue (character.AsDroid().PrimaryFunction)
            sprintf "Primary Function: %s" fn

    let getCharacterAppearsIn (character:Character) =
        match character.IsHuman() with
        | true ->
            character.AsHuman().AppearsIn
            |> Array.map (fun x -> x.ToString())
        | _ ->
            character.AsDroid().AppearsIn
            |> Array.map (fun x -> x.ToString())

    let mkLabelChildren (lst:string[]) =
        let mkLabel =
            (fun value -> View.HighlightedLabel(text = value,
                                                textColor = Color.White,
                                                backgroundColor = Color.SafetyOrange))
        lst
        |> Array.map mkLabel
        |> Array.toList

    let mkChildren model =
        [ View.Label(text = sprintf "Nome: %s" (getCharacterName model.Character),
                     horizontalOptions = LayoutOptions.Center,
                     heightRequest = 30.0,
                     horizontalTextAlignment = TextAlignment.Center)
          View.Label(text = (getCharacterHomePlanetOrPrimaryFunction model.Character),
                     horizontalOptions = LayoutOptions.Center,
                     heightRequest = 30.0,
                     horizontalTextAlignment = TextAlignment.Center)
          View.Label(text = "Appears In:",
                     horizontalOptions = LayoutOptions.Center,
                     heightRequest = 100.0,
                     horizontalTextAlignment = TextAlignment.Center)
          View.StackLayout(padding = Thickness(20., 5., 20., 0.),
                           children = mkLabelChildren (getCharacterAppearsIn model.Character)) ]

    let view model dispatch =
        dependsOn model (fun model mModel ->
            View.ContentPage(
                title = getCharacterName mModel.Character,
                content = View.StackLayout(
                    padding = Thickness(20.),
                    children = mkChildren mModel
                )
            )
        )

    